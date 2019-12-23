#include <fstream>
#include <iostream>
#include <list>
#include <map>
#include <set>
#include <string>
#include <vector>

auto load_program(std::ifstream & s) -> std::vector<std::int64_t>
{
  std::vector<std::int64_t> values;
  std::string str;
  while (getline(s, str, ',')) {
    values.push_back(stoll(str));
  }
  return values;
}

auto num_params(std::int64_t opcode) -> std::size_t
{
  switch(opcode)
  {
    case 1: return 3;
    case 2: return 3;
    case 3: return 1;
    case 4: return 1;
    case 5: return 2;
    case 6: return 2;
    case 7: return 3;
    case 8: return 3;
    case 9: return 1;
    case 99: return 0;
    default:
      std::cerr << "Bad opcode " << opcode << std::endl;
      abort();
  }
}

void dump(std::vector<std::int64_t> const & memory)
{
  std::for_each(memory.begin(), memory.end(), [](std::int64_t val){
    std::cout << val << std::endl;
  });
}

class input_required
{};

class intcode_computer
{
public:
  explicit intcode_computer(std::vector<std::int64_t> const & program)
  : memory{program}, pc{0}, base{0}
  {
  }

  void input(std::int64_t i)
  {
    inputs.push_back(i);
  }

  auto available() const -> std::size_t
  {
    return outputs.size();
  }

  std::int64_t output()
  {
    if(outputs.empty())
    {
      throw std::string("No outputs available");
    }
    auto ret = outputs.front();
    outputs.pop_front();
    return ret;
  }

  void run()
  {
    while(true)
    {
      //std::cout << "Decoding instruction from offset " << pc << std::endl;
      auto op = memory.at(pc) % 100;
      auto mode = memory.at(pc) / 100;


      auto params = num_params(op);

      std::vector<std::int64_t*> datas(params);

      for(std::int64_t i = 0; i < params; ++i)
      {
        auto m = mode % 10;
        mode /= 10;

        auto ref = memory.at(pc+1+i);
        switch(m)
        {
          case 0:
            // Position mode
            if(ref >= memory.size())
              memory.resize(ref+1, 0);
            datas.at(i) = &memory.at(ref);
            //std::cout << "Operand " << i << " from position mode at memory location " << ref << std::endl;
            break;
          case 1:
            // Immediate mode
            datas.at(i) = &memory.at(pc+1+i);
            //std::cout << "Operand " << i << " from immediate at memory location " << pc+1+i << std::endl;
            break;
          case 2:
            // Relative mode
            if(base + ref >= memory.size())
              memory.resize(base+ref+1, 0);
            datas.at(i) = &memory.at(base + ref);
            //std::cout << "Operand " << i << " from relative at memory location " << base + ref << std::endl;
            break;
          default:
            std::cerr << "Bad access mode at memory location " << pc << ":" << memory.at(pc) << std::endl;
            abort();
        }
      }

      //std::cout << "Operating on opcode " << op << " from offset " << pc << std::endl;

      switch(op)
      {
        case 1:
          // Add together next two referenced numbers and store at third reference
          *datas.at(2) = *datas.at(0) + *datas.at(1);
          pc += 4;
          break;
        case 2:
          // Multiply together next two referenced numbers and store at third reference
          *datas.at(2) = *datas.at(0) * *datas.at(1);
          pc += 4;
          break;
        case 3:
          // Save an input to the first reference
          if(inputs.empty())
          {
            throw input_required{};
          }
          *datas.at(0) = inputs.front();
          inputs.pop_front();
          pc += 2;
          break;
        case 4:
          // Output data at first reference
          outputs.push_back(*datas.at(0));
          pc += 2;
          break;
        case 5:
          // Jump if true
          if(*datas.at(0))
          {
            pc = *datas.at(1);
          }
          else
            pc += 3;
          break;
        case 6:
          // Jump if false
          if(!*datas.at(0))
            pc = *datas.at(1);
          else
            pc += 3;
          break;
        case 7:
          // Less than comparison
          if(*datas.at(0) < *datas.at(1))
            *datas.at(2) = 1;
          else
            *datas.at(2) = 0;
          pc += 4;
          break;
        case 8:
          // Equals than comparison
          if(*datas.at(0) == *datas.at(1))
            *datas.at(2) = 1;
          else
            *datas.at(2) = 0;
          pc += 4;
          break;
        case 9:
          // Adjust parameter by parameter
          base += *datas.at(0);
          pc += 2;
          break;
        case 99:
          return;
        default:
          std::cerr << "Bad opcode at memory location " << pc << ":" << op << std::endl;
          abort();
      }
    }
  }

private:
  std::vector<std::int64_t> memory;
  std::int64_t pc;
  std::int64_t base;

  std::list<std::int64_t> inputs, outputs;
};

using location = std::pair<int64_t, int64_t>;

auto in_beam(std::vector<std::int64_t> const & program, int64_t x, int64_t y) -> bool
{
  intcode_computer machine{program};

  machine.input(x);
  machine.input(y);

  try{machine.run();}
  catch(input_required i){}

  auto res = machine.output();

  switch(res)
  {
    case 0: return false;
    case 1: return true;
    default: std::cout << "Nonsense" << std::endl; abort();
  }

}

auto corner_100_by_100(std::vector<std::int64_t> const & program, int64_t x, int64_t y) -> bool
{
  return in_beam(program, x, y) && in_beam(program, x+99, y) &&
         in_beam(program, x, y+99) && in_beam(program, x+99, y+99);
}

using packet = std::pair<std::int64_t, std::int64_t>;

using packet_queue = std::list<packet>;

int main(int argc, char *argv[])
{
  if(argc != 2)
  {
    std::cerr << "need program" << std::endl;
    return 0;
  }
  std::ifstream progfile(argv[1]);
  const std::vector<std::int64_t> program = load_program(progfile);

  std::vector<intcode_computer> network{};
  packet_queue nat, previous;
  bool active = true;

  while(network.size() < 50)
  {
    auto address = network.size();
    network.emplace_back(intcode_computer{program});
    network.back().input(address);
  }

  std::size_t idx = 0;

  while(true)
  {
    std::size_t idle = 0;
    std::for_each(network.begin(), network.end(), [&network, &nat, &idle](auto & machine)
    {
      try{machine.run();}
      catch(input_required i)
      {
        machine.input(-1);

        if(machine.available() == 0)
        {
          ++idle;
        }
      }

      // Send any output packets
      while(machine.available())
      {
        auto dst_addr = machine.output();
        packet p{machine.output(), machine.output()};

        if(dst_addr < network.size())
        {
          auto & destination = network.at(dst_addr);
          destination.input(p.first);
          destination.input(p.second);
        }

        if(dst_addr == 255)
        {
          if(nat.empty())
          {
            std::cout << "First packet to 255 has y value " << p.second << std::endl;
          }
          nat.push_back(p);
        }
      }
    });

    if(idle == network.size())
    {
      if(!nat.empty())
      {
        // Send most recent NAT packet to machine 0
        auto p = nat.back();
        network.front().input(p.first);
        network.front().input(p.second);

        if(!previous.empty() && (previous.back().second == p.second))
        {
          std::cout << "First repeated y NAT value " << p.second << std::endl;
          return 0;
        }

        previous.push_back(p);
      }
    }
  }

  return 0;
}
