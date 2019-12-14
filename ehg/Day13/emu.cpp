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

  // Add two quarters
  values.at(0) = 2;
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

class position
{
public:
  position(int64_t x, int64_t y)
  : m_x{x}, m_y{y}
  {}

  auto x() const -> int64_t {return m_x;}
  auto y() const -> int64_t {return m_y;}

  auto operator<(position const & rhs) const -> bool
  {
    if(m_y == rhs.m_y)
      return m_x < rhs.m_x;
    return m_y < rhs.m_y;
  }
private:
  int64_t m_x, m_y;
};

int64_t update_screen(std::vector<std::string> & screen, intcode_computer & machine, int64_t & bat_x, int64_t & ball_x)
{
  int64_t score = 0;
  while(machine.available() > 0)
  {
    auto x = machine.output();
    auto y = machine.output();
    auto id = machine.output();

    if(x == -1)
    {
      if(y != 0)
      {
        std::cerr << "Bad score sequence" << std::endl;
        abort();
      }
      std::cout << "Got score " << id << std::endl; 
      score = id;
    }
    else
    {
      if(y >= screen.size())
      {
        screen.resize(y+1);
      }

      auto & line = screen.at(y);

      while(x >= line.size())
      {
        line.push_back(' ');
      }
      switch(id)
      {
        case 0: line.at(x) = ' '; break;
        case 1: line.at(x) = '#'; break;
        case 2: line.at(x) = '.'; break;
        case 3: line.at(x) = '-'; bat_x = x; break;
        case 4: line.at(x) = 'o'; ball_x = x; break;
        default: break;
      }
    }
  }

  return score;
}

void draw_screen(std::vector<std::string> const & screen)
{
  std::for_each(screen.begin(), screen.end(), [](auto line){
    std::cout << line << std::endl;
  });
}
auto count_blocks(std::vector<std::string> const & screen) -> std::size_t
{
  std::size_t blocks = 0;
  std::for_each(screen.begin(), screen.end(), [&blocks](auto line){
    std::for_each(line.begin(), line.end(), [&blocks](auto c){
      if(c == '.')
        ++blocks;
    });
  });
  return blocks;
}

int main(int argc, char *argv[])
{
  if(argc != 2)
  {
    std::cerr << "need program" << std::endl;
    return 0;
  }
  std::ifstream progfile(argv[1]);
  const std::vector<std::int64_t> program = load_program(progfile);

  intcode_computer machine(program);

  std::vector<std::string> screen;

  int64_t bat_x, ball_x;

  int64_t score;

  while(true)
  {
    try{
      machine.run();
      break;
    }
    catch(input_required r)
    {
    }

    std::cout << "updating screen " << machine.available() << std::endl;

    score = update_screen(screen, machine, bat_x, ball_x);

    auto blocks = count_blocks(screen);

    std::cout << "Blocks = " << blocks << std::endl;
    std::cout << "Score = " << score << std::endl;

    draw_screen(screen);

    // Keep bat under ball
    if(bat_x < ball_x)
      machine.input(1);
    else if(bat_x > ball_x)
      machine.input(-1);
    else
      machine.input(0);
  }

  update_screen(screen, machine, bat_x, ball_x);

  auto blocks = count_blocks(screen);

  std::cout << "Blocks = " << blocks << std::endl;
  std::cout << "Score = " << score << std::endl;


  return 0;
}
