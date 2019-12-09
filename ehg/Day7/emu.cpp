#include <fstream>
#include <iostream>
#include <string>
#include <vector>

auto load_program(std::ifstream & s) -> std::vector<int>
{
  std::vector<int> values;
  std::string str;
  while (getline(s, str, ',')) {
    values.push_back(stoi(str));
  }
  return values;
}

auto num_params(int opcode) -> std::size_t
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
    case 99: return 0;
    default:
      std::cerr << "Bad opcode " << opcode << std::endl;
      abort();
  }
}

void dump(std::vector<int> const & memory)
{
  std::for_each(memory.begin(), memory.end(), [](int val){
    std::cout << val << std::endl;
  });
}

class amplifier
{
public:
  explicit amplifier(std::vector<int> const & program, int phase)
  : memory{program}, pc{0}
  {
    // Run, to first input
    try{run(phase);}
    catch(std::string) {};
  }

  int run(int & power)
  {
    bool got = false;

    while(true)
    {
      //std::cerr << "Working on operation from offset " << pc << std::endl;
      auto op = memory.at(pc) % 100;
      auto mode = memory.at(pc) / 100;

      int refs[3];
      int data[3];
      std::size_t instruction_size;

      auto params = num_params(op);

      for(int i = 0; i < params; ++i)
      {
        auto m = mode % 10;
        mode /= 10;

        refs[i] = memory.at(pc+1+i);
        if(m)
          data[i] = refs[i];
        else
          data[i] = memory.at(refs[i]);
      }

      switch(op)
      {
        case 1:
          // Add together next two referenced numbers and store at third reference
          memory.at(refs[2]) = data[0] + data[1];
          pc += 4;
          break;
        case 2:
          // Multiply together next two referenced numbers and store at third reference
          memory.at(refs[2]) = data[0] * data[1];
          pc += 4;
          break;
        case 3:
          // Input gets saved to first reference
          if(!got)
          {
            memory.at(refs[0]) = power;
            got = true;
          }
          else
          {
            throw(std::string("Multiple inputs per run"));
          }
          pc += 2;
          break;
        case 4:
          // Output data at first reference
          pc += 2;
          power = data[0];
          return power;
        case 5:
          // Jump if true
          if(data[0])
          {
            pc = data[1];
          }
          else
            pc += 3;
          break;
        case 6:
          // Jump if false
          if(!data[0])
          {
            pc = data[1];
          }
          else
            pc += 3;
          break;
        case 7:
          // Less than comparison
          if(data[0] < data[1])
            memory.at(refs[2]) = 1;
          else
            memory.at(refs[2]) = 0;
          pc += 4;
          break;
        case 8:
          // Equals than comparison
          //std::cerr << "Compare " << data[0] << "=" << data[1] << " to " << refs[2] << std::endl;
          if(data[0] == data[1])
            memory.at(refs[2]) = 1;
          else
            memory.at(refs[2]) = 0;
          pc += 4;
          break;
        case 99:
          //std::cout << "Program stopped" << std::endl;
          throw(42);
        default:
          std::cerr << "Bad opcode at memory location " << pc << ":" << op << std::endl;
          abort();
      }
    }
  }

private:
  std::vector<int> memory;
  std::size_t pc;
};

int main(int argc, char *argv[])
{
  if(argc != 2)
  {
    std::cerr << "need program" << std::endl;
    return 0;
  }
  std::ifstream progfile(argv[1]);
  const std::vector<int> program = load_program(progfile);
  std::size_t pc = 0;

  std::vector<int> phases(5);

  int best = 0;

  for(int i = 0; i < 3125; ++i)
  {
    // Populate phases
    int current = i;
    int power = 0;

    std::bitset<5> used;
    bool good = true;
    std::for_each(phases.begin(), phases.end(), [&current, &used, &good](auto &ph){
      ph = current % 5 + 5;
      current /= 5;
      std::cout << ph;

      if(used[ph-5])
      {
        good = false;
      }
      used[ph-5] = true;
    });

    if(good)
    {
      std::vector<amplifier> amps;

      for(int i = 0; i < 5; ++i)
      {
        amps.emplace_back(program, phases[i]);
      }

      int power = 0;
      int idx = 0;

      while(true)
      {
        try{
          amps.at(idx).run(power);
          idx = (idx + 1) % 5;
        }
        catch(int)
        {
          break;
        }
      }

      std::cout << ":" << power << std::endl;

      if(power > best)
      {
        best = power;
      }
    }
    else
    {
      std::cout << "!" << std::endl;
    }
  }

std::cout << "Done" << std::endl;

  std::cout << best << std::endl;

  /*

  for(int i = 0; i < 3125; ++i)
  {
    // Populate phases
    int current = i;
    int power = 0;

    std::bitset<5> used;
    bool good = true;
    std::for_each(phases.begin(), phases.end(), [&current, &used, &good](auto &ph){
      ph = current % 5 + 5;
      current /= 5;
      std::cout << ph;

      if(used[ph-5])
      {
        good = false;
      }
      used[ph-5] = true;
    });

    if(good)
    {
      std::cout << ":";
      // Make the 5 amplifiers
      std::vector<amplifier> amps;

      std::for_each(phases.begin(), phases.end(), [&program, &amps](auto ph){
        amps.emplace_back(program, ph);
      });

      // Keep running until it throws
      int power = 0;
      int idx = 0;

      while(true)
      {
        try{
          power = amps.at(idx).run(power);
          std::cout << power << ",";
          idx = (idx + 1) % 5;
        }
        catch(int a)
        {
          if(idx == 0)
            break;
        }

        if(idx == 0)
        {
          if(power > best)
          {
            best = power;
          }
        }

      }

      std::cout << std::endl;

    }
    else
      std::cout << std::endl;

  }

  std::cout << best << std::endl;*/
}
