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

auto run(std::vector<int> const & program) -> int
{
  auto memory = program;

  std::size_t pc = 0;

  while(true)
  {
    std::cerr << "Working on operation from offset " << pc << std::endl;
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
        std::cerr << "Add " << data[0] << " by " << data[1] << " to " << refs[2] << std::endl;
        // Add together next two referenced numbers and store at third reference
        memory.at(refs[2]) = data[0] + data[1];
        instruction_size = 4;
        break;
      case 2:
        std::cerr << "Mul " << data[0] << " by " << data[1] << " to " << refs[2] << std::endl;
        // Multiply together next two referenced numbers and store at third reference
        memory.at(refs[2]) = data[0] * data[1];
        instruction_size = 4;
        break;
      case 3:
        std::cerr << "Input at save to " << refs[0] << std::endl;
        // Input gets saved to first reference
        std::cin >> memory.at(refs[0]);
        instruction_size = 2;
        break;
      case 4:
        // Output data at first reference
        std::cerr << "Output " << data[0] << std::endl;
        std::cout << "Result = " << data[0] << std::endl;
        instruction_size = 2;
        break;
      case 5:
        // Jump if true
        std::cerr << "Jump if " << data[0] << std::endl;
        if(data[0])
        {
          pc = data[1];
          instruction_size = 0;
        }
        else
          instruction_size = 3;
        break;
      case 6:
        // Jump if false
        std::cerr << "Jump if not " << data[0] << " "<< std::endl;
        if(!data[0])
        {
          pc = data[1];
          instruction_size = 0;
        }
        else
          instruction_size = 3;
        break;
      case 7:
        // Less than comparison
        std::cerr << "Compare " << data[0] << "<" << data[1] << " to " << refs[2] << std::endl;
        if(data[0] < data[1])
          memory.at(refs[2]) = 1;
        else
          memory.at(refs[2]) = 0;
        instruction_size = 4;
        break;
      case 8:
        // Equals than comparison
        std::cerr << "Compare " << data[0] << "=" << data[1] << " to " << refs[2] << std::endl;
        if(data[0] == data[1])
          memory.at(refs[2]) = 1;
        else
          memory.at(refs[2]) = 0;
        instruction_size = 4;
        break;
      case 99:
        return memory.at(0);
      default:
        std::cerr << "Bad opcode at memory location " << pc << ":" << op << std::endl;
        abort();
    }
    pc += instruction_size;
  }
}

void dump(std::vector<int> const & memory)
{
  std::for_each(memory.begin(), memory.end(), [](int val){
    std::cout << val << std::endl;
  });
}

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

  std::cout << "Program size " << program.size() << std::endl;

  run(program);
}
