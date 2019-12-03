#include <iostream>
#include <string>
#include <vector>

auto load_program() -> std::vector<int>
{
  std::vector<int> values;
  std::string str;
  while (getline(std::cin, str, ',')) {
    values.push_back(stoi(str));
  }
  return values;
}

auto run(std::vector<int> const & program, int verb, int noun) -> int
{
  auto memory = program;
  memory[1] = verb;
  memory[2] = noun;

  std::size_t pc = 0;

  while(true)
  {
    auto op = memory.at(pc);
    int ref0, ref1, ref2;

    if(op <= 2)
    {
      ref0 = memory.at(pc+1);
      ref1 = memory.at(pc+2);
      ref2 = memory.at(pc+3);
    }

    switch(op)
    {
      case 1:
        // Add together next two referenced numbers and store at third reference
        memory.at(ref2) = memory.at(ref0) + memory.at(ref1);
        break;
      case 2:
        // Multiply together next two referenced numbers and store at third reference
        memory.at(ref2) = memory.at(ref0) * memory.at(ref1);
        break;
      case 99:
        return memory.at(0);
      default:
        std::cerr << "Bad opcode at " << pc << ":" << op << std::endl;
        abort();
    }
    pc += 4;
  }
}

void dump(std::vector<int> const & memory)
{
  std::for_each(memory.begin(), memory.end(), [](int val){
    std::cout << val << std::endl;
  });
}

int main()
{
  const std::vector<int> program = load_program();
  std::size_t pc = 0;

  std::cout << "Program size " << program.size() << std::endl;

  for(int verb = 0; verb < 100; ++verb)
  {
    for(int noun = 0; noun < 100; ++noun)
    {
      auto res = run(program, verb, noun);
      if(res == 19690720)
      {
        std::cout << verb << noun << std::endl;
      }
    }
  }


}
