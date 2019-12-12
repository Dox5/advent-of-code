#include <fstream>
#include <iostream>
#include <list>
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

  auto available() const -> bool
  {
    return !outputs.empty();
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
  position(int x, int y)
  : m_x{x}, m_y{y}
  {}

  auto x() const -> int {return m_x;}
  auto y() const -> int {return m_y;}

  auto operator<(position const & rhs) const -> bool
  {
    if(m_y == rhs.m_y)
      return m_x < rhs.m_x;
    return m_y < rhs.m_y;
  }
private:
  int m_x, m_y;
};

class robot
{
public:
  robot(int x, int y)
  : m_x{x}, m_y{y}, m_direction{0}
  {}

  void rotate(int right)
  {
    if(right == 0)
      m_direction = (m_direction + 3) % 4;
    else
      m_direction = (m_direction + 1) % 4;
  }

  void move()
  {
    switch(m_direction)
    {
      case 0: ++m_y; return;
      case 1: ++m_x; return;
      case 2: --m_y; return;
      case 3: --m_x; return;
      default:
        std::cerr << "Robot's direction makes no sense" << std::endl;
        abort();
    }
  }

  auto x() const -> int {return m_x;}
  auto y() const -> int {return m_y;}
  auto pos() const -> position {return position{m_x, m_y};}

private:
  int m_x, m_y;
  int m_direction;
};

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

  robot robbie{0, 0};

  bool done = false;

  std::set<position> reached;
  std::set<position> white;

  reached.insert(robbie.pos());
  // Starting panel is white
  white.insert(robbie.pos());

  while(!done)
  {
    // Give a one if the current space is white, otherwise zero
    machine.input(white.count(robbie.pos()));

    try
    {
      machine.run();
      done = true;
    }
    catch(input_required x)
    {
      // There should be two outputs
      auto colour = machine.output();
      auto rotate = machine.output();
      if(colour)
      {
        white.insert(robbie.pos());
      }
      else
      {
        white.erase(robbie.pos());
      }

      robbie.rotate(rotate);
      robbie.move();

      reached.insert(robbie.pos());
    }
  }

  // Need to normalise coordinates
  int min_x = 0;
  int min_y = 0;
  int max_x = 0;
  int max_y = 0;

  std::for_each(white.begin(), white.end(), [&min_x, &min_y, &max_x, &max_y](auto pos){
    auto x = pos.x();
    auto y = pos.y();
    if(x < min_x) min_x = x;
    if(x > max_x) max_x = x;
    if(y < min_y) min_y = y;
    if(y > max_y) max_y = y;
  });

  auto diff_x = 1 + max_x - min_x;
  auto diff_y = 1 + max_y - min_y;

  std::cout << "Image is " << diff_x << " by " << diff_y << std::endl;

  std::string blackline;
  while(blackline.size() < diff_x)
  {
    blackline.push_back(' ');
  }

  std::vector<std::string> image(diff_y, blackline);

  std::for_each(white.begin(), white.end(), [min_x, min_y, &image](auto pos){
    auto x = pos.x() - min_x;
    auto y = pos.y() - min_y;
    std::cout << "Painting " << x << "," << y << " white" << std::endl;
    image.at(y).at(x) = '#';
  });

  std::for_each(image.rbegin(), image.rend(), [](auto line){
    std::cout << line << std::endl;
  });

  //std::cout << white.begin()->x() << "," << white.begin()->y() << std::endl;

  return 0;
}
