#include <chrono>
#include <fstream>
#include <iostream>
#include <list>
#include <map>
#include <set>
#include <string>
#include <thread>
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

using location = std::pair<std::int64_t, std::int64_t>;

auto next_location(location current, std::int64_t direction) -> location
{
  location next{current};
  switch(direction)
  {
    case 0: ++next.second; break;
    case 1: ++next.first; break;
    case 2: --next.second; break;
    case 3: --next.first; break;
    default:
      std::cout << "Bad direction" << std::endl;
      abort();
  }
  return next;
}

void draw_map(std::map<location, bool> const & explored, location droid)
{
  // Need to know min X and min Y
  int64_t min_x = 0;
  int64_t min_y = 0;
  int64_t max_x = 0;
  int64_t max_y = 0;
  std::for_each(explored.begin(), explored.end(), [&min_x, &min_y, &max_x, &max_y](auto info){
    int64_t x = info.first.first;
    int64_t y = info.first.second;
    if(x < min_x) min_x = x;
    if(x > max_x) max_x = x;
    if(y < min_y) min_y = y;
    if(y > max_y) max_y = y;
  });

  auto size_x = max_x + 1 - min_x;
  auto size_y = max_y + 1 - min_y;

  std::vector<std::string> map;
  std::string line;
  while(line.size() < size_x) line.push_back(' ');
  while(map.size() < size_y) map.push_back(line);

  std::for_each(explored.begin(), explored.end(), [min_x, min_y, &map](auto info){
    int64_t x = info.first.first - min_x;
    int64_t y = info.first.second - min_y;

    auto & map_pos = map.at(y).at(x);

    if(info.second)
      map_pos = '#';
    else
      map_pos = '.';
  });

  auto & droid_pos = map.at(droid.second - min_y).at(droid.first - min_x);
  droid_pos = 'D';

  std::for_each(map.rbegin(), map.rend(), [](auto line){
    std::cout << line << std::endl;
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
  const std::vector<std::int64_t> program = load_program(progfile);

  intcode_computer machine(program);

  std::map<location, bool> explored;
  location current{0, 0};
  explored[current] = false;


  // We know there's a wall above us
  explored[next_location(current, 0)] = true;
  // So start going east
  std::int64_t direction = 1; // Right

  int64_t found = 1;

  while(found != 2)
  {
    draw_map(explored, current);

    auto next = next_location(current, direction);

    // Plan is to keep walls to the left, so do we know what is left of us?
    auto direction_left = (direction + 1) % 4;
    auto location_left = next_location(current, direction_left);
    //std::cout << "The location to the left of direction " << direction << " (" << direction_left << ") is " << location_left.first << "," << location_left.second << std::endl;
    auto known_left = explored.find(location_left);
    if(known_left == explored.end() || known_left->second == false)
    {
      //std::cout << "There's not a known wall to our left" << std::endl;
      // It's not a wall, so turn left
      direction = direction_left;
      next = location_left;
    }

    // Time to walk forwards
    //std::cout << "Moving " << direction << " from " << current.first << "," << current.second << " to " << next.first << "," << next.second << std::endl;
    machine.input(direction+1);
    try{machine.run();}
    catch(input_required ir) {found = machine.output();}

    //std::cout << "Machine returned " << found << std::endl;

    switch(found)
    {
      case 0: // Blocked by wall
        explored[next] = true; // Remember it is a wall
        direction = (direction + 3) % 4; // Turn right (to keep the wall on the left)
        break;
      case 1: // Move worked fine
        current = next;
        //std::cout << "New position is " << current.first << "," << current.second << std::endl;
        explored[current] = false; // Remember it's not a wall
        break;
      case 2: // Found the target
        current = next;
        explored[current] = false; // Remember it's not a wall
        std::cout << "Found the target at " << current.first << "," << current.second << std::endl;
        abort();
    }

    if(current.second < -30)
    {
      abort();
    }

    //using namespace std::chrono_literals;
    //std::this_thread::sleep_for(500ms);
  }

  // Have we walked this way before?

  return 0;
}
