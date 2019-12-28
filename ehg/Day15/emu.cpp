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

void draw_map(std::map<location, char> & explored)
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

    map_pos = info.second;
  });

  std::for_each(map.rbegin(), map.rend(), [](auto line){
    std::cout << line << std::endl;
  });
}

int64_t direction_translate(int64_t eds_version)
{
  switch(eds_version)
  {
    case 0:
      return 1;
    case 1:
      return 4;
    case 2:
      return 2;
    case 3:
      return 3;
  }

  std::cout << "Badness" << std::endl;
  abort();
}

void explore_from(location position, intcode_computer machine, std::map<location, char> & map)
{
  for(std::int64_t direction = 0; direction < 4; ++direction)
  {
    auto next = next_location(position, direction);

    // Have we already been to the next location?
    auto found = map.find(next);
    if(found == map.end())
    {
      auto machine_to_move = machine;
      machine_to_move.input(direction_translate(direction));

      try{machine_to_move.run();}
      catch(input_required ir) {}

      auto res = machine_to_move.output();

      switch(res)
      {
        case 0: // Blocked by wall, do nothing
          map[next] = '#';
          break;
        case 2: // Found the target
          std::cout << "Found target at " << next.first << "," << next.second << std::endl;
          map[next] = 'O';
          explore_from(next, machine_to_move, map);
          break;
        case 1: // Move worked fine, keep exploring in that direction
          map[next] = '.';
          explore_from(next, machine_to_move, map);
          break;
      }
    }
  }
}

struct state
{
  location position;
  std::size_t distance;
};

auto measure_distance(std::map<location, char> const & map, location start = location{0,0}, bool animate = false) -> state
{
  std::map<location, char> updated = map;
  std::set<location> explored;
  std::list<state> q;

  state current{start, 0};
  q.push_back(current);

  std::size_t distance = 0;

  while(!q.empty())
  {
    current = q.front();
    q.pop_front();

    auto found = map.find(current.position);

    if(found != map.end() && found->second != '#')
    {
      if(animate)
      {
        if(updated[current.position] == '.')
          updated[current.position] = 'o';


        if(current.distance > distance)
        {
          using namespace std::chrono_literals;
          std::this_thread::sleep_for(20ms);
          distance = current.distance;
          std::cout << "Working from distance " << distance << std::endl;
          draw_map(updated);
        }
      }

      // If we're looking for the oxygenator (not starting at it), check if we're there
      if(current.position != start && found->second == 'O')
      {
        std::cout << "Stopping exploration at " << current.position.first << ",";
        std::cout << current.position.second << " after " << explored.size();
        std::cout << " nodes because I found the oxygenator" << std::endl;
        // We've found the machine
        return current;
      }

      // We're in a non-wall position
      for(std::int64_t direction = 0; direction < 4; ++direction)
      {
        auto next = next_location(current.position, direction);
        if(explored.insert(next).second)
        {
          // We've not been to the next position before
          q.push_back(state{next, current.distance + 1});
        }
      }
    }
  }

  std::cout << "Completed exploration at " << current.position.first << ",";
  std::cout << current.position.second << " after " << explored.size();
  std::cout << " nodes because there are no more nodes to explore. Biggest distance was " << distance << std::endl;

  draw_map(updated);

  return state{start, distance};
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

  std::map<location, char> map;

  explore_from(location{0,0}, machine, map);

  map[location{0,0}] = 'D';
  draw_map(map);

  auto oxygenator = measure_distance(map);

  std::cout << oxygenator.distance << std::endl;

  std::cout << measure_distance(map, oxygenator.position, true).distance << std::endl;

  return 0;
}
