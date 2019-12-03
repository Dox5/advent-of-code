#include <algorithm>
#include <iostream>
#include <list>
#include <set>
#include <sstream>
#include <string>
#include <vector>

struct stage
{
  char direction;
  int length;
};

struct coordinates
{
  int x;
  int y;

  bool operator<(coordinates const & rhs) const
  {
    if(x != rhs.x)
    {
      return x < rhs.x;
    }
    else
    {
      return y < rhs.y;
    }
  }
};

using wire = std::list<stage>;

auto read_wire(std::string const & line) -> wire
{
  std::istringstream stream(line);

  wire stages;

  std::string str;
  while (getline(stream, str, ','))
  {
    stage s{str[0], stoi(str.substr(1))};
    stages.push_back(s);
  }
  return stages;
}

auto plot_coordinates(wire const & w) -> std::set<coordinates>
{
  std::set<coordinates> reached;

  coordinates current{0, 0};

  std::for_each(w.begin(), w.end(), [&reached, &current](stage const & s){
    for(int i = 0; i < s.length; ++i)
    {
      switch(s.direction)
      {
        case 'U':
          ++current.y;
          break;
        case 'D':
          --current.y;
          break;
        case 'L':
          --current.x;
          break;
        case 'R':
          ++current.x;
          break;
        default:
          std::cout << "Direction " << s.direction << " makes no sense" << std::endl;
          throw(5);
      }

      reached.insert(current);
    }
  });

  return reached;
}

void dump_stages(wire const & w)
{
  std::for_each(w.begin(), w.end(), [](stage const & s){
    std::cout << s.direction << " " << s.length << ":";
  });
  std::cout << std::endl;
}

auto read_wires() -> std::vector<std::list<stage> >
{
  std::vector<wire> wires;
  std::string line;
  while(getline(std::cin, line))
  {
    wires.emplace_back(read_wire(line));
  }

  return wires;
}

int main()
{
  auto wires = read_wires();

  std::set<coordinates> first, second;

  first = plot_coordinates(wires.at(0));
  second = plot_coordinates(wires.at(1));

  std::set<coordinates> intersection;

  std::set_intersection(first.begin(), first.end(),
                        second.begin(), second.end(),
                        std::inserter(intersection, intersection.begin()));

  std::set<int> distances;

  std::for_each(intersection.begin(), intersection.end(), [&distances](coordinates const & c){
    auto distance = abs(c.x) + abs(c.y);
    distances.insert(distance);
  });

  std::cout << "Found " << distances.size() << " intersections, closeset to origin is at distance " << *distances.begin() << std::endl;

  return 0;
}
