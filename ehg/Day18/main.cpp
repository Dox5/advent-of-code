#include <iostream>
#include <map>
#include <set>
#include <string>
#include <vector>

using location = std::pair<int, int>;

auto read_map() -> std::vector<std::string>
{
  std::vector<std::string> map;
  std::string line;
  while (getline(std::cin, line))
  {
    map.push_back(line);
  }
  return map;
}

int main()
{
  auto map = read_map();

  std::map<char, location> keys;
  std::map<char, location> doors;
  std::set<location> navigable;
  location start_pos;

  for(int y = 0; y < map.size(); ++y)
  {
    auto line = map.at(y);
    for(int x = 0; x < line.size(); ++x)
    {
      location pos{x, y};
      char val = line.at(x);

      if((val >= 'a' ) && (val <= 'z'))
      {
        keys[val] = pos;
      }
      else if((val >= 'A' ) && (val <= 'Z'))
      {
        doors[val] = pos;
      }
      else if(val != '#')
      {
        navigable.insert(pos);
        start_pos = pos;
      }
    }
  }

  std::cout << "Found " << keys.size() << " keys, " << doors.size() << " doors" << std::endl;
}
