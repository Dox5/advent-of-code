#include <iostream>
#include <list>
#include <map>
#include <string>

using object = std::string;
using relationship = std::pair<object, object>;

auto get_orbits() -> std::list<relationship>
{
  std::string str;
  std::list<relationship> r;
  while (getline(std::cin, str)) {
    auto middle = str.find_first_of(')');
    auto parent = str.substr(0, middle);
    auto child = str.substr(middle+1);
    r.push_back(relationship{parent, child});
  }
  return r;
}

auto count_parents(object const & child, std::map<object, object> const & child_map) -> std::size_t
{
  std::size_t total = 0;
  auto found = child_map.find(child);

  if(found != child_map.end())
  {
    ++total;
    total += count_parents(found->second, child_map);
  }
  return total;
}

void map_distances(object const & node, std::size_t distance, std::map<object, object> const & child_map, std::map<object, std::size_t> & distances)
{
  auto found = child_map.find(node);
  if(found != child_map.end())
  {
    distances.insert(std::make_pair(node, distance));
    map_distances(found->second, distance + 1, child_map, distances);
  }
}

int main()
{
  std::map<object, object> child_map;

  auto relationships = get_orbits();

  std::for_each(relationships.begin(), relationships.end(), [&child_map](relationship r){
    auto fits = child_map.insert(relationship{r.second, r.first}).second;
    if(!fits)
    {
      std::cerr << "Child already mapped" << std::endl;
      abort();
    }
  });

  std::size_t total = 0;

  std::for_each(child_map.begin(), child_map.end(), [&child_map, &total](auto source){
    total += count_parents(source.first, child_map);
  });
  std::cout << total << std::endl;

  // Find shortest route from "YOU" to "SAN"
  std::map<object, std::size_t> distances_a;
  std::map<object, std::size_t> distances_b;

  map_distances("YOU", 0, child_map, distances_a);
  map_distances("SAN", 0, child_map, distances_b);

  std::size_t shortest = 10000000;

  std::for_each(distances_a.begin(), distances_a.end(), [&distances_b, &shortest](auto d){
    auto found = distances_b.find(d.first);
    if(found != distances_b.end())
    {
      auto distance = d.second + found->second;
      if(distance < shortest)
        shortest = distance;
    }
  });

  std::cout << shortest << std::endl;
}
