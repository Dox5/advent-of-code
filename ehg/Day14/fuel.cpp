#include <iostream>
#include <list>
#include <map>
#include <set>
#include <string>
#include <regex>

int64_t div_ceil(int64_t x, int64_t y)
{
  return (x + y - 1) / y;
}

struct chemical
{
  chemical(int64_t c, std::string const & n)
  : count{c}, name{n}
  {}

  auto operator<(chemical const & rhs) const -> bool
  {
    return name < rhs.name;
  }

  int64_t count;
  std::string name;
};

using requirements = std::list<chemical>;

void read_rule(std::string line, std::map<chemical, requirements> & reqs)
{
  requirements chems;
  std::regex rgx("([0-9]+) ([A-Z]+)(, | => )?");
  std::smatch matches;

  while(std::regex_search(line, matches, rgx))
  {
    auto match_length = matches[0].str().size();

    chems.emplace_back(chemical{stoi(matches[1].str()), matches[2].str()});

    line = line.substr(match_length);
  }

  auto product = chems.back();
  chems.pop_back();

  reqs[product] = chems;
}

void use_up_spares(std::set<chemical> & spare, chemical & target)
{
  auto found = spare.find(target);

  if(found == spare.end())
    return;

  if(target.count < found->count)
  {
    // Just use a little
    chemical remaining = *found;
    spare.erase(found);
    remaining.count -= target.count;
    spare.insert(remaining);
    // Completely satisfy the requirement
    target.count = 0;
    return;
  }
  else
  {
    // Use up all of the spare
    target.count -= found->count;
    spare.erase(found);
  }
}

auto find_ore_cost_of(std::map<chemical, requirements> const & rules, std::set<chemical> & spare, chemical target) -> std::size_t
{
  // Is what we need already produced?
  use_up_spares(spare, target);

  if(target.name == "ORE")
  {
    return target.count;
  }

  std::size_t ore_needed = 0;

  auto found_recipe = rules.find(target);
  if(found_recipe == rules.end())
  {
    std::cout << "Can't find recipe for " << target.name << std::endl;
  }

  auto created = found_recipe->first;
  auto ingredients = found_recipe->second;

  auto multiply = div_ceil(target.count, created.count);

  created.count *= multiply;

  std::for_each(ingredients.begin(), ingredients.end(), [&ore_needed, &rules, &spare, multiply](auto need){
    need.count *= multiply;
    ore_needed += find_ore_cost_of(rules, spare, need);
  });

  created.count -= target.count;
  target.count = 0;

  // Store any leftover
  if(created.count > 0)
  {
    auto spare_iter = spare.find(created);
    if(spare_iter == spare.end())
    {
      spare.insert(created);
    }
    else
    {
      chemical remaining = *spare_iter;
      spare.erase(spare_iter);
      remaining.count += created.count;
      spare.insert(remaining);
    }
  }

  return ore_needed;
}

auto try_to_make(std::map<chemical, requirements> const & rules, chemical target) -> std::size_t
{
  std::set<chemical> spare_ore;
  spare_ore.insert(chemical(1000000000000, "ORE"));

  return find_ore_cost_of(rules, spare_ore, target);
}

int main()
{
  std::map<chemical, requirements> rules;
  std::string line;
  while(getline(std::cin, line))
    read_rule(line, rules);

  chemical fuel{1, "FUEL"};
  std::set<chemical> spare;

  auto ore_required = find_ore_cost_of(rules, spare, fuel);
  std::cout << ore_required << std::endl;

  std::size_t cost = 0;

  chemical aim{1, "FUEL"};

  while(try_to_make(rules, aim) == 0)
  {
    aim.count = aim.count * 2;
    std::cout << "Trying to make " << aim.count << std::endl;
  }

  std::cout << aim.count << " is too high, as it needed " << cost << std::endl;

  // Need to search between
  auto min = aim.count / 2;
  auto max = aim.count;

  while(min != max)
  {
    aim.count = (max + min) / 2;

    std::cout << "Trying to make " << aim.count << " between " << min << " and " << max << std::endl;

    auto extra = try_to_make(rules, aim);

    if(extra > 0)
    {
      std::cout << "Too high, needed an extra " << extra << std::endl;
      max = aim.count;
    }
    else
    {
      std::cout << "Too low" << std::endl;
      if(aim.count == max - 1)
      {
        std::cout << "Final answer = " << aim.count << std::endl;
        abort();
      }
      min = aim.count;
    }

  }

}
