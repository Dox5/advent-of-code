#include <iostream>
#include <set>
#include <string>

bool check_rules(int i)
{
  auto s = std::to_string(i);
  std::set<int> repeats;

  if(s.size() != 6)
  {
    return false;
  }

  for(int i = 0; i < 6; ++i)
  {
    if(s[i-1] > s[i])
    {
      return false;
    }

    if(s[i-1] == s[i])
    {
      repeats.insert(s[i]);
    }
  }

  // For part one:
  //return repeats.size() > 0;

  bool pure_pair = false;

  // Is there a pair on its own?
  std::for_each(repeats.begin(), repeats.end(), [&s, &pure_pair](char c){
    std::string triple;
    triple.push_back(c);
    triple.push_back(c);
    triple.push_back(c);
    if(s.find(triple) == std::string::npos)
    {
      pure_pair = true;
    }
  });

  return pure_pair;
}

int main()
{
  int min = 272091;
  int max = 815432;

  int count = 0;

  for(int i = min; i <= max; ++i)
  {
    if(check_rules(i))
    {
      ++count;
    }
  }

  std::cout << count << std::endl;
}
