#include <bitset>
#include <iostream>
#include <set>

static const std::size_t size{5};

class outofrange
{};

class area
{
public:
  area()
  : m_bugs{0}
  {}

  void infest(std::size_t x, std::size_t y)
  {
    m_bugs.set(index(x, y));
  }

  void kill(std::size_t x, std::size_t y)
  {
    m_bugs.reset(index(x, y));
  }

  auto get(std::size_t x, std::size_t y) const -> std::uint32_t
  {
    if(m_bugs.test(index(x,y)))
      return 1;
    return 0;
  }

  auto rating() const -> std::uint32_t
  {
    return m_bugs.to_ulong();
  }

  auto next() const -> area
  {
    area ret = *this;
    for(std::size_t y = 0; y < size; ++y)
    {
      for(std::size_t x = 0; x < size; ++x)
      {
        auto count = bugs_adjacent(x, y);
        if(get(x, y))
        {
          //std::cout << "Bug at " << x << "," << y << " has " << count << " bugs adjacent" << std::endl;
          // This space is a bug
          if(count != 1)
          {
            ret.kill(x, y);
          }
        }
        else
        {
          //std::cout << "Space at " << x << "," << y << " has " << count << " bugs adjacent" << std::endl;
          // This is an empty space
          if((count == 1) || (count == 2))
          {
            ret.infest(x, y);
          }
        }
      }
    }
    return ret;
  }

  auto operator<(area const & rhs) const -> bool
  {
    return m_bugs.to_ulong() < rhs.m_bugs.to_ulong();
  }

  void print() const
  {
    for(std::size_t y = 0; y < size; ++y)
    {
      for(std::size_t x = 0; x < size; ++x)
      {
        if(get(x, y))
        {
          std::cout << '#';
        }
        else
        {
          std::cout << '.';
        }
      }
      std::cout << std::endl;
    }
  }

private:
  static auto index(std::size_t x, std::size_t y) -> std::size_t
  {
    if((x >= size) || (y >= size))
      throw(outofrange{});
    return y * size + x;
  }

  auto bugs_adjacent(std::size_t x, std::size_t y) const -> std::size_t
  {
    std::size_t count = 0;
    try{count += get(x-1, y);} catch(outofrange o) {}
    try{count += get(x+1, y);} catch(outofrange o) {}
    try{count += get(x, y-1);} catch(outofrange o) {}
    try{count += get(x, y+1);} catch(outofrange o) {}
    return count;
  }

  std::bitset<size*size> m_bugs;
};

auto read_area() -> area
{
  area read;
  std::string in;
  std::size_t y = 0;
  while (getline(std::cin, in))
  {
    for(std::size_t x = 0; x < size; ++x)
    {
      if(in.at(x) == '#')
        read.infest(x, y);
    }
    ++y;
  }
  return read;
}

int main()
{
  std::set<area> previous;
  auto current = read_area();

  auto found = previous.end();

  while(found == previous.end())
  {
    std::cout << "After " << previous.size() << " rounds" << std::endl;
    current.print();
    previous.insert(current);
    current = current.next();
    found = previous.find(current);
  }

  std::cout << current.rating() << std::endl;
  return 0;
}
