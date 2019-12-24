#include <bitset>
#include <iostream>
#include <list>
#include <set>

static const std::size_t size{5};

class outofrange
{};

class child_dimension
{};

class area
{
public:
  area()
  : m_bugs{0}
  {}

  void infest(std::size_t x, std::size_t y) {m_bugs.set(index(x, y));}

  void kill(std::size_t x, std::size_t y) {m_bugs.reset(index(x, y));}

  auto get(std::size_t x, std::size_t y, bool multidimensional = false) const -> std::uint32_t
  {
    if(multidimensional && (x == 2) && (y == 2))
      throw child_dimension{};
    if(m_bugs.test(index(x,y)))
      return 1;
    return 0;
  }

  auto rating() const -> std::uint32_t {return m_bugs.to_ulong();}

  auto present() const -> std::size_t {
    auto actual_bugs = m_bugs;
    // Middle slot doesn't really exist
    actual_bugs.reset(index(2,2));
    return actual_bugs.count();
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

  auto next(area const & parent, area const & child) const -> area
  {
    area ret = *this;
    for(std::size_t y = 0; y < size; ++y)
    {
      for(std::size_t x = 0; x < size; ++x)
      {
        if(x == 2 && y == 2)
        {
          continue;
        }
        auto count = bugs_adjacent(x, y, parent, child);

        /*if(x == 2 && y == 4)
        {
          std::cout << "Parent = " << std::endl;
          parent.print();
          std::cout << "Current = " << std::endl;
          print();
          std::cout << "Child = " << std::endl;
          child.print();
          std::cout << "Count of nearby bugs = " << count << std::endl;
        }*/

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
        if(x == 2 && y == 2)
        {
          std::cout << "?";
          continue;
        }
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

  auto bugs_adjacent(std::size_t x, std::size_t y, area const & parent, area const & child) const -> std::size_t
  {
    std::size_t count = 0;
    try{count += get(x-1, y, true);} catch(outofrange o) {count += parent.get(1,2);} catch (child_dimension c) {count += child.right();}
    try{count += get(x+1, y, true);} catch(outofrange o) {count += parent.get(3,2);} catch (child_dimension c) {count += child.left();}
    try{count += get(x, y-1, true);} catch(outofrange o) {count += parent.get(2,1);} catch (child_dimension c) {count += child.bottom();}
    try{count += get(x, y+1, true);} catch(outofrange o) {count += parent.get(2,3);} catch (child_dimension c) {count += child.top();}
    return count;
  }

  auto right() const -> std::uint32_t
  {
    std::uint32_t total = 0;
    for(std::size_t y = 0; y < size; ++y)
    {
      total += get(size-1, y);
    }
    return total;
  }

  auto left() const -> std::uint32_t
  {
    std::uint32_t total = 0;
    for(std::size_t y = 0; y < size; ++y)
    {
      total += get(0, y);
    }
    return total;
  }

  auto top() const -> std::uint32_t
  {
    std::uint32_t total = 0;
    for(std::size_t x = 0; x < size; ++x)
    {
      total += get(x, 0);
    }
    return total;
  }

  auto bottom() const -> std::uint32_t
  {
    std::uint32_t total = 0;
    for(std::size_t x = 0; x < size; ++x)
    {
      total += get(x, size-1);
    }
    return total;
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

void part1(area const & start)
{
  auto current = start;
  std::set<area> previous;

  auto found = previous.end();

  while(found == previous.end())
  {
    /*std::cout << "After " << previous.size() << " rounds" << std::endl;
    current.print();*/
    previous.insert(current);
    current = current.next();
    found = previous.find(current);
  }

  std::cout << current.rating() << std::endl;
}

auto dimension_next(std::list<area> current) -> std::list<area>
{
  // Start with a copy:
  std::list<area> next;
  area parent{};

  if(current.front().present() > 0) current.emplace_front(area{});
  if(current.back().present() > 0) current.emplace_back(area{});

  for(auto iter = current.begin(); iter != current.end(); ++iter)
  {
    area child{};
    auto child_iter = iter;
    std::advance(child_iter, 1);

    if(child_iter != current.end())
    {
      child = *child_iter;
    }

    next.emplace_back(iter->next(parent, child));

    parent = *iter;
  }

  return next;
}

void part2(area const & start)
{
  std::list<area> current;
  current.emplace_back(start);

  for(std::size_t gen = 0; gen < 200; ++gen)
  {
    current = dimension_next(current);
  }

  std::size_t count = 0;

  std::for_each(current.begin(), current.end(), [&count](auto level)
  {
    /*std::cout << "A dimension:" << std::endl;
    level.print();*/
    count += level.present();
  });

  std::cout << count << std::endl;
}

int main()
{
  auto start = read_area();
  part1(start);
  part2(start);
  return 0;
}
