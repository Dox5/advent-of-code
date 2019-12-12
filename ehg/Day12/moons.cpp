#include <iostream>
#include <set>
#include <vector>

static const std::size_t dimensions = 3;

class coordinates{
public:
  coordinates(int x, int y, int z)
  : m_vals{dimensions}
  {
    m_vals.resize(dimensions);
    m_vals.at(0) = x;
    m_vals.at(1) = y;
    m_vals.at(2) = z;
  }

  void display() const
  {
    std::cout << m_vals.at(0) << "," << m_vals.at(1) << "," << m_vals.at(2);
  }

  auto abs_sum() const -> int
  {
    int sum = 0;
    std::for_each(m_vals.begin(), m_vals.end(), [&sum](auto v){
      sum += abs(v);
    });
    return sum;
  }

  auto towards(coordinates const & rhs) const -> coordinates
  {
    coordinates directions{0,0,0};
    for(int dim = 0; dim < dimensions; ++dim)
    {
      if(m_vals.at(dim) != rhs.m_vals.at(dim))
      {
        if(m_vals.at(dim) < rhs.m_vals.at(dim))
          directions.m_vals.at(dim) = 1;
        else
          directions.m_vals.at(dim) = -1;
      }
    }
    return directions;
  }

  auto operator+(coordinates const & rhs) const -> coordinates
  {
    coordinates res = rhs;
    for(int dim = 0; dim < dimensions; ++dim)
    {
      res.m_vals.at(dim) += m_vals.at(dim);
    }
    return res;
  }

  auto operator[](std::size_t i) const -> int
  {
    return m_vals.at(i);
  }

  auto operator<(coordinates const & rhs) const -> bool
  {
    return m_vals < rhs.m_vals;
  }

  auto operator==(coordinates const & rhs) const -> bool
  {
    return m_vals == rhs.m_vals;
  }

  auto x() const -> int {return m_vals.at(0);}
  auto y() const -> int {return m_vals.at(1);}
  auto z() const -> int {return m_vals.at(2);}
  auto x() -> int & {return m_vals.at(0);}
  auto y() -> int & {return m_vals.at(1);}
  auto z() -> int & {return m_vals.at(2);}

private:
  std::vector<int> m_vals;
};

class moon
{
public:
  moon(coordinates const & pos)
  : m_pos{pos}, m_vel{0,0,0}
  {}

  void display() const
  {
    m_pos.display();
    std::cout << ":";
    m_vel.display();
    std::cout << std::endl;
  }

  void accelerate(moon const & rhs)
  {
    auto dv = m_pos.towards(rhs.m_pos);
    m_vel = m_vel + dv;
  }

  void accelerate(std::vector<moon> const & others)
  {
    std::for_each(others.begin(), others.end(), [this](auto m){
      accelerate(m);
    });
  }

  void move()
  {
    m_pos = m_pos + m_vel;
  }

  auto potential() const -> int
  {
    return m_pos.abs_sum();
  }

  auto kinetic() const -> int
  {
    return m_vel.abs_sum();
  }

  auto operator<(moon const & rhs) const -> bool
  {
    if(m_pos == rhs.m_pos)
    {
      return m_vel < rhs.m_vel;
    }
    return m_pos < rhs.m_pos;
  }

  auto dim(int i) const -> int
  {
    return m_pos[i];
  }

  auto vel(int i) const -> int
  {
    return m_vel[i];
  }

private:
  coordinates m_pos;
  coordinates m_vel;
};

int main()
{
  std::vector<moon> moons;

  moons.emplace_back(moon{coordinates{5, 4, 4}});
  moons.emplace_back(moon{coordinates{-11, -11, -3}});
  moons.emplace_back(moon{coordinates{0, 7, 0}});
  moons.emplace_back(moon{coordinates{-13, 2, 10}});

  std::vector<moon> original = moons;

  std::vector<std::size_t> returns;

  while(returns.size() < dimensions)
  {
    returns.push_back(0);
  }

  int step = 0;

  for(int step = 0; step < 1000000; ++step)
  {
    // Check if any dimensions have returned to their starting value
    for(int i = 0; i < dimensions; ++i)
    {
      bool good = true;
      for(int moon = 0; moon < moons.size(); ++moon)
      {
        if(moons.at(moon).dim(i) != original.at(moon).dim(i) ||
           moons.at(moon).vel(i) != 0)
        {
          good = false;
        }
      }

      if(good && (returns.at(i) == 0))
      {
        std::cout << "Found repeat of dimension " << i << " after step " << step << std::endl;
        returns.at(i) = step;
      }

    }

    if(step == 1000)
    {
      int energy = 0;

      std::for_each(moons.begin(), moons.end(), [&energy](auto m){
        auto k = m.kinetic();
        auto p = m.potential();
        energy += k * p;
      });

      std::cout << energy << std::endl;

    }

    std::for_each(moons.begin(), moons.end(), [&moons](auto & m){
      //m.display();
      m.accelerate(moons);
    });

    std::for_each(moons.begin(), moons.end(), [](auto & m){
      m.move();
    });


  }




  return 0;
}
