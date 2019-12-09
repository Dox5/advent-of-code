#include <iostream>
#include <string>
#include <vector>

static const std::size_t width = 25;
static const std::size_t height = 6;

using row = std::string;

class layer
{
public:
  layer(std::string & data)
  {
    for(int i = 0; i < height; ++i)
    {
      rows.push_back(data.substr(0, width));
      data = data.substr(width);
    }
  }

  auto count(char c) const -> std::size_t
  {
    std::size_t found = 0;
    std::for_each(rows.begin(), rows.end(), [&found, c](auto row){
      std::for_each(row.begin(), row.end(), [&found, c](auto x){
        if(x == c) ++found;
      });
    });
    return found;
  }

  void draw() const
  {
    std::for_each(rows.begin(), rows.end(), [](auto row){
      auto to_show = row;
      std::for_each(to_show.begin(), to_show.end(), [](auto & c){
        if(c == '0')
        {
          c = ' ';
        }
      });
      std::cout << to_show << std::endl;
    });
  }

  void cover(layer const & rhs)
  {
    for(int i = 0; i < height; ++i)
    {
      for(int j = 0; j < width; ++j)
      {
        if(rhs.rows.at(i).at(j) != '2')
        {
          rows.at(i).at(j) = rhs.rows.at(i).at(j);
        }
      }
    }
  }

private:
  std::vector<row> rows;
};

int main()
{
  std::string data;
  getline(std::cin, data);

  std::vector<layer> layers;

  while(data.size() > 0)
  {
    layers.emplace_back(data);
  }

  std::size_t fewest_zeros = 8452937528;
  std::size_t best_layer;

  for(std::size_t i = 0; i < layers.size(); ++i)
  {
    auto num_zeros = layers.at(i).count('0');
    std::cout << "Layer " << i << " has " << num_zeros << " zeros" << std::endl;
    if(num_zeros < fewest_zeros)
    {
      fewest_zeros = num_zeros;
      best_layer = i;
    }
  }

  auto ones = layers.at(best_layer).count('1');
  auto twos = layers.at(best_layer).count('2');
  auto magic = ones * twos;

  std::cout << "Best layer is " << best_layer << " with " << fewest_zeros << " that had " << ones << "," << twos << " and a magic number of " << magic << std::endl;

  layer final = layers.back();

  for(int i = layers.size() - 2; i >= 0; --i)
  {
    final.cover(layers.at(i));
  }

  final.draw();

}
