#include <iostream>
#include <sstream>
#include <string>
#include <vector>

static const int base_pattern[4] = {0, 1, 0, -1};

auto str2vec(std::string const & input) -> std::vector<int>
{
  std::vector<int> vec;
  vec.reserve(input.size());
  std::for_each(input.begin(), input.end(), [&vec](auto c){
    vec.push_back(c - '0');
  });
  return vec;
}

auto vec2str(std::vector<int> const & vec) -> std::string
{
  std::ostringstream res;
  std::for_each(vec.begin(), vec.end(), [&res](auto val){
    res << val;
  });
  return res.str();
}

auto pattern_expand(std::size_t stride, std::size_t length) -> std::vector<int>
{
  std::vector<int> pattern;
  pattern.reserve(length);
  std::size_t count = 1;
  while(pattern.size() < length)
  {
    std::size_t base_idx = (count / stride) % 4;
    pattern.push_back(base_pattern[base_idx]);
    ++count;
  }
  return pattern;
}

auto fft(std::vector<int> const & data, std::vector<int> const & pattern) -> int
{
  int result = 0;
  for(std::size_t i = 0; i < data.size(); ++i)
  {
    std::cout << data.at(i) << "*" << pattern.at(i);
    if(i == data.size() - 1) std::cout << " = ";
    else std::cout << " + ";
    result += data.at(i) * pattern.at(i);
  }
  result = abs(result) % 10;
  std::cout << result << std::endl;
  return result;
}

auto fft(std::vector<int> const & data) -> std::vector<int>
{
  std::vector<int> result;

  for(std::size_t i = 0; i < data.size(); ++i)
  {
    auto stride = i + 1;
    auto pattern = pattern_expand(stride, data.size());

    result.push_back(fft(data, pattern));
  }
  return result;
}

int main()
{
  auto signal = str2vec("12345678");

  for(int i = 0; i < 4; ++i)
  {
    signal = fft(signal);
    std::cout << "After " << i + 1 << " phases:" << vec2str(signal) << std::endl;
  }


}
