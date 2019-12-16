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
    /*std::cout << data.at(i) << "*" << pattern.at(i);
    if(i == data.size() - 1) std::cout << " = ";
    else std::cout << " + ";*/
    result += data.at(i) * pattern.at(i);
  }
  result = abs(result) % 10;
  //std::cout << result << std::endl;
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
  auto signal = str2vec("59790132880344516900093091154955597199863490073342910249565395038806135885706290664499164028251508292041959926849162473699550018653393834944216172810195882161876866188294352485183178740261279280213486011018791012560046012995409807741782162189252951939029564062935408459914894373210511494699108265315264830173403743547300700976944780004513514866386570658448247527151658945604790687693036691590606045331434271899594734825392560698221510565391059565109571638751133487824774572142934078485772422422132834305704887084146829228294925039109858598295988853017494057928948890390543290199918610303090142501490713145935617325806587528883833726972378426243439037");

  for(int i = 0; i < 100; ++i)
  {
    signal = fft(signal);
    std::cout << "After " << i + 1 << " phases:" << vec2str(signal).substr(0, 8) << std::endl;
  }


}
