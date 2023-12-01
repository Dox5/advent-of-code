#include <algorithm>
#include <iostream>
#include <numeric>
#include <string>
#include <string_view>
#include <vector>

#include <framework.h>

using std::operator""sv;

namespace {

auto extract_calibration(std::string_view line) -> std::string
{
    constexpr auto digits = "0123456789"sv;

    auto const first = line.find_first_of(digits);
    auto const last = line.find_last_of(digits);

    return std::string{{line[first], line[last]}};
}

auto const tokens = std::vector<std::string_view> {
    "one", "1", "two", "2", "three", "3", "four", "4", "five", "5", "six", "6", "seven", "7", "eight", "8", "nine", "9"
};

struct match {
    unsigned long token_index;
    unsigned long location;
};

auto extract(auto finder, auto sorter)
{
    auto found = std::vector<match>{};

    for(auto i=0ul; i<tokens.size(); ++i) {
        if(auto loc = finder(tokens[i]); loc != std::string_view::npos) {
            found.push_back(match{i, loc});
        }
    }
    
    std::partial_sort(found.begin(), found.begin() + 1, found.end(), sorter);


    auto best = found.at(0);
    auto token_index = best.token_index;
    if (token_index % 2 == 0) {
        token_index++;
    }

    return std::string{tokens[token_index]};
}

auto extract_forward_calibration_value(std::string_view line) -> std::string
{
    auto finder = [line](std::string_view token) { return line.find(token); };
    auto sorter = [](match const &lhs, match const &rhs) { return lhs.location < rhs.location; };
    return extract(finder, sorter);
}

auto extract_backward_calibration_value(std::string_view line) -> std::string
{
    auto finder = [line](std::string_view token) { return line.rfind(token); };
    auto sorter = [](match const &lhs, match const &rhs) { return lhs.location > rhs.location; };
    return extract(finder, sorter);
}

auto extract_wordy_calibration(std::string_view line) -> std::string
{
    auto const forward = extract_forward_calibration_value(line);
    auto const backward = extract_backward_calibration_value(line);

    //std::cerr << "calibration values " << forward << " " << backward << "\n";

    return forward + backward;
}

int do_part_a(std::istream &input, aoc::options) {
    auto calibration_values = std::vector<int>{};

    while(input) {
        auto line_buf = std::vector<std::istream::char_type>{};
        line_buf.resize(4096);

        input.getline(line_buf.data(), line_buf.size());

        if(input.fail()) {
            if(input.eof()) {
                break;
            } else {
                std::cerr << "Reading failed!\n";
                return 3;
            }
        }

        line_buf.resize(input.gcount());
        auto const line = std::string{line_buf.begin(), line_buf.end()};

        auto num_str = extract_calibration(line);
        calibration_values.push_back(std::stoi(num_str));
    }


    auto result = std::accumulate(calibration_values.begin(), calibration_values.end(), int{});
    std::cout << "Calibration value is: " << result << "\n";
    return 0;
}

int do_part_b(std::istream &input, aoc::options) {
    auto calibration_values = std::vector<int>{};

    while(input) {
        auto line_buf = std::vector<std::istream::char_type>{};
        line_buf.resize(4096);

        input.getline(line_buf.data(), line_buf.size());

        if(input.fail()) {
            if(input.eof()) {
                break;
            } else {
                std::cerr << "Reading failed!\n";
                return 3;
            }
        }

        line_buf.resize(input.gcount());
        auto const line = std::string{line_buf.begin(), line_buf.end()};

        auto num_str = extract_wordy_calibration(line);
        calibration_values.push_back(std::stoi(num_str));
    }


    auto result = std::accumulate(calibration_values.begin(), calibration_values.end(), int{});
    std::cout << "Calibration value is: " << result << "\n";
    return 0;
}

}

int main(int argc, char **argv)
{
    auto runner = aoc::make_runner("day01", do_part_a, do_part_b);
    return runner(argc, argv);
}
