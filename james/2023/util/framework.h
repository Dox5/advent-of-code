#pragma once

#include <functional>
#include <istream>
#include <string_view>

namespace aoc {

struct options { };

using main_signature = auto (int, char **) -> int;
using part_runner = std::function<auto (std::istream &, options) -> int>;

// Create a runner with signature `main_signature` that calls the right part
// base on the parsed options
auto make_runner(std::string_view name, part_runner part_a, part_runner part_b) -> std::function<main_signature>;

}
