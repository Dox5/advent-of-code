cmake_minimum_required(VERSION 3.27)

project(day01 CXX)

add_executable(day01
    main.cpp
)


find_library(
    AOC_UTIL
    aocutil
    REQUIRED
)

target_link_libraries(day01
    PRIVATE ${AOC_UTIL}
)

set_target_propertieS(day01
    PROPERTIES
    CXX_STANDARD 23
)

target_compile_options(day01
    PRIVATE
    -Wall
    -Wextra
    -Wpedantic
    -Werror
)

install(TARGETS day01)
