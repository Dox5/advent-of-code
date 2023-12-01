cmake_minimum_required(VERSION 3.27)

project(day01 CXX)

add_executable(day01
    main.cpp
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
