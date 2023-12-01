cmake_minimum_required(VERSION 3.27)

project(aocutil CXX)

add_library(aocutil
    framework.cpp
)

set_target_propertieS(aocutil
    PROPERTIES
    CXX_STANDARD 23
)

target_compile_options(aocutil
    PRIVATE
    -Wall
    -Wextra
    -Wpedantic
    -Werror
)

install(TARGETS aocutil)

install(FILES
    framework.h
    TYPE INCLUDE
)
