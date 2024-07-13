cmake_minimum_required(VERSION 3.28)
project(crust)

set(CMAKE_CXX_STANDARD 17)

add_executable(crust main.cpp
        lexer.cpp
        lexer.h
        ast.cpp
        ast.h
        types.cpp
        types.h
        parser.cpp
        parser.h
        codegen.cpp
        codegen.h)
