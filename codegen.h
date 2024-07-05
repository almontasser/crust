//
// Created by mahmoud on 7/4/24.
//

#ifndef CODEGEN_H
#define CODEGEN_H
#include <cstdio>

#include "ast.h"

constexpr int SYSCALL_WRITE = 1;
constexpr int SYSCALL_EXIT = 60;

void generate_program(Node* ast, FILE* file);

#endif //CODEGEN_H
