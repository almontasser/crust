#include <iostream>
#include <unistd.h>

#include "codegen.h"
#include "lexer.h"
#include "parser.h"

int main() {
    char *filename = "../tests/test01.cr";
    const auto lexer = Lexer::create_from_file(filename);
    if (lexer == nullptr) {
        std::cerr << "Failed to open file: " << filename << std::endl;
        return 1;
    }

    // auto token = lexer->next();
    // while (token->type != TOKEN_EOF) {
    //     std::cout << token->to_string() << std::endl;
    //     token = lexer->next();
    // }

    auto ast = parse_program(lexer);

    char* asm_filename = "../tests/test01.yasm";
    char* obj_filename = "../tests/test01.o";
    char* executable_filename = "../tests/test01";

    auto out_file = fopen(asm_filename, "w");
    generate_program(ast, out_file);
    fclose(out_file);

    auto OS_IS_MACOS = false;

    char* cmd_args[10];
    cmd_args[0] = "/usr/bin/env";
    cmd_args[1] = "yasm";
    if (OS_IS_MACOS)
        cmd_args[2] = "-fmacho64";
    else
        cmd_args[2] = "-felf64";
    cmd_args[3] = "-o";
    cmd_args[4] = obj_filename;
    cmd_args[5] = asm_filename;
    cmd_args[6] = nullptr;

    if (fork() == 0) {
        execv("/usr/bin/env", cmd_args);
        std::cerr << "Failed to exec yasm" << std::endl;
    }

    // link the object file
    if (OS_IS_MACOS)
        cmd_args[1] = "clang";
    else
        cmd_args[1] = "ld";
    cmd_args[2] = "-o";
    cmd_args[3] = executable_filename;
    cmd_args[4] = obj_filename;
    if (OS_IS_MACOS)
        cmd_args[5] = "-Wl,-no_pie";
    else
        cmd_args[5] = nullptr;
    cmd_args[6] = nullptr;

    if (fork() == 0) {
        execv("/usr/bin/env", cmd_args);
        std::cerr << "Failed to exec linker" << std::endl;
    }

    // execute the object file
    cmd_args[0] = executable_filename;
    cmd_args[1] = nullptr;

    if (fork() == 0) {
        execv("/usr/bin/env", cmd_args);
        std::cerr << "Failed to exec object file" << std::endl;
    }
    return 0;
}
