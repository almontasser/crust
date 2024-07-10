#include <cstring>
#include <filesystem>
#include <iostream>
#include <unistd.h>

#include "codegen.h"
#include "lexer.h"
#include "parser.h"
#include "sys/wait.h"

void run_command_env(char** args, char** envp, bool echo) {
    if (echo) {
        std::cout << "[+]";
        for (int i = 0; args[i] != nullptr; i++) {
            std::cout << " " << args[i];
        }
        std::cout << std::endl;
    }

    auto pid = fork();
    if (pid == 0) {
        execve(args[0], args, envp);
        std::cerr << "Failed to exec: " << args[0] << std::endl;
        exit(1);
    }

    int status;
    if (wait4(-1, &status, 0, nullptr) < 0) {
        std::cerr << "Failed to wait for child process" << std::endl;
        exit(1);
    }

    if (WIFEXITED(status) && WEXITSTATUS(status) != 0) {
        std::cerr << "Command failed: " << args[0] << std::endl;
        exit(1);
    }
}

char* get_parent_path(const char* filename) {
    std::filesystem::path path = filename;
    return strdup(path.parent_path().c_str());
}

int main(char** argv, int argc, char** envp) {
    char *filename = "../tests/test08.cr";
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

    // add the current directory to the import paths
    import_paths.push_back(canonicalize_file_name("../"));
    import_paths.push_back(get_parent_path(canonicalize_file_name(filename)));

    auto ast = parse_program(lexer);

    char* asm_filename = "../tests/test08.yasm";
    char* obj_filename = "../tests/test08.o";
    char* executable_filename = "../tests/test08";

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

    // if (fork() == 0) {
    //     execv("/usr/bin/env", cmd_args);
    //     std::cerr << "Failed to exec yasm" << std::endl;
    // }
    run_command_env(cmd_args, envp, true);

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

    // if (fork() == 0) {
    //     execv("/usr/bin/env", cmd_args);
    //     std::cerr << "Failed to exec linker" << std::endl;
    // }
    run_command_env(cmd_args, envp, true);

    // execute the object file
    cmd_args[0] = executable_filename;
    cmd_args[1] = nullptr;

    // if (fork() == 0) {
    //     execv("/usr/bin/env", cmd_args);
    //     std::cerr << "Failed to exec object file" << std::endl;
    // }
    run_command_env(cmd_args, envp, true);
    return 0;
}
