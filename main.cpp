#include <assert.h>
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

char* filename = nullptr;
char* output_filename = "build/output";
bool dump_ast = false;
bool run_program = false;
bool silent = false;

void print_usage_and_exit(char* name, int exit_code) {
    std::cout << "Usage: " << name << " [options] <file>" << std::endl;
    std::cout << "Options:" << std::endl;
    std::cout << "  -h          Show this help message" << std::endl;
    std::cout << "  -s          Silence debug output" << std::endl;
    std::cout << "  -r          Run the compiled program" << std::endl;
    std::cout << "  -o <file>   Output file" << std::endl;
    std::cout << "  -d          Dump the AST" << std::endl;
    std::cout << "Compilation options:" << std::endl;
    std::cout << "  --alloc     Chanfe the default allocator" << std::endl;
    std::cout << "  --dealloc   Change the default deallocator" << std::endl;
    std::cout << "Output file will be named '" << output_filename << "' by default" << std::endl;
    exit(exit_code);
}

char** parse_cli_args(uint64_t argc, char** argv) {
    for (auto i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-s") == 0) {
            silent = true;
        } else if (strcmp(argv[i], "-r") == 0) {
            run_program = true;
        } else if (strcmp(argv[i], "-d") == 0) {
            dump_ast = true;
        } else if (strcmp(argv[i], "-o") == 0) {
            if (i + 1 >= argc) {
                std::cerr << "Expected an argument after -o" << std::endl;
                print_usage_and_exit(argv[0], 1);
            }
            output_filename = argv[i + 1];
            i++;
        } else if (strcmp(argv[i], "-h") == 0) {
            print_usage_and_exit(argv[0], 0);
        } else if (strcmp(argv[i], "--alloc") == 0) {
            if (i + 1 >= argc) {
                std::cerr << "Expected an argument after --alloc" << std::endl;
                print_usage_and_exit(argv[0], 1);
            }
            default_allocator = argv[i + 1];
            i++;
        } else if (strcmp(argv[i], "--dealloc") == 0) {
            if (i + 1 >= argc) {
                std::cerr << "Expected an argument after --dealloc" << std::endl;
                print_usage_and_exit(argv[0], 1);
            }
            default_deallocator = argv[i + 1];
            i++;
        } else if (filename == nullptr) {
            filename = argv[i];
        } else if (!run_program) {
            // No `-r` option passed, we shouldn't have any extra args.
            print_usage_and_exit(argv[0], 1);
        } else {
            return argv + i;
        }
    }
    if (filename == nullptr) {
        std::cerr << "No input file provided" << std::endl;
        print_usage_and_exit(argv[0], 1);
    }
    return argv + argc;
}

int main(int argc, char** argv, char** envp) {
    auto subproc_args = parse_cli_args(argc, argv);

    const auto lexer = Lexer::create_from_file(filename);
    if (lexer == nullptr) {
        std::cerr << "Failed to open file: " << filename << std::endl;
        return 1;
    }

    // add the current directory to the import paths
    import_paths.push_back(canonicalize_file_name("../"));
    import_paths.push_back(get_parent_path(canonicalize_file_name(filename)));

    auto ast = parse_program(lexer);

    if (dump_ast) {
        ast->dump(0);
    }

    // replace extension with .asm
    auto asm_filename = static_cast<char*>(malloc(strlen(filename) + 3));
    strcpy(asm_filename, filename);
    auto ext = strrchr(asm_filename, '.');
    if (ext == nullptr) {
        ext = asm_filename + strlen(asm_filename);
    }
    strcpy(ext, ".yasm");

    auto out_file = fopen(asm_filename, "w");
    generate_program(ast, out_file);
    fclose(out_file);

    auto obj_filename = static_cast<char*>(malloc(strlen(filename) + 1));
    strcpy(obj_filename, filename);
    ext = strrchr(obj_filename, '.');
    if (ext == nullptr) {
        ext = obj_filename + strlen(obj_filename);
    }
    strcpy(ext, ".o\0");

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
    cmd_args[6] = "-gdwarf2";
    cmd_args[7] = nullptr;

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
    cmd_args[3] = output_filename;
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
    cmd_args[0] = output_filename;
    cmd_args[1] = nullptr;

    // if (fork() == 0) {
    //     execv("/usr/bin/env", cmd_args);
    //     std::cerr << "Failed to exec object file" << std::endl;
    // }
    run_command_env(cmd_args, envp, true);
    return 0;
}
