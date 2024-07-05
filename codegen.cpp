//
// Created by mahmoud on 7/4/24.
//

#include "codegen.h"

#include <cstring>
#include <iostream>
#include <ostream>

#include "types.h"

FILE* gen_out_file;

void generate_block(Node * node);
void generate_expression(Node * node);

void emit_asm(const char* s) {
    fwrite(s, 1, strlen(s), gen_out_file);
}

void emit_asm2(const char* s1, const char* s2) {
    emit_asm(s1);
    emit_asm(s2);
}

void emit_asm3(const char* s1, const char* s2, const char* s3) {
    emit_asm(s1);
    emit_asm(s2);
    emit_asm(s3);
}

void emit_asm4(const char* s1, const char* s2, const char* s3, const char* s4) {
    emit_asm(s1);
    emit_asm(s2);
    emit_asm(s3);
    emit_asm(s4);
}

void emit_num(const int n) {
    char buf[32];
    snprintf(buf, sizeof(buf), "%d", n);
    emit_asm(buf);
}

char _function_name[256];
char* get_function_name(Node * node) {
    // TODO: Handle struct methods and constructors
    // concat "func_" with the function name
    snprintf(_function_name, sizeof(_function_name), "func_%s", node->function.name);
    return _function_name;
}

void generate_function_call(Node * node) {
    size_t total_size = 0;
    for (auto arg : node->call.args) {
        generate_expression(arg);
        if (is_float_type(arg->etype)) {
            emit_asm("\tsub rsp, 8\n");
            emit_asm("\tmovsd [rsp], xmm0\n");
        } else {
            emit_asm("\tpush rax\n");
        }

        // TODO: this might be an issue if we pass structs some day
        total_size = total_size + 8;
        // total_size += arg->etype->size;
    }
    char* name = get_function_name(node->call.function);
    emit_asm3("\tcall ", name, "\n");
    emit_asm("\tadd rsp, "); emit_num(total_size); emit_asm("\n");
}

void generate_expression(Node * node) {
    if (node->type == AST_LITERAL) {
        if (is_int_type(node->etype)) {
            emit_asm("\tmov rax, "); emit_num(node->literal.as_int); emit_asm("\n");
        }
    } else if (node->type == AST_FUNCTION_CALL) {
        generate_function_call(node);
    }
}

void generate_statement(Node * node) {
    if (node->type == AST_RETURN) {
        if (node->expr) {
            generate_expression(node->expr);
        } else {
            emit_asm("\txor rax, rax\n"); // Default to 0
        }

        // TODO: Implement deferred statements

        emit_asm("\tmov rsp, rbp\n");
        emit_asm("\tpop rbp\n");
        emit_asm("\tret\n");
    } else if (node->type == AST_BLOCK) {
        generate_block(node);
    } else {
        generate_expression(node);
    }
}

void generate_block(Node * node) {
    // TODO: Implement deferred statements
    for (auto child : node->block.children) {
        generate_statement(child);
    }
}

void generate_function(Node* node) {
    auto name = get_function_name(node);
    emit_asm2(name, ":\n");
    emit_asm("\tpush rbp\n");
    emit_asm("\tmov rbp, rsp\n");
    emit_asm("\tsub rsp, "); emit_num(node->function.max_locals_size); emit_asm("\n");

    generate_block(node->function.body);

    emit_asm("\tmov rsp, rbp\n");
    emit_asm("\tpop rbp\n");
    // Return 0 by default if we don't have a return statement
    emit_asm("\txor rax, rax\n");
    emit_asm("\tret\n");
}

void generate_syscall(const int syscall) {
    emit_asm("\tmov rax, "); emit_num(syscall); emit_asm("\n");
    emit_asm("\tsyscall\n");
}

void generate_builtins() {
    emit_asm("func_print:\n");
    emit_asm("\tmov rdi, [rsp+8]\n");
    emit_asm("\tmov r9, -3689348814741910323\n");
    emit_asm("\tsub rsp, 40\n");
    emit_asm("\tmov byte [rsp+31], 10\n");
    emit_asm("\tlea rcx, [rsp+30]\n");
    emit_asm("\tmov qword rbx, 0\n");
    emit_asm(".L2:\n");
    emit_asm("\tmov rax, rdi\n");
    emit_asm("\tlea r8, [rsp+32]\n");
    emit_asm("\tmul r9\n");
    emit_asm("\tmov rax, rdi\n");
    emit_asm("\tsub r8, rcx\n");
    emit_asm("\tshr rdx, 3\n");
    emit_asm("\tlea rsi, [rdx+rdx*4]\n");
    emit_asm("\tadd rsi, rsi\n");
    emit_asm("\tsub rax, rsi\n");
    emit_asm("\tadd eax, 48\n");
    emit_asm("\tmov byte [rcx], al\n");
    emit_asm("\tmov rax, rdi\n");
    emit_asm("\tmov rdi, rdx\n");
    emit_asm("\tmov rdx, rcx\n");
    emit_asm("\tsub rcx, 1\n");
    emit_asm("\tcmp rax, 9\n");
    emit_asm("\tja .L2\n");
    emit_asm("\tlea rax, [rsp+32]\n");
    emit_asm("\tmov edi, 1\n");
    emit_asm("\tsub rdx, rax\n");
    emit_asm("\txor eax, eax\n");
    emit_asm("\tlea rsi, [rsp+32+rdx]\n");
    emit_asm("\tmov rdx, r8\n");
    generate_syscall(SYSCALL_WRITE);
    emit_asm("\tadd rsp, 40\n");
    emit_asm("\tret\n");
}

void generate_program(Node *ast, FILE *file) {
    gen_out_file = file;

    auto n = ast->block.children.size();
    for (size_t i = 0; i < n; i++) {
        auto node = ast->block.children[i];
        if (node->type == AST_FUNCTION) {
            generate_function(node);
        } else {
            std::cerr << "Unknown node type: " << node->type << " in generate_program()" << std::endl;
            exit(1);
        }
    }

    auto OS_IS_MACOS = false;
    if (OS_IS_MACOS) {
        emit_asm("global _main\n");
        emit_asm("_main:\n");
        // Push envp
        emit_asm("\tmov rax, rdx\n");
        emit_asm("\tpush rax\n");
        // Push argv
        emit_asm("\tmov rax, rsi\n");
        emit_asm("\tpush rax\n");
        // Push argc
        emit_asm("\tmov rax, rdi\n");
        emit_asm("\tpush rax\n");
    } else {
        emit_asm("global _start\n");
        emit_asm("_start:\n");

        emit_asm("\tmov rbp, rsp\n");
        // Push envp
        emit_asm("\tmov rax, [rbp]\n");
        emit_asm("\tadd rax, 2\n");
        emit_asm("\tshl rax, 3\n");
        emit_asm("\tadd rax, rbp\n");
        emit_asm("\tpush rax\n");
        // Push argv
        emit_asm("\tmov rax, rbp\n");
        emit_asm("\tadd rax, 8\n");
        emit_asm("\tpush rax\n");
        // Push argc
        emit_asm("\tmov rax, [rbp]\n");
        emit_asm("\tpush rax\n");
    }

    // TODO: Initialize global variables

    emit_asm("\tcall func_main\n");
    emit_asm("\tmov rdi, rax\n");
    generate_syscall(SYSCALL_EXIT);

    generate_builtins();

    // emit_asm("section .bss\n");
    // emit_asm("\tgvars: resb "); emit_num(p_global_offset); emit_asm("\n");

    // // Global strings
    // emit_asm("section .data\n");
    // for (let i = 0; i < gen_string_literals.size; ++i) {
    //     emit_asm("\tgs_"); emit_num(i); emit_asm(": db ");
    //     let s: char* = gen_string_literals::at(i);
    //     while (*s) { emit_num(*s); emit_asm(","); ++s; }
    //     emit_asm("0\n");
    // }
    // for (let i = 0; i < gen_float_literals.size; ++i) {
    //     emit_asm("\tgf_"); emit_num(i);
    //     emit_asm3(": dq ", gen_float_literals::at(i), "\n");
    // }
}
