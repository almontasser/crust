//
// Created by mahmoud on 7/4/24.
//

#include "codegen.h"

#include <cstring>
#include <iostream>
#include <ostream>

#include "parser.h"
#include "types.h"

auto OS_IS_MACOS = false;

FILE *gen_out_file;
auto gen_label_counter = -1; // So the labels start at 0

auto gen_current_break = -2;

std::vector<char *> gen_string_literals = {};
std::vector<char *> gen_float_literals = {};

void generate_block(Node *node);

void generate_expression(Node *node);

void emit_asm(const char *s) {
    fwrite(s, 1, strlen(s), gen_out_file);
}

void emit_asm2(const char *s1, const char *s2) {
    emit_asm(s1);
    emit_asm(s2);
}

void emit_asm3(const char *s1, const char *s2, const char *s3) {
    emit_asm(s1);
    emit_asm(s2);
    emit_asm(s3);
}

void emit_asm4(const char *s1, const char *s2, const char *s3, const char *s4) {
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

char *get_function_name(Node *node) {
    // TODO: Handle struct methods and constructors
    // concat "func_" with the function name
    snprintf(_function_name, sizeof(_function_name), "func_%s", node->function.name);
    return _function_name;
}

void generate_function_call(Node *node) {
    size_t total_size = 0;
    auto n = node->call.args->size();
    for (int i = n-1; i >= 0; --i) {
        auto arg = node->call.args->at(i);
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
    char *name = get_function_name(node->call.function);
    emit_asm3("\tcall ", name, "\n");
    emit_asm("\tadd rsp, ");
    emit_num(total_size);
    emit_asm("\n");
}

const char * specifier_for_type(Type * type) {
    auto n = size_of_type(type);
    if (n == 1) return "byte";
    if (n == 2) return "word";
    if (n == 4) return "dword";
    if (n == 8) return "qword";
    std::cerr << "Unknown type size: " << n << " in specifier_for_type()" << std::endl;
    exit(1);
}

const char * subregister_for_type(Type * type) {
    auto n = size_of_type(type);
    if (n == 1) return "al";
    if (n == 2) return "ax";
    if (n == 4) return "eax";
    if (n == 8) return "rax";
    std::cerr << "Unknown type size: " << n << " in subregister_for_type()" << std::endl;
    exit(1);
}

void generate_lvalue_into_rax(Node * node) {
    if (node->type == AST_LOCAL_VAR) {
        auto offset = node->variable->offset;
        emit_asm("\tmov rax, rbp\n");
        emit_asm("\tsub rax, "); emit_num(offset); emit_asm("\n");
    } else if (node->type == AST_GLOBAL_VAR) {
        auto offset = node->variable->offset;
        emit_asm("\tmov rax, qword gvars\n");
        emit_asm("\tadd rax, "); emit_num(offset); emit_asm("\n");
    } else if (node->type == AST_MEMBER) {
        // TODO: Implement struct members
        // let offset = node->member.offset;
        // if (node.member.is_ptr)
        //     generate_expression(node.member.obj);
        // else
        //     generate_lvalue_into_rax(node.member.obj);
        // emit_asm("\tadd rax, "); emit_num(offset); emit_asm("\n");
    } else if (node->type == AST_DEREF) {
        generate_expression(node->expr);
    } else {
        std::cerr << "Unsupported type in generate_lvalue_into_rax: " << node->type << std::endl;
        exit(1);
    }
}

void generate_cmp_float(Node * node) {
    generate_expression(node->binary.rhs);
    emit_asm("\tsub rsp, 8\n");
    emit_asm("\tmovsd [rsp], xmm0\n");
    generate_expression(node->binary.lhs);
    emit_asm("\tmovsd xmm1, [rsp]\n");
    emit_asm("\tadd rsp, 8\n");
    emit_asm("\tucomisd xmm0, xmm1\n");
}

void generate_cmp_int(Node * node) {
    generate_expression(node->binary.rhs);
    emit_asm("\tpush rax\n");
    generate_expression(node->binary.lhs);
    emit_asm("\tpop rcx\n");
    emit_asm("\tcmp rax, rcx\n");
}

void generate_binop_float_arith(Node * node) {
    generate_expression(node->binary.rhs);
    emit_asm("\tsub rsp, 8\n");
    emit_asm("\tmovsd [rsp], xmm0\n");
    generate_expression(node->binary.lhs);
    emit_asm("\tmovsd xmm1, [rsp]\n");
    emit_asm("\tadd rsp, 8\n");

    char* op = nullptr;
    if (node->type == AST_PLUS) op = "addsd";
    else if (node->type == AST_MINUS) op = "subsd";
    else if (node->type == AST_MUL) op = "mulsd";
    else if (node->type == AST_DIV) op = "divsd";
    else {
        std::cerr << "Unsupported binary op in generate_binop_float_arith: " << node->type << std::endl;
        exit(1);
    }

    emit_asm3("\t", op, " xmm0, xmm1\n");
}

void generate_binop_int_arith(Node * node) {
    generate_expression(node->binary.rhs);
    emit_asm("\tpush rax\n");
    generate_expression(node->binary.lhs);
    emit_asm("\tpop rcx\n");

    char* op = nullptr;
    if (node->type == AST_PLUS) op = "add";
    else if (node->type == AST_MINUS) op = "sub";
    else if (node->type == AST_LSHIFT) op = "shl";
    else if (node->type == AST_RSHIFT) op = "shr";
    else if (node->type == AST_BWAND) op = "and";
    else if (node->type == AST_BWOR) op = "or";
    else if (node->type == AST_XOR) op = "xor";
    else if (node->type == AST_MUL) op = "imul";
    else if (node->type == AST_DIV) op = "idiv";
    else if (node->type == AST_MOD) op = "idiv";
    else {
        std::cerr << "Unsupported binary op in generate_binop_int_arith: " << node->type << std::endl;
        exit(1);
    }

    if (node->type == AST_DIV || node->type == AST_MOD) {
        emit_asm("\tcqo\n");
        emit_asm("\tidiv rcx\n");
        if (node->type == AST_MOD)
            emit_asm("\tmov rax, rdx\n");
    } else if (node->type == AST_LSHIFT || node->type == AST_RSHIFT) {
        emit_asm3("\t", op, " rax, cl\n");
    } else {
        emit_asm3("\t", op, " rax, rcx\n");
    }
}

void generate_expression(Node *node) {
    if (node->type == AST_LITERAL) {
        if (is_int_type(node->etype)) {
            emit_asm("\tmov rax, ");
            emit_num(node->literal.as_int);
            emit_asm("\n");
        } else if (node->etype->base == TYPE_POINTER) {
            auto s = node->literal.as_string;
            auto idx = gen_string_literals.size(); // TODO: check for duplicates
            gen_string_literals.push_back(s);
            emit_asm("\tmov rax, qword gs_");
            emit_num(idx);
            emit_asm("\n");
        } else if (is_float_type(node->etype)) {
            auto idx = gen_float_literals.size(); // TODO: check for duplicates
            gen_float_literals.push_back(node->literal.as_string);
            emit_asm("\tmov rax, [qword gf_");
            emit_num(idx);
            emit_asm("]\n");
            emit_asm("\tmov [rsp-8], rax\n");
            emit_asm("\tmovsd xmm0, [rsp-8]\n");
        } else {
            std::cerr << "Unsupported literal type in generate_expression: " << node->etype->base << std::endl;
            exit(1);
        }
    } else if (node->type == AST_ADDRESS_OF) {
        generate_lvalue_into_rax(node->expr);
    } else if (node->type == AST_CONDITIONAL) {
        auto label = ++gen_label_counter;
        generate_expression(node->conditional.condition);
        emit_asm("\tcmp rax, 0\n");
        emit_asm("\tje .cond_else_"); emit_num(label); emit_asm("\n");
        generate_expression(node->conditional.then);
        emit_asm("\tjmp .cond_end_"); emit_num(label); emit_asm("\n");
        emit_asm(".cond_else_"); emit_num(label); emit_asm(":\n");
        generate_expression(node->conditional.els);
        emit_asm(".cond_end_"); emit_num(label); emit_asm(":\n");
    } else if (node->type == AST_OR) {
        // With short circuiting!
        auto label = ++gen_label_counter;
        generate_expression(node->binary.lhs);
        // If left is true, we can short-circuit
        emit_asm("\tcmp rax, 0\n");
        emit_asm("\tje .or_right_"); emit_num(label); emit_asm("\n");
        emit_asm("\tmov rax, 1\n");
        emit_asm("\tjmp .or_end_"); emit_num(label); emit_asm("\n");
        emit_asm(".or_right_"); emit_num(label); emit_asm(":\n");
        generate_expression(node->binary.rhs);
        // Booleanize the result
        emit_asm("\tcmp rax, 0\n");
        emit_asm("\tsetne al\n");
        emit_asm(".or_end_"); emit_num(label); emit_asm(":\n");
    } else if (node->type == AST_AND) {
        auto label = ++gen_label_counter;
        generate_expression(node->binary.lhs);
        // If left is false, we can short-circuit
        emit_asm("\tcmp rax, 0\n");
        emit_asm("\tjne .and_right_"); emit_num(label); emit_asm("\n");
        emit_asm("\tmov rax, 0\n");
        emit_asm("\tjmp .and_end_"); emit_num(label); emit_asm("\n");
        emit_asm(".and_right_"); emit_num(label); emit_asm(":\n");
        generate_expression(node->binary.rhs);
        // Booleanize the result
        emit_asm("\tcmp rax, 0\n");
        emit_asm("\tsetne al\n");
        emit_asm(".and_end_"); emit_num(label); emit_asm(":\n");
    } else if (is_lvalue(node->type)) {
        auto node_sz = size_of_type(node->etype);
        generate_lvalue_into_rax(node);
        if (is_float_type(node->etype)) {
            emit_asm("\tmovsd xmm0, [rax]\n");
        } else if (node_sz == 8) {
            emit_asm("\tmov rax, [rax]\n");
        } else if (node_sz == 4) {
            emit_asm("\tmovsxd rax, dword [rax]\n");
        } else {
            emit_asm3("\tmovsx rax, ", specifier_for_type(node->etype), " [rax]\n");
        }
    } else if (is_binary_op(node->type)) {
        char* s_op;
        char* u_op;
        switch (node->type) {
            case AST_EQ: s_op = "sete"; u_op = "sete"; break;
            case AST_LT: s_op = "setl"; u_op = "setb"; break;
            case AST_GT: s_op = "setg"; u_op = "seta"; break;
            case AST_NEQ: s_op = "setne"; u_op = "setne"; break;
            case AST_LEQ: s_op = "setle"; u_op = "setna"; break;
            case AST_GEQ: s_op = "setge"; u_op = "setnb"; break;

            default: {
                if (is_float_type(node->etype)) {
                    return generate_binop_float_arith(node);
                }
                // Assume it's a an arithmetic operation
                return generate_binop_int_arith(node);
            }
        }

        if (is_float_type(node->binary.rhs->etype)) {
            generate_cmp_float(node);
            emit_asm3("\t", u_op, " al\n");
        } else {
            generate_cmp_int(node);
            emit_asm3("\t", s_op, " al\n");
        }
        emit_asm("\tmovzx rax, al\n");
    } else if (node->type == AST_BWINV) {
        generate_expression(node->expr);
        emit_asm("\tnot rax\n");
    } else if (node->type == AST_NEG) {
        generate_expression(node->expr);
        if (is_float_type(node->expr->etype)) {
            emit_asm("\txorps xmm1, xmm1\n");
            emit_asm("\tsubps xmm1, xmm0\n");
            emit_asm("\tmovsd xmm0, xmm1\n");
        } else {
            emit_asm("\tneg rax\n");
        }
    } else if (node->type == AST_NOT) {
        generate_expression(node->expr);
        emit_asm("\tcmp rax, 0\n");
        emit_asm("\tsete al\n");
        emit_asm("\tmovzx rax, al\n");
    } else if (node->type == AST_ASSIGN) {
        auto var = node->assign.lhs;
        generate_lvalue_into_rax(var);
        emit_asm("\tpush rax\n");
        generate_expression(node->assign.rhs);
        emit_asm("\tpop rcx\n");
        if (is_float_type(node->assign.rhs->etype)) {
            emit_asm("\tmovsd [rcx], xmm0\n");
        } else {
            emit_asm3("\tmov [rcx], ", subregister_for_type(var->etype), "\n");
        }
    } else if (node->type == AST_FUNCTION_CALL) {
        generate_function_call(node);
    } else if (node->type == AST_CONVERT) {
        if (is_int_type(node->etype) && is_float_type(node->expr->etype)) {
            generate_expression(node->expr);
            emit_asm("\tcvttsd2si rax, xmm0\n");
        } else if (is_float_type(node->etype) && is_int_type(node->expr->etype)) {
            generate_expression(node->expr);
            emit_asm("\tcvtsi2sd xmm0, eax\n"); // TODO: Check if this is correct
        } else {
            std::cerr << "Unsupported conversion in generate_expression" << std::endl;
            exit(1);
        }
    } else {
        std::cerr << "Unsupported type in generate_expression: " << node->type << std::endl;
        exit(1);
    }
}

void generate_statement(Node *node) {
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
    } else if (node->type == AST_VAR_DECLARATION) {
        if (node->var_decl.init != nullptr) {
            generate_expression(node->var_decl.init);
            auto offset = node->var_decl.var.offset;
            if (is_float_type(node->var_decl.init->etype)) {
                emit_asm("\tmovsd [rbp-"); emit_num(offset); emit_asm("], xmm0\n");
            } else {
                emit_asm("\tmov [rbp-"); emit_num(offset); emit_asm3("], ", subregister_for_type(node->var_decl.var.type), "\n");
            }
        }
    } else if (node->type == AST_WHILE) {
        auto label = ++gen_label_counter;
        auto prev_break = gen_current_break;
        gen_current_break = label;
        emit_asm(".loop_start_"); emit_num(label); emit_asm(":\n");
        emit_asm(".loop_continue_"); emit_num(label); emit_asm(":\n");
        generate_expression(node->loop.condition);
        emit_asm("\tcmp rax, 0\n");
        emit_asm("\tje .loop_end_"); emit_num(label); emit_asm("\n");
        generate_statement(node->loop.body);
        emit_asm("\tjmp .loop_start_"); emit_num(label); emit_asm("\n");
        emit_asm(".break_"); emit_num(label); emit_asm(":\n");
        emit_asm(".loop_end_"); emit_num(label); emit_asm(":\n");
        gen_current_break = prev_break;
    } else if (node->type == AST_CONTINUE) {
        // TODO: Implement continue
        std::cerr << "Continue statement not implemented" << std::endl;
    } else if (node->type == AST_BREAK) {
        if (gen_current_break < 0) {
            std::cerr << "Break statement outside of loop, should have been caught by parser" << std::endl;
            exit(1);
        }

        // TODO: Implement deferred statements

        emit_asm("\tjmp .break_"); emit_num(gen_current_break); emit_asm("\n");
    } else {
        generate_expression(node);
    }
}

void generate_block(Node *node) {
    // TODO: Implement deferred statements
    for (auto child: *node->block.children) {
        generate_statement(child);
    }
}

void generate_function(Node *node) {
    auto name = get_function_name(node);
    emit_asm2(name, ":\n");
    emit_asm("\tpush rbp\n");
    emit_asm("\tmov rbp, rsp\n");
    emit_asm("\tsub rsp, ");
    emit_num(node->function.max_locals_size);
    emit_asm("\n");

    generate_block(node->function.body);

    emit_asm("\tmov rsp, rbp\n");
    emit_asm("\tpop rbp\n");
    // Return 0 by default if we don't have a return statement
    emit_asm("\txor rax, rax\n");
    emit_asm("\tret\n");
}

void generate_syscall(const int syscall) {
    emit_asm("\tmov rax, ");
    emit_num(syscall);
    emit_asm("\n");
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

    // Syscalls
    char *x86_64_sysc_regs[10];
    x86_64_sysc_regs[0] = "rax";
    x86_64_sysc_regs[1] = "rdi";
    x86_64_sysc_regs[2] = "rsi";
    x86_64_sysc_regs[3] = "rdx";
    x86_64_sysc_regs[4] = "rcx";
    x86_64_sysc_regs[5] = "r8";
    x86_64_sysc_regs[6] = "r9";
    x86_64_sysc_regs[7] = "r10";
    x86_64_sysc_regs[8] = "r11";
    x86_64_sysc_regs[9] = "r12";

    for (auto sysc_args = 0; sysc_args < 7; ++sysc_args) {
        emit_asm("func_syscall");
        emit_num(sysc_args);
        emit_asm(":\n");
        for (auto i = 0; i < sysc_args + 1; ++i) {
            emit_asm("\tmov ");
            emit_asm(x86_64_sysc_regs[i]);
            emit_asm(", [rsp+");
            emit_num((i + 1) * 8);
            emit_asm("]\n");
        }
        emit_asm("\tsyscall\n");
        if (OS_IS_MACOS) {
            emit_asm("\tjnc .L2\n");
            emit_asm("\tneg rax\n");
            emit_asm(".L2:\n");
        }
        emit_asm("\tret\n");
    }
}

void generate_program(Node *ast, FILE *file) {
    gen_out_file = file;

    auto n = ast->block.children->size();
    for (size_t i = 0; i < n; i++) {
        auto node = ast->block.children->at(i);
        if (node->type == AST_FUNCTION) {
            generate_function(node);
        } else if (node->type == AST_VAR_DECLARATION) {
            // Do nothing
        } else {
            std::cerr << "Unknown node type: " << node->type << " in generate_program()" << std::endl;
            exit(1);
        }
    }

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
    for (const auto node : *ast->block.children) {
        if (node->type == AST_VAR_DECLARATION && node->var_decl.init != nullptr) {
            const auto expr = node->var_decl.init;
            generate_expression(expr);
            const auto offset = node->var_decl.var.offset;
            emit_asm("\tmov rcx, qword gvars\n");
            emit_asm("\tadd rcx, "); emit_num(offset); emit_asm("\n");
            emit_asm3("\tmov [rcx], ", subregister_for_type(expr->etype), "\n");
        }
    }


    emit_asm("\tcall func_main\n");
    emit_asm("\tmov rdi, rax\n");
    generate_syscall(SYSCALL_EXIT);

    generate_builtins();

    emit_asm("section .bss\n");
    emit_asm("\tgvars: resb "); emit_num(global_offset); emit_asm("\n");

    // Global strings
    emit_asm("section .data\n");
    for (size_t i = 0; i < gen_string_literals.size(); ++i) {
        emit_asm("\tgs_");
        emit_num(i);
        emit_asm(": db ");
        auto s = gen_string_literals[i];
        while (*s) {
            emit_num(*s);
            emit_asm(",");
            ++s;
        }
        emit_asm("0\n");
    }
    for (auto i = 0; i < gen_float_literals.size(); ++i) {
        emit_asm("\tgf_"); emit_num(i);
        emit_asm3(": dq ", gen_float_literals.at(i), "\n");
    }
}
