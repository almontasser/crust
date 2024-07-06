//
// Created by mahmoud on 7/4/24.
//

#include "parser.h"

#include <assert.h>
#include <cstring>
#include <cstdlib>
#include <iostream>
#include <ostream>

#include "types.h"

auto builtin_functions = std::vector<Node *>();
auto all_functions = std::vector<Node *>();

auto lexer_stack = std::vector<Lexer *>();
auto current_lexer_index = 0;
Node *current_function = nullptr;
auto block_stack = std::vector<Node *>();
size_t curr_stack_offset = 0;
auto breakable_stack = std::vector<Node *>();


Node *parse_expression(Lexer *lexer);

size_t align_up(size_t val, int align) {
    return (val + align - 1) & ~(align - 1);
}

void builtin_create_syscall(const char *name, int num_args) {
    const auto node = new_node(AST_BUILTIN);
    node->etype = new_type(TYPE_ANY);
    node->function.name = static_cast<char*>(malloc(strlen(name) + 1));
    strcpy(node->function.name, name);
    node->function.args = new std::vector<Variable *>();
    node->function.args->push_back(new_variable("val", new_type(TYPE_U64), 0)); // syscall number
    for (int i = 0; i < num_args; i++) {
        node->function.args->push_back(new_variable("arg", new_type(TYPE_ANY), 0));
    }
    builtin_functions.push_back(node);
}

void initialize_builtins() {
    Node *node = new_node(AST_BUILTIN);
    node->etype = new_type(TYPE_VOID);
    node->function.name = "print";
    node->function.args = new std::vector<Variable *>();
    node->function.args->push_back(new_variable("val", new_type(TYPE_ANY), 0));
    builtin_functions.push_back(node);

    builtin_create_syscall("syscall0", 0);
    builtin_create_syscall("syscall1", 1);
    builtin_create_syscall("syscall2", 2);
    builtin_create_syscall("syscall3", 3);
    builtin_create_syscall("syscall4", 4);
    builtin_create_syscall("syscall5", 5);
    builtin_create_syscall("syscall6", 6);
    builtin_create_syscall("syscall7", 7);
}

Type *parse_type(Lexer *lexer) {
    auto token = lexer->next();
    Type *type;

    static_assert(NUM_BASE_TYPES == 13, "Exhaustive match in parse_type()");
    switch (token->type) {
        case TOKEN_I8: type = new_type(TYPE_I8);
            break;
        case TOKEN_I16: type = new_type(TYPE_I16);
            break;
        case TOKEN_I32: type = new_type(TYPE_I32);
            break;
        case TOKEN_I64: type = new_type(TYPE_I64);
            break;
        case TOKEN_U8: type = new_type(TYPE_U8);
            break;
        case TOKEN_U16: type = new_type(TYPE_U16);
            break;
        case TOKEN_U32: type = new_type(TYPE_U32);
            break;
        case TOKEN_U64: type = new_type(TYPE_U64);
            break;
        case TOKEN_F32: type = new_type(TYPE_F32);
            break;
        case TOKEN_F64: type = new_type(TYPE_F64);
            break;
        case TOKEN_BOOL: type = new_type(TYPE_BOOL);
            break;
        case TOKEN_VOID: type = new_type(TYPE_VOID);
            break;
        default: {
            if (token->type != TOKEN_IDENTIFIER) {
                std::cerr << "Unknown type: " << token->type << " at " << token->location->filename << ":" << token->
                        location->line << ":" << token->location->column << std::endl;
                exit(1);
            }
            // TODO: find compound type
            exit(1);
        }
    }

    while (true) {
        token = lexer->peek();
        if (token->type == TOKEN_STAR) {
            lexer->next();
            auto ptr = new_type(TYPE_POINTER);
            ptr->ptr = type;
            type = ptr;
        } else if (token->type == TOKEN_LBRACKET) {
            lexer->next();
            auto array = new_type(TYPE_ARRAY);
            array->ptr = type;
            array->array_size = 0; // TODO: parse array size
            lexer->expect(TOKEN_RBRACKET);
            type = array;
        } else {
            break;
        }
    }

    return type;
}

void block_stack_push(Node *node) {
    block_stack.push_back(node);
}

void block_stack_pop() {
    auto block = block_stack.back();
    block_stack.pop_back();
    curr_stack_offset -= block->block.locals->size();
}

Node *find_function(char *name, const std::vector<Node *> &nodes) {
    for (const auto node: nodes) {
        if (strcmp(node->function.name, name) == 0) {
            return node;
        }
    }
    return nullptr;
}

Variable *find_local_variable(char *name) {
    if (current_function == nullptr) return nullptr;

    auto n = block_stack.size();
    for (int i = n-1; i >= 0; --i) {
        auto block = block_stack[i];
        for (auto var: *block->block.locals) {
            if (strcmp(var->name, name) == 0) {
                return var;
            }
        }
    }

    for (auto arg: *current_function->function.args) {
        if (strcmp(arg->name, name) == 0) {
            return arg;
        }
    }

    return nullptr;
}

bool identifier_exists(char *name) {
    if (find_local_variable(name) != nullptr) return true;
    if (find_function(name, builtin_functions) != nullptr) return true;
    if (find_function(name, all_functions) != nullptr) return true;
    return false;
}


// This is used for functions and methods, for methods obj_ptr represents self
Node *parse_function_call_args(Lexer *lexer, Node *func, Node *obj_ptr) {
    Node *node = new_node(AST_FUNCTION_CALL);
    node->call.function = func;
    node->call.args = new std::vector<Node *>();
    node->etype = func->etype;

    lexer->expect(TOKEN_LPAREN);

    if (obj_ptr != nullptr) {
        node->call.args->push_back(obj_ptr);
    }

    auto token = lexer->peek();
    while (token->type != TOKEN_RPAREN) {
        auto arg = parse_expression(lexer);
        node->call.args->push_back(arg);

        token = lexer->peek();
        if (token->type == TOKEN_COMMA) {
            lexer->next();
            token - lexer->peek();
        }
    }
    lexer->expect(TOKEN_RPAREN);
    if (node->call.args->size() != func->function.args->size()) {
        std::cerr << "Expected " << func->function.args->size() << " arguments at " << token->location->filename << ":"
                << token->location->line << ":" << token->location->column << std::endl;
        exit(1);
    }

    for (size_t i = 0; i < node->call.args->size(); i++) {
        auto arg = node->call.args->at(i);
        auto var = func->function.args->at(i);

        auto conv = convert_type(var->type, arg);
        if (conv == nullptr) {
            std::cerr << "Cannot convert " << arg->etype->base << " to " << var->type->base << " at " << token->location
                    ->filename << ":"
                    << token->location->line << ":" << token->location->column << std::endl;
            exit(1);
        }
        node->call.args->at(i) = conv;
    }

    return node;
}

Node *parse_identifier(Lexer *lexer) {
    auto token = lexer->expect(TOKEN_IDENTIFIER);
    auto name = token->value.as_string;

    Node *node;
    auto var = find_local_variable(name);
    if (var != nullptr) {
        node = new_node(AST_LOCAL_VAR);
        node->variable = var;
        node->etype = var->type;
        return decay_array_to_pointer(node, token);
    }

    auto func = find_function(name, builtin_functions);
    if (func != nullptr) {
        return parse_function_call_args(lexer, func, nullptr);
    }

    func = find_function(name, all_functions);
    if (func != nullptr) {
        return parse_function_call_args(lexer, func, nullptr);
    }

    // TODO: check for local and global variables
    std::cerr << "Unknown identifier: " << name << " at " << token->location->filename << ":" << token->location->line
            << ":" << token->location->column << std::endl;
    exit(1);
}

Node *parse_literal(Lexer *lexer) {
    auto node = new_node(AST_LITERAL);
    auto token = lexer->next();

    switch (token->type) {
        case TOKEN_INTLIT:
            node->literal.as_int = token->value.as_int;
            node->etype = new_type(TYPE_I64);
            break;
        case TOKEN_STRINGLIT:
            node->literal.as_string = token->value.as_string;
            node->etype = new_ptr_type(TYPE_U8); // TODO: Maybe add char type
            break;
        // TODO: Add floats
        default:
            std::cerr << "Unknown literal type: " << token->type << " at " << token->location->filename << ":" << token
                    ->
                    location->line << ":" << token->location->column << std::endl;
            exit(1);
    }
    return node;
}

Node *parse_factor(Lexer *lexer) {
    Node *expr = nullptr;

    auto token = lexer->peek();
    if (is_literal_token(token->type)) {
        expr = parse_literal(lexer);
    } else if (token->type == TOKEN_IDENTIFIER) {
        expr = parse_identifier(lexer);
    } else if (token->type == TOKEN_STAR) {
        lexer->next();

        auto subexp = parse_factor(lexer);
        if (subexp->etype->base != TYPE_POINTER) {
            std::cerr << "Cannot dereference non-pointer type at " << token->location->filename << ":" << token->
                    location->line
                    << ":" << token->location->column << std::endl;
            exit(1);
        }

        expr = new_node(AST_DEREF);
        expr->expr = subexp;
        expr = type_check_unary(expr, token);
    } else {
        std::cerr << "Unexpected token: " << token->type << " at " << token->location->filename << ":" << token->
                location->line << ":" << token->location->column << std::endl;
        exit(1);
    }

    return expr;
}

Node *parse_term(Lexer *lexer) {
    auto lhs = parse_factor(lexer);
    // TODO: parse TOKEN_STAR, TOKEN_SLASH, TOKEN_PERCENT
    return lhs;
}

Node *parse_additive(Lexer *lexer) {
    auto lhs = parse_term(lexer);
    // TODO: parse TOKEN_MINUS, TOKEN_LSHIFT, TOKEN_RSHIFT
    auto token = lexer->peek();
    while (token->type == TOKEN_PLUS) {
        lexer->next();
        auto op = new_node(binary_token_to_op(token->type));
        auto rhs = parse_term(lexer);
        op->binary.lhs = lhs;
        op->binary.rhs = rhs;
        lhs = type_check_binary(op, token);
        token = lexer->peek();
    }
    return lhs;
}

Node *parse_relational(Lexer *lexer) {
    auto lhs = parse_additive(lexer);
    // TODO: parse TOKEN_LT, TOKEN_GT, TOKEN_LE, TOKEN_GE
    return lhs;
}

Node *parse_equality(Lexer *lexer) {
    auto lhs = parse_relational(lexer);
    auto token = lexer->peek();
    while (token->type == TOKEN_EQ || token->type == TOKEN_NEQ) {
        lexer->next();
        auto op = new_node(binary_token_to_op(token->type));
        auto rhs = parse_equality(lexer);
        op->binary.lhs = lhs;
        op->binary.rhs = rhs;
        lhs = type_check_binary(op, token);
        token = lexer->peek();
    }
    return lhs;
}

Node *parse_and(Lexer *lexer) {
    auto lhs = parse_equality(lexer);
    // TODO: parse TOKEN_AMPERSAND
    return lhs;
}

Node *parse_exclusive_or(Lexer *lexer) {
    auto lhs = parse_and(lexer);
    // TODO: parse TOKEN_CARET
    return lhs;
}

Node *parse_inclusive_or(Lexer *lexer) {
    auto lhs = parse_exclusive_or(lexer);
    // TODO: parse TOKEN_BAR
    return lhs;
}

Node *parse_logical_and(Lexer *lexer) {
    auto lhs = parse_inclusive_or(lexer);
    // TODO: parse TOKEN_AND
    return lhs;
}

Node *parse_logical_or(Lexer *lexer) {
    auto lhs = parse_logical_and(lexer);
    // TODO: parse TOKEN_OR
    return lhs;
}

Node *prase_conditional_expression(Lexer *lexer) {
    auto lhs = parse_logical_or(lexer);
    // TODO: parse ternary operator ?:
    return lhs;
}

Node *parse_expression(Lexer *lexer) {
    auto lhs = prase_conditional_expression(lexer);
    if (is_lvalue(lhs->type)) {
        auto token = lexer->peek();
        if (token->type == TOKEN_ASSIGN) {
            lexer->next();
            auto node = new_node(AST_ASSIGN);
            auto rhs = parse_expression(lexer);

            auto conv = convert_type(lhs->etype, rhs);
            if (conv == nullptr) {
                std::cerr << "Cannot convert " << rhs->etype->base << " to " << lhs->etype->base << " at " << token->
                        location->filename << ":"
                        << token->location->line << ":" << token->location->column << std::endl;
                exit(1);
            }

            node->etype = lhs->etype;
            node->assign.lhs = lhs;
            node->assign.rhs = rhs;
            lhs = node;
        }
    }
    return lhs;
}

void add_variable_to_current_block(Variable *var) {
    assert(current_function != nullptr);

    // Set offset for variable
    Node *cur_block = block_stack.back();
    auto var_size = align_up(size_of_type(var->type), 8);

    // Add to the block
    // FIXME: Use a map here
    cur_block->block.locals->push_back(var);
    // Update current stack offset (w.r.t function stack frame) and block size
    curr_stack_offset += var_size;
    cur_block->block.locals_size = cur_block->block.locals_size + var_size;
    var->offset = curr_stack_offset;

    // Update function's max locals size
    auto max_offset = std::max(current_function->function.max_locals_size, curr_stack_offset);
    current_function->function.max_locals_size = max_offset;
}

Node *parse_var_declaration(Lexer *lexer) {
    lexer->expect(TOKEN_LET);
    auto token = lexer->expect(TOKEN_IDENTIFIER);

    if (identifier_exists(token->value.as_string)) {
        std::cerr << "Identifier " << token->value.as_string << " already exists at " << token->location->filename <<
                ":" << token->
                location->line << ":" << token->location->column << std::endl;
        exit(1);
    }

    auto is_global = current_function == nullptr;

    auto node = new_node(AST_VAR_DECLARATION);
    node->var_decl.var.name = token->value.as_string;

    auto missing_type = true;
    token = lexer->peek();
    if (token->type == TOKEN_COLON) {
        lexer->next();
        node->var_decl.var.type = parse_type(lexer);
        missing_type = false;
        token = lexer->peek();
    }

    if (token->type == TOKEN_ASSIGN) {
        lexer->next();
        node->var_decl.init = parse_expression(lexer);

        if (missing_type) {
            node->var_decl.var.type = node->var_decl.init->etype;
        } else {
            auto conv = convert_type(node->var_decl.var.type, node->var_decl.init);
            if (conv == nullptr) {
                std::cerr << "Cannot convert " << node->var_decl.init->etype->base << " to " << node->var_decl.var.type
                        ->base << " at " << token->location->filename << ":"
                        << token->location->line << ":" << token->location->column << std::endl;
                exit(1);
            }
            node->var_decl.init = conv;
        }
        node->etype = node->var_decl.init->etype;
    } else if (missing_type) {
        std::cerr << "Missing type for variable " << node->var_decl.var.name << " at " << token->location->filename <<
                ":" << token->location->line
                << ":" << token->location->column << std::endl;
        exit(1);
    }

    if (is_global) {
        // TODO: add to global variables
    } else {
        add_variable_to_current_block(&node->var_decl.var);
    }

    return node;
}

Node *parse_statement(Lexer *lexer) {
    Node *node = nullptr;

    auto token = lexer->peek();
    if (token->type == TOKEN_LBRACE) {
        node = parse_block(lexer);
        token = lexer->peek();
        if (token->type == TOKEN_SEMICOLON) {
            lexer->next();
        }
    } else if (token->type == TOKEN_RETURN) {
        lexer->next();
        node = new_node(AST_RETURN);

        token = lexer->peek();
        if (token->type != TOKEN_SEMICOLON) {
            // TODO: check for constructor

            auto expr = parse_expression(lexer);
            auto convert = convert_type(current_function->etype, expr);
            if (!convert) {
                std::cerr << "Cannot convert " << expr->etype->base << " to " << current_function->etype->base << " at "
                        << token->location->filename << ":" << token->location->line << ":" << token->location->column
                        << std::endl;
                exit(1);
            }
            node->expr = convert;
        } else {
            node->expr = nullptr;
            // TODO: check for constructor
            if (current_function->etype->base != TYPE_VOID) {
                std::cerr << "Expected return value at " << token->location->filename << ":" << token->location->line
                        << ":" << token->location->column << std::endl;
                exit(1);
            } // TODO: check for constructor
        }
        node->etype = current_function->etype;
        lexer->expect(TOKEN_SEMICOLON);
    } else if (token->type == TOKEN_LET) {
        node = parse_var_declaration(lexer);
        lexer->expect(TOKEN_SEMICOLON);
    } else if (token->type == TOKEN_WHILE) {
        lexer->next();
        node = new_node(AST_WHILE);
        lexer->expect(TOKEN_LPAREN);
        node->loop.condition = parse_expression(lexer);
        lexer->expect(TOKEN_RPAREN);

        breakable_stack.push_back(node);
        node->loop.body = parse_statement(lexer);
        breakable_stack.pop_back();
    } else if (token->type == TOKEN_CONTINUE) {
        if (breakable_stack.empty()) {
            std::cerr << "Unexpected continue at " << token->location->filename << ":" << token->location->line << ":"
                    << token->location->column << std::endl;
            exit(1);
        }
        lexer->next();
        node = new_node(AST_CONTINUE);
        lexer->expect(TOKEN_SEMICOLON);
    } else if (token->type == TOKEN_BREAK) {
        if (breakable_stack.empty()) {
            std::cerr << "Unexpected break at " << token->location->filename << ":" << token->location->line << ":" <<
                    token->location->column << std::endl;
            exit(1);
        }
        lexer->next();
        // TODO: Implement breaking of multiple loops
        node = new_node(AST_BREAK);
        lexer->expect(TOKEN_SEMICOLON);
    } else {
        // Default to expression statement
        node = parse_expression(lexer);
        lexer->expect(TOKEN_SEMICOLON);
    }

    return node;
}

Node *parse_block(Lexer *lexer) {
    lexer->expect(TOKEN_LBRACE);

    auto block = new_node(AST_BLOCK);
    block->block.children = new std::vector<Node *>();
    block->block.locals = new std::vector<Variable *>();

    block_stack_push(block);

    auto token = lexer->peek();
    while (token->type != TOKEN_RBRACE) {
        auto child = parse_statement(lexer);

        if (child != nullptr) {
            block->block.children->push_back(child);
        }
        token = lexer->peek();
    }
    lexer->expect(TOKEN_RBRACE);

    block_stack_pop();
    return block;
}

void parse_function_params(Lexer *lexer, Node *func) {
    func->function.args = new std::vector<Variable *>();

    auto token = lexer->peek();
    while (token->type != TOKEN_RPAREN) {
        token = lexer->expect(TOKEN_IDENTIFIER);
        auto name = token->value.as_string;

        if (identifier_exists(name)) {
            std::cerr << "Identifier " << name << " already exists at " << token->location->filename << ":" << token->
                    location->line << ":" << token->location->column << std::endl;
            exit(1);
        }

        lexer->expect(TOKEN_COLON);
        auto type = parse_type(lexer);

        auto var = new_variable(name, type, 0);
        func->function.args->push_back(var);

        token = lexer->peek();
        if (token->type == TOKEN_COMMA) {
            lexer->next();
            token = lexer->peek();
        }
    }

    // Set the offsets for the arguments

    // IMPORTANT: We want to skip the saved ret_addr+old_rbp that we
    //            pushed on the stack. Each of these is 8 bytes.
    auto offset = -16;
    for (size_t i = 0; i < func->function.args->size(); i++) {
        auto arg = func->function.args->at(i);
        arg->offset = offset;
        // TODO: (Here and other uses of `size_for_type`):
        //      Should we only align to max(8, type.size) instead?
        auto var_size = align_up(size_of_type(arg->type), 8);
        offset -= var_size;
    }
}

Node *parse_function(Lexer *lexer) {
    lexer->expect(TOKEN_FN);
    const auto name = lexer->expect(TOKEN_IDENTIFIER);

    const auto func = new_node(AST_FUNCTION);
    func->function.name = name->value.as_string;

    // TODO: Check if function exists
    if (identifier_exists(func->function.name)) {
        std::cerr << "Function " << func->function.name << " already exists" << std::endl;
        exit(1);
    } else {
        all_functions.push_back(func);
    }

    current_function = func;

    lexer->expect(TOKEN_LPAREN);
    parse_function_params(lexer, func);
    lexer->expect(TOKEN_RPAREN);

    auto token = lexer->peek();
    if (token->type == TOKEN_COLON) {
        lexer->next();
        func->etype = parse_type(lexer);
    } else {
        func->etype = new_type(TYPE_VOID);
    }

    func->function.body = parse_block(lexer);

    // // Skip function body
    // lexer->expect(TOKEN_LBRACE);
    // auto lbrace_count = 1;
    // while (lbrace_count > 0) {
    //     token = lexer->next();
    //     if (token->type == TOKEN_LBRACE) {
    //         lbrace_count++;
    //     } else if (token->type == TOKEN_RBRACE) {
    //         lbrace_count--;
    //     }
    // }

    return func;
}

Node *parse_program(Lexer *lexer) {
    initialize_builtins();

    const auto node = new_node(AST_PROGRAM);
    node->block.children = new std::vector<Node *>();

    lexer_stack.push_back(lexer);

    // first pass
    auto token = lexer->peek();
    while (token->type != TOKEN_EOF) {
        Node *child = nullptr;
        if (token->type == TOKEN_IMPORT) {
            lexer->next();
            auto path = lexer->expect(TOKEN_STRINGLIT);

            auto already_imported = false;
            const auto absolute_path = canonicalize_file_name(path->value.as_string);
            for (const auto l: lexer_stack) {
                if (strcmp(l->filename, absolute_path) == 0) {
                    already_imported = true;
                    break;
                }
            }

            if (!already_imported) {
                lexer_stack.push_back(Lexer::create_from_file(absolute_path));
                lexer = lexer_stack[++current_lexer_index];
            }
        } else if (token->type == TOKEN_FN) {
            child = parse_function(lexer);
            std::cout << "Function: " << child->function.name << std::endl;
        }

        if (child != nullptr) {
            node->block.children->push_back(child);
        }

        token = lexer->peek();
        while (token->type == TOKEN_EOF && current_lexer_index > 0) {
            current_lexer_index--;
            lexer = lexer_stack[current_lexer_index];
            token = lexer->peek();
        }
    }

    return node;
}
