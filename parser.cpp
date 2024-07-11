//
// Created by mahmoud on 7/4/24.
//

#include "parser.h"

#include <assert.h>
#include <cstring>
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <ostream>

#include "types.h"

auto builtin_functions = std::vector<Node *>();
auto all_functions = std::vector<Node *>();

auto lexer_stack = std::vector<Lexer *>();
auto imported_files = std::vector<std::string>();
Node *current_function = nullptr;
auto block_stack = std::vector<Node *>();
size_t curr_stack_offset = 0;
auto breakable_stack = std::vector<Node *>();
auto compound_types = std::vector<Type *>();
auto constants = std::vector<Node *>();

// Default memory allocator for new
char* default_allocator = "malloc";
char* default_deallocator = "free";

Node *parse_expression(Lexer *lexer);

Type *find_compound_type(char *name);

Node *parse_statement(Lexer *lexer);

int64_t parse_constant_expression(Lexer *lexer);

void builtin_create_syscall(const char *name, int num_args) {
    const auto node = new_node(AST_BUILTIN);
    node->etype = new_type(TYPE_ANY);
    node->function.name = static_cast<char *>(malloc(strlen(name) + 1));
    strcpy(node->function.name, name);
    node->function.is_method = false;
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
    node->function.is_method = false;
    node->function.is_constructor = false;
    node->function.is_disposer = false;
    node->function.args = new std::vector<Variable *>();
    node->function.args->push_back(new_variable("val", new_type(TYPE_ANY), 0));
    builtin_functions.push_back(node);

    node = new_node(AST_BUILTIN);
    node->etype = new_type(TYPE_F64);
    node->function.name = "sqrt";
    node->function.args = new std::vector<Variable *>();
    node->function.args->push_back(new_variable("val", new_type(TYPE_F64), 0));
    builtin_functions.push_back(node);

    // The return value of fork() is weird on macOS, parent/child is returned in rdx
    // and the child pid is returned in rax, so we'll make our own wrapper.
    node = new_node(AST_BUILTIN);
    node->etype = new_type(TYPE_U64);
    node->function.name = "fork";
    node->function.args = new std::vector<Variable *>();
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
                std::cerr << "Unknown type: " << token->value.as_string << " at " << token->location->filename << ":" << token->
                        location->line << ":" << token->location->column << std::endl;
                exit(1);
            }
            // TODO: find compound type
            type = find_compound_type(token->value.as_string);
            if (type == nullptr) {
                std::cerr << "Unknown type: " << token->value.as_string << " at " << token->location->filename << ":" << token->
                        location->line << ":" << token->location->column << std::endl;
                exit(1);
            }
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
            array->array_size = parse_constant_expression(lexer);
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
    curr_stack_offset -= block->block.locals_size;
}

Node *find_function(char *name, const std::vector<Node *> *nodes) {
    for (const auto node: *nodes) {
        if (strcmp(node->function.name, name) == 0) {
            return node;
        }
    }
    return nullptr;
}

Variable *find_local_variable(char *name) {
    if (current_function == nullptr) return nullptr;

    auto n = block_stack.size();
    for (int i = n - 1; i >= 0; --i) {
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

Variable *find_global_variable(char *name) {
    for (auto var: global_variables) {
        if (strcmp(var->name, name) == 0) {
            return var;
        }
    }
    return nullptr;
}

Type *find_compound_type(char *name) {
    for (auto type: compound_types) {
        if (strcmp(type->struct_name, name) == 0) {
            return type;
        }
    }
    return nullptr;
}

Node *find_constant(char *name, const std::vector<Node *> *constants_vector) {
    for (auto constant: *constants_vector) {
        if (strcmp(constant->constant.name, name) == 0) {
            return constant;
        }
    }
    return nullptr;
}

bool identifier_exists(char *name) {
    if (find_local_variable(name) != nullptr) return true;
    if (find_global_variable(name) != nullptr) return true;
    if (find_function(name, &builtin_functions) != nullptr) return true;
    if (find_function(name, &all_functions) != nullptr) return true;
    if (find_compound_type(name) != nullptr) return true;
    if (find_constant(name, &constants) != nullptr) return true;
    return false;
}


// This is used for functions and methods, for methods obj_ptr represents self
Node *parse_function_call_args(Lexer *lexer, Node *func, Node *obj_ptr, bool is_disposer = false) {
    Node *node = new_node(AST_FUNCTION_CALL);
    node->call.function = func;
    node->call.args = new std::vector<Node *>();
    node->etype = func->etype;

    if (obj_ptr != nullptr) {
        node->call.args->push_back(obj_ptr);
    }

    auto token = lexer->peek();
    if (is_disposer == false) {
        lexer->expect(TOKEN_LPAREN);

        token = lexer->peek();
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
    }

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

    var = find_global_variable(name);
    if (var != nullptr) {
        node = new_node(AST_GLOBAL_VAR);
        node->variable = var;
        node->etype = var->type;
        return decay_array_to_pointer(node, token);
    }

    auto func = find_function(name, &builtin_functions);
    if (func != nullptr) {
        return parse_function_call_args(lexer, func, nullptr);
    }

    func = find_function(name, &all_functions);
    if (func != nullptr) {
        return parse_function_call_args(lexer, func, nullptr);
    }

    auto type = find_compound_type(name);
    if (type != nullptr) {
        if (type->base == TYPE_ENUM) {
            lexer->expect(TOKEN_COLON_COLON);
            token = lexer->expect(TOKEN_IDENTIFIER);
            auto constant = find_constant(token->value.as_string, type->variants);
            if (constant == nullptr) {
                std::cerr << "Variant \"" << token->value.as_string << "\" not found at " << token->location->filename
                        << ":" << token->location->line << ":" << token->location->column << std::endl;
                exit(1);
            }
            return constant;
        } else {
            std::cerr << "Not implemented at parse_identifier()" << std::endl;
            exit(1);
        }
    }

    auto constant = find_constant(name, &constants);
    if (constant != nullptr) {
        // TODO: make sure constant is an int literal
        return constant->constant.value;
    }

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
        case TOKEN_CHARLIT:
            node->literal.as_int = token->value.as_int;
            node->etype = new_type(TYPE_U8);
            break;
        case TOKEN_FLOATLIT:
            node->literal.as_string = token->value.as_string;
            node->etype = new_type(TYPE_F64);
            break;
        default:
            std::cerr << "Unknown literal type: " << token->type << " at " << token->location->filename << ":" << token
                    ->
                    location->line << ":" << token->location->column << std::endl;
            exit(1);
    }
    return node;
}

Node * call_allocator(Node * allocator, size_t size) {
    auto node = new_node(AST_FUNCTION_CALL);
    node->call.function = allocator;
    node->call.args = new std::vector<Node*>();
    node->etype = allocator->etype;

    auto size_node = node_from_int_literal(size);
    node->call.args->push_back(size_node);

    return node;
}

Node *parse_factor(Lexer *lexer) {
    Node *expr = nullptr;

    auto token = lexer->peek();
    if (token->type == TOKEN_MINUS) {
        lexer->next();
        expr = new_node(AST_NEG);
        expr->expr = parse_factor(lexer);
        expr = type_check_unary(expr, token);
    } else if (token->type == TOKEN_TILDE) {
        lexer->next();
        expr = new_node(AST_BWINV);
        expr->expr = parse_factor(lexer);
        expr = type_check_unary(expr, token);
    } else if (token->type == TOKEN_PLUS_PLUS) {
        // ++x => x = x + 1
        lexer->next();
        expr = new_node(AST_ASSIGN);
        expr->assign.lhs = parse_factor(lexer);
        if (!is_lvalue(expr->assign.lhs->type)) {
            std::cerr << "Expected lvalue at " << token->location->filename << ":" << token->location->line << ":" <<
                    token
                    ->location->column << std::endl;
            exit(1);
        }
        auto plus = new_node(AST_PLUS);
        plus->binary.lhs = expr->assign.lhs;
        plus->binary.rhs = node_from_int_literal(1);
        expr->assign.rhs = type_check_binary(plus, token);
        expr->etype = expr->assign.lhs->etype;
    } else if (token->type == TOKEN_MINUS_MINUS) {
        lexer->next();
        expr = new_node(AST_ASSIGN);
        expr->assign.lhs = parse_factor(lexer);
        if (!is_lvalue(expr->assign.lhs->type)) {
            std::cerr << "Expected lvalue at " << token->location->filename << ":" << token->location->line << ":" <<
                    token
                    ->location->column << std::endl;
            exit(1);
        }
        auto minus = new_node(AST_MINUS);
        minus->binary.lhs = expr->assign.lhs;
        minus->binary.rhs = node_from_int_literal(1);
        expr->assign.rhs = type_check_binary(minus, token);
        expr->etype = expr->assign.lhs->etype;
    } else if (token->type == TOKEN_SIZEOF) {
        lexer->next();
        lexer->expect(TOKEN_LPAREN);
        auto type = parse_type(lexer);
        lexer->expect(TOKEN_RPAREN);
        expr = node_from_int_literal(size_of_type(type));
    } else if (token->type == TOKEN_EXCLAMATION) {
        lexer->next();
        expr = new_node(AST_NOT);
        expr->expr = parse_factor(lexer);
        expr = type_check_unary(expr, token);
    } else if (token->type == TOKEN_LPAREN) {
        lexer->next();
        expr = parse_expression(lexer);
        lexer->expect(TOKEN_RPAREN);
    } else if (is_literal_token(token->type)) {
        expr = parse_literal(lexer);
    } else if (token->type == TOKEN_IDENTIFIER) {
        expr = parse_identifier(lexer);
    } else if (token->type == TOKEN_AMPERSAND) {
        lexer->next();
        expr = new_node(AST_ADDRESS_OF);
        expr->expr = parse_factor(lexer);
        if (!is_lvalue(expr->expr->type)) {
            std::cerr << "Expected lvalue at " << token->location->filename << ":" << token->location->line << ":" <<
                    token
                    ->location->column << std::endl;
            exit(1);
        }
        expr = type_check_unary(expr, token);
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
    } else if (token->type == TOKEN_TRUE) {
        lexer->next();
        expr = node_from_int_literal(1);
        expr->etype = new_type(TYPE_BOOL);
    } else if (token->type == TOKEN_FALSE) {
        lexer->next();
        expr = node_from_int_literal(0);
        expr->etype = new_type(TYPE_BOOL);
    // } else if (token->type == TOKEN_NULL) {
    //     lexer->next();
    //     expr = node_from_int_literal(0);
    //     expr->etype = new_type(TYPE_POINTER);
    //     expr->etype->ptr = new_type(TYPE_VOID);
    } else if (token->type == TOKEN_NEW) {
        lexer->next();
        token = lexer->expect(TOKEN_IDENTIFIER);
        auto compond_name = token;

        // Check if the compound type exists
        auto compound = find_compound_type(compond_name->value.as_string);
        if (compound == nullptr) {
            std::cerr << "Could not find type with name: " << compond_name->value.as_string << " at " << token->location
                    ->filename << ":" << token->
                    location->line << ":" << token->location->column << std::endl;
            exit(1);
        }

        auto func = compound->constructor;
        if (func == nullptr) {
            std::cerr << "No constructor found for type: " << compond_name->value.as_string << " at " << token->location
                    ->filename << ":" << token->
                    location->line << ":" << token->location->column << std::endl;
            exit(1);
        }

        auto allocator_func = find_function(default_allocator, &all_functions);

        if (allocator_func == nullptr) {
            std::cerr << "Could not find default allocator function: " << default_allocator << " at " << token->location
                    ->filename << ":" << token->
                    location->line << ":" << token->location->column << std::endl;
            exit(1);
        }

        auto signature_msg = "Allocator doesn't have a correct signature. It should take an i32 as a first arg and return a void pointer.";

        if (allocator_func->function.args->empty()){
            std::cerr << signature_msg << " at " << token->location->filename << ":" << token->location->line << ":" << token->location->column << std::endl;
            exit(1);
        }
        auto var_arg = allocator_func->function.args->at(0);

        // Checking whether allocator has a correct signature
        if (var_arg->type->base != TYPE_U64 || allocator_func->etype->base != TYPE_POINTER){
            std::cerr << signature_msg << " at " << token->location->filename << ":" << token->location->line << ":" << token->location->column << std::endl;
        }

        if (allocator_func->etype->ptr->base != TYPE_VOID){
            std::cerr << signature_msg << " at " << token->location->filename << ":" << token->location->line << ":" << token->location->column << std::endl;
        }

        expr = parse_function_call_args(lexer, func, call_allocator(allocator_func, compound->size));
    } else {
        std::cerr << "Unexpected token: " << token->type << " at " << token->location->filename << ":" << token->
                location->line << ":" << token->location->column << std::endl;
        exit(1);
    }

    while (true) {
        token = lexer->peek();

        if (token->type == TOKEN_DOT) {
            lexer->next();
            if (!is_struct_or_structptr(expr->etype)) {
                std::cerr << "Cannot access member of non-struct type" << std::endl;
                exit(1);
            }

            auto is_ptr = expr->etype->base == TYPE_POINTER;
            auto struct_type = is_ptr ? expr->etype->ptr : expr->etype;

            token = lexer->expect(TOKEN_IDENTIFIER);
            auto name = token->value.as_string;

            auto prev_token = token;
            token = lexer->peek();
            if (token->type == TOKEN_LPAREN) {
                auto method = compound_find_method(struct_type, name);

                if (method == nullptr) {
                    std::cerr << "Method not found: " << name << " at " << token->location->filename << ":" << token->
                            location->line << ":" << token->location->column << std::endl;
                    exit(1);
                }

                Node *obj_ptr;

                if (is_ptr) {
                    obj_ptr = expr;
                } else {
                    obj_ptr = new_node(AST_ADDRESS_OF);
                    obj_ptr->expr = expr;
                    obj_ptr = type_check_unary(obj_ptr, prev_token);
                }

                expr = parse_function_call_args(lexer, method, obj_ptr);
            } else {
                auto field = compound_find_field(struct_type, name);

                if (field == nullptr) {
                    std::cerr << "Field not found: " << name << " at " << token->location->filename << ":" << token->
                            location->line << ":" << token->location->column << std::endl;
                    exit(1);
                }

                auto member = new_node(AST_MEMBER);

                member->etype = field->type;
                member->member.obj = expr;
                member->member.offset = field->offset;
                member->member.is_ptr = is_ptr;

                expr = decay_array_to_pointer(member, prev_token);
            }
        } else if (token->type == TOKEN_LBRACKET) {
            if (expr->etype->base != TYPE_POINTER) {
                std::cerr << "Cannot index non-pointer type at " << token->location->filename << ":" << token->location
                        ->line << ":" << token->location->column << std::endl;
                exit(1);
            }
            token = lexer->next();
            auto index = parse_expression(lexer);
            auto offset = new_node(AST_PLUS);
            offset->binary.lhs = expr;
            offset->binary.rhs = index;
            offset = type_check_binary(offset, token);

            expr = new_node(AST_DEREF);
            expr->expr = offset;
            expr = type_check_unary(expr, token);
            lexer->expect(TOKEN_RBRACKET);
        } else if (token->type == TOKEN_COLON_COLON) {
            // TODO: implement struct access
            std::cerr << "Namespace access not implemented at " << token->location->filename << ":" << token->location->
                    line
                    << ":" << token->location->column << std::endl;
            exit(1);
        } else {
            break;
        }
    }

    return expr;
}

Node *parse_term(Lexer *lexer) {
    auto lhs = parse_factor(lexer);
    // TODO: check precedence
    auto token = lexer->peek();
    while (token->type == TOKEN_STAR || token->type == TOKEN_SLASH || token->type == TOKEN_PERCENT) {
        lexer->next();
        auto op = new_node(binary_token_to_op(token->type));
        auto rhs = parse_factor(lexer);
        op->binary.lhs = lhs;
        op->binary.rhs = rhs;
        lhs = type_check_binary(op, token);
        token = lexer->peek();
    }
    return lhs;
}

Node *parse_additive(Lexer *lexer) {
    auto lhs = parse_term(lexer);
    // TODO: check precedence
    auto token = lexer->peek();
    while (token->type == TOKEN_PLUS || token->type == TOKEN_MINUS || token->type == TOKEN_LSHIFT || token->type ==
           TOKEN_RSHIFT) {
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
    auto token = lexer->peek();
    while (token->type == TOKEN_LT || token->type == TOKEN_GT || token->type == TOKEN_LEQ || token->type == TOKEN_GEQ) {
        lexer->next();
        auto op = new_node(binary_token_to_op(token->type));
        auto rhs = parse_additive(lexer);
        op->binary.lhs = lhs;
        op->binary.rhs = rhs;
        lhs = type_check_binary(op, token);
        token = lexer->peek();
    }
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
    auto token = lexer->peek();
    while (token->type == TOKEN_AMPERSAND) {
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

Node *parse_exclusive_or(Lexer *lexer) {
    auto lhs = parse_and(lexer);
    auto token = lexer->peek();
    while (token->type == TOKEN_CARET) {
        lexer->next();
        auto op = new_node(binary_token_to_op(token->type));
        auto rhs = parse_and(lexer);
        op->binary.lhs = lhs;
        op->binary.rhs = rhs;
        lhs = type_check_binary(op, token);
        token = lexer->peek();
    }
    return lhs;
}

Node *parse_inclusive_or(Lexer *lexer) {
    auto lhs = parse_exclusive_or(lexer);
    auto token = lexer->peek();
    while (token->type == TOKEN_BAR) {
        lexer->next();
        auto op = new_node(binary_token_to_op(token->type));
        auto rhs = parse_exclusive_or(lexer);
        op->binary.lhs = lhs;
        op->binary.rhs = rhs;
        lhs = type_check_binary(op, token);
        token = lexer->peek();
    }
    return lhs;
}

Node *parse_logical_and(Lexer *lexer) {
    auto lhs = parse_inclusive_or(lexer);
    auto token = lexer->peek();
    while (token->type == TOKEN_AND) {
        lexer->next();
        auto op = new_node(binary_token_to_op(token->type));
        auto rhs = parse_inclusive_or(lexer);
        op->binary.lhs = lhs;
        op->binary.rhs = rhs;
        lhs = type_check_binary(op, token);
        token = lexer->peek();
    }
    return lhs;
}

Node *parse_logical_or(Lexer *lexer) {
    auto lhs = parse_logical_and(lexer);
    auto token = lexer->peek();
    while (token->type == TOKEN_OR) {
        lexer->next();
        auto op = new_node(binary_token_to_op(token->type));
        auto rhs = parse_logical_and(lexer);
        op->binary.lhs = lhs;
        op->binary.rhs = rhs;
        lhs = type_check_binary(op, token);
        token = lexer->peek();
    }
    return lhs;
}

Node *parse_conditional_expression(Lexer *lexer) {
    auto lhs = parse_logical_or(lexer);
    auto token = lexer->peek();
    while (token->type == TOKEN_QUESTION) {
        lexer->next();
        auto then_expr = parse_expression(lexer);
        lexer->expect(TOKEN_COLON);
        auto else_expr = parse_expression(lexer);

        auto cond = new_node(AST_CONDITIONAL);
        cond->conditional.condition = lhs;
        cond->conditional.then = then_expr;
        cond->conditional.els = else_expr;

        if (!types_equal(then_expr->etype, else_expr->etype)) {
            std::cerr << "Types in conditional expression do not match at " << token->location->filename << ":" << token
                    ->location->line << ":" << token->location->column << std::endl;
            exit(1);
        }

        lhs = cond;
        lhs->etype = then_expr->etype;
        token = lexer->peek();
    }
    return lhs;
}

Node *parse_expression(Lexer *lexer) {
    auto lhs = parse_conditional_expression(lexer);
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
        } else if (token->type == TOKEN_PLUS_EQUAL || token->type == TOKEN_MINUS_EQUAL) {
            // TODO: implement other compound assignments
            lexer->next();
            auto node = new_node(AST_ASSIGN);
            auto rhs = parse_expression(lexer);

            auto op = new_node(compound_assignment_token_to_op(token->type));
            op->binary.lhs = lhs;
            op->binary.rhs = rhs;
            op = type_check_binary(op, token);

            auto conv = convert_type(lhs->etype, op);
            if (conv == nullptr) {
                std::cerr << "Cannot convert " << op->etype->base << " to " << lhs->etype->base << " at " << token->
                        location->filename << ":"
                        << token->location->line << ":" << token->location->column << std::endl;
                exit(1);
            }

            node->etype = lhs->etype;
            node->assign.lhs = lhs;
            node->assign.rhs = op;
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

void add_global_variable(Variable *var) {
    var->offset = global_offset;
    global_offset += align_up(size_of_type(var->type), 8);
    global_variables.push_back(var);
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
                // TODO: print enum name
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
    } else {
        node->var_decl.init = nullptr;
    }

    if (is_global) {
        add_global_variable(&node->var_decl.var);
    } else {
        add_variable_to_current_block(&node->var_decl.var);
    }

    return node;
}

Node *parse_for_loop(Lexer *lexer) {
    lexer->expect(TOKEN_FOR);

    auto loop = new_node(AST_FOR);
    lexer->expect(TOKEN_LPAREN);

    // NOTE: We're going to put the for loop in it's own block
    //       so that any declarations in the init of the loop
    //       can only be referenced within the loop.
    auto node = new_node(AST_BLOCK);
    node->block.children = new std::vector<Node *>();
    node->block.locals = new std::vector<Variable *>();
    node->block.children->push_back(loop);
    block_stack.push_back(node);

    // All of the expressions in the for loop are optional
    auto token = lexer->peek();
    if (token->type == TOKEN_LET) {
        loop->loop.init = parse_var_declaration(lexer);
    } else if (token->type != TOKEN_SEMICOLON) {
        loop->loop.init = parse_expression(lexer);
    }
    lexer->expect(TOKEN_SEMICOLON);

    token = lexer->peek();
    if (token->type != TOKEN_SEMICOLON) {
        loop->loop.condition = parse_expression(lexer);
    }
    lexer->expect(TOKEN_SEMICOLON);

    token = lexer->peek();
    if (token->type != TOKEN_RPAREN) {
        loop->loop.step = parse_expression(lexer);
    }
    lexer->expect(TOKEN_RPAREN);

    breakable_stack.push_back(loop);
    loop->loop.body = parse_statement(lexer);
    breakable_stack.pop_back();
    block_stack_pop();

    return node;
}

Node * parse_match_statement(Lexer * lexer) {
    lexer->expect(TOKEN_MATCH);

    auto node = new_node(AST_MATCH);

    lexer->expect(TOKEN_LPAREN);

    node->match.expr = parse_expression(lexer);
    if (!is_int_type(node->match.expr->etype)) {
        std::cerr << "Match expression must be an integer type at " << lexer->peek()->location->filename << ":" << lexer
                ->peek()->location->line << ":" << lexer->peek()->location->column << std::endl;
        exit(1);
    }

    node->match.cases = new std::vector<Node *>();
    // breakable_stack.push_back(node);

    lexer->expect(TOKEN_RPAREN);
    lexer->expect(TOKEN_LBRACE);

    auto token = lexer->peek();
    while (token->type != TOKEN_RBRACE) {
        if (token->type == TOKEN_DEFAULT) {
            lexer->next();
            lexer->expect(TOKEN_COLON);
            token = lexer->peek();

            auto stmt = parse_statement(lexer);
            node->match.defolt = stmt;

            token = lexer->peek();
            if (token->type != TOKEN_RBRACE) {
                std::cerr << "Expected '}' at " << token->location->filename << ":" << token->location->line << ":" << token
                        ->location->column << std::endl;
                exit(1);
            }
        } else if (token->type == TOKEN_INTLIT || token->type == TOKEN_IDENTIFIER) {
            auto case_stmt = new_node(AST_CASE);
            case_stmt->case_stmt.value = parse_constant_expression(lexer);

            lexer->expect(TOKEN_COLON);

            auto stmt = parse_statement(lexer);
                case_stmt->case_stmt.stmt = stmt;

            node->match.cases->push_back(case_stmt);
        } else {
            std::cerr << "Unexpected token: " << token->type << " at " << token->location->filename << ":" << token->
                    location->line << ":" << token->location->column << std::endl;
            exit(1);
        }
        token = lexer->peek();
    }
    // breakable_stack.pop_back();
    lexer->expect(TOKEN_RBRACE);
    return node;
}

Node * parse_delete_statement(Lexer * lexer) {
    lexer->expect(TOKEN_DELETE);

    auto token = lexer->peek();
    auto node = parse_identifier(lexer);
    if (node == nullptr) {
        std::cerr << "Variable " << token->value.as_string << " not found at " << token->location->filename << ":" << token
                ->location->line << ":" << token->location->column << std::endl;
        exit(1);
    }

    if (node->variable->type->base != TYPE_POINTER) {
        std::cerr << "Cannot free non-pointer type at " << token->location->filename << ":" << token->location->line << ":"
                << token->location->column << std::endl;
        exit(1);
    }

    Node* disposer = nullptr;
    if (node->variable->type->ptr->base == TYPE_STRUCT || node->variable->type->ptr->base == TYPE_UNION) {
        disposer = node->variable->type->ptr->disposer;
    }

    Node* expr = new_node(AST_BLOCK);
    expr->block.children = new std::vector<Node*>();

    auto pos = lexer->position;
    auto line = lexer->line;
    auto column = lexer->column;
    if (disposer != nullptr) {
        expr->block.children->push_back(parse_function_call_args(lexer, disposer, node, true));
        lexer->position = pos;
        lexer->line = line;
        lexer->column = column;
    }

    auto deallocator = find_function(default_deallocator, &all_functions);
    if (deallocator == nullptr) {
        std::cerr << "Could not find default deallocator function: " << default_deallocator << " at " << token->location
                ->filename << ":" << token->
                location->line << ":" << token->location->column << std::endl;
        exit(1);
    }

    expr->block.children->push_back(parse_function_call_args(lexer, deallocator, node, true));

    // assign null to the variable
    auto null_var = find_global_variable("null");
    auto null = new_node(AST_GLOBAL_VAR);
    null->variable = null_var;
    null->etype = null_var->type;

    auto assign = new_node(AST_ASSIGN);
    assign->assign.lhs = node;
    assign->assign.rhs = null;
    assign->etype = node->variable->type;
    expr->block.children->push_back(assign);

    lexer->expect(TOKEN_SEMICOLON);

    return expr;
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
    } else if (token->type == TOKEN_IF) {
        lexer->next();

        node = new_node(AST_IF);

        lexer->expect(TOKEN_LPAREN);
        node->conditional.condition = parse_expression(lexer);
        lexer->expect(TOKEN_RPAREN);

        node->conditional.then = parse_statement(lexer);

        token = lexer->peek();
        if (token->type == TOKEN_ELSE) {
            lexer->next();
            node->conditional.els = parse_statement(lexer);
        }
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
    } else if (token->type == TOKEN_FOR) {
        node = parse_for_loop(lexer);
    } else if (token->type == TOKEN_MATCH) {
        node = parse_match_statement(lexer);
    } else if (token->type == TOKEN_DEFER) {
        lexer->next();
        node = new_node(AST_DEFER);
        node->expr = parse_statement(lexer);
    } else if (token->type == TOKEN_DELETE) {
        node = parse_delete_statement(lexer);
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
    if (func->function.args == nullptr) {
        func->function.args = new std::vector<Variable *>();
    }

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

Node *parse_function(Lexer *lexer, bool first_pass) {
    lexer->expect(TOKEN_FN);
    const auto name = lexer->expect(TOKEN_IDENTIFIER);

    Node *func;
    if (first_pass) {
        func = new_node(AST_FUNCTION);
        func->function.max_locals_size = 0;
        func->function.name = name->value.as_string;
        func->function.args = new std::vector<Variable *>();
        func->function.is_method = false;
        func->function.is_constructor = false;
        func->function.is_disposer = false;

        if (identifier_exists(name->value.as_string)) {
            std::cerr << "Function " << name->value.as_string << " already exists" << std::endl;
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

        // Skip function body if first pass
        lexer->expect(TOKEN_LBRACE);
        auto lbrace_count = 1;
        while (lbrace_count > 0) {
            token = lexer->next();
            if (token->type == TOKEN_LBRACE) {
                lbrace_count++;
            } else if (token->type == TOKEN_RBRACE) {
                lbrace_count--;
            }
        }

        current_function = nullptr;

        return func;
    } else {
        func = find_function(name->value.as_string, &all_functions);

        current_function = func;

        // Skip function parameters and return type
        auto token = lexer->peek();
        while (token->type != TOKEN_LBRACE) {
            lexer->next();
            token = lexer->peek();
        }

        func->function.body = parse_block(lexer);

        current_function = nullptr;

        return nullptr;
    }
}

Node *constant_push(char *name, size_t value, std::vector<Node *> *constants_vector) {
    auto node = new_node(AST_CONSTANT);
    node->constant.name = name;
    node->constant.value = node_from_int_literal(value);
    constants_vector->push_back(node);
    return node;
}

void parse_enum_declaration(Lexer *lexer) {
    lexer->expect(TOKEN_ENUM);

    auto token = lexer->expect(TOKEN_IDENTIFIER);

    auto type = new_type(TYPE_ENUM);
    type->variants = new std::vector<Node *>();
    type->struct_name = token->value.as_string;
    type->size = 1; // TODO: check this

    compound_types.push_back(type);

    lexer->expect(TOKEN_LBRACE);

    size_t enum_count = 0;
    token = lexer->peek();
    while (token->type != TOKEN_RBRACE) {
        token = lexer->expect(TOKEN_IDENTIFIER);

        if (find_constant(token->value.as_string, type->variants)) {
            std::cerr << "Variant already exists at " << token->location->filename << ":" << token->location->line <<
                    ":" << token->location->column << std::endl;
            exit(1);
        }

        auto constant = constant_push(token->value.as_string, enum_count++, type->variants);
        constant->etype = type;

        token = lexer->peek();
        if (token->type == TOKEN_COMMA) {
            lexer->next();
            token = lexer->peek();
        }
    }
    lexer->expect(TOKEN_RBRACE);
}

Node *parse_method(Lexer *lexer, Type *compound) {
    lexer->expect(TOKEN_FN);
    auto token = lexer->next();

    auto is_constructor = token->type == TOKEN_NEW;
    auto is_disposer = token->type == TOKEN_DELETE;

    auto func = new_node(AST_FUNCTION);
    func->function.name = token->value.as_string;
    func->function.method_of = compound;
    func->function.is_constructor = is_constructor;
    func->function.is_method = !is_constructor && !is_disposer;
    func->function.is_disposer = is_disposer;

    func->function.args = new std::vector<Variable *>();

    auto ptr_type = new_type(TYPE_POINTER);
    ptr_type->ptr = compound;

    auto self_var = new_variable("self", ptr_type, 0);
    func->function.args->push_back(self_var);

    if (is_constructor) {
        if (compound->constructor != nullptr) {
            std::cerr << "Constructor already exists at " << token->location->filename << ":" << token->location->line
                    << ":" << token->location->column << std::endl;
            exit(1);
        }

        func->etype = ptr_type;
        compound->constructor = func;
    } else if (is_disposer) {
        if (compound->disposer != nullptr) {
            std::cerr << "Disposer already exists at " << token->location->filename << ":" << token->location->line <<
                    ":" << token->location->column << std::endl;
            exit(1);
        }

        func->etype = new_type(TYPE_VOID);
        compound->disposer = func;
    } else {
        if (find_function(token->value.as_string, compound->methods) != nullptr) {
            std::cerr << "Method " << token->value.as_string << " already exists at " << token->location->filename <<
                    ":"
                    << token->location->line << ":" << token->location->column << std::endl;
        }

        compound->methods->push_back(func);
    }

    current_function = func;

    lexer->expect(TOKEN_LPAREN);
    parse_function_params(lexer, func);
    lexer->expect(TOKEN_RPAREN);

    token = lexer->peek();
    if (token->type == TOKEN_COLON) {
        if (is_constructor) {
            std::cerr << "Constructor cannot have a return type at " << token->location->filename << ":" << token->
                    location->line << ":" << token->location->column << std::endl;
            exit(1);
        }
        lexer->next();
        func->etype = parse_type(lexer);
    } else if (!is_constructor) {
        func->etype = new_type(TYPE_VOID);
    }

    func->function.body = parse_block(lexer);
    if (is_constructor || is_disposer) {
        auto ret = new_node(AST_RETURN);
        ret->expr = new_node(AST_LOCAL_VAR);
        ret->expr->variable = find_local_variable("self");
        ret->expr->etype = ret->expr->variable->type;
        ret->etype = func->etype;

        func->function.body->block.children->push_back(ret);
    }
    current_function = nullptr;
    return func;
}

// FIXME: This should just be part of `parse_type()`, and we should be allowed
//        to parse a type without a name. Probably also need to handle converstions
//        between structs with similar embedded types.
// FIXME: We need to prevent circular references in structs.
Type *parse_struct_union_declaration(Lexer *lexer, bool top_level, int base_offset) {
    auto token = lexer->next();

    if (token->type != TOKEN_STRUCT && token->type != TOKEN_UNION) {
        std::cerr << "Expected struct or union at " << token->location->filename << ":" << token->location->line << ":"
                << token->location->column << std::endl;
        exit(1);
    }

    auto compound = new_type(token->type == TOKEN_STRUCT ? TYPE_STRUCT : TYPE_UNION);
    compound->fields = new std::vector<Variable *>();
    compound->methods = new std::vector<Node *>();

    token = lexer->peek();

    if (token->type != TOKEN_IDENTIFIER && top_level) {
        std::cerr << "Expected identifier at " << token->location->filename << ":" << token->location->line << ":" <<
                token->location->column << std::endl;
        exit(1);
    }

    // But if they do provide one, we'll add it to the list of defined structs so they
    // it can referenced internally.
    if (token->type == TOKEN_IDENTIFIER) {
        compound->struct_name = token->value.as_string;
        compound_types.push_back(compound);
        token = lexer->next();
    } else {
        compound->struct_name = "<anonymous>";
    }

    lexer->expect(TOKEN_LBRACE);

    token = lexer->peek();
    while (token->type != TOKEN_RBRACE) {
        if (token->type == TOKEN_FN) {
            // method
            parse_method(lexer, compound);
        } else {
            // field
            char *name = nullptr;

            // We have a named field.
            if (token->type == TOKEN_IDENTIFIER) {
                lexer->next();
                // TODO: Make sure the name doesn't conflict with an existing field. This is a bit
                //       more annoying than it sounds because we have no-named types.
                name = token->value.as_string;
                lexer->expect(TOKEN_COLON);
                token = lexer->peek();
            }

            // We want to allow nested temporary structs.
            Type *type;
            if (token->type == TOKEN_STRUCT || token->type == TOKEN_UNION) {
                // Compute the "base offset" for the nested struct, which is the offset from the start
                // of the struct the last **named** field in the heirarchy above
                size_t cur_base_offset = 0;
                if (name == nullptr) {
                    cur_base_offset = base_offset + (compound->base == TYPE_UNION ? 0 : compound->size);
                }

                // If the field didn't have a name, we can assign one now, since we know it's a
                // temporary struct. This name will never be directly referenced, so it's fine.
                if (name == nullptr) {
                    name = "<anonymous>";
                }

                // Nested structs live in their own "namespace", can't be accessed
                // from outside, so we will pop them off the stack once done.
                auto prev_compound_count = compound_types.size();
                type = parse_struct_union_declaration(lexer, false, cur_base_offset);
                while (compound_types.size() > prev_compound_count) {
                    compound_types.pop_back();
                }
            } else if (name != nullptr) {
                type = parse_type(lexer);
            } else {
                std::cerr << "Expected a name for a non-compound field in struct/union at " << token->location->filename
                        <<
                        ":" << token->location->line << ":" << token->location->column << std::endl;
                exit(1);
            }

            compound_push_field(compound, name, type, base_offset);
            lexer->expect(TOKEN_SEMICOLON);
        }

        token = lexer->peek();
    }
    lexer->expect(TOKEN_RBRACE);
    return compound;
}

int64_t eval_constexpr(Node *node, Token *token) {
    if (node->type == AST_LITERAL) {
        if (is_int_type(node->etype)) {
            return node->literal.as_int; // FIXME: i64 and u64
        }
        std::cerr << "Expected integer literal at " << token->location->filename << ":" << token->location->line << ":"
                << token->location->column << std::endl;
        exit(1);
    }
    if (is_binary_op(node->type)) {
        auto left = eval_constexpr(node->binary.lhs, token);
        auto right = eval_constexpr(node->binary.rhs, token);
        switch (node->type) {
            case AST_PLUS: return left + right;
            case AST_MINUS: return left - right;
            case AST_BWOR: return left | right;
            case AST_BWAND: return left & right;
            case AST_XOR: return left ^ right;
            case AST_MUL: return left * right;
            case AST_DIV: return left / right;
            case AST_MOD: return left % right;
            case AST_EQ: return left == right;
            case AST_NEQ: return left != right;
            case AST_LT: return left < right;
            case AST_LEQ: return left <= right;
            case AST_GT: return left > right;
            case AST_GEQ: return left >= right;
            case AST_LSHIFT: return left << right;
            case AST_RSHIFT: return left >> right;
            default: {
                std::cerr << "Unexpected binary operator " << node->type << " at " << token->location->filename << ":"
                        << token->location->line << ":" << token->location->column << std::endl;
                exit(1);
            }
        }
    }
    if (node->type == AST_NEG) return -eval_constexpr(node->expr, token);
    if (node->type == AST_NOT) return !eval_constexpr(node->expr, token);
    if (node->type == AST_BWINV) return ~eval_constexpr(node->expr, token);
    std::cerr << "Unexpected node type " << node->type << " at " << token->location->filename << ":" << token->location
            ->line
            << ":" << token->location->column << std::endl;
    exit(1);
}

int64_t parse_constant_expression(Lexer *lexer) {
    auto token = lexer->peek();
    auto node = parse_expression(lexer);
    return eval_constexpr(node, token);
}

Node *parse_constant_declaration(Lexer *lexer) {
    lexer->expect(TOKEN_CONST);
    auto token = lexer->expect(TOKEN_IDENTIFIER);
    if (identifier_exists(token->value.as_string)) {
        std::cerr << "Identifier " << token->value.as_string << " already exists at " << token->location->filename <<
                ":"
                << token->location->line << ":" << token->location->column << std::endl;
        exit(1);
    }

    auto constant_name = token->value.as_string;
    // All constants are implicitly `int`, but we'll allow it for consistency
    // if (token->type == TOKEN_COLON) {
    //     lexer->next();
    //     if (!is_int_type(token->type))
    //         die_loc(here, &token.loc, "Expected 'int' type for constant");
    //     lexer::peek(&token);
    // }

    lexer->expect(TOKEN_ASSIGN);
    constant_push(constant_name, parse_constant_expression(lexer), &constants);

    lexer->expect(TOKEN_SEMICOLON);
}

Node *parse_program(Lexer *lexer) {
    initialize_builtins();

    const auto node = new_node(AST_PROGRAM);
    node->block.children = new std::vector<Node *>();

    lexer_stack.push_back(lexer);

    bool first_pass = true;
    while (true) {
        auto token = lexer->peek();
        while (token->type != TOKEN_EOF) {
            Node *child = nullptr;
            if (token->type == TOKEN_IMPORT) {
                lexer->next();
                auto path = lexer->expect(TOKEN_STRINGLIT);

                auto already_imported = false;
                if (path->value.as_string[0] == '.') {
                    std::cerr << "Relative imports are not supported at " << path->location->filename << ":" << path->
                            location->line << ":" << path->location->column << std::endl;
                    exit(1);
                }
                char* absolute_path = nullptr;
                for (auto ip: import_paths) {
                    auto joined = new char[strlen(ip) + strlen(path->value.as_string) + 2];
                    strcpy(joined, ip);
                    if (ip[strlen(ip) - 1] != '/') {
                        strcat(joined, "/");
                    }
                    strcat(joined, path->value.as_string);

                    // Check if file exists
                    if (std::filesystem::exists(joined)) {
                        absolute_path = joined;
                        break;
                    }
                }

                if (absolute_path == nullptr) {
                    std::cerr << "File " << path->value.as_string << " not found at " << path->location->filename << ":"
                            << path->location->line << ":" << path->location->column << std::endl;
                    exit(1);
                }

                for (const auto f: imported_files) {
                    if (f == absolute_path) {
                        already_imported = true;
                        break;
                    }
                }

                if (!already_imported) {
                    lexer = Lexer::create_from_file(absolute_path);
                    lexer_stack.push_back(lexer);
                    imported_files.push_back(absolute_path);
                }
            } else if (token->type == TOKEN_FN) {
                child = parse_function(lexer, first_pass);
            } else if (token->type == TOKEN_LET) {
                if (!first_pass) {
                    // Skip
                    while (token->type != TOKEN_SEMICOLON) token = lexer->next();
                } else {
                    child = parse_var_declaration(lexer);
                    lexer->expect(TOKEN_SEMICOLON);
                }
            } else if (token->type == TOKEN_CONST) {
                if (!first_pass) {
                    // Skip
                    while (token->type != TOKEN_SEMICOLON) token = lexer->next();
                } else {
                    parse_constant_declaration(lexer);
                }
            } else if (token->type == TOKEN_ENUM) {
                if (!first_pass) {
                    // Skip
                    while (token->type != TOKEN_SEMICOLON) token = lexer->next();
                } else {
                    parse_enum_declaration(lexer);
                    lexer->expect(TOKEN_SEMICOLON); // TODO: maybe remove semicolon
                }
            } else if (token->type == TOKEN_STRUCT || token->type == TOKEN_UNION) {
                if (!first_pass) {
                    lexer->next(); // Struct or union keyword
                    lexer->next(); // Struct or union name
                    token = lexer->expect(TOKEN_LBRACE);
                    // Skip
                    size_t brace_count = 1;
                    while (brace_count > 0) {
                        token = lexer->next();
                        if (token->type == TOKEN_LBRACE) {
                            brace_count++;
                        } else if (token->type == TOKEN_RBRACE) {
                            brace_count--;
                        }
                    }
                } else {
                    auto compound = parse_struct_union_declaration(lexer, true, 0);
                    // Add compond methods to block to get generated later, hack
                    for (auto method: *compound->methods) {
                        node->block.children->push_back(method);
                    }
                    if (compound->constructor != nullptr) {
                        node->block.children->push_back(compound->constructor);
                    }
                    if (compound->disposer != nullptr) {
                        node->block.children->push_back(compound->disposer);
                    }
                }
            } else {
                std::cerr << "Unexpected token " << token->type << " at " << token->location->filename << ":" << token->
                        location->line << ":" << token->location->column << std::endl;
                exit(1);
            }

            if (child != nullptr) {
                node->block.children->push_back(child);
            }

            token = lexer->peek();
            while (token->type == TOKEN_EOF && lexer_stack.size() > 1) {
                lexer_stack.pop_back();
                lexer = lexer_stack.back();
                token = lexer->peek();
            }
        }

        if (first_pass) {
            first_pass = false;
            lexer->reset();
            imported_files.clear();
        } else {
            break;
        }
    }

    return node;
}
