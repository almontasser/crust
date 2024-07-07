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
auto compound_types = std::vector<Type *>();
auto constants = std::vector<Node *>();

Node *parse_expression(Lexer *lexer);
Type *find_compound_type(char *name);

size_t align_up(size_t val, int align) {
    return (val + align - 1) & ~(align - 1);
}

void builtin_create_syscall(const char *name, int num_args) {
    const auto node = new_node(AST_BUILTIN);
    node->etype = new_type(TYPE_ANY);
    node->function.name = static_cast<char *>(malloc(strlen(name) + 1));
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
            type = find_compound_type(token->value.as_string);
            if (type == nullptr) {
                std::cerr << "Unknown type: " << token->type << " at " << token->location->filename << ":" << token->
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
    if (find_function(name, builtin_functions) != nullptr) return true;
    if (find_function(name, all_functions) != nullptr) return true;
    if (find_compound_type(name) != nullptr) return true;
    if (find_constant(name, &constants) != nullptr) return true;
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

    var = find_global_variable(name);
    if (var != nullptr) {
        node = new_node(AST_GLOBAL_VAR);
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

    auto type = find_compound_type(name);
    if (type != nullptr) {
        if (type->base == TYPE_ENUM) {
            lexer->expect(TOKEN_COLON_COLON);
            token = lexer->expect(TOKEN_IDENTIFIER);
            auto constant = find_constant(token->value.as_string, type->variants);
            if (constant == nullptr) {
                std::cerr << "Variant \"" << token->value.as_string << "\" not found at " << token->location->filename
                    << ":" << token->location->line << ":" <<token->location->column << std::endl;
                exit(1);
            }
            return constant;
        } else {
            std::cerr << "Not implemented at parse_identifier()" << std::endl;
            exit(1);
        }
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
    } else if (token->type == TOKEN_FALSE) {
        lexer->next();
        expr = node_from_int_literal(0);
    } else {
        // TODO: implement new keyword
        std::cerr << "Unexpected token: " << token->type << " at " << token->location->filename << ":" << token->
                location->line << ":" << token->location->column << std::endl;
        exit(1);
    }

    while (true) {
        token = lexer->peek();

        if (token->type == TOKEN_LBRACKET) {
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
        } else if (token->type == TOKEN_DOT) {
            // TODO: implement struct access
            std::cerr << "Struct access not implemented at " << token->location->filename << ":" << token->location->
                    line
                    << ":" << token->location->column << std::endl;
            exit(1);
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
    }

    if (is_global) {
        add_global_variable(&node->var_decl.var);
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

Node *parse_function(Lexer *lexer, bool first_pass) {
    lexer->expect(TOKEN_FN);
    const auto name = lexer->expect(TOKEN_IDENTIFIER);

    Node *func;
    if (first_pass) {
        func = new_node(AST_FUNCTION);
        func->function.name = name->value.as_string;

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

        return func;
    } else {
        func = find_function(name->value.as_string, all_functions);

        current_function = func;

        // Skip function parameters and return type
        auto token = lexer->peek();
        while (token->type != TOKEN_LBRACE) {
            lexer->next();
            token = lexer->peek();
        }

        func->function.body = parse_block(lexer);

        return nullptr;
    }
}

Node* constant_push(char * name, size_t value, std::vector<Node *> * constants_vector) {
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
                child = parse_function(lexer, first_pass);
            } else if (token->type == TOKEN_LET) {
                if (!first_pass) {
                    // Skip
                    while (token->type != TOKEN_SEMICOLON) token = lexer->next();
                } else {
                    child = parse_var_declaration(lexer);
                    lexer->expect(TOKEN_SEMICOLON);
                }
            } else if (token->type == TOKEN_ENUM) {
                if (!first_pass) {
                    // Skip
                    while (token->type != TOKEN_SEMICOLON) token = lexer->next();
                } else {
                    parse_enum_declaration(lexer);
                    lexer->expect(TOKEN_SEMICOLON);
                }
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

        if (first_pass) {
            first_pass = false;
            lexer->reset();
        } else {
            break;
        }
    }

    return node;
}
