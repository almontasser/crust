//
// Created by mahmoud on 7/3/24.
//

#ifndef AST_H
#define AST_H
#include <cstdint>
#include <vector>

#include "lexer.h"

struct Type;

struct Variable {
    char *name;
    Type *type;
    size_t offset;
};

enum NodeType {
    // AST Types
    AST_LITERAL,
    AST_FUNCTION,
    AST_RETURN,
    AST_PROGRAM,
    AST_BLOCK,
    AST_CONVERT,
    AST_BUILTIN,
    AST_LOCAL_VAR,
    AST_GLOBAL_VAR,
    AST_DEREF,
    AST_MEMBER,
    AST_FUNCTION_CALL,
    AST_VAR_DECLARATION,
    AST_WHILE,
    AST_CONTINUE,
    AST_BREAK,
    AST_ADDRESS_OF,
    AST_NEG,
    AST_NOT,
    AST_MUL,
    AST_EQ,
    AST_NEQ,
    AST_ASSIGN,
    AST_PLUS,
    AST_CONDITIONAL,
    AST_BWINV,
    AST_MINUS,
    AST_DIV,
    AST_MOD,
    AST_LSHIFT,
    AST_RSHIFT,
    AST_AND,
    AST_BWAND,
    AST_OR,
    AST_BWOR,
    AST_XOR,
    AST_LT,
    AST_LEQ,
    AST_GT,
    AST_GEQ,
    AST_IF,
    AST_ENUM,
    AST_CONSTANT,

    NUM_NODE_TYPES,
};

struct Node {
    NodeType type;
    Type *etype;

    union {
        Node *expr;

        Variable *variable;

        struct {
            char *name;
            Node *body;
            size_t max_locals_size;
            std::vector<Variable *> *args;
            bool is_method;
            Type * method_of;
        } function;

        struct {
            std::vector<Node *> *children;
            std::vector<Variable *> *locals;
            size_t locals_size;
        } block;

        struct {
            uint64_t as_int;
            char *as_string;
        } literal;

        struct {
            Node *function;
            std::vector<Node *> *args;
        } call;

        struct {
            Variable var;
            Node *init;
        } var_decl;

        struct {
            Node *condition;
            Node *body;
            // for loop
            Node *init;
            Node *step;
        } loop;

        struct {
            Node *lhs;
            Node *rhs;
        } binary;

        struct {
            Node *lhs;
            Node *rhs;
        } assign;

        struct {
            Node *condition;
            Node *then;
            Node *els;
        } conditional;

        struct {
            char* name;
            Node* value; // must be int literal
        } constant;

        struct {
            Node* obj;
            size_t offset;
            bool is_ptr;
        } member;
    };
};

Node *new_node(NodeType type);

Node *convert_type(Type *to, Node *from_node);

Variable *new_variable(char *name, Type *type, size_t offset);

bool is_lvalue(NodeType type);

Node *decay_array_to_pointer(Node *node, Token *token);

Node *type_check_unary(Node *node, Token *token);

Node *type_check_binary(Node *node, Token *token);

NodeType binary_token_to_op(TokenType type);

Node *new_node_binop(NodeType type, Node *lhs, Node *rhs);

Node *node_from_int_literal(uint64_t value);

bool is_binary_op(NodeType type);

size_t compound_push_field(Type * compound, char * name, Type * type, size_t base_offset);

inline size_t align_up(size_t val, int align) {
    return (val + align - 1) & ~(align - 1);
}

Variable* compound_find_field(Type* type, char* name);

Node* compound_find_method(Type* type, char* name);

#endif //AST_H
