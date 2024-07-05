//
// Created by mahmoud on 7/3/24.
//

#ifndef AST_H
#define AST_H
#include <cstdint>
#include <vector>

struct Type;
struct Variable;

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

    NUM_NODE_TYPES,
};

struct Node {
    NodeType type;
    Type* etype;

    union {
        Node* expr;

        struct {
            char* name;
            Node* body;
            size_t max_locals_size;
            std::vector<Variable*> args;
        } function;

        struct {
            std::vector<Node*> children;
            std::vector<Variable> locals;
            size_t locals_size;
        } block;

        struct {
            uint64_t as_int;
            char* as_string;
        } literal;

        struct {
            Node* function;
            std::vector<Node*> args;
        } call;
    };
};

struct Variable {
    char* name;
    Type* type;
    size_t offset;
};

Node* new_node(NodeType type);
Node* convert_type(Type* to, Node* from_node);

Variable* new_variable(char* name, Type* type, size_t offset);

bool is_lvalue(NodeType type);

#endif //AST_H
