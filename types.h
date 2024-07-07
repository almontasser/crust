//
// Created by mahmoud on 7/3/24.
//

#ifndef TYPES_H
#define TYPES_H

#include <vector>

struct Node;
struct Variable;

enum BaseType {
    TYPE_I8,
    TYPE_I16,
    TYPE_I32,
    TYPE_I64,
    TYPE_U8,
    TYPE_U16,
    TYPE_U32,
    TYPE_U64,
    TYPE_F32,
    TYPE_F64,
    TYPE_BOOL,
    TYPE_VOID,
    TYPE_ANY,

    NUM_BASE_TYPES,

    TYPE_POINTER,
    TYPE_ARRAY,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ENUM,

    NUM_TYPES,
};

struct Type {
    BaseType base;
    size_t size;
    Type* ptr;
    char* struct_name;
    size_t base_offset;
    size_t array_size;

    std::vector<Variable> fields;
    std::vector<Node> members;
    Node* constructor;

    std::vector<Node*>* variants;
};

size_t size_of_base_type(BaseType type);

size_t size_of_type(const Type *type);

Type* create_new_base_type(BaseType base);

Type* new_type(BaseType base);

Type* new_ptr_type(BaseType base);

bool is_float_type(const Type* type);

bool is_int_type(const Type* type);

bool types_equal(const Type* a, const Type* b);

#endif //TYPES_H
