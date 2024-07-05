//
// Created by mahmoud on 7/3/24.
//

#include "ast.h"

#include <cstdlib>
#include <iostream>
#include <ostream>

#include "types.h"

Node *new_node(NodeType type) {
    static size_t counter = 0;

    const auto node = static_cast<Node *>(malloc(sizeof(Node)));
    node->type = type;
    return node;
}

Node *convert_to_int(Node *node) {
    if (is_int_type(node->etype)) return node;

    if (!is_float_type(node->etype)) {
        std::cerr << "Cannot convert to int from type: " << node->etype->base << std::endl;
        exit(1);
    }

    auto convert = new_node(AST_CONVERT);
    convert->etype = new_type(TYPE_I64); // TODO: check this
    convert->expr = node;
    return convert;
}

Node *convert_to_float(Node *node) {
    if (is_float_type(node->etype)) return node;

    if (!is_int_type(node->etype)) {
        std::cerr << "Cannot convert to float from type: " << node->etype->base << std::endl;
        exit(1);
    }

    auto convert = new_node(AST_CONVERT);
    convert->etype = new_type(TYPE_F64); // TODO: check this
    convert->expr = node;
    return convert;
}

Node *convert_type(Type *to, Node *from_node) {
    auto from = from_node->etype;

    if (is_float_type(from) && is_int_type(to)) return convert_to_int(from_node);

    if (is_int_type(from) && is_float_type(to)) return convert_to_float(from_node);

    if (from->base == TYPE_ANY || to->base == TYPE_ANY) return from_node;

    // Allow converstions to and from void* to any other pointer type
    if (from->base == TYPE_POINTER && to->base == TYPE_POINTER) {
        if (from->ptr->base == TYPE_VOID || to->ptr->base == TYPE_VOID) return from_node;
    }

    // TODO: Should we raise a warning if the target type is narrower
    //       than the source type?
    if (is_int_type(from) && is_int_type(to)) return from_node;

    if (types_equal(from, to)) return from_node;

    return nullptr;
}

Variable *new_variable(char *name, Type *type, size_t offset) {
    Variable *var = static_cast<Variable *>(malloc(sizeof(Variable)));
    var->name = name;
    var->type = type;
    var->offset = offset;
    return var;
}

bool is_lvalue(NodeType type) {
    static_assert(NUM_NODE_TYPES == 12, "Exhaustive match in is_lvalue()");
    if (type == AST_LOCAL_VAR) return true;
    if (type == AST_GLOBAL_VAR) return true;
    if (type == AST_MEMBER) return true;
    if (type == AST_DEREF) return true;
    return false;
}
