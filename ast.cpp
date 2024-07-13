//
// Created by mahmoud on 7/3/24.
//

#include "ast.h"

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <ostream>

#include "types.h"

Node *new_node(NodeType type) {
    const auto node = static_cast<Node *>(calloc(1, sizeof(Node)));
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

    if (to->base == TYPE_POINTER && is_int_type(from)) {
        return from_node;
    }

    // TODO: Should we raise a warning if the target type is narrower
    //       than the source type?
    if (is_int_type(from) && is_int_type(to)) return from_node;

    if (types_equal(from, to)) {
        if (from->base == TYPE_ENUM) {
            return from_node->constant.value;
        }
        return from_node;
    }

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
    static_assert(NUM_NODE_TYPES == 47, "Exhaustive match in is_lvalue()");
    if (type == AST_LOCAL_VAR) return true;
    if (type == AST_GLOBAL_VAR) return true;
    if (type == AST_MEMBER) return true;
    if (type == AST_DEREF) return true;
    return false;
}

Node *decay_array_to_pointer(Node *node, Token *token) {
    // We can only take the address of an lvalue, so we need to ensure that
    if (is_lvalue(node->type) && node->etype->base == TYPE_ARRAY) {
        auto address = new_node(AST_ADDRESS_OF);
        address->expr = node;
        address = type_check_unary(address, token);
        node = address;
    }
    return node;
}

Node *type_check_unary(Node *node, Token *token) {
    static_assert(NUM_NODE_TYPES == 47, "Exhaustive match in type_check_unary()");

    auto old_type = node->expr->etype;

    if (node->type != AST_ADDRESS_OF && old_type->base == TYPE_STRUCT) {
        std::cerr << "Performing invalid unary operation on struct type at " << token->location->filename << token->
                location->line << ":" << token->location->column << std::endl;
        exit(1);
    }

    if (is_float_type(old_type) && node->type != AST_ADDRESS_OF && node->type != AST_NEG) {
        std::cerr << "Performing invalid unary operation on float type at " << token->location->filename << token->
                location->line << ":" << token->location->column << std::endl;
        exit(1);
    }

    if (node->type == AST_NOT) {
        node->etype = new_type(TYPE_BOOL);
    } else if (node->type == AST_ADDRESS_OF) {
        auto ptr = new_type(TYPE_POINTER);
        // The address of an array is a pointer to the first element
        ptr->ptr = old_type->base == TYPE_ARRAY ? old_type->ptr : old_type;
        node->etype = ptr;
    } else if (node->type == AST_DEREF) {
        if (old_type->base != TYPE_POINTER) {
            std::cerr << "Cannot dereference non-pointer type at " << token->location->filename << token->location->line
                    << ":" << token->location->column << std::endl;
            exit(1);
        }
        node->etype = old_type->ptr;
        // If the dereferenced type is an array, we need to decay it to a
        // pointer to the first element.
        node = decay_array_to_pointer(node, token);
    } else if (node->type == AST_NEG) {
        if (!is_int_type(old_type) && !is_float_type(old_type)) {
            std::cerr << "Cannot negate non-integer type at " << token->location->filename << token->location->line
                    << ":" << token->location->column << std::endl;
            exit(1);
        }

        Type* type = old_type;
        if (old_type->base == TYPE_U8) old_type = new_type(TYPE_I16);
        if (old_type->base == TYPE_U16) old_type = new_type(TYPE_I32);
        if (old_type->base == TYPE_U32) old_type = new_type(TYPE_I64);
        if (old_type->base == TYPE_U64) old_type = new_type(TYPE_I64); // TODO: Check for overflow
        node->etype = old_type;
    } else {
        // Default to not changing the type
        node->etype = old_type;
    }
    return node;
}

Node *type_check_binary(Node *node, Token *token) {
    static_assert(NUM_NODE_TYPES == 47, "Exhaustive match in type_check_binary()");

    auto lhs = node->binary.lhs->etype;
    auto rhs = node->binary.rhs->etype;

    if (lhs->base == TYPE_STRUCT || rhs->base == TYPE_STRUCT) {
        std::cerr << "Performing invalid binary operation on struct type at " << token->location->filename << token->
                location->line << ":" << token->location->column << std::endl;
        exit(1);
    }

    // TODO: Fix this
    if (node->type == AST_PLUS) {
        if (is_int_type(lhs) && is_int_type(rhs)) {
            // TODO: Check for overflow
            node->etype = new_type(TYPE_I64);
        } else if (lhs->base == TYPE_POINTER) {
            if (!is_int_type(rhs)) {
                std::cerr << "Cannot add non-integer type to pointer at " << token->location->filename << token->
                        location->line << ":" << token->location->column << std::endl;
                exit(1);
            }

            node->etype = lhs;
            // Pointer arithmetic
            if (size_of_type(lhs->ptr) != 1) {
                auto mul = new_node_binop(AST_MUL, node->binary.rhs, node_from_int_literal(size_of_type(lhs->ptr)));
                node->binary.rhs = mul;
            }
        } else if (rhs->base == TYPE_POINTER) {
            if (!is_int_type(lhs)) {
                std::cerr << "Cannot add non-integer type to pointer at " << token->location->filename << token->
                        location->line << ":" << token->location->column << std::endl;
                exit(1);
            }

            node->etype = rhs;
            // Pointer arithmetic
            if (size_of_type(rhs->ptr) != 1) {
                auto mul = new_node_binop(AST_MUL, node->binary.lhs, node_from_int_literal(size_of_type(rhs->ptr)));
                node->binary.rhs = mul;
            }
        } else if (is_float_type(lhs) || is_float_type(rhs)) {
            // TODO: Handle different sized floats
            node->etype = new_type(TYPE_F64);
            node->binary.lhs = convert_to_float(node->binary.lhs);
            node->binary.rhs = convert_to_float(node->binary.rhs);
        } else {
            std::cerr << "Invalid types for addition at " << token->location->filename << ":" << token->location->line << ":"
                    << token->location->column << std::endl;
            exit(1);
        }
    } else if (node->type == AST_MINUS) {
        if (is_int_type(lhs) && is_int_type(rhs)) {
            node->etype = new_type(TYPE_U64);
        } else if (lhs->base == TYPE_POINTER && is_int_type(rhs)) {
            node->etype = lhs;
            // Pointer arithmetic
            if (size_of_type(lhs->ptr) != 1) {
                auto mul = new_node_binop(AST_MUL, node->binary.rhs, node_from_int_literal(size_of_type(lhs->ptr)));
                node->binary.rhs = mul;
            }
        } else if (is_int_type(lhs) && rhs->base == TYPE_POINTER) {
            node->etype = rhs;
            // Pointer arithmetic
            if (size_of_type(rhs->ptr) != 1) {
                auto mul = new_node_binop(AST_MUL, node->binary.lhs, node_from_int_literal(size_of_type(rhs->ptr)));
                node->binary.rhs = mul;
            }
        } else if (lhs->base == TYPE_POINTER && rhs->base == TYPE_POINTER) {
            node->etype = new_type(TYPE_U64);
            if (!types_equal(lhs->ptr, rhs->ptr)) {
                std::cerr << "Cannot subtract pointers of different types at " << token->location->filename << token->
                        location->line << ":" << token->location->column << std::endl;
                exit(1);
            }

            // Pointer arithmetic! (Divide by size of element)
            if (size_of_type(lhs->ptr) != 1) {
                auto mul = new_node_binop(AST_MUL, node->binary.lhs, node_from_int_literal(size_of_type(lhs->ptr)));
                node->binary.lhs = mul;
            }
        } else if (is_float_type(lhs) || is_float_type(rhs)) {
            // TODO: Handle different sized floats
            node->etype = new_type(TYPE_F64);
            node->binary.lhs = convert_to_float(node->binary.lhs);
            node->binary.rhs = convert_to_float(node->binary.rhs);
        } else {
            std::cerr << "Invalid types for subtraction at " << token->location->filename <<":" << token->location->line <<
                    ":"
                    << token->location->column << std::endl;
            exit(1);
        }
    } else if (node->type == AST_MUL || node->type == AST_DIV || node->type == AST_MOD) {
        if (is_int_type(lhs) && is_int_type(rhs)) {
            node->etype = lhs; // TODO: Check for overflow
        } else if (is_float_type(lhs) || is_float_type(rhs)) {
            if (node->type == AST_MOD) {
                std::cerr << "Cannot perform modulo on float types at " << token->location->filename << ":" << token->location
                        ->line
                        << ":" << token->location->column << std::endl;
                exit(1);
            }

            // TODO: Handle different sized floats
            node->etype = new_type(TYPE_F64);
            node->binary.lhs = convert_to_float(node->binary.lhs);
            node->binary.rhs = convert_to_float(node->binary.rhs);
        } else {
            std::cerr << "Invalid types for multiplication/division/modulo at " << token->location->filename << token->
                    location->line << ":" << token->location->column << std::endl;
            exit(1);
        }
    } else if (node->type == AST_EQ || node->type == AST_NEQ || node->type == AST_LT || node->type == AST_LEQ || node->type
            == AST_GT || node->type == AST_GEQ) {
        if (is_int_type(lhs) && is_int_type(rhs)) {
            node->etype = new_type(TYPE_BOOL);
        } else if (is_float_type(lhs) || is_float_type(rhs)) {
            node->etype = new_type(TYPE_BOOL);
            node->binary.lhs = convert_to_float(node->binary.lhs);
            node->binary.rhs = convert_to_float(node->binary.rhs);
        } else if (lhs->base == TYPE_POINTER && rhs->base == TYPE_POINTER) {
            node->etype = new_type(TYPE_BOOL);
            // if (!types_equal(lhs->ptr, rhs->ptr)) {
            //     std::cerr << "Cannot compare pointers of different types at " << token->location->filename << ":" << token->
            //             location->line << ":" << token->location->column << std::endl;
            //     exit(1);
            // }
        } else {
            std::cerr << "Invalid types for comparison at " << token->location->filename << ":" << token->location->line
                    << ":" << token->location->column << std::endl;
            exit(1);
        }
    } else if (node->type == AST_AND || node->type == AST_OR) {
        if (lhs->base != TYPE_BOOL || rhs->base != TYPE_BOOL) {
            std::cerr << "Invalid types for logical operation at " << token->location->filename << ":" << token->location->line
                    << ":" << token->location->column << std::endl;
            exit(1);
        }
        node->etype = new_type(TYPE_BOOL);
    } else {
        // FIXME: This isn't very correct, but it's probably good enough for now
        node->etype = new_type(TYPE_U64);
        if (is_float_type(lhs) || is_float_type(rhs)) {
            node->binary.lhs = convert_to_float(node->binary.lhs);
            node->binary.rhs = convert_to_float(node->binary.rhs);
        }
    }

    return node;
}

NodeType binary_token_to_op(TokenType type) {
    static_assert(NUM_NODE_TYPES == 47, "Exhaustive match in binary_token_to_op()");

    if (type == TOKEN_PLUS) return AST_PLUS;
    if (type == TOKEN_MINUS) return AST_MINUS;
    if (type == TOKEN_STAR) return AST_MUL;
    if (type == TOKEN_SLASH) return AST_DIV;
    if (type == TOKEN_PERCENT) return AST_MOD;
    if (type == TOKEN_LSHIFT) return AST_LSHIFT;
    if (type == TOKEN_RSHIFT) return AST_RSHIFT;
    if (type == TOKEN_AND) return AST_AND;
    if (type == TOKEN_OR) return AST_OR;
    if (type == TOKEN_EQ) return AST_EQ;
    if (type == TOKEN_NEQ) return AST_NEQ;
    if (type == TOKEN_LT) return AST_LT;
    if (type == TOKEN_LEQ) return AST_LEQ;
    if (type == TOKEN_GT) return AST_GT;
    if (type == TOKEN_GEQ) return AST_GEQ;
    if (type == TOKEN_AMPERSAND) return AST_BWAND;
    if (type == TOKEN_BAR) return AST_BWOR;
    if (type == TOKEN_CARET) return AST_XOR;

    std::cerr << "Unknown binary operator: " << type << std::endl;
    exit(1);
}

NodeType compound_assignment_token_to_op(TokenType type) {
    static_assert(NUM_NODE_TYPES == 47, "Exhaustive match in assignment_token_to_op()");

    if (type == TOKEN_PLUS_EQUAL) return AST_PLUS;
    if (type == TOKEN_MINUS_EQUAL) return AST_MINUS;
    if (type == TOKEN_STAR_EQUAL) return AST_MUL;
    if (type == TOKEN_SLASH_EQUAL) return AST_DIV;
    if (type == TOKEN_PERCENT_EQUAL) return AST_MOD;
    if (type == TOKEN_LSHIFT_EQUAL) return AST_LSHIFT;
    if (type == TOKEN_RSHIFT_EQUAL) return AST_RSHIFT;
    if (type == TOKEN_AND_EQUAL) return AST_AND;
    if (type == TOKEN_OR_EQUAL) return AST_OR;
    if (type == TOKEN_CARET_EQUAL) return AST_XOR;
    if (type == TOKEN_AMPERSAND_EQUAL) return AST_BWAND;
    if (type == TOKEN_BAR_EQUAL) return AST_BWOR;

    std::cerr << "Unknown assignment operator: " << type << std::endl;
    exit(1);
}

Node *new_node_binop(NodeType type, Node *lhs, Node *rhs) {
    auto node = new_node(type);
    node->binary.lhs = lhs;
    node->binary.rhs = rhs;
    return type_check_binary(node, nullptr);
}

Node *node_from_int_literal(uint64_t value) {
    auto node = new_node(AST_LITERAL);
    node->literal.as_int = value;
    node->etype = new_type(TYPE_U64);
    return node;
}

bool is_binary_op(NodeType type) {
    static_assert(NUM_NODE_TYPES == 47, "Exhaustive match in is_binary_op");
    if (type == AST_PLUS) return true;
    if (type == AST_MINUS) return true;
    if (type == AST_MUL) return true;
    if (type == AST_DIV) return true;
    if (type == AST_MOD) return true;
    if (type == AST_LSHIFT) return true;
    if (type == AST_RSHIFT) return true;
    if (type == AST_AND) return true;
    if (type == AST_BWAND) return true;
    if (type == AST_OR) return true;
    if (type == AST_BWOR) return true;
    if (type == AST_XOR) return true;
    if (type == AST_EQ) return true;
    if (type == AST_NEQ) return true;
    if (type == AST_LT) return true;
    if (type == AST_LEQ) return true;
    if (type == AST_GT) return true;
    if (type == AST_GEQ) return true;
    return false;
}

bool is_unary_op(NodeType type) {
    static_assert(NUM_NODE_TYPES == 47, "Exhaustive match in is_unary_op");
    if (type == AST_NEG) return true;
    if (type == AST_NOT) return true;
    if (type == AST_BWINV) return true;
    if (type == AST_ADDRESS_OF) return true;
    if (type == AST_DEREF) return true;
    return false;
}

size_t compound_push_field(Type *compound, char *name, Type *type, size_t base_offset) {
    if (compound->base != TYPE_STRUCT && compound->base != TYPE_UNION) {
        std::cerr << "Cannot push field to non-compound type" << std::endl;
        exit(1);
    }

    auto is_union = compound->base == TYPE_UNION;

    size_t field_size = size_of_type(type);
    auto offset_factor = std::min(field_size, (size_t)8);
    auto offset = is_union ? 0 : align_up(compound->size, offset_factor);
    compound->size = is_union ? std::max(compound->size, field_size) : offset + field_size;
    compound->fields->push_back(new_variable(name, type, base_offset + offset));

    return offset;
}

Variable * compound_find_field(Type *type, char *name) {
    for (auto field: *type->fields) {
        if (strcmp(field->name, name) == 0) {
            return field;
        }

        // If this is an anonymous field, we look inside it:
        if (strcmp(field->name, "<anonymous>") == 0) {
            auto inner = compound_find_field(field->type, name);
            if (inner != nullptr) return inner;
        }
    }

    return nullptr;
}

Node * compound_find_method(Type *type, char *name) {
    for (auto method: *type->methods) {
        if (strcmp(method->function.name, name) == 0) {
            return method;
        }
    }
    return nullptr;
}

char* node_type_to_string(NodeType type) {
    static_assert(NUM_NODE_TYPES == 47, "Exhaustive match in node_type_to_string()");
    switch (type) {
        case AST_LITERAL: return "AST_LITERAL";
        case AST_FUNCTION: return "AST_FUNCTION";
        case AST_RETURN: return "AST_RETURN";
        case AST_PROGRAM: return "AST_PROGRAM";
        case AST_BLOCK: return "AST_BLOCK";
        case AST_CONVERT: return "AST_CONVERT";
        case AST_BUILTIN: return "AST_BUILTIN";
        case AST_LOCAL_VAR: return "AST_LOCAL_VAR";
        case AST_GLOBAL_VAR: return "AST_GLOBAL_VAR";
        case AST_DEREF: return "AST_DEREF";
        case AST_MEMBER: return "AST_MEMBER";
        case AST_FUNCTION_CALL: return "AST_FUNCTION_CALL";
        case AST_VAR_DECLARATION: return "AST_VAR_DECLARATION";
        case AST_WHILE: return "AST_WHILE";
        case AST_CONTINUE: return "AST_CONTINUE";
        case AST_BREAK: return "AST_BREAK";
        case AST_ADDRESS_OF: return "AST_ADDRESS_OF";
        case AST_NEG: return "AST_NEG";
        case AST_NOT: return "AST_NOT";
        case AST_MUL: return "AST_MUL";
        case AST_EQ: return "AST_EQ";
        case AST_NEQ: return "AST_NEQ";
        case AST_ASSIGN: return "AST_ASSIGN";
        case AST_PLUS: return "AST_PLUS";
        case AST_CONDITIONAL: return "AST_CONDITIONAL";
        case AST_BWINV: return "AST_BWINV";
        case AST_MINUS: return "AST_MINUS";
        case AST_DIV: return "AST_DIV";
        case AST_MOD: return "AST_MOD";
        case AST_LSHIFT: return "AST_LSHIFT";
        case AST_RSHIFT: return "AST_RSHIFT";
        case AST_AND: return "AST_AND";
        case AST_BWAND: return "AST_BWAND";
        case AST_OR: return "AST_OR";
        case AST_BWOR: return "AST_BWOR";
        case AST_XOR: return "AST_XOR";
        case AST_LT: return "AST_LT";
        case AST_LEQ: return "AST_LEQ";
        case AST_GT: return "AST_GT";
        case AST_GEQ: return "AST_GEQ";
        case AST_IF: return "AST_IF";
        case AST_ENUM: return "AST_ENUM";
        case AST_CONSTANT: return "AST_CONSTANT";
        case AST_FOR: return "AST_FOR";
        case AST_MATCH: return "AST_MATCH";
        case AST_CASE: return "AST_CASE";
        case AST_DEFER: return "AST_DEFER";
        case NUM_NODE_TYPES: return "NUM_NODE_TYPES";
    }

    std::cerr << "Unknown node type in node_type_to_string(): " << type << std::endl;
    exit(1);
}

void Node::dump(int depth) {
    for (auto i = 0; i < depth; i++) {
        std::cout << "  ";
    }
    if (this->type == AST_PROGRAM || this->type == AST_BLOCK) {
        std::cout << node_type_to_string(this->type) << std::endl;
        for (auto stmt: *this->block.children) {
            stmt->dump(depth + 1);
        }
    } else if (is_binary_op(this->type)) {
        std::cout << node_type_to_string(this->type) << std::endl;
        this->binary.lhs->dump(depth + 1);
        this->binary.rhs->dump(depth + 1);
    } else if (is_unary_op(this->type) || this->type == AST_RETURN) {
        std::cout << node_type_to_string(this->type) << std::endl;
        this->expr->dump(depth + 1);
    } else if (this->type == AST_CONDITIONAL || this->type == AST_IF) {
        std::cout << node_type_to_string(this->type) << std::endl;
        this->conditional.condition->dump(depth + 1);
        this->conditional.then->dump(depth + 1);
        if (this->conditional.els) {
            this->conditional.els->dump(depth + 1);
        }
    } else if (this->type == AST_LITERAL) {
        if (is_int_type(this->etype)) {
            std::cout << this->literal.as_int << std::endl;
        } else if (this->etype->base == TYPE_POINTER) {
            std::cout << "\"" << this->literal.as_string << "\"" << std::endl;
            // TODO: add char
        } else if (is_float_type(this->etype)) {
            std::cout << this->literal.as_string << std::endl;
        } else if (this->etype->base == TYPE_BOOL) {
            std::cout << (this->literal.as_int ? "true" : "false") << std::endl;
        } else {
            std::cerr << "Unknown literal type" << std::endl;
            exit(1);
        }
    } else if (this->type == AST_FUNCTION) {
        std::cout << "fn " << this->function.name << "()" << std::endl;
        if (this->function.body != nullptr) {
            this->function.body->dump(depth + 1);
        }
    } else if (this->type == AST_VAR_DECLARATION) {
        puts("let "); puts(this->var_decl.var.name);
        if (this->var_decl.var.type->base == TYPE_POINTER) {
            puts(": ");
            puts(create_type_string(this->var_decl.var.type));
        }
        if (this->var_decl.init) {
            puts(" =\n");
            this->var_decl.init->dump(depth + 1);
        } else {
            std::cout << std::endl;
        }
    } else if (this->type == AST_ASSIGN) {
        std::cout << node_type_to_string(this->type) << std::endl;
        this->assign.lhs->dump(depth + 1);
        this->assign.rhs->dump(depth + 1);
    } else if (this->type == AST_LOCAL_VAR || this->type == AST_GLOBAL_VAR) {
        std::cout << this->variable->name << std::endl;
    } else if (this->type == AST_CONVERT) {
        std::cout<< "(" << create_type_string(this->etype) << ")" << std::endl;
        this->expr->dump(depth + 1);
    } else if (this->type == AST_CASE) {
        std::cout << "case " << this->case_stmt.value << ":" << std::endl;
        this->case_stmt.stmt->dump(depth + 1);
    } else if (this->type == AST_MATCH) {
        std::cout << "MATCH" << std::endl;
        this->match.expr->dump(depth + 1);
        for (auto i = 0; i < this->match.cases->size(); ++i) {
            this->match.cases->at(i)->dump(depth + 2);
        }
        if (this->match.defolt) {
            for (auto i = 0; i < (depth+2); ++i)
                std::cout << "  ";
            std::cout << "default:" << std::endl;
            this->match.defolt->dump(depth + 3);
        }
    } else {
        std::cout << node_type_to_string(this->type) << std::endl;
    }
}
