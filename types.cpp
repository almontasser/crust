//
// Created by mahmoud on 7/3/24.
//

#include "types.h"
#include "ast.h"

#include <cstdlib>
#include <cstring>
#include <iostream>

size_t size_of_base_type(const BaseType type) {
    static_assert(NUM_BASE_TYPES == 13, "Exhaustive match in size_of_base_type()");

    switch (type) {
        case TYPE_I8:
        case TYPE_U8:
        case TYPE_BOOL:
            return 1;
        case TYPE_I16:
        case TYPE_U16:
            return 2;
        case TYPE_I32:
        case TYPE_U32:
        case TYPE_F32:
            return 4;
        case TYPE_I64:
        case TYPE_U64:
        case TYPE_F64:
        case TYPE_POINTER:
        case TYPE_ANY:
            return 8;
        default:
            return 0;
    }
}

size_t size_of_type(const Type *type) { // NOLINT(*-no-recursion)
    static_assert(NUM_BASE_TYPES == 13, "Exhaustive match in size_of_type()");

    if (type->base < NUM_BASE_TYPES) return type->size;
    if (type->base == TYPE_POINTER) return type->size;
    if (type->base == TYPE_STRUCT) return type->size;
    if (type->base == TYPE_UNION) return type->size;
    if (type->base == TYPE_ENUM) return type->size;
    if (type->base == TYPE_ARRAY) return type->array_size * size_of_type(type->ptr);

    std::cerr << "Unknown type: " << type->base << std::endl;
    exit(1);
}

Type * create_new_base_type(const BaseType base) {
    const auto type = static_cast<Type*>(calloc(1, sizeof(Type)));
    type->base = base;
    type->size = size_of_base_type(base);
    return type;
}

Type * new_type(const BaseType base) {
    static Type* _type_i8 = create_new_base_type(TYPE_I8);
    static Type* _type_i16 = create_new_base_type(TYPE_I16);
    static Type* _type_i32 = create_new_base_type(TYPE_I32);
    static Type* _type_i64 = create_new_base_type(TYPE_I64);
    static Type* _type_u8 = create_new_base_type(TYPE_U8);
    static Type* _type_u16 = create_new_base_type(TYPE_U16);
    static Type* _type_u32 = create_new_base_type(TYPE_U32);
    static Type* _type_u64 = create_new_base_type(TYPE_U64);
    static Type* _type_f32 = create_new_base_type(TYPE_F32);
    static Type* _type_f64 = create_new_base_type(TYPE_F64);
    static Type* _type_bool = create_new_base_type(TYPE_BOOL);
    static Type* _type_void = create_new_base_type(TYPE_VOID);
    static Type* _type_any = create_new_base_type(TYPE_ANY);

    switch (base) {
        case TYPE_I8: return _type_i8;
        case TYPE_I16: return _type_i16;
        case TYPE_I32: return _type_i32;
        case TYPE_I64: return _type_i64;
        case TYPE_U8: return _type_u8;
        case TYPE_U16: return _type_u16;
        case TYPE_U32: return _type_u32;
        case TYPE_U64: return _type_u64;
        case TYPE_F32: return _type_f32;
        case TYPE_F64: return _type_f64;
        case TYPE_BOOL: return _type_bool;
        case TYPE_VOID: return _type_void;
        case TYPE_ANY: return _type_any;
        default: {
            const auto type = static_cast<Type*>(calloc(1, sizeof(Type)));
            type->base = base;
            type->size = size_of_base_type(base);
            return type;
        };
    }
}

Type * new_ptr_type(BaseType base) {
    Type* type = new_type(TYPE_POINTER);
    type->ptr = new_type(base);
    return type;
}

bool is_float_type(const Type *type) {
    return type->base == TYPE_F32 || type->base == TYPE_F64;
}

bool is_int_type(const Type *type) {
    return type->base == TYPE_I8 || type->base == TYPE_I16 || type->base == TYPE_I32 || type->base == TYPE_I64 ||
           type->base == TYPE_U8 || type->base == TYPE_U16 || type->base == TYPE_U32 || type->base == TYPE_U64;
}

bool types_equal(const Type *a, const Type *b) {
    if (a == nullptr && b == nullptr) return true;
    if (a == nullptr || b == nullptr) return false;
    if (a->base == TYPE_ANY || b->base == TYPE_ANY) return true;

    if (a->base == b->base) {
        if (a->base == TYPE_STRUCT || a->base == TYPE_UNION || a->base == TYPE_ENUM) {
            return strcmp(a->struct_name, b->struct_name) == 0;
        }
        return types_equal(a->ptr, b->ptr);
    }
    return false;
}

bool is_struct_or_structptr(Type *type) {
    if (type->base == TYPE_STRUCT || type->base == TYPE_UNION) return true;
    if (type->base == TYPE_POINTER) {
        if (type->ptr->base == TYPE_STRUCT || type->ptr->base == TYPE_UNION) return true;
    }
    return false;
}
