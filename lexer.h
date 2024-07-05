//
// Created by mahmoud on 7/3/24.
//

#ifndef LEXER_H
#define LEXER_H
#include <cstdint>
#include <vector>

enum TokenType {
    // Keywords
    TOKEN_FN,
    TOKEN_RETURN,
    TOKEN_I8,
    TOKEN_I16,
    TOKEN_I32,
    TOKEN_I64,
    TOKEN_U8,
    TOKEN_U16,
    TOKEN_U32,
    TOKEN_U64,
    TOKEN_F32,
    TOKEN_F64,
    TOKEN_BOOL,
    TOKEN_VOID,
    TOKEN_ANY,
    TOKEN_IMPORT,

    // Literals
    TOKEN_INTLIT,
    TOKEN_STRINGLIT,
    TOKEN_IDENTIFIER,

    // Operators
    TOKEN_STAR,

    // Punctuation
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_COLON,
    TOKEN_SEMICOLON,
    TOKEN_COMMA,

    // Misc
    TOKEN_EOF
};

struct Location;
struct Token;

struct Lexer {
    char *source;
    char *filename;

    size_t position;
    size_t line;
    size_t column;

    static Lexer *create(char *source, char *filename);

    static Lexer *create_from_file(const char *filename);

    [[nodiscard]] Location *location() const;

    void advance(size_t count);

    Token *make_token(TokenType type, size_t advance);

    Token* next();
    Token* peek();
    void reset() {
        position = 0;
        line = 1;
        column = 1;
    }

    Token* expect(TokenType type);
};

struct Location {
    char *filename;
    size_t line;
    size_t column;
};

struct Token {
    TokenType type;
    Location *location;

    union {
        uint64_t as_int;
        char* as_string;
    } value;

    char* to_string() const;
};

inline bool is_literal_token(TokenType type) {
    // TODO: Add floats
    return type == TOKEN_INTLIT || type == TOKEN_STRINGLIT;
}

#endif //LEXER_H
