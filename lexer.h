//
// Created by mahmoud on 7/3/24.
//

#ifndef LEXER_H
#define LEXER_H
#include <cstdint>
#include <cstdlib>
#include <iostream>
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
    TOKEN_STAR_EQUAL,
    TOKEN_SLASH,
    TOKEN_SLASH_EQUAL,
    TOKEN_PLUS,
    TOKEN_PLUS_EQUAL,
    TOKEN_PLUS_PLUS,
    TOKEN_MINUS,
    TOKEN_MINUS_EQUAL,
    TOKEN_MINUS_MINUS,
    TOKEN_ASSIGN,
    TOKEN_EXCLAMATION,
    TOKEN_EQ,
    TOKEN_NEQ,
    TOKEN_F64,
    TOKEN_BOOL,
    TOKEN_VOID,
    TOKEN_ANY,
    TOKEN_IMPORT,
    TOKEN_LET,
    TOKEN_WHILE,
    TOKEN_CONTINUE,
    TOKEN_BREAK,
    TOKEN_SIZEOF,
    TOKEN_IF,
    TOKEN_ELSE,
    TOKEN_STRUCT,
    TOKEN_ENUM,
    TOKEN_UNION,
    TOKEN_NEW,
    TOKEN_DELETE,
    TOKEN_DEFER,
    TOKEN_MATCH,
    TOKEN_DEFAULT,
    TOKEN_FOR,
    TOKEN_TRUE,
    TOKEN_FALSE,
    TOKEN_CONST,
//    TOKEN_NULL, TODO: Add null keyword

    NUM_KEYWORDS,

    // Literals
    TOKEN_INTLIT,
    TOKEN_STRINGLIT,
    TOKEN_IDENTIFIER,
    TOKEN_FLOATLIT,
    TOKEN_CHARLIT,

    // Operators
    TOKEN_STAR,
    TOKEN_GEQ,
    TOKEN_GT,
    TOKEN_LEQ,
    TOKEN_LT,
    TOKEN_LSHIFT,
    TOKEN_LSHIFT_EQUAL,
    TOKEN_RSHIFT,
    TOKEN_RSHIFT_EQUAL,
    TOKEN_AMPERSAND,
    TOKEN_AMPERSAND_EQUAL,
    TOKEN_AND,
    TOKEN_AND_EQUAL,
    TOKEN_BAR,
    TOKEN_BAR_EQUAL,
    TOKEN_OR,
    TOKEN_OR_EQUAL,
    TOKEN_CARET,
    TOKEN_CARET_EQUAL,
    TOKEN_PERCENT,
    TOKEN_PERCENT_EQUAL,
    TOKEN_QUESTION,
    TOKEN_TILDE,

    // Punctuation
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_COLON,
    TOKEN_COLON_COLON,
    TOKEN_SEMICOLON,
    TOKEN_COMMA,
    TOKEN_DOT,

    // Misc
    TOKEN_EOF,

    NUM_TOKEN_TYPES,
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

    char get_escaped_char(char c) {
        switch (c) {
            case 'n': return '\n';
            case 'r': return '\r';
            case 't': return '\t';
            case '"': return '"';
            case '\\': return '\\';
            case 39: return 39; // Single quote
            case '0': return 0;
            default: {
                std::cerr << filename << ":" << line << ":" << column << ": Unknown escape char: " << c << " (" << (int)
                        c << ")" << std::endl;
                exit(1);
            }
        }
    }

    Token *next();

    Token *peek();

    void reset() {
        position = 0;
        line = 1;
        column = 1;
    }

    Token *expect(TokenType type);
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
        char *as_string;
    } value;

    char *to_string() const;
};

inline bool is_literal_token(TokenType type) {
    return type == TOKEN_INTLIT || type == TOKEN_STRINGLIT || type == TOKEN_CHARLIT || type == TOKEN_FLOATLIT;
}

#endif //LEXER_H
