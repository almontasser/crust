//
// Created by mahmoud on 7/3/24.
//

#include "lexer.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

Lexer *Lexer::create(char *source, char *filename) {
    auto *lexer = static_cast<Lexer *>(malloc(sizeof(Lexer)));
    lexer->source = source;
    lexer->filename = filename;
    lexer->position = 0;
    lexer->line = 1;
    lexer->column = 1;
    return lexer;
}

Lexer *Lexer::create_from_file(const char *filename) {
    const auto absolute_path = canonicalize_file_name(filename);
    const auto file = fopen(absolute_path, "r");
    if (file == nullptr) {
        return nullptr;
    }

    fseek(file, 0, SEEK_END);
    const auto size = ftell(file);
    fseek(file, 0, SEEK_SET);

    auto *source = static_cast<char *>(malloc(size + 1));
    fread(source, 1, size, file);
    source[size] = '\0';

    fclose(file);

    return Lexer::create(source, absolute_path);
}

const char *token_type_to_string(const TokenType type) {
    static_assert(NUM_TOKEN_TYPES == 89, "Exhaustive match in token_type_to_string()");

    switch (type) {
        case TOKEN_FN: return "TOKEN_FN";
        case TOKEN_RETURN: return "TOKEN_RETURN";
        case TOKEN_I8: return "TOKEN_I8";
        case TOKEN_I16: return "TOKEN_I16";
        case TOKEN_I32: return "TOKEN_I32";
        case TOKEN_I64: return "TOKEN_I64";
        case TOKEN_U8: return "TOKEN_U8";
        case TOKEN_U16: return "TOKEN_U16";
        case TOKEN_U32: return "TOKEN_U32";
        case TOKEN_U64: return "TOKEN_U64";
        case TOKEN_F32: return "TOKEN_F32";
        case TOKEN_F64: return "TOKEN_F64";
        case TOKEN_BOOL: return "TOKEN_BOOL";
        case TOKEN_VOID: return "TOKEN_VOID";
        case TOKEN_ANY: return "TOKEN_ANY";
        case TOKEN_IMPORT: return "TOKEN_IMPORT";
        case TOKEN_LET: return "TOKEN_LET";
        case TOKEN_WHILE: return "TOKEN_WHILE";
        case TOKEN_CONTINUE: return "TOKEN_CONTINUE";
        case TOKEN_BREAK: return "TOKEN_BREAK";
        case TOKEN_SIZEOF: return "TOKEN_SIZEOF";
        case TOKEN_IF: return "TOKEN_IF";
        case TOKEN_ELSE: return "TOKEN_ELSE";
        case TOKEN_STRUCT: return "TOKEN_STRUCT";
        case TOKEN_ENUM: return "TOKEN_ENUM";
        case TOKEN_UNION: return "TOKEN_UNION";
        case TOKEN_NEW: return "TOKEN_NEW";
        case TOKEN_DELETE: return "TOKEN_DELETE";
        case TOKEN_DEFER: return "TOKEN_DEFER";
        case TOKEN_MATCH: return "TOKEN_MATCH";
        case TOKEN_DEFAULT: return "TOKEN_DEFAULT";
        case TOKEN_FOR: return "TOKEN_FOR";
        case TOKEN_TRUE: return "TOKEN_TRUE";
        case TOKEN_FALSE: return "TOKEN_FALSE";
        case TOKEN_CONST: return "TOKEN_CONST";
        // case TOKEN_NULL: return "TOKEN_NULL";

        case TOKEN_INTLIT: return "TOKEN_INTLIT";
        case TOKEN_IDENTIFIER: return "TOKEN_IDENTIFIER";
        case TOKEN_STRINGLIT: return "TOKEN_STRINGLIT";
        case TOKEN_FLOATLIT: return "TOKEN_FLOATLIT";
        case TOKEN_CHARLIT: return "TOKEN_CHARLIT";

        case TOKEN_STAR: return "TOKEN_STAR";
        case TOKEN_STAR_EQUAL: return "TOKEN_STAR_EQUAL";
        case TOKEN_SLASH: return "TOKEN_SLASH";
        case TOKEN_SLASH_EQUAL: return "TOKEN_SLASH_EQUAL";
        case TOKEN_PLUS: return "TOKEN_PLUS";
        case TOKEN_PLUS_EQUAL: return "TOKEN_PLUS_EQUAL";
        case TOKEN_PLUS_PLUS: return "TOKEN_PLUS_PLUS";
        case TOKEN_MINUS: return "TOKEN_MINUS";
        case TOKEN_MINUS_EQUAL: return "TOKEN_MINUS_EQUAL";
        case TOKEN_MINUS_MINUS: return "TOKEN_MINUS_MINUS";
        case TOKEN_ASSIGN: return "TOKEN_ASSIGN";
        case TOKEN_EXCLAMATION: return "TOKEN_EXCLAMATION";
        case TOKEN_EQ: return "TOKEN_EQ";
        case TOKEN_NEQ: return "TOKEN_NEQ";
        case TOKEN_GEQ: return "TOKEN_GEQ";
        case TOKEN_GT: return "TOKEN_GT";
        case TOKEN_LEQ: return "TOKEN_LEQ";
        case TOKEN_LT: return "TOKEN_LT";
        case TOKEN_LSHIFT: return "TOKEN_LSHIFT";
        case TOKEN_LSHIFT_EQUAL: return "TOKEN_LSHIFT_EQUAL";
        case TOKEN_RSHIFT: return "TOKEN_RSHIFT";
        case TOKEN_RSHIFT_EQUAL: return "TOKEN_RSHIFT_EQUAL";
        case TOKEN_AMPERSAND: return "TOKEN_AMPERAND";
        case TOKEN_AMPERSAND_EQUAL: return "TOKEN_AMPERSAND_EQUAL";
        case TOKEN_AND: return "TOKEN_AND";
        case TOKEN_AND_EQUAL: return "TOKEN_AND_EQUAL";
        case TOKEN_BAR: return "TOKEN_BAR";
        case TOKEN_BAR_EQUAL: return "TOKEN_BAR_EQUAL";
        case TOKEN_OR: return "TOKEN_OR";
        case TOKEN_OR_EQUAL: return "TOKEN_OR_EQUAL";
        case TOKEN_CARET: return "TOKEN_CARET";
        case TOKEN_CARET_EQUAL: return "TOKEN_CARET_EQUAL";
        case TOKEN_PERCENT: return "TOKEN_PERCENT";
        case TOKEN_PERCENT_EQUAL: return "TOKEN_PERCENT_EQUAL";
        case TOKEN_QUESTION: return "TOKEN_QUESTION";
        case TOKEN_TILDE: return "TOKEN_TILDE";
        case TOKEN_LPAREN: return "TOKEN_LPAREN";
        case TOKEN_RPAREN: return "TOKEN_RPAREN";
        case TOKEN_LBRACE: return "TOKEN_LBRACE";
        case TOKEN_RBRACE: return "TOKEN_RBRACE";
        case TOKEN_LBRACKET: return "TOKEN_LBRACKET";
        case TOKEN_RBRACKET: return "TOKEN_RBRACKET";
        case TOKEN_COLON: return "TOKEN_COLON";
        case TOKEN_COLON_COLON: return "TOKEN_COLON_COLON";
        case TOKEN_SEMICOLON: return "TOKEN_SEMICOLON";
        case TOKEN_COMMA: return "TOKEN_COMMA";
        case TOKEN_DOT: return "TOKEN_DOT";
        case TOKEN_EOF: return "TOKEN_EOF";
    }

    std::cerr << "Unknown token type: " << type << std::endl;
    exit(1);
}

Location *Lexer::location() const {
    const auto loc = static_cast<Location *>(malloc(sizeof(Location)));
    loc->filename = filename;
    loc->line = line;
    loc->column = column;
    return loc;
}

void Lexer::advance(const size_t count) {
    position += count;
    column += count;
}

Token *Lexer::make_token(const TokenType type, const size_t advance) {
    const auto loc = location();
    const auto token = static_cast<Token *>(malloc(sizeof(Token)));
    token->type = type;
    token->location = loc;
    this->advance(advance);
    return token;
}

Token *Lexer::next() {
    while (source[position] != '\0') {
        switch (const auto c = source[position]) {
            case ' ':
            case '\t':
            case '\r':
                advance(1);
                break;

            case '\n':
                line++;
                column = 1;
                position++;
                break;

            case '*': {
                if (source[position + 1] == '=') {
                    return make_token(TOKEN_STAR_EQUAL, 2);
                }
                return make_token(TOKEN_STAR, 1);
            }
            case '/': {
                if (source[position + 1] == '/') {
                    while (source[position] != '\n') {
                        position++;
                    }
                    break;
                }
                if (source[position + 1] == '*') {
                    while (source[position] != '*' && source[position + 1] != '/') {
                        position++;
                    }
                    position += 2;
                    break;
                }
                if (source[position + 1] == '=') {
                    return make_token(TOKEN_SLASH_EQUAL, 2);
                }
                return make_token(TOKEN_SLASH, 1);
            }
            case '+': {
                if (source[position + 1] == '+') {
                    return make_token(TOKEN_PLUS_PLUS, 2);
                }
                if (source[position + 1] == '=') {
                    return make_token(TOKEN_PLUS_EQUAL, 2);
                }
                return make_token(TOKEN_PLUS, 1);
            }
            case '-': {
                if (source[position + 1] == '-') {
                    return make_token(TOKEN_MINUS_MINUS, 2);
                }
                if (source[position + 1] == '=') {
                    return make_token(TOKEN_MINUS_EQUAL, 2);
                }
                return make_token(TOKEN_MINUS, 1);
            }
            case '!': {
                if (source[position + 1] == '=') {
                    return make_token(TOKEN_NEQ, 2);
                }
                return make_token(TOKEN_EXCLAMATION, 1);
            }
            case '=': {
                if (source[position + 1] == '=') {
                    return make_token(TOKEN_EQ, 2);
                }
                return make_token(TOKEN_ASSIGN, 1);
            }
            case '>': {
                if (source[position + 1] == '=') {
                    return make_token(TOKEN_GEQ, 2);
                }
                if (source[position + 1] == '>') {
                    if (source[position + 2] == '=') {
                        return make_token(TOKEN_RSHIFT_EQUAL, 3);
                    }
                    return make_token(TOKEN_RSHIFT, 2);
                }
                return make_token(TOKEN_GT, 1);
            }
            case '<': {
                if (source[position + 1] == '=') {
                    return make_token(TOKEN_LEQ, 2);
                }
                if (source[position + 1] == '<') {
                    if (source[position + 2] == '=') {
                        return make_token(TOKEN_LSHIFT_EQUAL, 3);
                    }
                    return make_token(TOKEN_LSHIFT, 2);
                }
                return make_token(TOKEN_LT, 1);
            }
            case '&': {
                if (source[position + 1] == '&') {
                    if (source[position + 2] == '=') {
                        return make_token(TOKEN_AND_EQUAL, 3);
                    }
                    return make_token(TOKEN_AND, 2);
                }
                if (source[position + 1] == '=') {
                    return make_token(TOKEN_AMPERSAND_EQUAL, 2);
                }
                return make_token(TOKEN_AMPERSAND, 1);
            }
            case '|': {
                if (source[position + 1] == '|') {
                    if (source[position + 2] == '=') {
                        return make_token(TOKEN_OR_EQUAL, 3);
                    }
                    return make_token(TOKEN_OR, 2);
                }
                if (source[position + 1] == '=') {
                    return make_token(TOKEN_BAR_EQUAL, 2);
                }
                return make_token(TOKEN_BAR, 1);
            }
            case '^': {
                if (source[position + 1] == '=') {
                    return make_token(TOKEN_CARET_EQUAL, 2);
                }
                return make_token(TOKEN_CARET, 1);
            }
            case '%': {
                if (source[position + 1] == '=') {
                    return make_token(TOKEN_PERCENT_EQUAL, 2);
                }
                return make_token(TOKEN_PERCENT, 1);
            }
            case '?': return make_token(TOKEN_QUESTION, 1);
            case '~': return make_token(TOKEN_TILDE, 1);

            case '(': return make_token(TOKEN_LPAREN, 1);
            case ')': return make_token(TOKEN_RPAREN, 1);
            case '{': return make_token(TOKEN_LBRACE, 1);
            case '}': return make_token(TOKEN_RBRACE, 1);
            case '[': return make_token(TOKEN_LBRACKET, 1);
            case ']': return make_token(TOKEN_RBRACKET, 1);
            case ':': {
                if (source[position + 1] == ':') {
                    return make_token(TOKEN_COLON_COLON, 2);
                }
                return make_token(TOKEN_COLON, 1);
            }
            case ';': return make_token(TOKEN_SEMICOLON, 1);
            case ',': return make_token(TOKEN_COMMA, 1);
            case '.': return make_token(TOKEN_DOT, 1);

            default: {
                if (isdigit(c)) {
                    size_t count = 1;
                    bool is_float = false;
                    size_t value = 0;

                    // Hexadecimal
                    if (source[position] == '0' && source[position + 1] == 'x') {
                        count += 2;
                        while (isxdigit(source[position + count])) {
                            count++;
                        }
                        value = std::stoull(std::string(source + position, count), nullptr, 16);
                        // Octal 0 prefixed
                    } else if (source[position] == '0' && isdigit(source[position + 1])) {
                        while (isdigit(source[position + count])) {
                            count++;
                        }
                        value = std::stoull(std::string(source + position, count), nullptr, 8);
                        // Binary 0b prefixed
                    } else if (source[position] == '0' && source[position + 1] == 'b') {
                        count += 2;
                        while (source[position + count] == '0' || source[position + count] == '1') {
                            count++;
                        }
                        value = std::stoull(std::string(source + position, count), nullptr, 2);
                        // Base 10
                    } else {
                        while (position + count < strlen(source) && isdigit(source[position + count])) {
                            count++;
                        }
                        if (position + count < strlen(source) && source[position + count] == '.') {
                            is_float = true;
                            count++;
                            while (position + count < strlen(source) && isdigit(source[position + count])) {
                                count++;
                            }
                        } else {
                            value = std::stoull(std::string(source + position, count));
                        }
                    }

                    while (isdigit(source[position + count])) {
                        count++;
                    }

                    if (!is_float) {
                        const auto token = make_token(TOKEN_INTLIT, count);
                        token->value.as_int = value;
                        return token;
                    } else {
                        char *float_str = static_cast<char *>(malloc(count + 1));
                        strncpy(float_str, source + position, count);
                        float_str[count] = '\0';
                        const auto token = make_token(TOKEN_FLOATLIT, count);
                        token->value.as_string = float_str;
                        return token;
                    }
                }
                if (isalpha(c) || c == '_') {
                    size_t count = 1;
                    while (isalnum(source[position + count]) || source[position + count] == '_') {
                        count++;
                    }

                    if (const auto keyword = std::string(source + position, count); keyword == "fn") {
                        return make_token(TOKEN_FN, count);
                    } else {
                        if (keyword == "return") {
                            return make_token(TOKEN_RETURN, count);
                        }
                        if (keyword == "i8") {
                            return make_token(TOKEN_I8, count);
                        }
                        if (keyword == "i16") {
                            return make_token(TOKEN_I16, count);
                        }
                        if (keyword == "i32") {
                            return make_token(TOKEN_I32, count);
                        }
                        if (keyword == "i64") {
                            return make_token(TOKEN_I64, count);
                        }
                        if (keyword == "u8") {
                            return make_token(TOKEN_U8, count);
                        }
                        if (keyword == "u16") {
                            return make_token(TOKEN_U16, count);
                        }
                        if (keyword == "u32") {
                            return make_token(TOKEN_U32, count);
                        }
                        if (keyword == "u64") {
                            return make_token(TOKEN_U64, count);
                        }
                        if (keyword == "f32") {
                            return make_token(TOKEN_F32, count);
                        }
                        if (keyword == "f64") {
                            return make_token(TOKEN_F64, count);
                        }
                        if (keyword == "bool") {
                            return make_token(TOKEN_BOOL, count);
                        }
                        if (keyword == "void") {
                            return make_token(TOKEN_VOID, count);
                        }
                        if (keyword == "any") {
                            return make_token(TOKEN_ANY, count);
                        }
                        if (keyword == "import") {
                            return make_token(TOKEN_IMPORT, count);
                        }
                        if (keyword == "let") {
                            return make_token(TOKEN_LET, count);
                        }
                        if (keyword == "while") {
                            return make_token(TOKEN_WHILE, count);
                        }
                        if (keyword == "continue") {
                            return make_token(TOKEN_CONTINUE, count);
                        }
                        if (keyword == "break") {
                            return make_token(TOKEN_BREAK, count);
                        }
                        if (keyword == "sizeof") {
                            return make_token(TOKEN_SIZEOF, count);
                        }
                        if (keyword == "if") {
                            return make_token(TOKEN_IF, count);
                        }
                        if (keyword == "else") {
                            return make_token(TOKEN_ELSE, count);
                        }
                        if (keyword == "struct") {
                            return make_token(TOKEN_STRUCT, count);
                        }
                        if (keyword == "enum") {
                            return make_token(TOKEN_ENUM, count);
                        }
                        if (keyword == "union") {
                            return make_token(TOKEN_UNION, count);
                        }
                        if (keyword == "new") {
                            return make_token(TOKEN_NEW, count);
                        }
                        if (keyword == "delete") {
                            return make_token(TOKEN_DELETE, count);
                        }
                        if (keyword == "defer") {
                            return make_token(TOKEN_DEFER, count);
                        }
                        if (keyword == "match") {
                            return make_token(TOKEN_MATCH, count);
                        }
                        if (keyword == "default") {
                            return make_token(TOKEN_DEFAULT, count);
                        }
                        if (keyword == "for") {
                            return make_token(TOKEN_FOR, count);
                        }
                        if (keyword == "true") {
                            return make_token(TOKEN_TRUE, count);
                        }
                        if (keyword == "false") {
                            return make_token(TOKEN_FALSE, count);
                        }
                        if (keyword == "const") {
                            return make_token(TOKEN_CONST, count);
                        }
                        // if (keyword == "null") {
                        //     return make_token(TOKEN_NULL, count);
                        // }

                        // Handle the "here" keyword
                        if (keyword == "here") {
                            char* s = static_cast<char*>(malloc(128)); // should be enough
                            sprintf(s, "%s:%zu:%zu", filename, line, column);

                            auto token = make_token(TOKEN_STRINGLIT, count);
                            token->value.as_string = s;
                            return token;
                        }

                        const auto identifier = static_cast<char *>(malloc(count + 1));
                        strncpy(identifier, source + position, count);
                        identifier[count] = '\0';
                        const auto token = make_token(TOKEN_IDENTIFIER, count);
                        token->value.as_string = identifier;
                        return token;
                    }
                }

                if (c == '"') {
                    auto pos = position + 1;
                    const auto len = strlen(source);
                    while (pos < len) {
                        if (source[pos] == '"' && source[pos - 1] != '\\') {
                            break;
                        }
                        pos++;
                    }

                    const auto loc = location();
                    if (pos == len) {
                        std::cerr << "Unterminated string literal at " << loc->filename << ":" << loc->line << ":" <<
                                loc->column << std::endl;
                        exit(1);
                    }

                    const auto count = pos - position + 1;
                    auto value = static_cast<char *>(malloc(count - 1));
                    strncpy(value, source + position + 1, count - 2);
                    value[count - 2] = '\0';

                    size_t o_pos = 0;
                    size_t n_pos = 0;
                    while (o_pos < count - 1) {
                        if (value[o_pos] == '\\') {
                            value[n_pos] = get_escaped_char(value[++o_pos]);
                        } else {
                            value[n_pos] = value[o_pos];
                        }
                        o_pos++;
                        n_pos++;
                    }

                    const auto token = make_token(TOKEN_STRINGLIT, count);
                    token->value.as_string = value;
                    return token;
                }

                if (c == '\'') {
                    auto pos = position + 1;
                    auto c = source[pos];
                    if (c == '\\') {
                        c = get_escaped_char(source[++pos]);
                    }
                    auto loc = location();
                    if (source[pos + 1] != '\'') {
                        std::cerr << "Unterminated character literal at " << loc->filename << ":" << loc->line << ":" <<
                                loc->column << std::endl;
                        exit(1);
                    }
                    auto token = make_token(TOKEN_CHARLIT, pos - position + 2);
                    token->value.as_int = c;
                    return token;
                }

                std::cerr << "Unknown character: " << c << std::endl;
                exit(1);
            }
        }
    }

    return make_token(TOKEN_EOF, 0);
}

Token *Lexer::peek() {
    const auto pos = position;
    const auto line = this->line;
    const auto column = this->column;
    const auto token = next();
    position = pos;
    this->line = line;
    this->column = column;
    return token;
}

Token *Lexer::expect(const TokenType type) {
    const auto token = next();
    if (token->type != type) {
        std::cerr << "Expected " << token_type_to_string(type) << " but got " << token->to_string() << std::endl;
        exit(1);
    }
    return token;
}

char *Token::to_string() const {
    const auto type = token_type_to_string(this->type);
    const auto loc = this->location;
    const auto filename = loc->filename;
    const auto line = loc->line;
    const auto column = loc->column;
    // const auto value = this->type == TOKEN_INTLIT ? std::to_string(this->value.as_int) : "";
    const auto value = this->type == TOKEN_INTLIT
                           ? std::to_string(this->value.as_int)
                           : this->type == TOKEN_STRINGLIT
                                 ? this->value.as_string
                                 : "";

    const auto str = static_cast<char *>(malloc(256));
    sprintf(str, "%s (%s:%zu:%zu) %s", type, filename, line, column, value.c_str());
    return str;
}
