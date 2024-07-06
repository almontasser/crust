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
        case TOKEN_INTLIT: return "TOKEN_INTLIT";
        case TOKEN_IDENTIFIER: return "TOKEN_IDENTIFIER";
        case TOKEN_STRINGLIT: return "TOKEN_STRINGLIT";
        case TOKEN_STAR: return "TOKEN_STAR";
        case TOKEN_PLUS: return "TOKEN_PLUS";
        case TOKEN_ASSIGN: return "TOKEN_ASSIGN";
        case TOKEN_EXCLAMATION: return "TOKEN_EXCLAMATION";
        case TOKEN_EQ: return "TOKEN_EQ";
        case TOKEN_NEQ: return "TOKEN_NEQ";
        case TOKEN_LPAREN: return "TOKEN_LPAREN";
        case TOKEN_RPAREN: return "TOKEN_RPAREN";
        case TOKEN_LBRACE: return "TOKEN_LBRACE";
        case TOKEN_RBRACE: return "TOKEN_RBRACE";
        case TOKEN_LBRACKET: return "TOKEN_LBRACKET";
        case TOKEN_RBRACKET: return "TOKEN_RBRACKET";
        case TOKEN_COLON: return "TOKEN_COLON";
        case TOKEN_SEMICOLON: return "TOKEN_SEMICOLON";
        case TOKEN_COMMA: return "TOKEN_COMMA";
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

            case '*': return make_token(TOKEN_STAR, 1);
            case '+': return make_token(TOKEN_PLUS, 1);
            case '!': {
                if (source[position+1] == '=') {
                    return make_token(TOKEN_NEQ, 2);
                }
                return make_token(TOKEN_EXCLAMATION, 1);
            }
            case '=': {
                if (source[position+1] == '=') {
                    return make_token(TOKEN_EQ, 2);
                }
                return make_token(TOKEN_ASSIGN, 1);
            }

            case '(': return make_token(TOKEN_LPAREN, 1);
            case ')': return make_token(TOKEN_RPAREN, 1);
            case '{': return make_token(TOKEN_LBRACE, 1);
            case '}': return make_token(TOKEN_RBRACE, 1);
            case '[': return make_token(TOKEN_LBRACKET, 1);
            case ']': return make_token(TOKEN_RBRACKET, 1);
            case ':': return make_token(TOKEN_COLON, 1);
            case ';': return make_token(TOKEN_SEMICOLON, 1);
            case ',': return make_token(TOKEN_COMMA, 1);

            default: {
                if (isdigit(c)) {
                    size_t count = 1;
                    while (isdigit(source[position + count])) {
                        count++;
                    }

                    const auto value = std::stoi(std::string(source + position, count));
                    const auto token = make_token(TOKEN_INTLIT, count);
                    token->value.as_int = value;
                    return token;
                }
                if (isalpha(c)) {
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

                    // TODO: handle escape sequences
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
