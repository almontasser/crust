//
// Created by mahmoud on 7/4/24.
//

#ifndef PARSER_H
#define PARSER_H
#include "ast.h"
#include "lexer.h"

inline auto global_variables = std::vector<Variable *>();
inline auto global_offset = 0;
inline auto import_paths = std::vector<char *>();

Node* parse_program(Lexer* lexer);
Node * parse_block(Lexer * lexer);

#endif //PARSER_H
