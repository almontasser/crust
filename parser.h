//
// Created by mahmoud on 7/4/24.
//

#ifndef PARSER_H
#define PARSER_H
#include "ast.h"
#include "lexer.h"

Node* parse_program(Lexer* lexer);
Node * parse_block(Lexer * lexer);

#endif //PARSER_H
