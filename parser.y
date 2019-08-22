%require "3.2"
%language "c++"
%define api.value.type variant
%define api.token.constructor
%locations
%define parse.trace
%define parse.error verbose


%code requires {
namespace seagol {
class Lexer;
}
}

%{
#include "parser.hpp"
namespace yy {
parser::symbol_type yylex( seagol::Lexer & );
}
%}

%lex-param { seagol::Lexer &lexer }
%parse-param { seagol::Lexer &lexer }

%token U END_OF_FILE
%token '(' ')'
%start file

%%

file
    : foo END_OF_FILE

foo
    : %empty
    | foo U
    | foo '(' foo ')'
    ;
