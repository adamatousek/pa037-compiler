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
class Compiler;
}
}

%{
#include "compiler.hpp"
#include "parser.hpp"
namespace yy {
parser::symbol_type yylex( seagol::Lexer & );
}
%}

%lex-param { seagol::Lexer &lexer }
%parse-param { seagol::Compiler &compiler }
%parse-param { seagol::Lexer &lexer }

%token END_OF_FILE 0 "end of file"
%token TODO "unimplemented"

%token <std::string> IDENTIFIER "identifier"
%token <uint16_t> TYPE_NAME "type name"
%token <long> CONSTANT_I "integral constant"

%token RETURN "return"

%token ';' '(' ')' '{' '}' '!' '?' ':'
%token L_OR "||"
%token L_AND "&&"
%token EQ "=="
%token NEQ "!="
%token LEQ "<="
%token GEQ ">="
%token SHL "<<"
%token SHR ">>"

%left L_OR
%left L_AND
%left '|'
%left '^'
%left '&'
%nonassoc EQ NEQ
%nonassoc '<' '>' LEQ GEQ
%left SHL SHR
%left '+' '-'
%left '*' '/' '%'

/* %type <seagol::ExprAttr> expr */

%start toplevel

%%

toplevel
    : toplevel_entry_list END_OF_FILE
    ;

toplevel_entry_list
    : toplevel_entry
    | toplevel_entry_list toplevel_entry
    ;

toplevel_entry
    : declaration
    | function_definition
    ;

declaration
    : fn_header ';'
    ;

function_definition
    : fn_header block_stmt
    ;

fn_header
    : type IDENTIFIER arguments
    ;

type
    : TYPE_NAME
    ;

arguments
    : '(' ')'
    | '(' argument_decl_list ')'
    ;

argument_decl_list
    : argument_decl
    | argument_decl_list ',' argument_decl
    ;

argument_decl
    : type
    | type IDENTIFIER
    ;

block_stmt
    : '{' '}'
    | '{' stmt_list '}'
    ;

stmt_list
    : statement
    | stmt_list statement
    ;

statement
    : block_stmt
    | RETURN ';'
    | RETURN expression ';'
    ;

expression
    : conditional_expr
    | unary_expr '=' expression
    ;

conditional_expr
    : expr
    | expr '?' expression ':' conditional_expr
    ;

expr
    : cast_expr
    | expr L_OR  expr
    | expr L_AND expr
    | expr  '+'  expr
    | expr  '-'  expr
    | expr  '*'  expr
    | expr  '/'  expr
    | expr  '%'  expr
    | expr  '|'  expr
    | expr  '^'  expr
    | expr  '&'  expr
    | expr  SHL  expr
    | expr  SHR  expr
    ;

cast_expr
    : unary_expr
    | '(' type ')' cast_expr
    ;

unary_expr
    : postfix_expr
    | unary_op cast_expr
    ;

unary_op
    : '!'
    | '-'
    ;

postfix_expr
    : primary_expr
    ;

primary_expr
    : IDENTIFIER
    | CONSTANT_I
    | '(' expression ')'
    ;
