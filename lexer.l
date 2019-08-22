%option c++
%option noyywrap nodefault
%option yyclass="Lexer"
%option debug

%{

#include "lexer.hpp"
typedef yy::parser::token tok;

#define YY_USER_ACTION loc.columns( yyleng );
#define yyterminate() yy::parser::make_END_OF_FILE( loc );
%}

%%

%{
    auto & l = loc;
    l.step();
%}

[[:blank:]]+    l.step();
\n+             l.lines( yyleng ); l.step();

[()]            return {yytext[0], l};
.               return {tok::U, l};
<<EOF>>         return yyterminate();
