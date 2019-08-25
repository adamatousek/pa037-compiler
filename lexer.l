%option c++
%option noyywrap nodefault
%option yyclass="Lexer"
%option debug

%{

#include "compiler.hpp"
typedef yy::parser::token tok;

#define YY_USER_ACTION last_line += yytext; loc.columns( yyleng );
#define yyterminate() yy::parser::make_END_OF_FILE( loc );

typedef yy::parser p;
%}

HEAD    [a-zA-Z_]
WORD    [a-zA-Z0-9_]
ES      (\\[ntabvfr'"\\\\?])

/*
<INITIAL>"/*"           { BEGIN( COMMENT ); assert( comment_level == 0 ); }
*/

%x COMMENT
%x REST_OF_LINE

%%

%{
    auto & l = loc;
    l.step();
%}

[[:blank:]]+            l.step();
<INITIAL,COMMENT>\n+    l.lines( yyleng ); l.step(); last_line.clear();

"//".*                  { /* single-line comment */ }
"/*"                    { BEGIN( COMMENT ); assert( comment_level++ == 0 ); }
<COMMENT>"*"+"/"        { if ( 0 == --comment_level ) { BEGIN( INITIAL ); } }
<COMMENT>"/*"           { ++comment_level; }
<COMMENT>[^/*\n]*       { }
<COMMENT>\*+[^/\n]      { }
<COMMENT>\/+[^*\n]      { }
<COMMENT>[*/]$          { }
<COMMENT><<EOF>>        { throw p::syntax_error( dummy_location,
                            "file ended inside comment" ); }

[(){}\[\];,+\-*/]       return {yytext[0], l};

[1-9][0-9]*             return p::make_CONSTANT_I( strtol( yytext, NULL, 10 ), l );
0[xX][0-9a-fA-F]+       return p::make_CONSTANT_I( strtol( yytext, NULL, 16 ), l );
0[0-9]*                 return p::make_CONSTANT_I( strtol( yytext, NULL, 8 ), l );
[01]+[bB]               return p::make_CONSTANT_I( strtol( yytext, NULL, 2 ), l );
\'([^'\\\n]|{ES})\'     return p::make_CONSTANT_I( unescape( yytext + 1 ), l );
void                    return p::make_TYPE_NAME( TYPEID::VOID, l);
int                     return p::make_TYPE_NAME( TYPEID::INT, l);
char                    return p::make_TYPE_NAME( TYPEID::CHAR, l);

return                  return {tok::RETURN, l};

{HEAD}{WORD}*           return make_identifier_or_type( yytext );

[[:print:]]             { throw p::syntax_error( l,
                            std::string( "unexpected character '" ) +
                            yytext[0] + "'" ); }
.                       { throw p::syntax_error( l, std::string(
                            "unexpected character, code " ) +
                            std::to_string( (unsigned char)(yytext[0]) ) ); }

<<EOF>>                 return yyterminate();

<REST_OF_LINE>.*$       return yyterminate();

%%

void seagol::Lexer::readRestOfLine()
{
    yy_push_state( REST_OF_LINE );
    next();
}

