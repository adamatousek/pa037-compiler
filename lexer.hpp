#pragma once
#ifndef SEAGOL_LEXER_HPP
#define SEAGOL_LEXER_HPP

#undef YY_DECL
#define YY_DECL yy::parser::symbol_type seagol::Lexer::next()

#ifndef yyFlexLexerOnce
#include <FlexLexer.h>
#endif

#include "parser.hpp"

namespace seagol {

struct Lexer : yyFlexLexer {
    /* Forward constructors */
    using yyFlexLexer::yyFlexLexer;

    /* Equivalent of yylex(), implemented in Flex-generated lexer.cpp */
    yy::parser::symbol_type next();

    /* Lexer state */
    yy::location loc;
    yy::location dummy_location;
    unsigned comment_level = 0;
    std::string last_line;

    /* Helper functions */
    int unescape( const char *str )
    {
        if ( str[0] != '\\' )
            return str[0];
        switch ( str[1] ) {
        case '\'':
        case '\"':
        case '\\':
        case '\?': return str[1];
        case 'n': return '\n';
        case 't': return '\t';
        case 'a': return '\a';
        case 'b': return '\b';
        case 'v': return '\v';
        case 'f': return '\f';
        case 'r': return '\r';
        }
        _error( "unknown escape sequence" );
    }

    std::string unescape( const char *begin, const char *end )
    {
        std::string str;
        while ( begin < end ) {
            str += unescape( begin );
            if ( *begin == '\\' )
                ++begin;
            ++begin;
        }
        return str;
    }

    auto make_identifier_or_type( const char *iden )
    {
        // TODO: ask the driver whether the identifier names a type
        return yy::parser::make_IDENTIFIER( iden, loc );
    }

    [[noreturn]] void _error( const std::string & msg )
    {
        throw yy::parser::syntax_error( loc, "lexer error: " + msg );
    }

    void readRestOfLine(); // implementated in lexer.l
};

} /* seagol */

#endif /* end of include guard: SEAGOL_LEXER_HPP */
