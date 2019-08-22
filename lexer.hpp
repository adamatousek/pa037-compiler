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
    using yyFlexLexer::yyFlexLexer;
    yy::parser::symbol_type next();

    yy::location loc;
};

} /* seagol */

#endif /* end of include guard: SEAGOL_LEXER_HPP */
