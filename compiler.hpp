#pragma once

#ifndef SEAGOL_COMPILER_HPP
#define SEAGOL_COMPILER_HPP

#include <istream>
#include <ostream>
#include <stdexcept>

#include "lexer.hpp"
#include "parser.hpp"


namespace seagol {

/*
 * Compiler -- the great driver of things and keeper of state
 */

struct Compiler {
    std::istream &inf;
    std::ostream &outf;
    std::ostream &errf;

    Lexer lexer;
    yy::parser parser;

    bool debug = false;

    Compiler( std::istream &in, std::ostream &out, std::ostream &err )
        : inf( in )
        , outf( out )
        , errf( err )
        , lexer( in, out )
        , parser( *this, lexer )
    {
        outf << "This is Seagol compiler version 0.0.0-alpha." << std::endl;
    }

    int run();

    void setLocation( const std::string &file,
                      unsigned line = 1, unsigned column = 1);
};

/*
 * Semantic types for parser
 */

struct ExprAttr {
    /*llvm::Type*/ void *type;
    /*llvm::Value*/ void *val;
};

/*
 * Various declarations
 */

/* IDs of primitive types */
struct TYPEID {
    enum : uint16_t {
        UNKNOWN = 0,
        VOID,
        INT,
        CHAR,
        _last_
    };
};


} /* seagol */

#endif /* end of include guard: SEAGOL_COMPILER_HPP */
