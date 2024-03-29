#pragma once

#ifndef SEAGOL_COMPILER_HPP
#define SEAGOL_COMPILER_HPP

#include <istream>
#include <ostream>
#include <stdexcept>

SEAGOL_RELAX_WARNINGS
#include <llvm/Target/TargetMachine.h>
SEAGOL_UNRELAX_WARNINGS

#include "lexer.hpp"
#include "parser.hpp"
#include "context.hpp"


namespace seagol {

/*
 * Compiler -- the great driver of things and keeper of state
 */

struct Compiler {
    std::istream &inf;
    std::ostream &outf;
    std::ostream &errf;

    std::string name;

    Context ctx;
    Lexer lexer;
    yy::parser parser;

    bool debug = false;

    Compiler( std::istream &in, std::ostream &out, std::ostream &err,
              const std::string &name )
        : inf( in )
        , outf( out )
        , errf( err )
        , name( name )
        , ctx( name )
        , lexer( ctx, in, out )
        , parser( ctx, lexer )
    {
        errf << "This is Seagol compiler version 0.0.0-alpha." << std::endl;
        parser.set_debug_stream( err );
    }

    int run();

    void setLocation( const std::string &file,
                      unsigned line = 1, unsigned column = 1);

    void traceLexer( bool trace = true ) { lexer.set_debug( trace ); }
    void traceParser( bool trace = true ) { parser.set_debug_level( trace ); }

private:
    std::string target_triple;
    llvm::TargetMachine *target_machine;
    bool prepare_target();
};

} /* seagol */

#endif /* end of include guard: SEAGOL_COMPILER_HPP */
