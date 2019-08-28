#include "compiler.hpp"

namespace seagol {

int Compiler::run()
{
    auto putn = [this]( int n, char c ){
        for ( int i = 0; i < n; ++i )
            errf << c;
    };

    int parse_error = 1;
    try {
        parse_error = parser.parse();
    } catch ( yy::parser::syntax_error &e ) {
        if ( e.location == lexer.dummy_location ) {
            errf << e.what() << std::endl;
        } else {
            errf << e.location << ": " << e.what() << std::endl;
            lexer.readRestOfLine();
            errf << lexer.last_line << std::endl;
            auto from = e.location.begin.column - 1;
            putn( from, ' ' );
            putn( e.location.end.column - from - 1, '^' );
            errf << std::endl;
        }
    }
    if ( parse_error ) {
        errf << "seagolc: compilation failed." << std::endl;
        return 1;
    }

    errf << "seagolc: compilation successful\nLLVM bitcode:\n" << std::endl;

    // TODO: select correct stream
    ctx.llmodule->print(llvm::outs(), nullptr);
    return 0;
}

void Compiler::setLocation( const std::string &file, unsigned line,
                            unsigned column)
{
    auto &l = lexer.loc;
    delete l.begin.filename;
    delete l.end.filename;
    l.end.filename = new std::string( file );
    l.end.line = line;
    l.end.column = column;
    l.step();
}

} /* seagol */

namespace yy {
/* Flex-Bison Glue */
parser::symbol_type yylex( seagol::Lexer &lexer ) { return lexer.next(); }

/* Bison++ needs this defined */
void parser::error( const location_type &l, const std::string &m ) {
    throw syntax_error( l, m );
}
}

