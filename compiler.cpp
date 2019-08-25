#include "compiler.hpp"

namespace seagol {

int Compiler::run()
{
    try {
        return parser.parse();
    } catch ( yy::parser::syntax_error &e ) {
        if ( e.location == lexer.dummy_location ) {
            errf << e.what() << std::endl;
            return 1;
        }

        errf << e.location << ": " << e.what() << std::endl;
        lexer.readRestOfLine();
        errf << lexer.last_line << std::endl;
        auto from = e.location.begin.column - 1;
        auto putn = [this]( int n, char c ){
            for ( int i = 0; i < n; ++i )
                errf << c;
        };
        putn( from, ' ' );
        putn( e.location.end.column - from - 1, '^' );
        errf << std::endl;
        return 1;
    }
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
[[noreturn]]
void parser::error( const location_type &l, const std::string &m ) {
    throw syntax_error( l, m );
}
}

