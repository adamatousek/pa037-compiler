#include <iostream>
#include <stdexcept>

#include "lexer.hpp"
#include "parser.hpp"

namespace yy {
parser::symbol_type yylex( seagol::Lexer &lexer ) { return lexer.next(); }

[[noreturn]]
void parser::error( const location_type &l, const std::string &m ) {
    throw syntax_error( l, m );
}
}

int main()
{
    seagol::Lexer lexer( std::cin, std::cout );
    yy::parser p( lexer );
    try {
        return p.parse();
    } catch ( yy::parser::syntax_error &e ) {
        std::cerr << e.location << ": " << e.what() << std::endl;
        return 3;
    }
}
