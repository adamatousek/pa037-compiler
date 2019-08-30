#include <iostream>
#include <fstream>

#include "compiler.hpp"

int main( int argc, char **argv )
{
    std::string filename = "<stdin>";
    std::ifstream ifs;
    std::ofstream ofs;
    bool from_stdin = true;
    if ( argc > 1 ) {
        filename = argv[1];
        ifs.open( filename );
        if ( !ifs ) {
            std::cerr << "seagolc: cannot open file." << std::endl;
            return 1;
        }
        ofs.open( filename + ".ll" );
        if ( !ifs ) {
            std::cerr << "seagolc: cannot open output file." << std::endl;
            return 1;
        }
        from_stdin = false;
    }

    seagol::Compiler compiler( from_stdin ? std::cin : ifs,
                               from_stdin ? std::cout : ofs, std::cerr,
                               from_stdin ? "output" : filename );
    compiler.setLocation( filename );
    compiler.traceLexer( false );
    compiler.traceParser( false );
    return compiler.run();
}

namespace llvm {
void Value::dump() const {
    this->print( llvm::outs(), true );
}
void Type::dump() const {
    this->print( llvm::outs(), true );
}
} /* llvm */
