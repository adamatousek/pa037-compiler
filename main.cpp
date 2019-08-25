#include <iostream>
#include <fstream>

#include "compiler.hpp"

int main( int argc, char **argv )
{
    std::string filename = "<stdin>";
    std::ifstream ifs;
    bool from_stdin = true;
    if ( argc > 1 ) {
        filename = argv[1];
        ifs.open( filename );
        if ( !ifs ) {
            std::cerr << "seagolc: cannot open file." << std::endl;
            return 1;
        }
        from_stdin = false;
    }

    seagol::Compiler compiler( from_stdin ? std::cin : ifs, std::cout, std::cerr );
    compiler.setLocation( filename );
    compiler.debug = true;
    return compiler.run();
}
