#include <iostream>

extern "C" {
    int factorial( int );
}

int main()
{
    for ( int i = 0; i < 10; ++i )
    {
        std::cout << i << "! = " << factorial( i ) << std::endl;
    }
    return 0;
}
