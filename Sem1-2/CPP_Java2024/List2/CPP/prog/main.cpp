#include <pascals_triangle.hpp>
#include <converter_lib.hpp>

#include <vector>
#include <iostream>

int main(const int argc, const char* const argv[])
{
    if(argc < 2)
    {
        std::cerr << "Not enough arguments\n";
        return 1;
    }

    try
    {
        int n = ConverterLIB::ConvertStringTo<int>(std::string(argv[1]));
        PascalsTriangle PT(n);

        for(unsigned int i = 2; i < argc; ++i)
        {
            std::cout << argv[i] << " - ";
            try
            {
                int m = ConverterLIB::ConvertStringTo<int>(std::string(argv[i]));
                std::cout << PT.element(m) << '\n';
            }
            catch(const std::exception& e)
            {
                std::cout << e.what() << '\n';
            }
        }
    }
    catch(const std::exception& e)
    {
        std::cerr << argv[1] << " - " << e.what() << '\n';
        return 1;
    }

    return 0;
}
