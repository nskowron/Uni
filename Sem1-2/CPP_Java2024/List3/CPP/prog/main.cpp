#include <Shape.hpp>
#include <ShapeInstantiator.hpp>

#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

int main(const int argc, const char* argv[])
{
    if(argc < 3)
    {
        std::cout << "Not enough arguments\n";
        return 0;
    }

    Shape* shape;
    try
    {
        std::string type(argv[1]);
        std::vector<double> data = {};
        for(int i = 2; i < argc; ++i)
        {
            data.push_back(std::stod(std::string(argv[i])));
        }
        shape = ShapeInstantiator::CreateShape(type, data);

        std::cout << shape->Name() << ": ";
        std::cout << "\nCircumference - " << shape->Circumference();
        std::cout << "\nArea - " << shape->Area();
    }
    catch(const std::invalid_argument& e)
    {
        std::cout << e.what() << '\n';
    }
    delete shape;
}