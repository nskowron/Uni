#include <Square.hpp>
#include <utils.hpp>

#include <stdexcept>

Square::Square(double side) : Quad("Square"), side(side)
{
    if(side < 0)
    {
        throw std::invalid_argument(LOG_LOC() + "Square side cannot be negative, got: " + std::to_string(side));
    }
}

double Square::Circumference() const
{
    return 4.0 * side;
}

double Square::Area() const
{
    return side * side;
}