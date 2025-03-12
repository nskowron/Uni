#include <Rectangle.hpp>
#include <utils.hpp>

#include <stdexcept>

Rectangle::Rectangle(double side_1, double side_2) : Quad("Rectangle"), side_1(side_1), side_2(side_2)
{
    if(side_1 < 0 || side_2 < 0)
    {
        throw std::invalid_argument(LOG_LOC() + "Rectangle sides cannot be negative, got: " + std::to_string(side_1) + " " + std::to_string(side_2));
    }
}

double Rectangle::Circumference() const
{
    return 2.0 * side_1 + 2.0 * side_2;
}

double Rectangle::Area() const
{
    return side_1 * side_2;
}