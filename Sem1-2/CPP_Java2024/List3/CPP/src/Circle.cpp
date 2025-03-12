#include <Circle.hpp>
#include <utils.hpp>

#include <stdexcept>
#include <cmath>

Circle::Circle(double radius) : Shape("Circle"), radius(radius)
{
    if(radius < 0)
    {
        throw std::invalid_argument(LOG_LOC() + "Circle radius cannot be negative, got: " + std::to_string(radius));
    }
}

double Circle::Circumference() const
{
    return 2.0 * M_PI * radius;
}

double Circle::Area() const
{
    return M_PI * radius * radius;
}