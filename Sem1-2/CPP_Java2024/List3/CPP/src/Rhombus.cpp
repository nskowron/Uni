#include <Rhombus.hpp>
#include <utils.hpp>

#include <stdexcept>
#include <cmath>

Rhombus::Rhombus(double side, double angle) : Quad("Rhombus"), side(side), angle(angle)
{
    if(side < 0)
    {
        throw std::invalid_argument(LOG_LOC() + "Rhombus side cannot be negative, got: " + std::to_string(side));
    }
    if(angle < 0 || angle > 180)
    {
        throw std::invalid_argument(LOG_LOC() + "Rhombus angle can be neither negative nor reflex, got: " + std::to_string(angle));
    }
}

double Rhombus::Circumference() const
{
    return 4.0 * side;
}

double Rhombus::Area() const
{
    return side * side * std::sin(angle * (M_PI / 180.0));
}