#include <Hexagon.hpp>
#include <utils.hpp>

#include <stdexcept>
#include <cmath>

Hexagon::Hexagon(double side) : Shape("Hexagon"), side(side)
{
    if(side < 0)
    {
        throw std::invalid_argument(LOG_LOC() + "Hexagon side cannot be negative, got: " + std::to_string(side));
    }
}

double Hexagon::Circumference() const
{
    return 6.0 * side;
}

double Hexagon::Area() const
{
    return 1.5 * std::sqrt(3) * side * side;
}