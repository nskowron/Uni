#include <Pentagon.hpp>
#include <utils.hpp>

#include <stdexcept>
#include <cmath>

Pentagon::Pentagon(double side) : Shape("Pentagon"), side(side)
{
    if(side < 0)
    {
        throw std::invalid_argument(LOG_LOC() + "Pentagon side cannot be negative, got: " + std::to_string(side));
    }
}

double Pentagon::Circumference() const
{
    return 5.0 * side;
}

double Pentagon::Area() const
{
    return 1.25 * side * side * (1.0 / std::tan(36 * (M_PI / 180.0)));
}