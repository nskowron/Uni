#ifndef HEXAGON_HPP
#define HEXAGON_HPP

#include <Shape.hpp>

class Hexagon : public Shape
{
protected:
    const double side;

public:
    Hexagon(double side) noexcept(false);

    double Circumference() const override;
    double Area() const override;
};

#endif