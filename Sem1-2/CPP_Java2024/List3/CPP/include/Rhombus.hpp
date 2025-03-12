#ifndef RHOMBUS_HPP
#define RHOMBUS_HPP

#include <Quad.hpp>

class Rhombus : public Quad
{
protected:
    const double side;
    const double angle;

public:
    Rhombus(double side, double angle) noexcept(false);

    double Circumference() const override;
    double Area() const override;
};

#endif