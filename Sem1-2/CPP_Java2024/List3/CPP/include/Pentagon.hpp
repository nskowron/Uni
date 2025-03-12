#ifndef PENTAGON_HPP
#define PENTAGON_HPP

#include <Shape.hpp>

class Pentagon : public Shape
{
protected:
    const double side;

public:
    Pentagon(double side) noexcept(false);

    double Circumference() const override;
    double Area() const override;
};

#endif