#ifndef CIRCLE_HPP
#define CIRCLE_HPP

#include <Shape.hpp>

class Circle : public Shape
{
protected:
    const double radius;

public:
    Circle(double radius) noexcept(false);

    double Circumference() const override;
    double Area() const override;
};

#endif