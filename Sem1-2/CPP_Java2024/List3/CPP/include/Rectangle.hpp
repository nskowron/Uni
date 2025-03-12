#ifndef RECTANGLE_HPP
#define RECTANGLE_HPP

#include <Quad.hpp>

class Rectangle : public Quad
{
protected:
    const double side_1;
    const double side_2;

public:
    Rectangle(double side_1, double side_2) noexcept(false);

    double Circumference() const override;
    double Area() const override;
};

#endif