#ifndef SQUARE_HPP
#define SQUARE_HPP

#include <Quad.hpp>

class Square : public Quad
{
protected:
    const double side;

public:
    Square(double side) noexcept(false);

    double Circumference() const override;
    double Area() const override;
};

#endif