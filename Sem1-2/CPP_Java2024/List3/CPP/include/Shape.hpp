#ifndef SHAPE_HPP
#define SHAPE_HPP

#include <string>

class Shape
{
protected:
    std::string name;

    Shape(std::string name);

public:
    virtual ~Shape() {}

    virtual double Area() const = 0;
    virtual double Circumference() const = 0;
    virtual std::string Name() const;
};

#endif