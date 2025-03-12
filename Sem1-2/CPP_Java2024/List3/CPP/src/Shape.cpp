#include <Shape.hpp>

Shape::Shape(std::string name) : name(name)
{}

std::string Shape::Name() const
{
    return name;
}