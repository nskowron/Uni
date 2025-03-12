#ifndef SHAPE_INSTANTIATOR_HPP
#define SHAPE_INSTANTIATOR_HPP

#include <Shape.hpp>

#include <string>
#include <vector>

class ShapeInstantiator
{
    ShapeInstantiator() = delete;
    ~ShapeInstantiator() = delete;

public:
    static Shape* CreateShape(std::string type, std::vector<double> data) noexcept(false);
};

#endif