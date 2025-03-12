#ifndef QUAD_HPP
#define QUAD_HPP

#include <Shape.hpp>

#include <string>

class Quad : public Shape
{
protected:
    Quad(std::string name) : Shape(name) {}

public:
    virtual ~Quad() override {}
};

#endif