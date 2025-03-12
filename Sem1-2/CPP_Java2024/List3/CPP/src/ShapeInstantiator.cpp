#include <ShapeInstantiator.hpp>

#include <Shape.hpp>
#include <Circle.hpp>
#include <Pentagon.hpp>
#include <Hexagon.hpp>
#include <Square.hpp>
#include <Rectangle.hpp>
#include <Rhombus.hpp>
#include <utils.hpp>

#include <stdexcept>
#include <string>
#include <vector>
#include <algorithm>

Shape* ShapeInstantiator::CreateShape(std::string type, std::vector<double> data)
{
    if(type == "c")
    {
        if(data.size() != 5)
        {
            throw std::invalid_argument(LOG_LOC() + "Wrong amount of data. Quad requires 5, got: " + std::to_string(data.size()));
        }

        std::vector<double> sides = {data[0], data[1], data[2], data[3]};
        double angle = data[4];

        if(sides[0] == sides[1] && sides[1] == sides[2] && sides[2] == sides[3])
        {
            if(angle == 90.0)
            {
                return new Square(sides[0]);
            }
            else
            {
                return new Rhombus(sides[0], angle);
            }
        }
        else
        {
            std::sort(sides.begin(), sides.end());
            if(sides[0] == sides[1] && sides[2] == sides[3] && angle == 90.0)
            {
                return new Rectangle(sides[0], sides[2]);
            }
            else
            {
                throw std::invalid_argument(LOG_LOC() + "Unknown quad for input: {" + std::to_string(sides[0]) + ", " + std::to_string(sides[1]) + ", " + std::to_string(sides[2]) + ", " + std::to_string(sides[3]) + "}, " + std::to_string(angle));
            }
        }
    }
    else if(type == "o")
    {
        if(data.size() != 1)
        {
            throw std::invalid_argument(LOG_LOC() + "Wrong amount of data. Circle requires 1, got: " + std::to_string(data.size()));
        }

        return new Circle(data[0]);
    }
    else if(type == "p")
    {
        if(data.size() != 1)
        {
            throw std::invalid_argument(LOG_LOC() + "Wrong amount of data. Pentagon requires 1, got: " + std::to_string(data.size()));
        }

        return new Pentagon(data[0]);
    }
    else if(type == "s")
    {
        if(data.size() != 1)
        {
            throw std::invalid_argument(LOG_LOC() + "Wrong amount of data. Hexagon requires 1, got: " + std::to_string(data.size()));
        }

        return new Hexagon(data[0]);
    }
    else
    {
        throw std::invalid_argument(LOG_LOC() + "Unknown type: " + type);
    }
}