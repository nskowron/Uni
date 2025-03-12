#include <Shape.hpp>
#include <Circle.hpp>
#include <Pentagon.hpp>
#include <Hexagon.hpp>
#include <Square.hpp>
#include <Rectangle.hpp>
#include <Rhombus.hpp>
#include <ShapeInstantiator.hpp>

#include <cassert>
#include <vector>
#include <stdexcept>
#include <cmath>

int main(void)
{
    // Square Tests
    {
        Square square(5.0);
        assert(square.Name() == "Square");
        assert(square.Circumference() == 20.0);
        assert(square.Area() == 25.0);
    }

    // Circle Tests
    {
        Circle circle(3.0);
        assert(circle.Name() == "Circle");
        assert(circle.Circumference() == 6.0 * M_PI);
    }

    // Pentagon Tests
    {
        Pentagon pentagon(4.0);
        assert(pentagon.Name() == "Pentagon");
        assert(pentagon.Circumference() == 20.0);
    }

    // Hexagon Tests
    {
        Hexagon hexagon(3.0);
        assert(hexagon.Name() == "Hexagon");
        assert(hexagon.Circumference() == 18.0);
    }

    // Rectangle Tests
    {
        Rectangle rectangle(4.0, 6.0);
        assert(rectangle.Name() == "Rectangle");
        assert(rectangle.Circumference() == 20.0);
        assert(rectangle.Area() == 24.0);
    }

    // Rhombus Tests
    {
        Rhombus rhombus(4.0, 60.0);
        assert(rhombus.Name() == "Rhombus");
        assert(rhombus.Circumference() == 16.0);
    }

    // Square Test fail
    try
    {
        Square square(-5.0);
        assert(false);
    }
    catch (const std::invalid_argument& e)
    {
        assert(true);
    }

    try 
    {
        Rectangle rectangle(-5.0, 10.0);
        assert(false);
    } 
    catch (const std::invalid_argument& e)
    {
        assert(true);
    }

    // Test Rhombus
    try 
    {
        Rhombus rhombus(-5.0, 60.0);
        assert(false);
    }
    catch (const std::invalid_argument& e) 
    {
        assert(true);
    }

    // Test Hexagon
    try 
    {
        Hexagon hexagon(-5.0);
        assert(false);
    } 
    catch (const std::invalid_argument& e) 
    {
        assert(true);
    }

    // Test Pentagon
    try 
    {
        Pentagon pentagon(-5.0);
        assert(false);
    } 
    catch (const std::invalid_argument& e) 
    {
        assert(true);
    }

    // Test Circle
    try 
    {
        Circle circle(-5.0);
        assert(false);
    } 
    catch (const std::invalid_argument& e) 
    {
        assert(true);
    }

    // ShapeInstantiator Tests
    {
        std::vector<double> data = {3.0};
        Shape* shape0 = ShapeInstantiator::CreateShape("o", data);
        assert(shape0->Name() == "Circle");
        delete shape0;

        data = {3.0};
        Shape* shape1 = ShapeInstantiator::CreateShape("s", data);
        assert(shape1->Name() == "Hexagon");
        delete shape1;

        data = {4.0};
        Shape* shape2 = ShapeInstantiator::CreateShape("p", data);
        assert(shape2->Name() == "Pentagon");
        delete shape2;

        data = {4.0, 4.0, 5.0, 5.0, 90.0};
        Shape* shape3 = ShapeInstantiator::CreateShape("c", data);
        assert(shape3->Name() == "Rectangle");
        delete shape3;

        data = {1.0, 1.0, 1.0, 1.0, 90.0};
        Shape* shape4 = ShapeInstantiator::CreateShape("c", data);
        assert(shape4->Name() == "Square");
        delete shape4;
    }

    return 0;
}
