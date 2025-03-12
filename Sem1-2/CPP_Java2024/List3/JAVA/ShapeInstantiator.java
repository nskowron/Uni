import java.util.Arrays;

public class ShapeInstantiator
{
    private ShapeInstantiator() throws InstantiationError
    {
        throw new InstantiationError("Cannot create instance of class ShapeInstantiator");
    }

    public static IShape CreateShape(String type, double [] data) throws IllegalArgumentException
    {
        if(type.equals("c"))
        {
            if(data.length != 5)
            {
                throw new IllegalArgumentException("Wrong amount of data. Quad requires 5, got: " + data.length);
            }

            double [] sides = {data[0], data[1], data[2], data[3]};
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
                Arrays.sort(sides);
                if(sides[0] == sides[1] && sides[2] == sides[3] && angle == 90.0)
                {
                    return new Rectangle(sides[0], sides[2]);
                }
                else
                {
                    throw new IllegalArgumentException("Unknown quad for input: {" + sides[0] + ", " + sides[1] + ", " + sides[2] + ", " + sides[3] + "}, " + angle);
                }
            }
        }
        else if(type.equals("o"))
        {
            if(data.length != 1)
            {
                throw new IllegalArgumentException("Wrong amount of data. Circle requires 1, got: " + data.length);
            }

            return new Circle(data[0]);
        }
        else if(type.equals("p"))
        {
            if(data.length != 1)
            {
                throw new IllegalArgumentException("Wrong amount of data. Pentagon requires 1, got: " + data.length);
            }

            return new Pentagon(data[0]);
        }
        else if(type.equals("s"))
        {
            if(data.length != 1)
            {
                throw new IllegalArgumentException("Wrong amount of data. Hexagon requires 1, got: " + data.length);
            }

            return new Hexagon(data[0]);
        }
        else
        {
            throw new IllegalArgumentException("Unknown type: " + type);
        }
    }
}
