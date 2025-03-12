public final class Test 
{
    private Test() throws InstantiationError
    {
        throw new InstantiationError("Cannot create instance of static class Test");
    }

    public static void main(final String[] args)
    {   
        try
        {
            double [] data = {1, 1, 3, 3, 60};
            IShape s = ShapeInstantiator.CreateShape("c", data);
            assert false;
        }
        catch(IllegalArgumentException e)
        {
            assert true;
        }

        {
            Square s = new Square(2);
            assert s.Name() == "Square";
            assert s.Circumference() == 8;
            assert s.Area() == 4;
        }

        {
            Rectangle s = new Rectangle(2, 3);
            assert s.Name() == "Rectangle";
            assert s.Circumference() == 10;
            assert s.Area() == 6;
        }

        {
            Rhombus s = new Rhombus(2, 60);
            assert s.Name() == "Rhombus";
            assert s.Circumference() == 8;
            assert s.Area() == 2.0 * Math.sqrt(3);
        }

        {
            Pentagon s = new Pentagon(1);
            assert s.Name() == "Pentagon";
            assert s.Circumference() == 5;
        }

        {
            Hexagon s = new Hexagon(2);
            assert s.Name() == "Hexagon";
            assert s.Circumference() == 12;
            assert s.Area() == 6.0 * Math.sqrt(3);
        }

        try
        {
            new Circle(-1);
            assert false;
        }
        catch(IllegalArgumentException e)
        {
            assert true;
        }
        try
        {
            new Hexagon(-1);
            assert false;
        }
        catch(IllegalArgumentException e)
        {
            assert true;
        }
        try
        {
            new Pentagon(-1);
            assert false;
        }
        catch(IllegalArgumentException e)
        {
            assert true;
        }
        try
        {
            new Square(-1);
            assert false;
        }
        catch(IllegalArgumentException e)
        {
            assert true;
        }
        try
        {
            new Rectangle(-1, 1);
            assert false;
        }
        catch(IllegalArgumentException e)
        {
            assert true;
        }
        try
        {
            new Rhombus(-1, 60);
            assert false;
        }
        catch(IllegalArgumentException e)
        {
            assert true;
        }
        try
        {
            new Rhombus(1, 190);
            assert false;
        }
        catch(IllegalArgumentException e)
        {
            assert true;
        }
        try
        {
            new Rhombus(-1, -90);
            assert false;
        }
        catch(IllegalArgumentException e)
        {
            assert true;
        }
    }
}
