public class Circle extends Shape 
{
    protected final double radius;

    public Circle(double radius) throws IllegalArgumentException
    {
        super("Circle");
        if(radius < 0)
        {
            throw new IllegalArgumentException("Radius cannot be negative, got: " + radius);
        }
        this.radius = radius;
    }

    @Override
    public double Circumference()
    {
        return 2.0 * Math.PI * radius;
    }

    @Override
    public double Area()
    {
        return Math.PI * radius * radius;
    }
}
