public class Rectangle extends Quad
{
    protected final double side_1;
    protected final double side_2;

    public Rectangle(double side_1, double side_2) throws IllegalArgumentException
    {
        super("Rectangle");
        if(side_1 < 0 || side_2 < 0)
        {
            throw new IllegalArgumentException("Rectanle sides cannot be negative, got: " + side_1 + " " + side_2);
        }
        this.side_1 = side_1;
        this.side_2 = side_2;
    }

    @Override
    public double Circumference()
    {
        return 2.0 * side_1 + 2.0 * side_2;
    }

    @Override
    public double Area()
    {
        return side_1 * side_2;
    }
}
