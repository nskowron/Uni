public class Square extends Quad
{
    protected final double side;

    public Square(double side) throws IllegalArgumentException
    {
        super("Square");
        if(side < 0)
        {
            throw new IllegalArgumentException("Square side cannot be negative, got: " + side);
        }
        this.side = side;
    }

    @Override
    public double Circumference()
    {
        return 4.0 * side;
    }

    @Override
    public double Area()
    {
        return side * side;
    }
}
