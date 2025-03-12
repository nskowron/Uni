public class Hexagon extends Shape
{
    protected final double side;

    public Hexagon(double side) throws IllegalArgumentException
    {
        super("Hexagon");
        if(side < 0)
        {
            throw new IllegalArgumentException("Hexagon side cannot be negative, got: " + side);
        }
        this.side = side;
    }

    @Override
    public double Circumference()
    {
        return 6.0 * side;
    }

    @Override
    public double Area()
    {
        return 1.5 * Math.sqrt(3) * side * side;
    }
}
