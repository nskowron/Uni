public class Pentagon extends Shape
{
    protected final double side;

    public Pentagon(double side) throws IllegalArgumentException
    {
        super("Pentagon");
        if(side < 0)
        {
            throw new IllegalArgumentException("Pentagon side cannot be negative, got: " + side);
        }
        this.side = side;
    }

    @Override
    public double Circumference()
    {
        return 5.0 * side;
    }

    @Override
    public double Area()
    {
        return 1.25 * side * side * (1.0 / Math.tan(Math.toRadians(36)));
    }
}