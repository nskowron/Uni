public class Rhombus extends Quad
{
    protected final double side;
    protected final double angle;

    public Rhombus(double side, double angle)
    {
        super("Rhombus");
        if(side < 0)
        {
            throw new IllegalArgumentException("Rhombus side cannot be negative, got: " + side);
        }
        if(angle < 0 || angle > 180)
        {
            throw new IllegalArgumentException("Rhombus angle can be neither negative nor reflex, got: " + angle);
        }
        this.side = side;
        this.angle = angle;
    }

    @Override
    public double Circumference()
    {
        return 4.0 * side;
    }

    @Override
    public double Area()
    {
        return side * side * Math.sin(Math.toRadians(angle));
    }
}