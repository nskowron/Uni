public abstract class Shape implements IShape
{
    protected final String name;

    protected Shape(String name)
    {
        this.name = name;
    }

    @Override
    public String Name()
    {
        return name;
    }
}
