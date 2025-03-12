public final class Main
{
    public static void main(final String[] args)
    {
        if(args.length < 2)
        {
            System.out.println("Not enough arguments");
            return;
        }

        try 
        {
            String type = args[0];
            double [] data = new double [args.length - 1];
            for(int i = 1; i < args.length; ++i)
            {
                data[i-1] = Double.parseDouble(args[i]);
            }
            IShape shape = ShapeInstantiator.CreateShape(type, data);

            System.out.println(shape.Name() + ":");
            System.out.println("Circumference - " + shape.Circumference());
            System.out.println("Area - " + shape.Area());
        }
        catch(IllegalArgumentException e)
        {
            e.printStackTrace();
        }
    }
}