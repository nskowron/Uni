public final class Main
{
    public static void main(final String[] args)
    {
        if(args.length < 1)
        {
            System.out.println("Not enough arguments");
            return;
        }

        try
        {
            int n = Integer.parseInt(args[0]);
            PascalsTriangle PM = new PascalsTriangle(n);

            for(int i = 1; i < args.length; ++i)
            {
                try
                {
                    int m = Integer.parseInt(args[i]);
                    System.out.println(args[i] + " - " + PM.Element(m));
                }
                catch(final Exception e)
                {
                    System.out.println(args[i] + " - " + e.getStackTrace()[e.getStackTrace().length - 1] + " " + e.getMessage());
                }
            }
        }
        catch(final Exception e)
        {
            System.out.println(args[0] + " - " + e.getStackTrace()[e.getStackTrace().length - 1] + " " + e.getMessage());
        }
    }
}
