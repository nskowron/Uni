public class Math
{
    private Math() throws InstantiationError
    {
        throw new InstantiationError("Cannot create instance of a static class Math.");
    }

    public static long NewtonSymbol(final int n, final int k)
    {
        if(n < 0 || k < 0)
        {
            throw new IllegalArgumentException("Arguments cannot be negative (" + n + ", " + k + ")");
        }
        if(k > n)
        {
            throw new IllegalArgumentException("k must be <= n");
        }

        long result = 1;
        for(int i = 1; i <= k; ++i)
        {
            result *= n - k + i;
            result /= i;
        }
        return result;
    }
}