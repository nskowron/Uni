import java.util.ArrayList;

public class PascalsRow
{
    private int n;

    public PascalsRow(final int n) throws IllegalArgumentException
    {
        if(n < 0)
        {
            throw new IllegalArgumentException("n should be >= 0, got " + n);
        }
        this.n = n;
    }

    public long Element(final int m) throws IllegalArgumentException
    {
        if(m < 0 || m > n)
        {
            throw new IllegalArgumentException("m should be 0 <= m <= " + n + ", got " + m);
        }
        return Math.NewtonSymbol(n, m);
    }

    public ArrayList<Long> Row()
    {
        ArrayList<Long> result = new ArrayList<Long>(0);
        for(int m = 0; m <= n; ++m)
        {
            result.add(Math.NewtonSymbol(n, m));
        }
        return result;
    }
}