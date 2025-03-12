import java.util.ArrayList;

public class PascalsTriangle
{
    protected final ArrayList<PascalsRow> rows;

    public PascalsTriangle(final int n) throws IllegalArgumentException
    {
        if(n < 0)
        {
            throw new IllegalArgumentException("n should be >= 0, got " + n);
        }

        this.rows = new ArrayList<PascalsRow>(0);
        for(int i = 0; i <= n; ++i)
        {
            this.rows.add(new PascalsRow(i));
        }
    }

    public ArrayList< ArrayList<Long> > GetTriangle()
    {
        ArrayList< ArrayList<Long> > triangle = new ArrayList< ArrayList<Long> >(0);
        for(PascalsRow row : rows)
        {
            triangle.add(row.Row());
        }
        return triangle;
    }
}
