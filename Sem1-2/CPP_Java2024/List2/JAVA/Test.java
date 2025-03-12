public final class Test
{
    private Test() throws InstantiationError
    {
        throw new InstantiationError("Cannot create instance of a static class Test.");
    }

    public static void main(final String[] args)
    {
        TestNewtonSymbol();
        TestPascalsTriangle();
    }

    public static void TestPascalsTriangle()
    {
        {
            PascalsTriangle PT = new PascalsTriangle(5);
            assert PT.Element(1) == 5;
            assert PT.Element(2) == 10;
            assert PT.Element(3) == 10;
            assert PT.Element(4) == 5;
            assert PT.Element(5) == 1;

            long [] row = PT.Row();
            assert row[2] == 10;
            assert row[4] == 5;
            assert row[5] == 1;
        }

        try
        {
            PascalsTriangle PT = new PascalsTriangle(-12);
            assert false;
        }
        catch(final IllegalArgumentException e)
        {
            assert true;
        }

        try
        {
            PascalsTriangle PT = new PascalsTriangle(12);
            PT.Element(13);
            assert false;
        }
        catch(final IllegalArgumentException e)
        {
            assert true;
        }

        try
        {
            PascalsTriangle PT = new PascalsTriangle(30);
            PT.Element(-13);
            assert false;
        }
        catch(final IllegalArgumentException e)
        {
            assert true;
        }
    }

    public static void TestNewtonSymbol()
    {
        assert Math.NewtonSymbol(0, 0) == 1;
        assert Math.NewtonSymbol(4, 3) == 4;
        assert Math.NewtonSymbol(25, 7) == 480700;
        assert Math.NewtonSymbol(235, 8) == 204511270823145L;
    }
}