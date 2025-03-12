public final class Test
{
    private Test() throws InstantiationError
    {
        throw new InstantiationError("Cannot create instance of a static class Test.");
    }

    public static void main(final String[] args)
    {
        {
            PrimeNumbers PM1 = new PrimeNumbers(2);
            PrimeNumbers PM2 = new PrimeNumbers(23);
            PrimeNumbers PM3 = new PrimeNumbers(65973512);
        }
    
        {
            PrimeNumbers PM = new PrimeNumbers(87620545);
            assert PM.Number(0) == 2;
            assert PM.Number(4) == 11;
        }
    
        {
            PrimeNumbers PM = new PrimeNumbers(100);
            assert PM.Number(0) == 2;
            assert PM.Number(7) == 19;
            assert PM.Number(24) == 97;
    
            try
            {
                PM.Number(25);
    
                assert false;
            }
            catch(final IllegalArgumentException e)
            {
                assert true;
            }
        }
    
        {
            PrimeNumbers PM = new PrimeNumbers(53);
            assert PM.Number(15) == 53;
    
            try
            {
                PM.Number(16);
    
                assert false;
            }
            catch(final IllegalArgumentException e)
            {
                assert true;
            }
            try
            {
                PM.Number(-1);
    
                assert false;
            }
            catch(final IllegalArgumentException e)
            {
                assert true;
            }
        }
    
        try
        {
            PrimeNumbers PM = new PrimeNumbers(1);
    
            assert false;
        }
        catch(final IllegalArgumentException e)
        {
            assert true;
        }
        
        try
        {
            PrimeNumbers PM = new PrimeNumbers(0);
    
            assert false;
        }
        catch(final IllegalArgumentException e)
        {
            assert true;
        }
    }
}