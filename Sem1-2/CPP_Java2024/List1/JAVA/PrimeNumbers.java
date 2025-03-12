import java.util.ArrayList;

public class PrimeNumbers
{
    private ArrayList<Integer> nums;

    public PrimeNumbers(final int n) throws IllegalArgumentException
    {
        if(n < 2)
        {
            throw new IllegalArgumentException("n should be >= 2, got " + n);
        }
        nums = Primes.Sieve(n);
    }

    public int Number(int m)
    {
        if(m < 0 || m >= nums.size())
        {
            throw new IllegalArgumentException("m is out of range (" + m + ")");
        }

        return nums.get(m);
    }
}