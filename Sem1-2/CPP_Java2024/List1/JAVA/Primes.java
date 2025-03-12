import java.util.ArrayList;

public class Primes
{
    private Primes() throws InstantiationError
    {
        throw new InstantiationError("Cannot create instance of a static class Primes.");
    }

    public static ArrayList<Integer> Sieve(final int n)
    {
        if(n < 2)
        {
            throw new IllegalArgumentException("n should be >= 2, got " + n);
        }

        ArrayList<Integer> primes = new ArrayList<>(0);
        boolean[] is_prime = new boolean[n + 1];
        for(int i = 2; i <= n; ++i)
        {
            is_prime[i] = true;
        }

        for(int i = 2; i * i <= n; ++i)
        {
            if(is_prime[i])
            {
                for(int j = i * i; j <= n; j += i)
                    is_prime[j] = false;
            }
        }

        for(int i = 2; i <= n; ++i)
        {
            if(is_prime[i])
                primes.add(i);
        }
        return primes;
    }
}