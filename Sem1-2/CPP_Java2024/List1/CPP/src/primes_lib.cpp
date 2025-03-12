#include <primes_lib.hpp>
#include <utils.hpp>

#include <vector>
#include <stdexcept>


std::vector<std::size_t> PrimesLIB::Sieve(std::size_t n)
{
    if(n < 2)
    {
        throw std::out_of_range(LOG_LOC() + "n should be >= 2, got " + std::to_string(n));
    }

    std::vector<std::size_t> primes{};
    std::vector<bool> is_prime(n + 1, true);

    for(size_t i = 2; i * i <= n; ++i)
    {
        if (is_prime[i])
        {
            for(std::size_t j = i * i; j <= n; j += i)
                is_prime[j] = false;
        }
    }

    for(size_t i = 2; i <= n; ++i)
    {
        if(is_prime[i])
            primes.push_back(i);
    }

    return primes;
}
