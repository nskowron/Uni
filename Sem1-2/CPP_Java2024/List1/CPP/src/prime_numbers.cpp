#include <prime_numbers.hpp>
#include <primes_lib.hpp>
#include <utils.hpp>

#include <stdexcept>
#include <string>

PrimeNumbers::PrimeNumbers(long long n) noexcept(false)
{
    if(n < 2)
    {
        throw std::out_of_range(LOG_LOC() + "n should be >= 2, got " + std::to_string(n));
    }
    nums = PrimesLIB::Sieve(n);
}

std::size_t PrimeNumbers::number(int m) const noexcept(false)
{
    if(m < 0 || m >= nums.size())
    {
        throw std::out_of_range(LOG_LOC() + "m is out of range (" + std::to_string(m) + ")");
    }

    return static_cast<std::size_t>(nums.at(m));
}
