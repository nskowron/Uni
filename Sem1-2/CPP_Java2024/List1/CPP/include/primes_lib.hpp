#ifndef PRIMES_LIB_HPP
#define PRIMES_LIB_HPP

#include <vector>

class PrimesLIB
{
public:
    PrimesLIB() = delete;
    ~PrimesLIB() = delete;

    static std::vector<std::size_t> Sieve(std::size_t n) noexcept(false);
};

#endif
