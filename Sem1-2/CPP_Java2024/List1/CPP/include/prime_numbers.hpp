#ifndef PRIME_NUMBERS_HPP
#define PRIME_NUMBERS_HPP

#include <vector>
#include <exception>

class PrimeNumbers
{
private:
    std::vector<std::size_t> nums;

public:
    PrimeNumbers(long long n) noexcept(false);
    ~PrimeNumbers() = default;

    std::size_t number(int m) const noexcept(false);
};

#endif
