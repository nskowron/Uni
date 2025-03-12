#include <math_lib.hpp>
#include <utils.hpp>

#include <vector>
#include <stdexcept>


unsigned long long MathLIB::NewtonSymbol(int n, int k) noexcept(false)
{
    if(n < 0 || k < 0)
    {
        throw std::out_of_range(LOG_LOC() + "arguments should not be negative, got " + std::to_string(n) + " and " + std::to_string(k));
    }
    if(k > n)
    {
        throw std::out_of_range(LOG_LOC() + "k must be <= " + std::to_string(n) + ", got  " + std::to_string(k));
    }

    unsigned long long result = 1;
    for(int i = 1; i <= k; ++i)
    {
        result *= n - k + i;
        result /= i;
    }
    return result;
}
