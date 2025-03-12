#include <pascals_triangle.hpp>
#include <math_lib.hpp>
#include <utils.hpp>

#include <stdexcept>
#include <string>

PascalsTriangle::PascalsTriangle(int n) noexcept(false)
{
    if(n < 0)
    {
        throw std::out_of_range(LOG_LOC() + "n should be >= 0, got " + std::to_string(n));
    }
    this->n = n;
}

unsigned long long PascalsTriangle::element(int m) const noexcept(false)
{
    if(m < 0 || m > n)
    {
        throw std::out_of_range(LOG_LOC() + "m should be 0 <= m <= " + std::to_string(n) + ", got " + std::to_string(m));
    }

    return MathLIB::NewtonSymbol(n, m);
}

std::vector<unsigned long long> PascalsTriangle::row() const
{
    std::vector<unsigned long long> result(n + 1);
    for(int m = 0; m <= n; ++m)
    {
        result[m] = MathLIB::NewtonSymbol(n, m);
    }
    return result;
}
