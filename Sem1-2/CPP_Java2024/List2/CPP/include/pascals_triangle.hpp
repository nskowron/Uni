#ifndef PASCALS_TRIANGLE_HPP
#define PASCALS_TRIANGLE_HPP

#include <vector>
#include <exception>

class PascalsTriangle
{
private:
    unsigned int n;

public:
    PascalsTriangle(int n) noexcept(false);
    ~PascalsTriangle() = default;

    unsigned long long element(int m) const noexcept(false);
    std::vector<unsigned long long> row() const;
};

#endif
