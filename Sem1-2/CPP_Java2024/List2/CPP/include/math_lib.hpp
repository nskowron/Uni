#ifndef MATH_LIB_HPP
#define MATH_LIB_HPP

#include <vector>

class MathLIB
{
public:
    MathLIB() = delete;
    ~MathLIB() = delete;

    static unsigned long long NewtonSymbol(int n, int k) noexcept(false);
};

#endif
