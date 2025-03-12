// rozwiazanie.c
// Nel Skowronek 279679

#include "functions.h"
#include <stdio.h>

double rozwiazanie(double a, double b, double eps)
{
    double c;
    while(b - a > eps)
    {
        c = (a + b) / 2;

        if(f(c) == 0)
            return c;
        if(f(c) * f(a) < 0)
            b = c;
        else
            a = c;
    }
    return b;
}