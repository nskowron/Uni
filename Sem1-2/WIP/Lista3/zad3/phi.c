// phi.c
// Nel Skowronek 279679

#include "phi.h"

int phi(long int n)
{
    int result = 0;
    for(int i = 1; i <= n; ++i)
    {
        long int a = n, b = i, c;
        while(b != 0)
        {
            c = b;
            b = a % b;
            a = c;
        }
        if(a == 1)
            ++result;
    }
    return result;
}