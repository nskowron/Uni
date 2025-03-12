// palindrom.c
// Nel Skowronek 279679

#include <string.h>
#include "palindrom.h"

bool palindrom(char napis[])
{
    int length = strlen(napis) - 1;
    for(int i = 0; i <= length / 2; ++i)
    {
        if(napis[i] != napis[length - i])
            return false;
    }
    return true;
}