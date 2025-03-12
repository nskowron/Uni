// main.c
// Nel Skowronek 279679

#include "phi.h"
#include <stdio.h>

int main(void)
{
    long int n;
    printf("Podaj liczbe: ");
    scanf("%ld", &n);

    printf("Wynik: %d\n", phi(n));
}
