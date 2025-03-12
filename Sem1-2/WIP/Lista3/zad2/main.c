// main.c
// Nel Skowronek 279679

#include "functions.h"
#include <stdio.h>
#include <math.h>

int main(void)
{
    double a, b, eps;
    printf("Podaj poczatek przedzialu: ");
    scanf("%lf", &a);
    printf("Podaj koniec przedzialu: ");
    scanf("%lf", &b);

    if((a > b) || (f(a) * f(b) > 0))
    {
        printf("Zly przedzial!\n");
        return 0;
    }

    printf("Podaj dokladnosc: ");
    scanf("%lf", &eps);

    printf("Wynik: %lf\n", rozwiazanie(a, b, eps));
}