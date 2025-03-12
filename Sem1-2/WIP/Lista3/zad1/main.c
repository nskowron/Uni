// main.c
// Nel Skowronek 279679

#include <stdio.h>
#include <string.h>
#include "palindrom.h"

int main(void)
{
    char text[21];
    printf("Podaj lancuch znakow o dlugosci max 20: ");
    scanf("%s", text);

    if(strlen(text) > 20)
    {
        printf("Za dlugi lancuch!\n");
        return 0;
    }

    if(palindrom(text))
        printf("Jest palindromem.\n");
    else
        printf("Nie jest palindromem.\n");
}