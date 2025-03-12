// zad1.c
// Nel Skowronek 279679

#include <stdio.h>
#include <stdbool.h>

bool match(char *pattern, char *chain)
{
    if(pattern[0] == '\0' && chain[0] == '\0')
        return true;
    if(pattern[0] == '*')
    {
        if(chain[0] != '\0')
            return (match(pattern + 1, chain) || match(pattern, chain + 1));
        else
            return match(pattern + 1, chain);
    }  
    if(pattern[0] == '\0' || chain[0] == '\0')
        return false;
    if(pattern[0] == '?' || pattern[0] == chain[0])
        return match(pattern + 1, chain + 1);
    
    return false;
}

int main(void)
{
    char pattern[31];
    char chain[31];
    printf("Podaj wzorzec (max 30 znakow): ");
    scanf("%s", pattern);
    printf("Podaj lancuch (max 30 znakow): ");
    scanf("%s", chain);
    
    if(match(pattern, chain))
        printf("Jest match\n");
    else
        printf("Nie ma matcha:(\n");
}
