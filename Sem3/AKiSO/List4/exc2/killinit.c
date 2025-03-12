#include <signal.h>
#include <stdio.h>
#include <unistd.h>

int main(void)
{
    setuid(0);

    if (kill(1, SIGKILL) == -1) 
    {
        perror("Unable to SIGKILL init\n");
    } 
    else 
    {
        printf("That's it, we're all dead\n");
    }
}