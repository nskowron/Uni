#include <signal.h>
#include <stdio.h>

void handle_signal(int sig) {}

int main(void)
{
    for(int i = 1; i < NSIG; ++i)
    {
        if(signal(i, handle_signal) == SIG_ERR)
        {
            printf("Unable to register signal handling for signal id: %d\n", i);
        }
    }
}