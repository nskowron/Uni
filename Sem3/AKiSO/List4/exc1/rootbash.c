#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

int main(void)
{
    setuid(0);

    char *args[] = { "/bin/bash", NULL };
    char *envp[] = { NULL };

    if(execve("/bin/bash", args, envp) == -1)
    {
        perror("execve failed");
        exit(EXIT_FAILURE);
    }

    return 0;
}