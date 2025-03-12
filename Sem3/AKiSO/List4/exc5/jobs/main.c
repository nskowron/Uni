#include <execution.h>
#include <signals.h>
#include <global.h>

#include <signal.h>
#include <stdlib.h>
#include <unistd.h>


int main(void) {
    shell_pid = getpid();

    signal(SIGCHLD, sigchld_handler);
    signal(SIGINT, sigint_handler);
    signal(SIGTSTP, sigtstp_handler);

    lsh_loop();

    exit(EXIT_SUCCESS);
}