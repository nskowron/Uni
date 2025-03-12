#include <signals.h>
#include <global.h>
#include <jobs.h>

#include <stdlib.h>
#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>

#include <stdio.h>


void sigchld_handler(int signo) {
    (void)signo;
    if(getpid() == shell_pid) {
        pid_t child_pid;
        while (child_pid = waitpid(-1, NULL, WNOHANG) > 0) {
            remove_job(find_job(child_pid));
        }
    }
}

void sigint_handler(int signo) {
    (void)signo;
    if(getpid() == shell_pid) {
        if(active_pid > 0) {
            kill(active_pid, SIGINT);
        }
        printf("\n");
    } else {
        exit(EXIT_SUCCESS);
    }
}

void sigtstp_handler(int signo) {
    (void)signo;
    if(getpid() == shell_pid) {
        if(active_pid > 0) {
            paused = 1;
            kill(active_pid, SIGTSTP);
        }
        printf("\n");
    } else {
        signal(SIGTSTP, SIG_DFL);
        raise(SIGTSTP);
    }
}