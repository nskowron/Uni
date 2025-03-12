#ifndef PIPES_H
#define PIPES_H

#include <sys/wait.h>


extern pid_t pipe_pid;

void cont_pipe(int signo);
int handle_redirection(char **args);
int execute_pipeline(char **commands[], int num_commands);

#endif
