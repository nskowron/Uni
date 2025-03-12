#include <pipes.h>

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <sys/wait.h>
#include <signal.h>


pid_t pipe_pid;

void cont_pipe(int signo) {
    (void)signo;
    kill(pipe_pid, SIGCONT);
}

int handle_redirection(char **args) {
    for(int i = 0; args[i] != NULL; i++) {
        if(strcmp(args[i], ">") == 0) {
            int fd = open(args[i + 1], O_WRONLY | O_CREAT | O_TRUNC, 0644);
            if(fd == -1) {
                perror("lsh");
                return -1;
            }
            dup2(fd, STDOUT_FILENO);
            close(fd);
            args[i] = NULL;
        } else if(strcmp(args[i], "<") == 0) {
            int fd = open(args[i + 1], O_RDONLY);
            if(fd == -1) {
                perror("lsh");
                return -1;
            }
            dup2(fd, STDIN_FILENO);
            close(fd);
            args[i] = NULL;
        } else if(strcmp(args[i], "2>") == 0) {
            int fd = open(args[i + 1], O_WRONLY | O_CREAT | O_TRUNC, 0644);
            if(fd == -1) {
                perror("lsh");
                return -1;
            }
            dup2(fd, STDERR_FILENO);
            close(fd);
            args[i] = NULL;
        }
    }
    return 0;
}

int execute_pipeline(char **commands[], int num_commands) {
    int status;
    int pipefd[2];
    pid_t pid;
    int fd_in = 0;

    for (int i = 0; i < num_commands; i++) {
        pipe(pipefd);
        if((pid = fork()) == 0) {
            dup2(fd_in, STDIN_FILENO);
            if (i < num_commands - 1) {
                dup2(pipefd[1], STDOUT_FILENO);
            }
            close(pipefd[0]);
            if(handle_redirection(commands[i]) < 0) {
                exit(EXIT_FAILURE);
            }
            execvp(commands[i][0], commands[i]);
            perror("lsh");
            exit(EXIT_FAILURE);
        } else if(pid > 0) {
            pipe_pid = pid;
            waitpid(pid, &status, 0);
            if(WEXITSTATUS(status) == EXIT_FAILURE) return -1;
            close(pipefd[1]);
            fd_in = pipefd[0];
        } else {
            perror("lsh");
            return -1;
        }
    }
    return 1;
}