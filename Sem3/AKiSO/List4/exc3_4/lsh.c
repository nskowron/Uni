#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>
#include <fcntl.h>

#define BUFFER_SIZE 1024
#define TOKEN_SIZE 64
#define TOKEN_DELIM " \t\r\n\a"


volatile pid_t active_pid = -1;

void sigchld_handler(int signo) {
    (void)signo;
    while (waitpid(-1, NULL, WNOHANG) > 0);
}

void sigint_handler(int signo) {
    (void)signo;
    if (active_pid > 0) {
        kill(active_pid, SIGINT);
    }
}

int lsh_cd(char **args) {
    if (args[1] == NULL) {
        fprintf(stderr, "lsh: expected argument to \"cd\"\n");
    } else {
        if (chdir(args[1]) != 0) {
            perror("lsh");
        }
    }
    return 1;
}

int lsh_exit(char **args) {
    (void)args;
    exit(0);
}

char *builtin_str[] = { "cd", "exit" };
int (*builtin_func[]) (char **) = { &lsh_cd, &lsh_exit };
int lsh_num_builtins() {
    return sizeof(builtin_str) / sizeof(char *);
}

void handle_redirection(char **args) {
    for (int i = 0; args[i] != NULL; i++) {
        if (strcmp(args[i], ">") == 0) {
            int fd = open(args[i + 1], O_WRONLY | O_CREAT | O_TRUNC, 0644);
            if (fd == -1) perror("lsh");
            dup2(fd, STDOUT_FILENO);
            close(fd);
            args[i] = NULL;
        } else if (strcmp(args[i], "<") == 0) {
            int fd = open(args[i + 1], O_RDONLY);
            if (fd == -1) perror("lsh");
            dup2(fd, STDIN_FILENO);
            close(fd);
            args[i] = NULL;
        } else if (strcmp(args[i], "2>") == 0) {
            int fd = open(args[i + 1], O_WRONLY | O_CREAT | O_TRUNC, 0644);
            if (fd == -1) perror("lsh");
            dup2(fd, STDERR_FILENO);
            close(fd);
            args[i] = NULL;
        }
    }
}

int execute_pipeline(char **commands[], int num_commands) {
    int status;
    int pipefd[2];
    pid_t pid;
    int fd_in = 0;

    for (int i = 0; i < num_commands; i++) {
        pipe(pipefd);
        if ((pid = fork()) == 0) {
            dup2(fd_in, STDIN_FILENO);
            if (i < num_commands - 1) {
                dup2(pipefd[1], STDOUT_FILENO);
            }
            close(pipefd[0]);
            handle_redirection(commands[i]);
            execvp(commands[i][0], commands[i]);
            perror("lsh");
            exit(EXIT_FAILURE);
        } else if (pid > 0) {
            waitpid(pid, NULL, 0);
            close(pipefd[1]);
            fd_in = pipefd[0];
        } else {
            perror("lsh");
            return -1;
        }
    }
    exit(0);
}

int split_into_pipelines(char **args, char **commands[]) {
    int num_commands = 0;
    commands[num_commands++] = args;
    for (int i = 0; args[i] != NULL; i++) {
        if (strcmp(args[i], "|") == 0) {
            args[i] = NULL;
            commands[num_commands++] = &args[i + 1];
        }
    }
    return num_commands;
}

int lsh_launch(char **args, int run_in_background) {
    pid_t pid, wpid;
    int status;

    char **commands[TOKEN_SIZE];
    int num_commands = split_into_pipelines(args, commands);

    pid = fork();
    if (pid == 0) {
        /* if (execvp(args[0], args) == -1) {
            perror("lsh");
        }
        exit(EXIT_FAILURE); */
        execute_pipeline(commands, num_commands);
    } else if (pid < 0) {
        perror("lsh");
    } else {
        if (!run_in_background) {
            active_pid = pid;
            do {
                waitpid(pid, &status, WUNTRACED);
            } while (!WIFEXITED(status) && !WIFSIGNALED(status));
        } else {
            printf("[PID %d] Running in background\n", pid);
        }
    }

    return 1;
}

int lsh_execute(char **args) {
    if (args[0] == NULL) {
        return 1;
    }

    for (int i = 0; i < lsh_num_builtins(); i++) {
        if (strcmp(args[0], builtin_str[i]) == 0) {
            return (*builtin_func[i])(args);
        }
    }

    int run_in_background = 0;
    int i = 0;
    while (args[i] != NULL) i++;
    if (i > 0 && strcmp(args[i - 1], "&") == 0) {
        run_in_background = 1;
        args[i - 1] = NULL;
    }

    return lsh_launch(args, run_in_background);
}

char **lsh_split_line(char *line) {
    int bufsize = TOKEN_SIZE, position = 0;
    char **tokens = malloc(bufsize * sizeof(char*));
    char *token;

    if (!tokens) {
        fprintf(stderr, "lsh: allocation error\n");
        exit(EXIT_FAILURE);
    }

    token = strtok(line, TOKEN_DELIM);
    while (token != NULL) {
        tokens[position++] = token;

        if (position >= bufsize) {
            bufsize += TOKEN_SIZE;
            tokens = realloc(tokens, bufsize * sizeof(char*));
            if (!tokens) {
                fprintf(stderr, "lsh: allocation error\n");
                exit(EXIT_FAILURE);
            }
        }

        token = strtok(NULL, TOKEN_DELIM);
    }
    tokens[position] = NULL;
    return tokens;
}

char *lsh_read_line(void) {
    char *line = NULL;
    size_t bufsize = 0;
    
    if (getline(&line, &bufsize, stdin) == -1)
    {
        free(line);
        return NULL;
    }
    return line;
}

void lsh_loop(void) {
    char *line;
    char **args;
    int status;

    do {
        printf("> ");
        line = lsh_read_line();
        if (line == NULL) {
            printf("\n");
            exit(0);
        }
        args = lsh_split_line(line);
        status = lsh_execute(args);

        free(line);
        free(args);
    } while (status);
}

int main(void) {
    signal(SIGCHLD, sigchld_handler);
    signal(SIGINT, sigint_handler);

    lsh_loop();

    return EXIT_SUCCESS;
}
