#include <execution.h>
#include <pipes.h>
#include <global.h>
#include <builtin.h>
#include <jobs.h>
#include <utils.h>

#include <stdlib.h>
#include <stdio.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>


void lsh_loop(void) {
    char *line;
    char **args;
    int status;

    do {
        printf("> ");
        line = lsh_read_line();
        if(line == NULL) {
            printf("\n");
            break;
        }
        args = lsh_split_line(line);
        status = lsh_execute(args);

        free(line);
        free(args);
    } while(status);
}

int lsh_execute(char **args) {
    if(args[0] == NULL) {
        return 1;
    }

    for(int i = 0; i < lsh_num_builtins(); i++) {
        if(strcmp(args[0], builtin_str[i]) == 0) {
            return (*builtin_func[i])(args);
        }
    }

    if(num_jobs == MAX_JOBS) {
        perror("too many jobs");
        return 1;
    }

    int run_in_background = 0;
    int i = 0;
    while(args[i] != NULL) i++;
    if(i > 0 && strcmp(args[i - 1], "&") == 0) {
        run_in_background = 1;
        args[i - 1] = NULL;
    }

    lsh_run(args, run_in_background);
    return 1;
}

void lsh_run(char **args, int run_in_background) {
    pid_t pid;

    char **commands[TOKEN_SIZE];
    int num_commands = split_into_pipelines(args, commands);

    pid = fork();
    if (pid == 0) {
        signal(SIGCONT, cont_pipe);
        execute_pipeline(commands, num_commands);
        exit(EXIT_SUCCESS);
    } else if(pid < 0) {
        perror("lsh");
    } else {
        int idx = add_job(pid);
        if(run_in_background) {
            print_job(idx);
        }
        else {
            wait_job(idx);
        }
    }
}