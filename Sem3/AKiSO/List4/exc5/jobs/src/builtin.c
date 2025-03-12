#include <builtin.h>
#include <jobs.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


char *builtin_str[] = { "cd", "exit", "jobs", "fg", "bg" };
int (*builtin_func[]) (char **) = { &lsh_cd, &lsh_exit, &lsh_jobs, &lsh_fg, &lsh_bg };
int lsh_num_builtins() {
    return sizeof(builtin_str) / sizeof(char *);
}

int lsh_cd(char **args) {
    if(args[1] == NULL) {
        printf("lsh: expected argument to \"cd\"\n");
    } else {
        if(chdir(args[1]) != 0) {
            perror("lsh");
        }
    }
    return 1;
}

int lsh_exit(char **args) {
    (void)args;
    return 0;
}