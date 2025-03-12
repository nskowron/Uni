#include <utils.h>
#include <global.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define BUFFER_SIZE 1024
#define TOKEN_DELIM " \t\r\n\a"


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