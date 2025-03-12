#ifndef UTILS_H
#define UTILS_H


char *lsh_read_line(void);
char **lsh_split_line(char *line);
int split_into_pipelines(char **args, char **commands[]);

#endif