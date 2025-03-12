#ifndef BUILTIN_H
#define BUILTIN_H


extern char *builtin_str[];
extern int (*builtin_func[]) (char **);
int lsh_num_builtins();

int lsh_cd(char **args);
int lsh_exit(char **args);

#endif