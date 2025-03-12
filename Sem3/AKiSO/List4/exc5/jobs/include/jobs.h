#ifndef JOBS_H
#define JOBS_H

#include <global.h>

#include <sys/wait.h>


int lsh_jobs(char **args);
int lsh_fg(char **args);
int lsh_bg(char **args);

int add_job(pid_t pid);
pid_t remove_job(int idx);
int find_job(pid_t pid);
void print_job(int idx);
void wait_job(int idx);

#endif