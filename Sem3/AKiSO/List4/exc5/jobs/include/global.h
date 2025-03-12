#define TOKEN_SIZE 64   //max size of 'query'
#define MAX_JOBS 100

#ifndef GLOBAL_H
#define GLOBAL_H

#include <sys/wait.h>

extern pid_t shell_pid;
extern pid_t active_pid;
extern pid_t jobs_pid[MAX_JOBS];
extern int num_jobs;
extern int paused;

#endif