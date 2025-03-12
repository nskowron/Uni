#include <global.h>

#include <sys/wait.h>

pid_t shell_pid;
pid_t active_pid = -1;
pid_t jobs_pid[MAX_JOBS] = {-1};
int num_jobs = 0;
int paused = 0;