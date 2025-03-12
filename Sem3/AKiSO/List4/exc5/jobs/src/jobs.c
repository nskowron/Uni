#include <jobs.h>

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>


int lsh_jobs(char **args) {
    (void)args;
    for(int i = 0; i < num_jobs; ++i) {
        if(jobs_pid[i] != active_pid) print_job(i);
    }
    return 1;
}

int lsh_fg(char **args) {
    if(args[1] == NULL) {
        printf("lsh: expected argument to \"fg\"\n");
    } else {
        int idx = atoi(args[1]) - 1;
        if(idx < num_jobs) {
            kill(jobs_pid[idx], SIGCONT);
            wait_job(idx);
        }
    }
}

int lsh_bg(char **args) {
    if(args[1] == NULL) {
        printf("lsh: expected argument to \"fg\"\n");
    } else {
        int idx = atoi(args[1]) - 1;
        if(idx < num_jobs) {
            kill(jobs_pid[idx], SIGCONT);
            print_job(idx);
        }
    }
}

int add_job(pid_t pid) {
    if(num_jobs >= MAX_JOBS) {
        return -1;
    } else {
        jobs_pid[num_jobs++] = pid;
        return num_jobs - 1;
    }
}

pid_t remove_job(int idx) {
    if(idx >= num_jobs) {
        return -1;
    } else {
        pid_t pid = jobs_pid[idx];
        for(int i = idx; i < num_jobs - 1; ++i) {
            jobs_pid[i] = jobs_pid[i + 1];
        }
        jobs_pid[num_jobs--] = -1;
        return pid;
    }
}

int find_job(pid_t pid) {
    for(int i = 0; i < num_jobs; ++i) {
        if(jobs_pid[i] == pid) return i;
    }
    return -1;
}

void print_job(int idx) {
    if(idx < num_jobs) {
        printf("[%d] %d\n", idx + 1, jobs_pid[idx]);
    }
}

void wait_job(int idx) {
    if(idx < num_jobs) {
        active_pid = jobs_pid[idx];
        waitpid(active_pid, NULL,  WUNTRACED);
        active_pid = -1;
        if(!paused) {
            (void)remove_job(idx);
        } else {
            paused = 0;
        }
    }
}