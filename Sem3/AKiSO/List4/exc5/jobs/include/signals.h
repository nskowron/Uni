#ifndef SIGNALS_H
#define SIGNALS_H


void sigchld_handler(int signo);
void sigint_handler(int signo);
void sigtstp_handler(int signo);

#endif