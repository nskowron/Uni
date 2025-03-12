#!/bin/bash

(
    echo -e "PPID\tPID\tCOMM\tSTATE\tTTY\tRSS\tPGID\tSID\tOPEN_FILES"

    for proc in /proc/[0-9]*; do
        if [[ -r $proc/status && -r $proc/stat && -r $proc/fd ]]; then
            pid="${proc##*/}"

            ppid=$(awk '/^PPid:/ {print $2}' $proc/status)
            comm=$(awk '/^Name:/ {print $2}' $proc/status)
            state=$(awk '/^State:/ {print $2}' $proc/status)
            rss=$(awk '/^VmRSS:/ {print $2}' $proc/status)
            
            tty=$(awk '{print $7}' $proc/stat)
            pgid=$(awk '{print $5}' $proc/stat)
            sid=$(awk '{print $6}' $proc/stat)
            
            open_files=$(ls $proc/fd | wc -l)

            echo -e "$ppid\t$pid\t$comm\t$state\t$tty\t$rss\t$pgid\t$sid\t$open_files"
        fi
    done 
)| column -t -s $'\t'
