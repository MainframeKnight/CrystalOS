#ifndef CRYSTAL_PROCESS_H
#define CRYSTAL_PROCESS_H
#include<unistd.h>
#include<sys/wait.h>
#include<stdlib.h>
#include<string.h>

int execute_file_args(const char* filename, char **args) {
    int new_proc = fork();
    if (new_proc < 0) {
        return -1;
    }
    else if (!new_proc) {
        execv(filename, args);
        exit(1);
    }
    return 0;
}

int execute_file_args_PATH(const char* filename, char **args) {
    int new_proc = fork();
    if (new_proc < 0) {
        return -1;
    }
    else if (!new_proc) {
        execvp(filename, args);
        exit(1);
    }
    return 0;
}

int execute_file(const char* filename) {
    int new_proc = fork();
    if (new_proc < 0) {
        return -1;
    }
    else if (!new_proc) {
        char* args[2] = {0, 0};
        args[0] = (char*)calloc(strlen(filename) + 1, 1);
        strcpy(args[0], filename);
        execv(filename, args);
        free(args[0]);
        exit(1);
    }
    return 0;
}

int execute_file_PATH(const char* filename) {
    int new_proc = fork();
    if (new_proc < 0) {
        return -1;
    }
    else if (!new_proc) {
        char* args[2] = {0, 0};
        args[0] = (char*)calloc(strlen(filename) + 1, 1);
        strcpy(args[0], filename);
        execvp(filename, args);
        free(args[0]);
        exit(1);
    }
    return 0;
}

int wait_generated() {
    int n;
    wait(&n);
    if (WIFEXITED(n)) {
        return 1;
    } else if (WIFSTOPPED(n)) {
        return 2;
    } else if (WIFSIGNALED(n)) {
        return 3;
    }
}

int wait_generated_pid(pid_t pid) {
    int n;
    waitpid(pid, &n, 0);
    if (WIFEXITED(n)) {
        return 1;
    } else if (WIFSTOPPED(n)) {
        return 2;
    } else if (WIFSIGNALED(n)) {
        return 3;
    }
}

int send_signal(pid_t rec, int signal) {
    return syscall(62, rec, signal);
}

int terminate_process(pid_t proc) {
    return send_signal(proc, 9);
}
#endif