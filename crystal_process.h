#ifndef CRYSTAL_PROCESS_H
#define CRYSTAL_PROCESS_H
#include<unistd.h>
#include<sys/wait.h>

int execute_file_args(const char* filename, char **args) {
    int new_proc = fork();
    if (new_proc < 0) {
        return -1;
    }
    else if (!new_proc) {
        execv(filename, args);
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
    }
    return 0;
}

int execute_file(const char* filename) {
    return execute_file_args(filename, 0);
}

int execute_file_PATH(const char* filename) {
    return execute_file_args_PATH(filename, 0);
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
#endif