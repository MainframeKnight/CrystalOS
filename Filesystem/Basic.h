#ifndef BASIC_H
#define BASIC_H
#include<unistd.h>
#include<sys/stat.h>
#include<errno.h>
#include<fcntl.h>
#include<string.h>
#include<stdlib.h>

int file_exists(const char* filename) {
    struct stat info;
    return (stat(filename, &info) == -1 && errno == ENOENT) ? 0 : 1;
}

int file_open(const char* filename, int access_mode, int append, int create_if_none) {
    if (!file_exists(filename) && !create_if_none) {
        return -1;
    }
    int flags =  O_CREAT;
    if (access_mode == 1) {
        flags |= O_RDONLY;
    }
    else if (access_mode == 2) {
        flags |= O_WRONLY;
    }
    else if (access_mode == 3) {
        flags |= O_RDWR;
    }
    else {
        return -1;
    }
    if (append) {
        flags |= O_APPEND;
    }
    return open(filename, flags, S_IRWXU);
}

int file_close(int fd) {
    return close(fd);
}

int file_write(int fd, const char* text) {
    return write(fd, text, strlen(text));
}

char* file_readline(int fd) {
    char* str = (char *)malloc(2);
    char c;
    long i = 0, maxlen = 100;
    while (read(fd, &c, 1) > 0 && c != '\n') {
        str[i] = c;
        i++;
        if (i == maxlen - 1) {
            str[i] = '\0';
            char* str_temp = (char *)malloc(100);
            strcpy(str_temp, str);
            free(str);
            str = (char *)malloc(maxlen*2);
            strcpy(str, str_temp);
            free(str_temp);
            maxlen *= 2;
        }
    }
    str[i] = '\0';
    return str;
}
#endif