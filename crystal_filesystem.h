#ifndef CRYSTAL_FILESYSTEM_H
#define CRYSTAL_FILESYSTEM_H
#include<unistd.h>
#include<sys/stat.h>
#include<errno.h>
#include<fcntl.h>
#include<string.h>
#include<stdlib.h>
#include<dirent.h>
#include<stdio.h>

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

int file_rename(const char* old_name, const char* new_name) {
    return rename(old_name, new_name);
}

char* file_read(int fd, int* ok, size_t n_bytes) {
    char* str = (char *)calloc(n_bytes, 1);
    *ok = read(fd, str, n_bytes);
    return str;
}

char* file_readline(int fd) {
    char* str = (char *)calloc(100, 1);
    char c;
    long i = 0, maxlen = 100;
    while (read(fd, &c, 1) > 0 && c != '\n') {
        str[i] = c;
        i++;
        if (i == maxlen - 1) {
            str[i] = '\0';
            char* str_temp = (char *)calloc(maxlen, 1);
            strcpy(str_temp, str);
            free(str);
            str = (char *)calloc(maxlen*2, 1);
            strcpy(str, str_temp);
            free(str_temp);
            maxlen *= 2;
        }
    }
    str[i] = '\0';
    return str;
}

char* current_dir() {
    char* str = (char *)calloc(32, 1);
    int size = 32;
    while (getcwd(str, size) == 0 && errno == ERANGE) {
        free(str);
        size *= 2;
        str = (char *)calloc(size, 1);
    }
    return str;
}

int set_dir(char *new_path) {
    return chdir(new_path);
}

int create_dir(char* dirname) {
    return mkdir(dirname, S_IRWXU);
}

int delete_file(char* filename) {
    return unlink(filename);
}

int delete_dir(char* dirname) {
    int res = rmdir(dirname);
    if (res == -1 && errno == EEXIST) {
        return 1;
    }
    return (res == -1) ? -1 : 0;
}

char** list_dir(char* dirname, int* size) {
    DIR* dp;
    struct dirent *ep;
    int number_of_files = 0;
    dp = opendir(dirname);
    if (!dp) {
        *size = 0;
        return 0;
    }
    while (ep = readdir(dp)) {
        number_of_files++;
    }
    number_of_files -= 2;
    *size = number_of_files;
    char** res = (char**)calloc(number_of_files, sizeof(char*));
    rewinddir(dp);
    int i = 0;
    while ((ep = readdir(dp)) && i < number_of_files) {
        if (strcmp(ep->d_name, ".") && strcmp(ep->d_name, "..")) {
            res[i] = (char*)calloc(256, 1);
            strcpy(res[i], ep->d_name);
            i++;
        }
    }
    closedir(dp);
    return res;
}

int hard_link(const char* filename, const char* alias) {
    return link(filename, alias);
}

int symbolic_link(const char* filename, const char* alias) {
    return symlink(filename, alias);
}

char* read_symlink(const char* filename) {
    char* str = (char *)calloc(32, 1);
    int size = 32;
    while (1) {
        ssize_t sz = readlink(filename, str, size);
        if (sz <= 0) {
            return 0;
        }
        if (sz < size) {
            return str;
        }
        free(str);
        size *= 2;
        str = (char *)calloc(size, 1);
    }
    return str;
}

int test_perm(const char* filename, int perm) {
    return access(filename, perm);
}

int file_type(const char* filename) {
    struct stat info;
    if (stat(filename, &info) == -1) {
        return -1;
    }
    if (S_ISDIR(info.st_mode)) {
        return 1;
    }
    if (S_ISCHR(info.st_mode)) {
        return 2;
    }
    if (S_ISBLK(info.st_mode)) {
        return 3;
    }
    if (S_ISREG(info.st_mode)) {
        return 4;
    }
    if (S_ISFIFO(info.st_mode)) {
        return 5;
    }
    if (S_ISLNK(info.st_mode)) {
        return 6;
    }
    if ((info.st_mode & __S_IFMT) == __S_IFSOCK) {
        return 7;
    }
    return 0;
}
#endif