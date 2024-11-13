#ifndef CRYSTAL_AUXILLARY_H
#define CRYSTAL_AUXILLARY_H
#include<unistd.h>
#include<stdlib.h>
#include<time.h>

int random_int() {
    int num;
    getentropy(&num, sizeof(int));
    return num;
}

long random_long() {
    long num;
    getentropy(&num, sizeof(long));
    return num;
}

char* get_datetime() {
    char* res = (char*)calloc(128, 1);
    time_t unixtime = time(0);
    struct tm* t_res = localtime(&unixtime);
    strftime(res, 128, "%a %b %d %H:%M:%S %Y\n", t_res);
    return res;
}
#endif