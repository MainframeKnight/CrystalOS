#ifndef CRYSTAL_AUXILLARY_H
#define CRYSTAL_AUXILLARY_H
#include<unistd.h>
#include<stdlib.h>
#include<time.h>

unsigned int random_uint() {
    unsigned int num;
    getentropy(&num, sizeof(unsigned int));
    return num;
}

unsigned long random_ulong() {
    unsigned long num;
    getentropy(&num, sizeof(unsigned long));
    return num;
}

unsigned int random_unbiased_uint(unsigned int a, unsigned int b) {
    int rnd;
    while (1) {
        rnd = random_uint();
        if (rnd < 4294967295/(b - a) * (b - a)) {
            break;
        }
    }
    return a + rnd % (b - a);
}

char* get_datetime() {
    char* res = (char*)calloc(128, 1);
    time_t unixtime = time(0);
    struct tm* t_res = localtime(&unixtime);
    strftime(res, 128, "%a %b %d %H:%M:%S %Y\n", t_res);
    return res;
}
#endif