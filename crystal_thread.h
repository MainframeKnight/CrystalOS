#ifndef CRYSTAL_THREAD_H
#define CRYSTAL_THREAD_H
#include<pthread.h>
#include<stdlib.h>
#include<errno.h>
#include<unistd.h>
#include<sys/syscall.h>
#include<sys/time.h>
#include<semaphore.h>
#define gettid() syscall(SYS_gettid)

int thread_create(pthread_t* thrd, void*(*func) (void*)) {
    return pthread_create(thrd, 0, func, 0);
}

void thread_exit() {
    pthread_exit(0);
}

int thread_join(pthread_t thrd) {
    return pthread_join(thrd, 0);
}

int thread_detach(pthread_t thrd) {
    return pthread_detach(thrd);
}

pid_t get_thread_id() {
    return gettid();
}

void *create_mutex(int* ok) {
    pthread_mutex_t* new_m = (pthread_mutex_t*)malloc(sizeof(pthread_mutex_t));
    *ok = pthread_mutex_init(new_m, 0);
    return (void *)new_m;
}

int lock_mutex(void* mutex) {
    return pthread_mutex_lock((pthread_mutex_t*)mutex);
}

int trylock_mutex(void* mutex) {
    int ok = pthread_mutex_trylock((pthread_mutex_t*)mutex);
    if (!ok) {
        return 0;
    } else if (ok == EBUSY)
    {
        return 1;
    } else {
        return 2;
    }
}

int unlock_mutex(void* mutex) {
    return pthread_mutex_unlock((pthread_mutex_t*)mutex);
}

int destroy_mutex(void* mutex) {
    int ok = pthread_mutex_destroy((pthread_mutex_t*)mutex);
    free(mutex);
    return ok;
}

void* create_condvar(int* ok) {
    pthread_cond_t* cond = (pthread_cond_t*)malloc(sizeof(pthread_cond_t));
    *ok = pthread_cond_init(cond, 0);
    return cond;
}

int wait_condvar(void *condvar, void* mtx) {
    return pthread_cond_wait((pthread_cond_t*)condvar, (pthread_mutex_t*)mtx);
}

int timedwait_condvar(void *condvar, void* mtx, long msec) {
    struct timespec t_sp;
    struct timeval cur;
    gettimeofday(&cur, 0);
    t_sp.tv_sec = time(0) + msec / 1000;
    t_sp.tv_nsec = cur.tv_usec * 1000 + 1000000 * (msec % 1000);
    t_sp.tv_sec += t_sp.tv_nsec / 1000000000;
    t_sp.tv_nsec %= 1000000000;
    return pthread_cond_timedwait((pthread_cond_t*)condvar, (pthread_mutex_t*)mtx, &t_sp);
}

int signal_condvar(void *condvar) {
    return pthread_cond_signal((pthread_cond_t*)condvar);
}

int broadcast_condvar(void *condvar) {
    return pthread_cond_broadcast((pthread_cond_t*)condvar);
}

int destroy_condvar(void* condvar) {
    int ok = pthread_cond_destroy((pthread_cond_t*)condvar);
    free(condvar);
    return ok;
}

void *create_semaphore(int val, int* ok) {
    sem_t* sema = (sem_t*)calloc(1, sizeof(sem_t));
    *ok = sem_init(sema, 0, val);
    return (void *)sema;
}

int increment_semaphore(void *sema) {
    return sem_post((sem_t *)sema);
}

int get_semaphore(void *sema, int* res) {
    return sem_getvalue((sem_t *)sema, res);
}

int wait_dec_semaphore(void *sema) {
    return sem_wait((sem_t *)sema);
}

int try_wait_dec_semaphore(void *sema) {
    if (sem_trywait((sem_t *)sema) == -1) {
        if (errno == EAGAIN) {
            return 1;
        } else {
            return 2;
        }
    }
    return 0;
}

int destroy_semaphore(void *sema) {
    int ok = sem_destroy((sem_t *)sema);
    free(sema);
    return ok;
}
#endif