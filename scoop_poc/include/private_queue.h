#ifndef __PRIVATE_QUEUE_H_
#define __PRIVATE_QUEUE_H_
#include <dispatch/dispatch.h>
#include <stdint.h>
#include <pthread.h>

#include "queue.h"
#include "separate.h"

typedef struct {
  processor_t proc;
  void* ref;
} separate_;

typedef separate_* separate_t;

typedef struct {
  separate_t sep;
  void* ref;
  dispatch_queue_t queue;
  bool last_was_query;
} private_queue_;

typedef private_queue_* private_queue_t;

private_queue_t private_queue_create (separate_t sep) {
  private_queue_t this = (private_queue_t) malloc (sizeof(private_queue_));
  this->sep = sep;
  this->ref = sep->ref;
  this->last_was_query = false;
  this->queue = dispatch_queue_create (NULL, NULL);
  return this;
}


dispatch_queue_t private_queue_queue (private_queue_t q) {
  return q->queue;
}

void private_queue_log_call(private_queue_t q, void (^func)(void*)) {
  q->last_was_query = false;
  dispatch_async (q->queue, ^{func(q->ref);});
}

uint64_t private_queue_log_call_with_result (private_queue_t q, uint64_t (^func)(void*)) {
  uint64_t res;
  
  if (q->last_was_query) {
    res = func(q->ref);
  } else {
    // future_t future = future_create();
    uint64_t *val = &res;
    dispatch_semaphore_t sem = dispatch_semaphore_create (0);
    printf("privq: created semaphore\n");
    dispatch_async (q->queue, ^{
        printf("about to run query\n");
        *val = func (q->ref);
        printf("ran query\n");
        dispatch_semaphore_signal (sem);
        printf("signalled semaphore\n");
        /* future_set (future, func (ref)); */
      });
    printf("privq: dispatched query\n");

    dispatch_semaphore_wait(sem, DISPATCH_TIME_FOREVER);
    res = *val; // future_get (future);
    //future_destroy (future);
  }
  q->last_was_query = true;
  return res;
}

void private_queue_unlock (private_queue_t q) {
  q->last_was_query = false;
  dispatch_suspend (q->queue);
}

#endif


/* typedef struct { */
/*   pthread_mutex_t *mutex; */
/*   pthread_cond_t *cv; */
/*   bool data_present; */
/*   uint64_t data; */
/* } future_; */

/* typedef future_* future_t; */

/* future_t future_create() { */
/*   pthread_mutex_t *mutex = (pthread_mutex_t*) malloc (sizeof(pthread_mutex_t)); */
/*   pthread_cond_t *cv = (pthread_cond_t*) malloc (sizeof(pthread_cond_t)); */
/*   pthread_mutex_init (mutex, NULL); */
/*   pthread_cond_init (cv, NULL); */

/*   future_t this = (future_t) malloc (sizeof(future_)); */

/*   this->mutex = mutex; */
/*   this->cv = cv; */
/*   this->data = 0; */
/*   this->data_present = false; */

/*   return this; */
/* } */

/* uint64_t future_get (future_t future) { */
/*   if (!future->data_present) { */
/*     pthread_mutex_lock(future->mutex); */
/*     while (!future->data_present) */
/*       pthread_cond_wait(future->cv, future->mutex); */
/*     pthread_mutex_unlock(future->mutex); */
/*   } */

/*   return future->data; */
/* } */

/* void future_set (future_t future, uint64_t data) { */
/*   future->data = data; */
/*   future->data_present = true; */
/*   pthread_cond_signal (future->cv); */
/* } */

/* void future_destroy (future_t future) { */
/*   pthread_cond_destroy (future->cv); */
/*   pthread_mutex_destroy (future->mutex); */
/*   free (future->cv); */
/*   free (future->mutex); */
/* } */
