#ifndef __PROCESSOR_H_
#define __PROCESSOR_H_

#include <pthread.h>
#include <stdlib.h>
#include <dispatch/dispatch.h>

typedef struct {
  dispatch_queue_t queue;
  pthread_mutex_t mutex;
  pthread_cond_t cv;
  bool available;
} processor_;

typedef processor_* processor_t; 

processor_t processor_create () {
  processor_t this = (processor_t) malloc (sizeof(processor_));;
  this->queue = dispatch_queue_create(NULL, NULL);
  pthread_mutex_init (&this->mutex, NULL);
  pthread_cond_init (&this->cv,NULL);
  this->available = true;
  return this;
}

void processor_add_queue (processor_t this, dispatch_queue_t queue) {
  __block dispatch_queue_t q = queue;
  dispatch_async (this->queue, ^{
      dispatch_resume(q);
    });
}

// We've locked this processor but are waiting on a wait-condition.
void processor_wait (processor_t this) {
  pthread_mutex_lock (&this->mutex);
  while (!this->available)
    pthread_cond_wait (&this->cv, &this->mutex);
  this->available = false;
  pthread_mutex_unlock (&this->mutex);
}

// unlocking/waiting should occur in the the ''unlock'' push of the 
// private_queue.
#endif
