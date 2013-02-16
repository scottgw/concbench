#include <dispatch/dispatch.h>
#include <stdio.h>
#include <assert.h>
#include <unistd.h>

#include "separate.h"
#include "private_queue.h"
#include "processor.h"

int max_iters = 5000;

void live (separate_t var, int signal) {
  private_queue_t private_queue = private_queue_create (var);
  printf("worker with %d\n", signal);
  for (int i = 0; i < max_iters; i++) {
    processor_t p = var->proc;
    bool wait_cond;

    // Lock the processor and check the wait-condition.
    // auto l_queue = m_queue->lock();
    separate_lock_with (var, private_queue);
    printf("worker locked\n");
    uint64_t (^wait_func)(void*) = ^ (void *x) {
      printf("in wait func\n");
      return (uint64_t)(*(int*)x % 2 == signal);
    };

    // auto fut = l_queue.log_call_with_result (wait_func);
    wait_cond = private_queue_log_call_with_result (private_queue, wait_func);
    printf("worker wait cond %d returned %d\n", signal, wait_cond);

    // If the wait-condition isn't satisfied, wait and recheck
    // when we're woken up (by another thread).
    while (!wait_cond) {
      // l_queue.unlock();
      private_queue_unlock (private_queue);
      processor_wait(p);
      // l_queue = m_queue->lock();
      separate_lock_with (var, private_queue);

      // auto fut = l_queue.log_call_with_result (wait_func);
      wait_cond = private_queue_log_call_with_result (private_queue, wait_func);
    }
        
    // log the call with result.
    void (^f)(void*) = f = ^(void *x) {
      (*(int*)x)++;
    };
    
    // std::future <int> res = l_queue.log_call_with_result (f);
    private_queue_log_call (private_queue, f);

    // end the queue
    // l_queue.unlock();
    private_queue_unlock (private_queue);
  }
}

int main (int argc, char** argv) {
  if (argc < 3) {
    printf("Not enough arguments, expecting 2.\n");
    return 1;
  }
  max_iters = atoi(argv[1]);
  int max_each = atoi(argv[2]);

  int x = 0;

  separate_t var;
  var = separate_create (&x);
  
  dispatch_queue_t* worker_queues = 
    (dispatch_queue_t*) malloc (2 * max_each * sizeof(dispatch_queue_t));
  dispatch_group_t worker_grp =
    dispatch_group_create();

  printf("finished creating work group\n");
  for (int i = 0; i < 2*max_each; i+=2) {
    worker_queues[i] = dispatch_queue_create(NULL, NULL);
    worker_queues[i+1] = dispatch_queue_create(NULL, NULL);

    dispatch_group_async (worker_grp, 
                          worker_queues[i], 
                          ^{live(var, 0);});
    dispatch_group_async (worker_grp, 
                          worker_queues[i+1], 
                          ^{live(var, 1);});
  }
  printf("finished dispatch\n");
  sleep (5);
  // dispatch_group_wait (worker_grp, DISPATCH_TIME_FOREVER);
  printf("finished group waiting\n");
  printf("%d\n", x);
  
  // destroy each queue?
  for (int i = 0; i < 2*max_each; i++) {
    dispatch_release (worker_queues[i]);
  }

  free (worker_queues);
  return 0;
}
