#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <stdint.h>
#include <ffi.h>
#include <glib.h>
#include <inttypes.h>

#include "libqs/bounded_queue.h"
#include "libqs/executor.h"
#include "libqs/notifier.h"
#include "libqs/processor.h"
#include "libqs/task.h"
#include "libqs/list.h"
#include "libqs/task_mutex.h"
#include "libqs/private_queue.h"

#include "libqs/sync_ops.h"

#define MAX_TASKS 20000

int num_each;
int num_iters;

GQueue *queue;

volatile int num_finished = 0;

void
add_value(processor_t proc, int64_t x)
{
  g_queue_push_head(queue, (void*)x);
}

int64_t
get_value(processor_t proc)
{
  return (int64_t)g_queue_pop_tail(queue);
}

int
is_empty(processor_t proc, int64_t i)
{
  /* printf("is_empty: %"PRIu64"\n", i); */
  return g_queue_is_empty(queue);
}


void
producer(processor_t proc, processor_t shared)
{
  printf("producer %p\n", proc);
  void ***args;
  clos_type_t *arg_types;
  priv_queue_t q = NULL;

  for (int i = 0; i < num_iters; i++)
    { 
      /* printf("producer logging call %d\n", i); */
      q = proc_get_queue(proc, shared);
      closure_t clos =
        closure_new(add_value,
                    closure_void_type(),
                    2,
                    &args,
                    &arg_types);

      arg_types[0] = closure_pointer_type();
      arg_types[1] = closure_sint_type();
      

      int64_t ii = i;
      *args[0] = shared;
      *args[1] = (void*)ii;

      priv_queue_lock(q, proc);
      /* printf("producer locked queue %d\n", i); */
      priv_queue_routine(q, clos, proc);
      /* printf("producer %p enqueued routine %d\n", proc, i); */
      priv_queue_unlock(q, proc);
      /* printf("producer unlocked queue %d\n", i); */
    }

 printf("producer pre shutdown\n");
 /* proc_shutdown(proc, proc); */

 printf("producer shutdown\n");
 if( __sync_add_and_fetch(&num_finished, 1) == 2*num_each)
   {
     exit(0);
     printf("shared shutdown %p\n", shared);
     /* proc_shutdown(shared, proc); */
   }
}


void
consumer(processor_t proc, processor_t shared) 
{
  fprintf(stderr, "%p consumer\n", proc);
  void ***args;
  clos_type_t *arg_types;
  priv_queue_t q = NULL;

  for (int i = 0; i < num_iters; i++)
    {
      int val;
      closure_t clos;
      q = proc_get_queue(proc, shared);
      clos =
        closure_new(is_empty,
                    closure_sint_type(),
                    2,
                    &args,
                    &arg_types);

      arg_types[0] = closure_pointer_type();
      arg_types[1] = closure_sint_type();
      *args[0] = shared;
      *args[1] = (void*)((int64_t)i);

      /* printf("consumer locking queue %d\n", i); */
      priv_queue_lock(q, proc);
      /* printf("consumer %p queueing wait func %d\n", proc, i); */
      priv_queue_function(q, clos, &val, proc);

      while (val == 1)
        {
          /* printf("consumer unlocking for retry %d\n", i); */
          priv_queue_unlock(q, proc);
          /* printf("consumer waiting for change %d\n", i); */
          proc_wait_for_available(shared, proc);

          /* printf("consumer locking for retry %d\n", i); */
          priv_queue_lock(q, proc);
          clos =
            closure_new(is_empty,
                        closure_sint_type(),
                        2,
                        &args,
                        &arg_types);

          arg_types[0] = closure_pointer_type();
          arg_types[1] = closure_sint_type();
          *args[0] = shared;
          *args[1] = (void*)((int64_t)i);

          /* printf("consumer queueing wait func retry %d\n", i); */
          priv_queue_function(q, clos, &val, proc);
        }

      /* printf("queue not empty, taking\n"); */
      clos =
        closure_new(get_value,
                    closure_void_type(),
                    1,
                    &args,
                    &arg_types);

      arg_types[0] = closure_pointer_type();
      *args[0] = shared;
      /* printf("consumer queueing func %d\n", i); */
      priv_queue_function(q, clos, &val, proc);
      /* printf("consumer unlocking final lock %d\n", i); */
      priv_queue_unlock(q, proc);
    }

 /* printf("consumer pre shutdown\n"); */
 /* proc_shutdown(proc, proc); */

 printf("consumer shutdown\n");
 if( __sync_add_and_fetch(&num_finished, 1) == 2*num_each)
   {
     exit(0);
     printf("shared shutdown %p\n", shared);
     /* proc_shutdown(shared, proc); */
   }
}

void
proc_main(processor_t proc)
{
  printf("Main processor starting\n");
  processor_t shared = proc_new(proc->executor);


  for (int i = 0; i < 2*num_each; i++)
    {
      processor_t worker_proc = proc_new(proc->executor);
      priv_queue_t q = proc_get_queue(proc, worker_proc);
      
      void ***args;
      clos_type_t *arg_types;
 
      closure_t clos =
        closure_new(i % 2 == 0 ? producer : consumer,
                    closure_void_type(),
                    2,
                    &args,
                    &arg_types);
      
      arg_types[0] = closure_pointer_type();
      arg_types[1] = closure_pointer_type();
      
      *args[0] = worker_proc;
      *args[1] = shared;

      priv_queue_lock(q, proc);
      priv_queue_routine(q, clos, proc);
      priv_queue_unlock(q, proc);
    }

  /* proc_deref_priv_queues(proc); */
}

int
main(int argc, char **argv)
{
  num_iters = atoi(argv[1]);
  num_each  = atoi(argv[2]);
  
  queue = g_queue_new();
  sync_data_t sync_data = sync_data_new(MAX_TASKS, 4);

  create_executors(sync_data, 4);
  executor_t exec = sync_data_random_exec(sync_data);
  processor_t proc = proc_new_root(exec, proc_main);

  {
    notifier_t notifier = notifier_spawn(sync_data);
    notifier_join(notifier);
  }

  join_executors();

  printf ("queue count: %d\n", g_queue_get_length(queue));
  sync_data_free(sync_data);
  g_queue_free(queue);
  return 0;
}
