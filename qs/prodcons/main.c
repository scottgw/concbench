#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <stdint.h>
#include <ffi.h>
#include <glib.h>

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
add_value(processor_t proc, int x)
{
  g_queue_push_head(queue, x);
}

int
get_value(processor_t proc)
{
  return g_queue_pop_tail(queue);
}

int
is_empty(processor_t proc)
{
  return g_queue_is_empty(queue);
}


void
producer(processor_t proc, processor_t shared)
{
  void ***args;
  clos_type_t *arg_types;
  priv_queue_t q = priv_queue_new(shared);

  for (int i = 0; i < num_iters; i++)
    { 
      closure_t clos =
        closure_new(add_value,
                    closure_void_type(),
                    2,
                    &args,
                    &arg_types);

      arg_types[0] = closure_pointer_type();
      arg_types[1] = closure_sint_type();
      
      *args[0] = shared;
      *args[1] = i;

      priv_queue_lock(q, shared, proc);
      priv_queue_routine(q, clos, proc);
      priv_queue_unlock(q, proc);
    }

  priv_queue_shutdown(q, shared, proc);
  printf("producer pre shutdown\n");
  proc_shutdown(proc, proc);

  printf("producer shutdown\n");
  if( __sync_add_and_fetch(&num_finished, 1) == 2*num_each)
    {
      printf("shared shutdown %p\n", shared);
      proc_shutdown(shared, proc);
    }
}


void
consumer(processor_t proc, processor_t shared) 
{
  fprintf(stderr, "%p consumer\n", proc);
  void ***args;
  clos_type_t *arg_types;
  priv_queue_t q = priv_queue_new(shared);

  for (int i = 0; i < num_iters; i++)
    {
      int val;
      closure_t clos;

      clos =
        closure_new(is_empty,
                    closure_sint_type(),
                    1,
                    &args,
                    &arg_types);

      arg_types[0] = closure_pointer_type();
      *args[0] = shared;

      priv_queue_lock(q, shared, proc);
      priv_queue_function(q, clos, &val, proc);

      while (val == 1)
        {
          priv_queue_unlock(q, proc);
          proc_wait_for_available(shared, proc);

          priv_queue_lock(q, shared, proc);
          clos =
            closure_new(is_empty,
                        closure_sint_type(),
                        1,
                        &args,
                        &arg_types);

          arg_types[0] = closure_pointer_type();
          *args[0] = shared;

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

      priv_queue_function(q, clos, &val, proc);
      priv_queue_unlock(q, proc);
    }

  priv_queue_shutdown(q, shared, proc);
  printf("consumer pre shutdown\n");
  proc_shutdown(proc, proc);

  printf("consumer shutdown\n");
  if( __sync_add_and_fetch(&num_finished, 1) == 2*num_each)
    {
      printf("shared shutdown %p\n", shared);
      proc_shutdown(shared, proc);
    }
}

void
proc_main(processor_t proc)
{
  processor_t shared = make_processor(proc->task->sync_data);
  
  for (int i = 0; i < 2*num_each; i++)
    {
      processor_t worker_proc = make_processor(proc->task->sync_data);
      priv_queue_t q = priv_queue_new(worker_proc);
      
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

      priv_queue_lock(q, worker_proc, proc);
      priv_queue_routine(q, clos, proc);
      priv_queue_unlock(q, proc);

      priv_queue_shutdown(q, worker_proc, proc);
    }
}

int
main(int argc, char **argv)
{
  num_iters = atoi(argv[1]);
  num_each  = atoi(argv[2]);
  
  queue = g_queue_new();
  sync_data_t sync_data = sync_data_new(MAX_TASKS);
  processor_t proc = make_root_processor(sync_data, proc_main);

  create_executors(sync_data, 4);

  {
    notifier_t notifier = notifier_spawn(sync_data);
    notifier_join(notifier);
  }

  join_executors();

  printf ("empty is: %d\n", is_empty(NULL));
  sync_data_free(sync_data);
  g_queue_free(queue);
  return 0;
}
