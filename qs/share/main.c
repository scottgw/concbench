#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <stdint.h>
#include <ffi.h>

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

int64_t *array;

volatile int num_finished = 0;

int64_t
get(processor_t proc, int64_t i)
{
  return array[i];
}

void
set(processor_t proc, int64_t i, int64_t x)
{
  array[i] = x;
}

void
worker(processor_t proc, processor_t shared)
{
  void ***args;
  clos_type_t *arg_types;
  priv_queue_t q = proc_get_queue(proc, shared);

  priv_queue_lock_sync(q, proc);

  for (int i = 0; i < num_iters; i++)
    { 
      int64_t val;
      priv_queue_sync(q, proc);
      val = get(shared, i);

      closure_t set_clos =
        closure_new(set,
                    closure_void_type(),
                    3,
                    &args,
                    &arg_types);

      arg_types[0] = closure_pointer_type();
      arg_types[1] = closure_sint_type();
      arg_types[2] = closure_sint_type();
      
      *args[0] = shared;
      *args[1] = i;
      *args[2] = val + 1;

      priv_queue_routine(q, set_clos, proc);
    }

  priv_queue_unlock(q, proc);

//  printf("worker pre shutdown\n");
//  proc_shutdown(proc, proc);

//printf("worker shutdown\n");
  if( __sync_add_and_fetch(&num_finished, 1) == num_each)
    {
//      printf("shared shutdown %p\n", shared);
      exit(0);
      proc_shutdown(shared, proc);
    }
}

void
proc_main(processor_t proc)
{
//  printf("mutex main\n");
  processor_t shared = proc_new_from_other(proc);
  
  for (int i = 0; i < num_each; i++)
    {
//      printf("creating worker %d\n", i);
      processor_t worker_proc = proc_new_from_other(proc);
      priv_queue_t q = proc_get_queue(proc, worker_proc);
      
      void ***args;
      clos_type_t *arg_types;
 
      closure_t clos =
        closure_new(worker,
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
  proc_deref_priv_queues(proc);
}

int
main(int argc, char **argv)
{
  num_iters = atoi(argv[1]);
  num_each  = atoi(argv[2]);
  array = (int64_t*) malloc(num_iters * sizeof(int64_t));
  sync_data_t sync_data = sync_data_new(MAX_TASKS);
  processor_t proc = proc_new_root(sync_data, proc_main);

  create_executors(sync_data, 4);

  {
    notifier_t notifier = notifier_spawn(sync_data);
    notifier_join(notifier);
  }

  join_executors();

  sync_data_free(sync_data);
  return 0;
}
