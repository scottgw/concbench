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

int x = 0;
volatile int num_finished = 0;

void
action(processor_t proc)
{
  x++;
}

int
get_value(processor_t proc)
{
  return x;
}

void worker(processor_t proc, processor_t shared, int flag) 
{
  fprintf(stderr, "%p worker with %d\n", proc, flag);
  void ***args;
  clos_type_t *arg_types;
  priv_queue_t q = NULL;

  for (int i = 0; i < num_iters; i++)
    {
      int val;
      closure_t clos;
      q = proc_get_queue (proc, shared);

      priv_queue_lock(q, proc);
      priv_queue_sync(q, proc);

      val = get_value(shared);

      while (val % 2 != flag)
        {
          priv_queue_unlock(q, proc);
          proc_wait_for_available(shared, proc);

          q = proc_get_queue (proc, shared);
          priv_queue_lock(q, proc);
          priv_queue_sync(q, proc);

          val = get_value(shared);
        }

      clos =
        closure_new(action,
                    closure_void_type(),
                    1,
                    &args,
                    &arg_types);

      arg_types[0] = closure_pointer_type();
      *args[0] = shared;

      priv_queue_routine(q, clos, proc);
      priv_queue_unlock(q, proc);
    }

  printf("worker pre shutdown\n");
//  proc_shutdown(proc, proc);

  printf("worker shutdown\n");
  if( __sync_add_and_fetch(&num_finished, 1) == 2*num_each)
    {
      printf("shared shutdown %p\n", shared);
      exit(0);
  //  proc_shutdown(shared, proc);
    }
}

void
proc_main(processor_t proc)
{
  processor_t shared = proc_new(proc->task->sync_data);
  
  for (int i = 0; i < 2*num_each; i++)
    {
      processor_t worker_proc = proc_new(proc->task->sync_data);
      priv_queue_t q = proc_get_queue(proc, worker_proc);
      
      void ***args;
      clos_type_t *arg_types;
 
      closure_t clos =
        closure_new(worker,
                    closure_void_type(),
                    3,
                    &args,
                    &arg_types);
      
      arg_types[0] = closure_pointer_type();
      arg_types[1] = closure_pointer_type();
      arg_types[2] = closure_sint_type();
      
      *args[0] = worker_proc;
      *args[1] = shared;
      *args[2] = i % 2 == 0;

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
  
  sync_data_t sync_data = sync_data_new(MAX_TASKS);
  processor_t proc = proc_new_root(sync_data, proc_main);

  create_executors(sync_data, 4);

  {
    notifier_t notifier = notifier_spawn(sync_data);
    notifier_join(notifier);
  }

  join_executors();

  printf ("x is: %d\n", x);
  sync_data_free(sync_data);
  return 0;
}
