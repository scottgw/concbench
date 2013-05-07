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

int x = 0;
volatile int num_finished = 0;

int
fib(int x)
{
  if (x < 2)
    return x;
  else
    return fib(x-1) + fib(x-2);
}

void
worker(processor_t proc)
{
  fib(40);

  proc_shutdown(proc, proc);

  printf("worker shutdown\n");
}

void
proc_main(processor_t proc)
{
  for (int i = 0; i < num_each; i++)
    {
      printf("creating worker %d\n", i);
      processor_t worker_proc = make_processor(proc->task->sync_data);
      priv_queue_t q = priv_queue_new(worker_proc);

      void ***args;
      clos_type_t *arg_types;
 
      closure_t clos =
        closure_new(worker,
                    closure_void_type(),
                    1,
                    &args,
                    &arg_types);
      
      arg_types[0] = closure_pointer_type();
      
      *args[0] = worker_proc;

      priv_queue_lock(q, worker_proc, proc);
      priv_queue_routine(q, clos, proc);
      priv_queue_unlock(q, proc);

      priv_queue_shutdown(q, worker_proc, proc);
    }
}

int
main(int argc, char **argv)
{
  num_each  = atoi(argv[1]);
  sync_data_t sync_data = sync_data_new(MAX_TASKS);
  processor_t proc = make_processor(sync_data);

  create_executors(sync_data, 4);

  priv_queue_t q = priv_queue_new(proc);

  void ***args;
  clos_type_t *arg_types;

  closure_t clos =
    closure_new(proc_main,
                closure_void_type(),
                1,
                &args,
                &arg_types);

  arg_types[0] = closure_pointer_type();
  *args[0] = proc;

  priv_queue_lock(q, proc, proc);
  priv_queue_routine(q, clos, proc);
  priv_queue_unlock(q, proc);

  printf("main shutdown\n");
  proc_shutdown(proc, proc);

  {
    notifier_t notifier = notifier_spawn(sync_data);
    notifier_join(notifier);
  }

  join_executors();
  priv_queue_free(q);


  printf ("x is: %d\n", x);
  sync_data_free(sync_data);
  return 0;
}
