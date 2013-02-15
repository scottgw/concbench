#include <iostream>
#include <vector>
#include <queue>
#include <future>
#include <assert.h>

#include "separate.h"
#include "private_queue.h"
#include "processor.h"

std::vector <processor*> g_processors;

int max_iters = 5000;

class worker {
public:
  separate <int*> *m_array;
  private_queue <int*> m_private_queue;

  worker (separate <int*>* array):
    m_private_queue (array->make_queue())
  {
    m_array = array;
  }

  void live() {
    run ();
  }

  void run() {
    // Lock the processor, setup the array.
    // auto l_queue = m_array->lock();
    m_array->lock_with(m_private_queue);

    for (int i = 0; i < max_iters; i++) {
      
      // log the call.
      m_private_queue.log_call ([=](int *arr[]) {
          (*arr)[i]++;
        });
    }
    
    // end the array
    m_private_queue.unlock();
  }
};



int main (int argc, char** argv) {
  max_iters = atoi(argv[1]);
  int max_each = atoi(argv[2]);
  std::vector <std::future <bool> > workers;
  int* arr = new int[max_iters];
  separate <int*> queue (&arr);

  g_processors.push_back (&queue.m_proc);

  for (int i = 0; i < max_each; i++) {
    workers.push_back 
      (std::async 
       (std::launch::async,
        [&queue]() {
         worker m (&queue);
         m.live();
         return true;
       }));
  }
  
  for (auto& worker : workers) {
    worker.get();
  }

  // example of final shutdown, this should be done by the GC in a real
  // implementation.
  for (auto& proc : g_processors) {
    proc->shutdown ();
    proc->join();
  }

  return 0;
}
