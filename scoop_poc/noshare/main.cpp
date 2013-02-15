#include <iostream>
#include <vector>
#include <queue>
#include <future>
#include <assert.h>

#include "separate.h"
#include "private_queue.h"
#include "processor.h"

tbb::concurrent_bounded_queue <processor*> g_processors;

class fibber {
 public:
  int fib (int i) {
    if (i < 2)
      return i;
    else
      return fib(i-1) + fib(i-2);
  }
};

class worker {
  separate <fibber> *m_fibber;
  private_queue <fibber> m_private_queue;

public:
  worker () {
    m_fibber = new separate <fibber> (new fibber());
    m_private_queue = m_fibber->make_queue();
    g_processors.push (&m_fibber->m_proc);
  }

  void live() {
    run ();
  }

  void run() {
    // Lock the processor, setup the fibber.
    // auto l_queue = m_fibber->lock();
    m_fibber->lock_with(m_private_queue);
    // log the call.
    m_private_queue.log_call ([](fibber *f) {
        f->fib(40);
      });
    
    // end the fibber
    m_private_queue.unlock();
  }
};



int main (int argc, char** argv) {
  int max_each = atoi(argv[1]);
  std::vector <std::future <bool> > workers;

  for (int i = 0; i < max_each; i++) {
    workers.push_back 
      (std::async 
       (std::launch::async,
        []() {
         worker m;
         m.live();
         return true;
       }));
  }
  
  for (auto& worker : workers) {
    worker.get();
  }

  // example of final shutdown, this should be done by the GC in a real
  // implementation.
  while (g_processors.size() != 0) {
    processor* proc;
    g_processors.pop(proc);
    proc->shutdown ();
    proc->join();
  }

  return 0;
}
