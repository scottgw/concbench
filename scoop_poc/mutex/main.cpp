#include <iostream>
#include <vector>
#include <queue>
#include <future>
#include <assert.h>

#include "separate.h"
#include "private_queue.h"
#include "processor.h"

std::vector <processor*> g_processors;

int max_iters = 20000;

class shared {
public:
  int i = 0;
  void update () {
    i++;
  }
};

class scoop_mutex {
public:
  separate <shared> *m_shared;
  private_queue <shared> m_private_queue;

  scoop_mutex (separate <shared>* shared):
    m_private_queue (shared->make_queue())
  {
    m_shared = shared;
  }

  void live() {
    for (int i = 0; i < max_iters; i++) {
      run (i);
    }
    std::cout << "done modifying" << std::endl;
  }

  void run(int i) {
    // Lock the processor, setup the shared.
    // auto l_queue = m_shared->lock();
    m_shared->lock_with(m_private_queue);

    // log the call.
    m_private_queue.log_call ([](shared *s) {
        s->update();
      });
    
    // end the shared
    m_private_queue.unlock();
  }
};



int main (int argc, char** argv) {
  max_iters = atoi(argv[1]);
  auto max_each = atoi(argv[2]);
  std::vector <std::future <bool> > workers;
  auto s = new shared();
  separate <shared> queue (s);

  g_processors.push_back (&queue.m_proc);

  for (int i = 0; i < max_each; i++) {
    workers.push_back 
      (std::async 
       (std::launch::async,
        [&queue]() {
         scoop_mutex m (&queue);
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


  std::cout << "final value: " << s->i << std::endl;
  return 0;
}
