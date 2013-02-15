#include <iostream>
#include <vector>
#include <queue>
#include <future>
#include <assert.h>
#include <cstdlib>

#include "separate.h"
#include "private_queue.h"
#include "processor.h"

std::vector <processor*> g_processors;

int max_iters = 5000;

class producer {
public:
  separate <int> *m_var;
  private_queue <int> m_private_queue;

  producer (separate <int> *var) : 
    m_private_queue (var->make_queue()) 
  {
    m_var = var;
  }

  void live() {
    for (int i = 0; i < max_iters; i++) {
      // std::cout << "producer run: " << i << std::endl;
      run ();
    }
    std::cout << "done producing" << std::endl;
  }

  void run() {
    processor *p = &m_var->m_proc;
    bool wait_cond;

    // Lock the processor and check the wait-condition.
    // auto l_queue = m_queue->lock();
    m_var->lock_with (m_private_queue);
    
    std::function <bool(int *)> wait_func = [](int *x) {
      return *x % 2 == 1;
    };

    // auto fut = l_queue.log_call_with_result (wait_func);
    wait_cond = m_private_queue.log_call_with_result (wait_func);

    // If the wait-condition isn't satisfied, wait and recheck
    // when we're woken up (by another thread).
    while (!wait_cond) {
      // l_queue.unlock();
      m_private_queue.unlock();
      p->wait_until_available();
      // l_queue = m_queue->lock();
      m_var->lock_with (m_private_queue);

      // auto fut = l_queue.log_call_with_result (wait_func);
      wait_cond = m_private_queue.log_call_with_result (wait_func);
    }
        
    // log the call with result.
    std::function<void(int *)> f = [](int *x) {
      (*x)++;
    };

    // std::future <int> res = l_queue.log_call_with_result (f);
    m_private_queue.log_call (f);

    // end the queue
    // l_queue.unlock();
    m_private_queue.unlock ();
  }

};


class consumer {
public:
  separate <int> *m_var;
  private_queue <int> m_private_queue;

  consumer (separate <int>* var):
    m_private_queue (var->make_queue())
  {
    m_var = var;
  }

  void live() {
    for (int i = 0; i < max_iters; i++) {
      // std::cout << "consumer run " << i << std::endl;
      run ();
    }
    std::cout << "done consuming" << std::endl;
  }

  void run() {
    processor *p = &m_var->m_proc;
    bool wait_cond;

    // Lock the processor and check the wait-condition.
    // auto l_queue = m_queue->lock();
    m_var->lock_with (m_private_queue);
    
    std::function <bool(int *)> wait_func = [](int *x) {
      return *x % 2 == 0;
    };

    // auto fut = l_queue.log_call_with_result (wait_func);
    wait_cond = m_private_queue.log_call_with_result (wait_func);

    // If the wait-condition isn't satisfied, wait and recheck
    // when we're woken up (by another thread).
    while (!wait_cond) {
      // l_queue.unlock();
      m_private_queue.unlock();
      p->wait_until_available();
      // l_queue = m_queue->lock();
      m_var->lock_with (m_private_queue);

      // auto fut = l_queue.log_call_with_result (wait_func);
      wait_cond = m_private_queue.log_call_with_result (wait_func);
    }
        
    // log the call with result.
    std::function<void(int *)> f = [](int *x) {
      (*x)++;
    };

    // std::future <int> res = l_queue.log_call_with_result (f);
    m_private_queue.log_call (f);

    // end the queue
    // l_queue.unlock();
    m_private_queue.unlock ();
  }
  
};

int main (int argc, char** argv) {
  max_iters = atoi(argv[1]);
  auto max_each = atoi(argv[2]);
  std::vector <std::future <bool> > workers;
  int *x = new int;
  *x = 0;
  separate <int> var (x);

  g_processors.push_back (&var.m_proc);

  for (int i = 0; i < max_each; i++) {
    workers.push_back 
      (std::async 
       (std::launch::async,
        [&var]() {
         producer p (&var);
         p.live();
         return true;
       }));

    workers.push_back 
      (std::async 
       (std::launch::async,
        [&var]() {
         consumer p (&var);
         p.live();
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
  std::cout << *x << std::endl;
  return 0;
}
