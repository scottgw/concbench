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

int max_iters = 20000;

class producer {
public:
  separate <std::queue <int> > *m_queue;
  private_queue <std::queue <int>> m_private_queue;

  producer (separate <std::queue <int> >* queue) : 
    m_private_queue (queue->make_queue()) 
  {
    m_queue = queue;
  }

  void live() {
    for (int i = 0; i < max_iters; i++) {
      // std::cout << "producer run: " << i << std::endl;
      run (i);
    }
    std::cout << "done producing" << std::endl;
  }

  void run(int i) {
    // Lock the processor, setup the queue.
    // auto l_queue = m_queue->lock ();
    m_queue->lock_with(m_private_queue);
    
    // // If we want the traditional SCOOP behaviour:
    // std::function <bool(std::queue<int>*)> wait = [](std::queue<int> *q) {
    //   return true;
    // };

    // auto fut = m_private_queue.log_call_with_result (wait);
    // fut.get();

    // log the call.
    m_private_queue.log_call ([=](std::queue<int> *q) {
        q->push(i);
      });
    
    // end the queue
    m_private_queue.unlock();
  }

};


class consumer {
public:
  separate <std::queue <int> > *m_queue;
  private_queue <std::queue <int>> m_private_queue;

  consumer (separate <std::queue <int> >* queue):
    m_private_queue (queue->make_queue())
  {
    m_queue = queue;
  }

  void live() {
    for (int i = 0; i < max_iters; i++) {
      // std::cout << "consumer run " << i << std::endl;
      run ();
    }
    std::cout << "done consuming" << std::endl;
  }

  void run() {
    processor *p = &m_queue->m_proc;
    bool wait_cond;

    // Lock the processor and check the wait-condition.
    // auto l_queue = m_queue->lock();
    m_queue->lock_with (m_private_queue);
    
    std::function <bool(std::queue<int> *)> wait_func = [](std::queue<int> *q) {
      return !q->empty();
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
      m_queue->lock_with (m_private_queue);

      // auto fut = l_queue.log_call_with_result (wait_func);
      wait_cond = m_private_queue.log_call_with_result (wait_func);
    }
        
    // log the call with result.
    std::function<int (std::queue<int> *)> f = [](std::queue<int> *q) {
      int tmp = q->front();
      q->pop();
      return tmp;
    };

    // std::future <int> res = l_queue.log_call_with_result (f);
    int res = m_private_queue.log_call_with_result (f);

    // end the queue
    // l_queue.unlock();
    m_private_queue.unlock ();
  }
  
};

int main (int argc, char** argv) {
  max_iters = atoi(argv[1]);
  auto max_each = atoi(argv[2]);
  std::vector <std::future <bool> > workers;
  auto q = new std::queue <int>() ;
  separate <std::queue <int> > queue (q);

  g_processors.push_back (&queue.m_proc);

  for (int i = 0; i < max_each; i++) {
    workers.push_back 
      (std::async 
       (std::launch::async,
        [&queue]() {
         producer p (&queue);
         p.live();
         return true;
       }));

    workers.push_back 
      (std::async 
       (std::launch::async,
        [&queue]() {
         consumer p (&queue);
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

  return 0;
}
