#ifndef __PROCESSOR_H_
#define __PROCESSOR_H_

#include <mutex>
#include <condition_variable>
#include <thread>

#include "tbb/concurrent_queue.h"

template <typename T>
T retry_pop (tbb::concurrent_bounded_queue <T> *q) {
  T elem;

  for (int i = 0; i < 1024; i++) {
    if (q->try_pop (elem)) {
      return elem;
    }
  }
  q->pop (elem);
  return elem;
}


class processor {
  // availablity synchronization (for waking up from wait-conditions).
  std::mutex m_available_mutex;
  std::condition_variable m_available_cv;

  // A queue where each member is another processor's run queue.
  tbb::concurrent_bounded_queue <
    tbb::concurrent_bounded_queue <
      std::function <void () >* > *> m_q_queue;

  // The thread this thing has spawned off.
  std::thread *m_thread;

  bool m_available = false;


public:
  processor (): 
    m_available_mutex()
    , m_available_cv()
    , m_q_queue()
  {
    // A bound here would allow a deadlock. For example, if two processors
    // had a capacity of 1, one could run into a classic deadlock situation
    // on those two processors with two other clients that put their queues
    // in the queue of queues in opposite order.
    
    // Possible traditional fix: add the run-queues in an order based on
    // the ordering of the processors id (thread id? anything would do).
    // In this case, the queues would only need enough space for every processor.
    // m_q_queue.set_capacity (32);
    m_thread = new std::thread ([&](){main_loop ();});
  }

  void main_loop () {
    for (;;) {
      tbb::concurrent_bounded_queue <std::function <void () >* > *l_queue;
      l_queue = retry_pop (&m_q_queue); // m_q_queue.pop(l_queue);
      if (l_queue != NULL) {
        for (;;) {
          std::function <void () >* fptr;
          fptr = retry_pop (l_queue); // l_queue->pop(fptr);
          
          if (fptr != NULL) {
            (*fptr)();
            delete fptr;
          } else {
            m_available = true;
            m_available_cv.notify_all();
            break;
          }
        }
      } else {
        break;
      }
    }
  }

  // We've locked this processor but are waiting on a wait-condition.
  void wait_until_available () {
    std::unique_lock<std::mutex> lock(m_available_mutex);
    m_available_cv.wait (lock, [&](){return m_available;});
    m_available = false;
  }

  void add_queue (tbb::concurrent_bounded_queue<std::function<void()>* >* q) {
    m_q_queue.push (q);
  }
  
  tbb::concurrent_bounded_queue <std::function<void()>* >* new_run_queue () {
    auto q = new tbb::concurrent_bounded_queue<std::function<void()>* > ();
    add_queue (q);
    return q;
  }
  
  void shutdown () {
    m_q_queue.push (NULL);
  }

  void join () {
    m_thread->join ();
  }
};
#endif
