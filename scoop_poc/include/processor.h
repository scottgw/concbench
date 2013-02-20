#ifndef __PROCESSOR_H_
#define __PROCESSOR_H_

#include <mutex>
#include <condition_variable>
#include <thread>

#include "tbb/concurrent_queue.h"


class work_queue {
public:
  typedef std::function<void()>* work;

  work_queue (): m_q() 
  {
  }

  void push (work elem) {
    m_q.push(elem);
  }

  work retry_pop () {
    work elem;

    for (int i = 0; i < 1024; i++) {
      if (m_q.try_pop (elem)) {
        return elem;
      }
    }
    m_q.pop (elem);
    return elem;
  }
  
  void set_capacity (int c) {
    m_q.set_capacity (c);
  }

private:
  tbb::concurrent_bounded_queue <work> m_q;


};


class processor {
  // availablity synchronization (for waking up from wait-conditions).
  std::mutex m_available_mutex;
  std::condition_variable m_available_cv;

  // A queue where each member is another processor's run queue.
  tbb::concurrent_bounded_queue <work_queue*> m_q_queue;

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
      work_queue *l_queue;
      m_q_queue.pop(l_queue);
      if (l_queue != NULL) {
        for (;;) {
          std::function <void () >* fptr;
          fptr = l_queue->retry_pop (); // l_queue->pop(fptr);
          
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

  void add_queue (work_queue* q) {
    m_q_queue.push (q);
  }
  
  work_queue* new_run_queue () {
    auto q = new work_queue ();
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
