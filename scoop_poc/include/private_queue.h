#ifndef __PRIVATE_QUEUE_H_
#define __PRIVATE_QUEUE_H_
#include <dispatch/dispatch.h>

#include "tbb/concurrent_queue.h"

template <class T>
class private_queue {
 protected:
  dispatch_queue_t m_local_queue;
  T* m_ref;

  bool m_last_was_query = false;
 public:
  private_queue ()
  {
  }

  private_queue (const private_queue <T> &other) : 
    m_local_queue (other.m_local_queue),
    m_ref (other.m_ref)
  {
  }
  
  private_queue (decltype (m_local_queue) q, T* ref) : 
    m_local_queue (q),
    m_ref (ref)
  {
  }

  private_queue (T* ref) : m_ref (ref)
  {
    m_local_queue = dispatch_queue_create (NULL, NULL);
  }
  
  
  decltype (m_local_queue) queue () {
    return m_local_queue;
  }

  void log_call(std::function<void(T*)> func) {
    auto ref = m_ref;
    auto fptr = new std::function <void()> ([=]()
      {
        func (ref);
      });
    m_last_was_query = false;
    dispatch_async (m_local_queue, ^{func(ref);});
  }
  
  template <typename R>
  R log_call_with_result (std::function<R (T*)> func) {
    R res;
    if (m_last_was_query) {
      res = func(m_ref);
    } else {
      auto p = new tbb::concurrent_bounded_queue<R>();
      auto ref = m_ref;
      auto fptr = new std::function <void()>
        ([=]() 
         {
           p->push (func (ref));
         });

      m_local_queue->push (fptr);
      p->pop(res);
    }
    m_last_was_query = true;
    return res;
  }

  void unlock () {
    m_last_was_query = false;
    m_local_queue->push (NULL);
  }

};
#endif
