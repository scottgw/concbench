#include <future>
#include "tbb/concurrent_queue.h"

#ifndef __PRIVATE_QUEUE_H_
#define __PRIVATE_QUEUE_H_
template <class T>
class private_queue {
 protected:
  tbb::concurrent_bounded_queue <std::function<void()>* > *m_local_queue;
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
    m_local_queue = new tbb::concurrent_bounded_queue <std::function<void()>* >();
    m_local_queue->set_capacity (128);
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
    m_local_queue->push (fptr);
  }
  
  template <typename R>
  R log_call_with_result (std::function<R (T*)> func) {
    R res;
    if (m_last_was_query) {
      res = func(m_ref);
    } else {
      auto p = new std::promise<R> ();
      auto ref = m_ref;
      auto fptr = new std::function <void()>
        ([=]() 
         {
           p->set_value (func (ref));
         });

      m_local_queue->push (fptr);
      res = p->get_future().get();
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
