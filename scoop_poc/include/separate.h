#ifndef __SEPARATE_H_
#define __SEPARATE_H_

#include "processor.h"
#include "private_queue.h"

template <class T>
class separate {
public:
  T *m_ref;
  processor m_proc;
  
  separate (T *ref): m_proc () {
    m_ref = ref;
  }

  private_queue <T> make_queue() {
    return private_queue <T> (m_ref);
  }

  void lock_with (private_queue <T> &q) {
    m_proc.add_queue (q.queue());
  }

  private_queue <T> lock () {
     auto m_local_queue = m_proc.new_run_queue ();
     return private_queue <T> (m_local_queue, m_ref);
  }
};
#endif
