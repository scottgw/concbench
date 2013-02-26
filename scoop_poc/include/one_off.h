#ifndef __ONE_OFF_H__
#define __ONE_OFF_H__

#include <tbb/task.h>

class one_off_task: public tbb::task {
  std::function<void()> f;

public:
  one_off_task (std::function<void()> &f_): f(f_) {}
  
  tbb::task* execute () 
  {
    f();
    return NULL;
  }
};

void one_off(std::function <void()> f)
{
  tbb::task::enqueue(*new(tbb::task::allocate_root()) one_off_task (f));
}

#endif
