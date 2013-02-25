#ifndef __WORK_ITEM_H__
#define __WORK_ITEM_H__

#include <functional>
#include <tbb/task.h>

#include "serializer.h"

class serializer;

class work_item {
  std::function<void()> f;
  
  serializer* s;
public:
  work_item (decltype(f) &f_, serializer* s_);

  void run();
};

class run_work_item: public tbb::task {
  task* execute();
  work_item *item;
public:
  run_work_item (work_item *item_);
};

#endif
