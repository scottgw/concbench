#ifndef __QOQ_H__
#define __QOQ_H__

#include <tbb/concurrent_queue.h>

#include "serializer.h"

class serializer;

class qoq {
  tbb::concurrent_queue <serializer*> big_queue;
  tbb::atomic<int> count;

public:
  bool task_running = false;

  qoq();

  void add(serializer *s);

  void note_completion();

  bool start_sub_queue();

};
#endif
