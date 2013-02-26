#ifndef __QOQ_H__
#define __QOQ_H__

#include <tbb/concurrent_queue.h>

#include "serializer.h"

class serializer;

class qoq {
  tbb::concurrent_queue <serializer*> big_queue;
  tbb::atomic<int> count;

public:
  qoq();

  void add(serializer *s);

  void note_completion();

  void start_sub_queue();

  void complete_task (serializer*);

};
#endif
