#ifndef __SERIALIZER_H__
#define __SERIALIZER_H__

#include <tbb/concurrent_queue.h>

#include "qoq.h"
#include "work_item.h"

class qoq;
class work_item;

typedef tbb::concurrent_queue<work_item*> work_queue;

class serializer {
  work_queue q;
  tbb::atomic<int> count;

  void move_to_ready_pile();

public:
  serializer();

  void add(work_item*);
  
  void add_end ();

  void start();

  void note_completion();
  qoq *parent;
};

#endif
