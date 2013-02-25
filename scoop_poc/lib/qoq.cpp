#include <assert.h>

#include "qoq.h"

qoq::qoq(): big_queue() {
  count.store(0);
}

void qoq::add(serializer *s)
{
  s->parent = this;
  big_queue.push(s);
  if (++count == 1)
    if (start_sub_queue()) // if there's more left
      {
        note_completion();
      }
      
}

void qoq::note_completion() {
  while (--count != 0) {
    if (start_sub_queue())
      return;
  }

  task_running = false;
}

bool qoq::start_sub_queue()
{
  serializer *s = NULL;
  big_queue.try_pop (s);
  assert (s);
  return s->start();
}
