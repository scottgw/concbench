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
    start_sub_queue();
}

void qoq::note_completion() {
  if (--count != 0)
    start_sub_queue();
}

void qoq::start_sub_queue()
{
  serializer *s = NULL;
  big_queue.try_pop (s);
  assert (s);
  s->start();
}
