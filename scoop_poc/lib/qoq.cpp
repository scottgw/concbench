#include <iostream>
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

void qoq::complete_task(serializer *s)
{
  int old_count = -1;
  do
    {
      bool cancelled = false;
      int new_sub_count = --(s->count);
      while (new_sub_count != 0 && !cancelled)
        {
          // std::cout << "subcount is " << new_sub_count << std::endl;
          cancelled = s->move_to_ready_no_task();
          if (cancelled) {
            // std::cout << "cancelled\n";
            // break;
          } else {
            // std::cout << "decrementing\n";
            new_sub_count = --(s->count);
          }
        }
      
      // we just plain return because we're out of work and
      // the serializer hasn't been cancelled yet
      if (!cancelled)
        {
          // std::cout << "ran out of work " << new_sub_count << "\n";
          return;
        }
      
      old_count = --count;
      if (old_count != 0) {
        big_queue.try_pop(s);
      }
      // std::cout << "old count is: " << old_count << std::endl;
    } while (old_count != 0);
}
