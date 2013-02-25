#include <tbb/task.h>

#include "serializer.h"
#include "qoq.h"

serializer::serializer() {
  count.store(1);
}

void serializer::add(work_item *work)
{
  q.push(work);
  int new_count = ++count;
  if (new_count == 1)
    move_to_ready_pile();
}
  
void serializer::add_end () {
  add (NULL);
}

void serializer::start()
{
  if (--count >= 1) { // is this right, double starts?
    move_to_ready_pile();
  }
}

void serializer::move_to_ready_pile()
{
  work_item *work;
  q.try_pop(work);
  if (work == NULL) {
    parent->note_completion();
  } else {
    tbb::task::enqueue(*new(tbb::task::allocate_root()) run_work_item (work));
  }
}


void serializer::note_completion() 
{
  if (--count != 0)
    move_to_ready_pile();
}


