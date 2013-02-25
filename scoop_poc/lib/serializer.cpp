#include <tbb/task.h>
#include <iostream>

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

bool serializer::start()
{
  if (parent->task_running) {
    std::cout << "serializer start\n";
    return note_completion();
  } else { 
    if (--count >= 1) {
      move_to_ready_pile();
      return false;
    }
  }
}

void serializer::move_to_ready_pile()
{
  work_item *work;
  q.try_pop(work);
  if (work == NULL) {
    parent->note_completion();
  } else {
    std::cout << "new task\n";
    tbb::task::enqueue(*new(tbb::task::allocate_root()) run_work_item (work));
  }
}


bool serializer::move_to_ready_no_task()
{
  work_item *work;
  q.try_pop(work);
  if (work == NULL) 
    {
      // parent->note_completion();
      // --count;
      std::cout << "end of serializer section\n";
      return false;
    } 
  else 
    {
      work->f();
      delete work;
      return true;
    }
}



bool serializer::note_completion() 
{
  while (--count != 0) 
    {
      if (!move_to_ready_no_task()) {
        std::cout << "serializer exit with more left\n";
        return true;
      }
    }

  parent->task_running = false;
  return false;
}
