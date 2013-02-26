#include <tbb/task.h>
#include <iostream>

#include "serializer.h"
#include "qoq.h"

serializer::serializer() {
  count.store(1);
  // std::cout << "serializer init " << count.load() << std::endl;
}

void serializer::add(work_item *work)
{
  q.push(work);
  int new_count = ++count;
  // std::cout << "serializer add " << new_count << std::endl;
  if (new_count == 1)
    move_to_ready_pile();
}
  
void serializer::add_end () {
  add (NULL);
}

void serializer::start()
{
  std::cout < "starting new task\n";
  if (--count >= 1)
    move_to_ready_pile();
}

void serializer::move_to_ready_pile()
{
  work_item *work;
  q.try_pop(work);
  if (work == NULL) 
    {
      parent->note_completion();
    } 
  else 
    {
      // std::cout << "new task\n";
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
      // std::cout << "end of serializer section\n";
      return true;
    } 
  else 
    {
      work->f();
      delete work;
      return false;
    }
}

// bool serializer::note_completion() 
// {
//   while (--count != 0) 
//     {
//       if (!move_to_ready_no_task()) 
//         {
//           std::cout << "serializer exit with more left\n";
//           parent->note_completion(true);
//           return true;
//         }
//     }
  
//   parent->note_completion(false);
//   return false;
// }
