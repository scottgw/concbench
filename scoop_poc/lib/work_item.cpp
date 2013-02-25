#include <assert.h>

#include "work_item.h"

run_work_item::run_work_item (work_item* item_): item(item_) {}

work_item::work_item (decltype(f) &f_, serializer* s_): f(f_), s(s_) 
{}

void work_item::run() {
  serializer* tmp_s = s;
  f();
  delete this;
  tmp_s->note_completion();
}


tbb::task* run_work_item::execute()
{
  assert (item != NULL);
  item->run();
  return NULL;
}
