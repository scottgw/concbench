#include <iostream>
#include <assert.h>
#include <atomic>
#include <functional>

#include <tbb/task.h>
#include <tbb/concurrent_queue.h>

using namespace tbb;
using namespace std;

class work_item_i {
public:
  work_item_i() {}
  virtual void run() = 0;
};

class run_work_item: public task {
  task* execute();
  work_item_i* item;
public:
  run_work_item (work_item_i* item_): item(item_) {}
};

class work_item;

class serializer {
  concurrent_queue <work_item_i*> q;
  tbb::atomic<int> count;
  std::atomic_bool has_started;
  function<void()> space_f;

  void move_to_ready_pile()
  {
    work_item_i* work = NULL;
    q.try_pop(work);
    task::enqueue(*new(task::allocate_root()) run_work_item (work));
  }
public:
  serializer() {
    
    space_f = [](){};
  }

  void add(work_item_i*);

  void note_completion() {
    if (--count != 0)
      move_to_ready_pile();
  }
};

class qoq {
  concurrent_queue <work_item*> work_queue;
  concurrent_queue <concurrent_queue<work_item*> > big_queue;
  tbb::atomic<int> count;

  

public:
  void add(serializer* s)
  {
    q.push(s);
  }

  void add_queue () {
    auto q = concurrent_queue <work_item*>();
    big_queue.push(q);
  }

  void swapin()
  {
    if (big_queue.try_pop (work_queue)) {
      
    }
  }
}


class work_item: public work_item_i {
  function<void()> f;
  
  serializer* s;
  void run() {
    f();
    delete this;
    s->note_completion();
  }

public:
  work_item (decltype(f) &f_, serializer* s_): work_item_i(), f(f_), s(s_) 
  {

  }
};


void serializer::add(work_item_i* work)
{
  q.push(work);
  has_started.store (true); // was_started = started.fetch_or(true);
  if (has_started.load() && ++count == 1)
    move_to_ready_pile();

  // work_item *space = new work_item (space_f, this);
  // q.push(space);
  // if (++count == 1)
  //   move_to_ready_pile();
}


task* run_work_item::execute()
{
  item->run();
  return NULL;
}


int main( int argc, char** argv )
{
  auto num_elems = atoi(argv[1]);
  auto num_workers = atoi(argv[2]);  

  int x = 0;
  auto s = new serializer();
  auto f = function<void()> ([&](){x++;});

  concurrent_bounded_queue<bool> q;

  for (int i = 0; i < num_elems*num_workers; ++i)
    {
      auto work = new work_item (f, s);
      s->add (work);
    }
  
  auto finisher = function<void()> ([&](){q.push(true);});
  s->add (new work_item (finisher, s));

  bool done;
  q.pop(done);
  
  cout << x << endl;
}
