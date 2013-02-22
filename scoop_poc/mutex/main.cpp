#include <iostream>
#include <assert.h>
#include <atomic>
#include <functional>
#include <thread>

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

typedef concurrent_queue<work_item_i*> work_queue;

class qoq;

class serializer {
  work_queue q;
  tbb::atomic<int> count;
  std::atomic_bool spawned;

  void move_to_ready_pile();

public:
  serializer() {
    count.store(1);
  }

  void add(work_item_i* work)
  {
    q.push(work);
    int new_count = ++count;
    if (new_count == 1)
      move_to_ready_pile();
  }
  
  // void add_end () {
  //   add ();
  // }

  void start()
  {
    if (--count >= 1) { // is this right, double starts?
      move_to_ready_pile();
    }
  }

  void note_completion() {
    if (--count != 0)
      move_to_ready_pile();
    // else
    //   spawned.store(false);
  }

  qoq *parent;
};

class qoq {
  concurrent_queue <serializer*> big_queue;
  tbb::atomic<int> count;

public:
  void add(serializer *s)
  {
    s->parent = this;
    big_queue.push(s);
    if (++count == 1)
      start_sub_queue();
  }

  void note_completion() {
    if (--count != 0)
      start_sub_queue();
  }

  void start_sub_queue()
  {
    serializer *s = NULL;
    big_queue.try_pop (s);
    // cout << "starting sub queue " << s << "\n";
    assert (s);
    s->start();
  }
};




class work_item: public work_item_i {
  function<void()> f;
  
  serializer* s;
  void run() {
    // cout << "processing queue item\n";
    f();
    // delete this;
    s->note_completion();
  }

public:
  work_item (decltype(f) &f_, serializer* s_): work_item_i(), f(f_), s(s_) 
  {

  }
};

void serializer::move_to_ready_pile()
  {
    // cout << "spawning off task\n";
    work_item_i* work = NULL;
    q.try_pop(work);
    if (work != NULL) {
      // if (spawned.load()) {
      //   work->run();
      // } else {
      //   spawned.store (true);
      task::enqueue(*new(task::allocate_root()) run_work_item (work));
      // }
    }
    else {
      // spawned.store (false);
      parent->note_completion();
    }
  }

task* run_work_item::execute()
{
  item->run();
  return NULL;
}

int num_elems;
concurrent_bounded_queue<bool> q;  
int x = 0;

void add_end(serializer *s) {
  serializer *s_ = s;
  s->add(NULL);
}

void spawn_worker_thread (qoq *qoq) {
  serializer *s = new serializer();
  auto f = function<void()> ([](){x++;});
  auto work = new work_item (f, s);

  for (int i = 0; i < num_elems; ++i)
    {
  
      qoq->add (s);
      s->add (work);      
      add_end(s);
    }

  auto finisher = function<void()> ([](){
      // cout << "prepush\n";
      q.push(true);
    });
  s = new serializer();
  qoq->add(s);
  s->add (new work_item (finisher, s));
  add_end(s);
}

int main( int argc, char** argv )
{
  num_elems = atoi(argv[1]);
  auto num_workers = atoi(argv[2]);  

  auto qoqs = new qoq();

  for (int i = 0; i < num_workers; ++i)
    {
      new std::thread([=](){spawn_worker_thread (qoqs);});
    }

  bool done;

  for (int i = 0; i < num_workers; ++i) {
    q.pop(done);
    cout << i << endl;
  }
    
  cout << x << endl;
}
