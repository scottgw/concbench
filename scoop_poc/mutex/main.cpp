#include <iostream>
#include <assert.h>
#include <atomic>
#include <functional>
#include <thread>

#include <tbb/concurrent_queue.h>

#include "serializer.h"
#include "qoq.h"

int num_elems;
tbb::concurrent_bounded_queue<bool> q;  
int x = 0;

void spawn_worker_thread (qoq *qoq) {
  serializer *s = new serializer();
  work_item *work;
  auto f = std::function<void()> ([](){x++;});

  for (int i = 0; i < num_elems; ++i)
    { 
      work = new work_item (f, s);
      qoq->add (s);
      s->add (work);      
      s->add_end();
    }

  auto finisher = std::function<void()> ([](){
      q.push(true);
    });

  qoq->add(s);
  s->add (new work_item (finisher, s));
  s->add_end();
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
    std::cout << i << std::endl;
  }
    
  std::cout << x << std::endl;
}
