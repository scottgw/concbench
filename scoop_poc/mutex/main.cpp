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
  std::cout << "start of worker\n";

  for (int i = 0; i < num_elems; ++i)
    { 
      // s = new serializer();
      auto f = std::function<void()> ([](){
          // std::cout << "incrment\n";
          x++;
        });
      auto work = new work_item (f, s);
      qoq->add (s);
      s->add (work);      
      s->add_end();
    }

  // s = new serializer();
  auto finisher = std::function<void()> ([](){
      // std::cout << "push\n";
      q.push(true);
    });

  work_item *finish_work = new work_item (finisher,s);

  qoq->add(s);
  s->add (finish_work);
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
