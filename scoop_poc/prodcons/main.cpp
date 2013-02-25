#include <iostream>
#include <vector>
#include <queue>
#include <future>
#include <assert.h>
#include <cstdlib>

#include <tbb/concurrent_queue.h>

#include "serializer.h"
#include "qoq.h"

int max_iters;
tbb::concurrent_bounded_queue<bool> q;
std::queue<int> shared_queue;


void spawn_producer_thread (qoq *qoq) {
  serializer *s = new serializer();
  // work_item work;

  for (int i = 0; i < max_iters; ++i)
    { 
      auto f = std::function<void()> ([&i](){
          shared_queue.push(i);
        });
      auto work = new work_item (f, s);
      
      qoq->add (s);
      s->add (work);      
      s->add_end();
    }

  auto finisher = std::function<void()> ([](){
      q.push(true);
    });

  work_item *finish_work = new work_item (finisher,s);

  qoq->add(s);
  s->add (finish_work);
  s->add_end();
}


void consumer_body (qoq *qoq, serializer *s, int i) {
  if (i < max_iters) {
    if (shared_queue.empty()) {
      auto f = std::function<void()> ([=](){
          consumer_body(qoq, s, i);
        });
      auto work = new work_item (f, s);
      
      qoq->add (s);
      s->add (work);      
      s->add_end();      
    } else {
      shared_queue.pop();
      auto f = std::function<void()> ([i, qoq, s](){
          consumer_body (qoq, s, i+1);
        });
      auto work = new work_item (f, s);
      
      qoq->add (s);
      s->add (work);      
      s->add_end();
    }
  } else {
    auto finisher = std::function<void()> ([](){
        q.push(true);
      });
    
    work_item *finish_work = new work_item (finisher,s);
  
    qoq->add(s);
    s->add (finish_work);
    s->add_end();    
  }
}

void spawn_consumer_thread(qoq *qoq) {
  serializer *s = new serializer();

  auto f = std::function<void()> ([qoq, s](){
      consumer_body (qoq, s, 0);
    });
  auto work = new work_item (f, s);
      
  qoq->add (s);
  s->add (work);
  s->add_end();
}

int main( int argc, char** argv )
{
  max_iters = atoi(argv[1]);
  auto num_workers = atoi(argv[2]);  

  auto qoqs = qoq();

  for (int i = 0; i < num_workers; ++i)
    {
      new std::thread([&qoqs](){spawn_producer_thread (&qoqs);});
      new std::thread([&qoqs](){spawn_consumer_thread (&qoqs);});
    }

  bool done;

  for (int i = 0; i < 2*num_workers; ++i) {
    q.pop(done);
    std::cout << i << std::endl;
  }

  return 0;
}
