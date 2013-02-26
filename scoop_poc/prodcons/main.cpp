#include <iostream>
#include <vector>
#include <queue>
#include <future>
#include <assert.h>
#include <cstdlib>

#include <tbb/concurrent_queue.h>

#include "one_off.h"
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

std::atomic<int> retries;

void consumer_body (qoq *qoq, serializer *s, int i);

void consumer_wait_body (qoq *qoq, serializer *s, int i) 
{
  // executed from qoq
  if (shared_queue.empty()) {
    // finish this serializer, it should be the last in the
    // currently executed serializer.
    s->add_end();

    consumer_body (qoq, s, i);
    // ++retries;
  } else { 
    // there's something in the queue
    // we'll put a new job on to fetch the item (which we know is there)
    // terminate the serializer and loop back.
    auto f = std::function<void()> ([s, qoq, i](){
        shared_queue.pop();
        s->add_end();
        
        auto next_body = std::function<void()> ([s, qoq, i](){
            consumer_body (qoq, s, i+1);
          });

        one_off (next_body);
      });
    auto work = new work_item (f, s);
    
    s->add (work);
  }  
}

void consumer_body (qoq *qoq, serializer *s, int i) 
{
  if (i < max_iters) {
    auto f = std::function<void()> ([=](){
        consumer_wait_body (qoq, s, i);
      });
    
    work_item *work = new work_item (f, s);
  
    qoq->add(s);
    s->add (work);
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

  consumer_body (qoq, s, 0);
}

int main( int argc, char** argv )
{
  max_iters = atoi(argv[1]);
  auto num_workers = atoi(argv[2]);  
  auto qoqs = qoq();
  retries.store(0);

  for (int i = 0; i < num_workers; ++i)
    {
      one_off([&qoqs](){spawn_consumer_thread (&qoqs);});
      one_off([&qoqs](){spawn_producer_thread (&qoqs);});
    }

  bool done;

  for (int i = 0; i < 2*num_workers; ++i) {
    q.pop(done);
    std::cout << i << std::endl;
  }

  std::cout << shared_queue.size() << std::endl;

  return 0;
}
