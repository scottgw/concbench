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

int x = 0;
int max_iters = 5000;
tbb::concurrent_bounded_queue<bool> q;
void worker_body (int signal, qoq *qoq, serializer *s, int i);

void worker_wait_body (int signal, qoq *qoq, serializer *s, int i) 
{
  // executed from qoq
  if (x % 2 == signal) {
    // finish this serializer, it should be the last in the
    // currently executed serializer.
    s->add_end();

    worker_body (signal, qoq, s, i);
    // ++retries;
  } else { 
    // there's something in the queue
    // we'll put a new job on to fetch the item (which we know is there)
    // terminate the serializer and loop back.
    auto f = std::function<void()> ([signal, s, qoq, i](){
        x++;
        s->add_end();
        
        auto next_body = std::function<void()> ([signal, s, qoq, i](){
            worker_body (signal, qoq, s, i+1);
          });

        one_off (next_body);
      });
    auto work = new work_item (f, s);
    
    s->add (work);
  }  
}

void worker_body (int signal, qoq *qoq, serializer *s, int i) 
{
  if (i < max_iters) {
    auto f = std::function<void()> ([=](){
        worker_wait_body (signal, qoq, s, i);
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

void spawn_worker_thread(qoq *qoq, int signal) {
  serializer *s = new serializer();

  worker_body (signal, qoq, s, 0);
}

int main( int argc, char** argv )
{
  max_iters = atoi(argv[1]);
  auto num_workers = atoi(argv[2]);  
  auto qoqs = qoq();

  for (int i = 0; i < num_workers; ++i)
    {
      one_off([&qoqs](){spawn_worker_thread (&qoqs, 0);});
      one_off([&qoqs](){spawn_worker_thread (&qoqs, 1);});
    }

  bool done;

  for (int i = 0; i < 2*num_workers; ++i) {
    q.pop(done);
    std::cout << i << std::endl;
  }

  std::cout << x << std::endl;

  return 0;
}
