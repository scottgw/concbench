#ifndef __SEPARATE_H_
#define __SEPARATE_H_

#include "processor.h"
#include "private_queue.h"

separate_t separate_create (void* data) {
  separate_t sep = (separate_t) malloc (sizeof(separate_));
  sep->ref = data;
  sep->proc = processor_create();
  return sep;
}

void separate_lock_with (separate_t sep, private_queue_t q) {
  processor_add_queue (sep->proc, private_queue_queue (q));
}
    
private_queue_t separate_make_private_queue (separate_t sep) {
  return private_queue_create (sep);
}

#endif
