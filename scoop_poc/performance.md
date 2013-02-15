Performance comparison
======================

ProdCons
--------

                   Style      Time  
------------------------   -------
Blocking (STL wrapper)        404s
TBB Non-blocking (sleep)      180s
ES 7.2                        155s
TBB Blocking                  108s
ES 7.1                         33s
Queue of queues                15s
QoQ +Spinning                 3.7s
QoQ +Local queries            2.5s


Condition
---------

  ES7.1  ES7.2     PoC
------- ------  ------
    16s   130s    1.4s
