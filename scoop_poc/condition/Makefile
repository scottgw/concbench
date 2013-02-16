CFLAGS=-rdynamic -g -I../include -I./ -fblocks -ldispatch -W -std=c99 -lBlocksRuntime -lpthread -lrt -lkqueue -pthread

main: main.c ../include/processor.h ../include/private_queue.h ../include/separate.h
	clang queue_lock_mutex.c main.c -o main ${CFLAGS} -O3
