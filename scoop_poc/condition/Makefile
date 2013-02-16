CFLAGS=-rdynamic -g -I../include -I./ -fblocks -ldispatch -W -std=c99 -lBlocksRuntime -lpthread -lrt -lkqueue

main: main.c
	clang queue_lock_mutex.c main.c -o main ${CFLAGS} -O3

