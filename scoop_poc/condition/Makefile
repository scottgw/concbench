CFLAGS=-pthread -rdynamic -g -I../include -I./ -fblocks -ldispatch -W -std=c99 -lBlocksRuntime

main: main.c
	clang queue_lock_mutex.c main.c -o main ${CFLAGS} -O3

