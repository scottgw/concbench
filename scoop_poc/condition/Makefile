CFLAGS=-pthread -ltbb -std=c++11 -rdynamic -g -I../include -Wall

main: main.cpp ../include/processor.h ../include/private_queue.h ../include/separate.h
	g++ main.cpp -o main ${CFLAGS} -O3

