CFLAGS=-pthread -ltbb -std=c++11 -rdynamic -g -I../include -Wall -flto

main: main.cpp ../include/serializer.h ../include/qoq.h ../include/work_item.h
	g++ main.cpp ../lib/libqoq.a -o main ${CFLAGS} -O3

clean:
	@rm -f main
