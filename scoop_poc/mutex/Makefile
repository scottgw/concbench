CFLAGS=-pthread -ltbb -std=c++11 -rdynamic -g -I../include -Wall

main: main.cpp ../include/serializer.h ../include/qoq.h ../include/work_item.h ../lib/libqoq.a
	g++ main.cpp ../lib/libqoq.a -o main ${CFLAGS} -O3

clean:
	@rm -f main
