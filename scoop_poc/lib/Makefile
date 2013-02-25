CPPFILES=qoq.cpp serializer.cpp work_item.cpp
HEADERS=../include/qoq.h ../include/serializer.h ../include/work_item.h

CCFLAGS=-I../include -O3 -std=c++11 -ltbb -flto -g -rdynamic

libqoq.a: qoq.o serializer.o work_item.o
	ar rvs $@ $?

%.o: %.cpp $(HEADERS)
	g++ ${CCFLAGS} -c $<

clean:
	@rm -f *.o *.a