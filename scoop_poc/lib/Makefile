CPPFILES=qoq.cpp serializer.cpp work_item.cpp
HEADERS=../include/qoq.h ../include/serializer.h ../include/work_item.h

CCFLAGS=-I../include -O2 -std=c++11 -ltbb

libqoq.a: qoq.o serializer.o work_item.o
	ar rvs $@ $?

%.o: %.cpp $(HEADERS)
	g++ ${CCFLAGS} -c $<
