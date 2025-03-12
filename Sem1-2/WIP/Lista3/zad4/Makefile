# Makefile
# Nel Skowronek 279679

all: main

main: main.o newagent.o north.o east.o south.o west.o distance.o
	clang -Wextra --pedantic -std=c11 -o main main.o newagent.o north.o east.o south.o west.o distance.o -lm

main.o: main.c
	clang -c main.c

newagent.o: newagent.c
	clang -c newagent.c

north.o: north.c
	clang -c north.c

east.o: east.c
	clang -c east.c

south.o: south.c
	clang -c south.c

west.o: west.c
	clang -c west.c

distance.o: distance.c
	clang -c distance.c

clean:
	rm -f *.o *~