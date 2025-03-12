# Makefile
# Nel Skowronek 279679

all: main

main: main.o phi.o
	clang -Wextra --pedantic -std=c11 -o main main.o phi.o

main.o: main.c
	clang -c main.c

phi.o: phi.c 
	clang -c phi.c

clean:
	rm -f *.o *~
