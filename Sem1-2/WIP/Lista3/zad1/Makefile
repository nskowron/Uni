# Makefile
# Nel Skowronek 279679

all: main

main: main.o palindrom.o
	clang -Wextra --pedantic -std=c11 -o main main.o palindrom.o

main.o: main.c
	clang -c main.c

palindrom.o: palindrom.c
	clang -c palindrom.c

clean:
	rm -f *.o *~
