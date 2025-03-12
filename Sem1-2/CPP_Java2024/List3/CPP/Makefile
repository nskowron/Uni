all:
	clang++ -Weverything -Wno-newline-eof -Wno-c++98-compat -Wno-c++98-compat-pedantic -Wno-unsafe-buffer-usage -Wno-weak-vtables -Wno-shadow-field -Wno-shadow-field-in-constructor ./prog/main.cpp ./src/*.cpp -I./include/ -o ./bin/main

test:
	clang++ -Weverything -Wno-newline-eof -Wno-c++98-compat -Wno-c++98-compat-pedantic -Wno-unsafe-buffer-usage -Wno-weak-vtables -Wno-shadow-field -Wno-shadow-field-in-constructor ./prog/test.cpp ./src/*.cpp -I./include/ -o ./bin/test