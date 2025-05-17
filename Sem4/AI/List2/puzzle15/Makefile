# Compiler
CXX := g++

# All .cpp files
SRC := $(wildcard *.cpp)

# Output binaries (strip .cpp)
BIN := $(SRC:.cpp=)

# Default target: build all binaries
all: $(BIN)

# Rule to build each binary from its .cpp file
%: %.cpp
	$(CXX) $< -o $@

# Clean target to remove binaries
clean:
	rm -f $(BIN)
