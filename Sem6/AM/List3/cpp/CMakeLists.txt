set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR}/bin)

file(GLOB SOURCE src/*.cpp)
include_directories(include)

add_executable(exc1 ${SOURCE} exc1.cpp)
add_executable(exc2 ${SOURCE} exc2.cpp)