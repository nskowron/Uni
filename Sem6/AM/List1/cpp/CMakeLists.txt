set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR}/bin)

file(GLOB SOURCE src/*.cpp)
include_directories(include)

add_executable(exc1 ${SOURCE} src/main/exc1.cpp)