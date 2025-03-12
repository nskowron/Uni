project(lsh)

set(EXECUTABLE_OUTPUT_PATH ${CMAKE_SOURCE_DIR})

include_directories(include)
set(SOURCES
    src/builtin.c
    src/execution.c
    src/jobs.c
    src/global.c
    src/pipes.c
    src/signals.c
    src/utils.c
    main.c
)

add_executable(lsh ${SOURCES})