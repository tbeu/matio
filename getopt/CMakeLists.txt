project(GETOPT C)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
add_definitions(-DREPLACE_GETOPT)
add_library(gnu STATIC getopt_long.c)

install(TARGETS gnu
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)
