if (NOT HAVE_GETOPT)
    add_library(gnu STATIC ${PROJECT_SOURCE_DIR}/getopt/getopt_long.c)
    target_compile_definitions(gnu
        PUBLIC -DREPLACE_GETOPT)
    target_include_directories(gnu
        PUBLIC ${PROJECT_SOURCE_DIR}/getopt/)
    set(GETOPT_LIB gnu)
endif()

set(getopt_SOURCES
	${PROJECT_SOURCE_DIR}/getopt/getopt_long.c
	${PROJECT_SOURCE_DIR}/getopt/getopt.h
)
add_library(getopt STATIC ${getopt_SOURCES} )
target_include_directories(getopt PUBLIC ${PROJECT_SOURCE_DIR}/getopt)

# FIXME this should be fixed in the sourcecode
target_compile_definitions(getopt PUBLIC -DREPLACE_GETOPT=1)
