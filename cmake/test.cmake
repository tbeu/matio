add_executable(test_mat ${PROJECT_SOURCE_DIR}/test/test_mat.c)
target_link_libraries(test_mat matio)
if(NOT HAVE_GETOPT)
    target_link_libraries(test_mat getopt)
endif()

if(NOT HAVE_SNPRINTF OR UNIX)
    add_executable(test_snprintf ${PROJECT_SOURCE_DIR}/test/test_snprintf.c)
    if(HAVE_LIBM)
        target_link_libraries(test_snprintf m)
    endif()
    target_include_directories(test_snprintf
        PUBLIC ${PROJECT_SOURCE_DIR}/src
        PUBLIC ${PROJECT_BINARY_DIR}/src
    )
endif()
