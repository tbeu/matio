add_executable(test_mat "${PROJECT_SOURCE_DIR}/test/test_mat.c")
target_link_libraries(test_mat matio)
if(NOT HAVE_GETOPT)
    target_link_libraries(test_mat getopt)
endif()

if(NOT HAVE_SNPRINTF OR UNIX)
    add_executable(test_snprintf
        "${PROJECT_SOURCE_DIR}/test/test_snprintf.c"
        "${PROJECT_SOURCE_DIR}/snprintf/snprintf.c"
    )
    if(HAVE_LIBM)
        target_link_libraries(test_snprintf m)
    endif()
    target_include_directories(test_snprintf PUBLIC
        "${PROJECT_SOURCE_DIR}/src"
        "${PROJECT_BINARY_DIR}/src"
    )
endif()

if(BUILD_TESTING)
    find_package(Python3 COMPONENTS Interpreter)
    if(Python3_Interpreter_FOUND)
        set(MATIO_CTESTS_DIR ${PROJECT_BINARY_DIR}/ctests)
        execute_process(
            COMMAND "${Python3_EXECUTABLE}" "${PROJECT_SOURCE_DIR}/test/convert_at_to_ctest.py" "${MATIO_CTESTS_DIR}"
            WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}/test"
        )
        file(GLOB CMAKE_TEST_FILES "${MATIO_CTESTS_DIR}/*.cmake")
        foreach(test_file ${CMAKE_TEST_FILES})
            message(STATUS "Include generated tests ${test_file}")
            include(${test_file})
        endforeach()
    else()
        message(WARNING
            " Python3 not found."
            " Set BUILD_TESTING to OFF to silence this warning."
        )
    endif()
endif()
