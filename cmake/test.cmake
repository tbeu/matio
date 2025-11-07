if(MATIO_BUILD_TESTING)
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

    find_package(Python3 COMPONENTS Interpreter)
    if(Python3_Interpreter_FOUND)
        set(MATIO_CTESTS_DIR ${PROJECT_BINARY_DIR}/tests)
        set(MATIO_TESTING_DIR ${MATIO_CTESTS_DIR}/temporary)
        file(TO_NATIVE_PATH "${PROJECT_SOURCE_DIR}/test/convert_at_to_ctest.py" NATIVE_CONVERT_SCRIPT)
        file(TO_NATIVE_PATH "${PROJECT_BINARY_DIR}/tests" NATIVE_CTESTS_DIR)
        file(TO_NATIVE_PATH "${PROJECT_SOURCE_DIR}/test" NATIVE_WORKING_DIR)
        execute_process(
            COMMAND "${Python3_EXECUTABLE}" "${NATIVE_CONVERT_SCRIPT}" "${NATIVE_CTESTS_DIR}"
            WORKING_DIRECTORY "${NATIVE_WORKING_DIR}"
            RESULT_VARIABLE TEST_CONVERT_RESULT
            OUTPUT_VARIABLE TEST_CONVERT_OUTPUT
            ERROR_VARIABLE TEST_CONVERT_ERROR
        )
        if(NOT TEST_CONVERT_RESULT EQUAL 0)
            message(FATAL_ERROR "Python script failed: ${TEST_CONVERT_ERROR}")
        endif()

        enable_testing()

        add_test(NAME create_temp_dir
            COMMAND ${CMAKE_COMMAND} -E make_directory ${MATIO_TESTING_DIR})
        set_tests_properties(create_temp_dir PROPERTIES FIXTURES_SETUP TEMPDIR)

        file(GLOB CMAKE_TEST_FILES "${MATIO_CTESTS_DIR}/*.cmake")
        foreach(test_file ${CMAKE_TEST_FILES})
            include(${test_file})
        endforeach()

        add_test(NAME remove_temp_dir
            COMMAND ${CMAKE_COMMAND} -E remove_directory ${MATIO_TESTING_DIR})
        set_tests_properties(remove_temp_dir PROPERTIES FIXTURES_CLEANUP TEMPDIR)
    else()
        message(WARNING
            " Python3 not found."
            " Set MATIO_BUILD_TESTING to OFF to silence this warning."
        )
    endif()
endif()
