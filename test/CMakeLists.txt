add_executable(test_mat test_mat.c)
target_link_libraries(test_mat matio ${GETOPT_LIB})

add_executable(test_snprintf test_snprintf.c ${MATIO_SOURCE_DIR}/src/snprintf.c)
target_link_libraries(test_snprintf ${GETOPT_LIB} m)

option(MATLAB_TESTING "Enable matlab read tests (requires a function matlab)" ON)
if (MATLAB_TESTING)
    find_program(MATLAB matlab)
else()
    set(MATLAB FALSE)
endif()

macro(PARSE_TEST_ARGUMENTS LIST_VARS DEFAULT_VAR)
    unset(${DEFAULT_VAR})
    foreach(var ${LIST_VARS})
        unset(${var})
    endforeach ()

    set(CURRENT_VAR ${DEFAULT_VAR})
    foreach (arg ${ARGN})
        set(skip_this_arg FALSE)
        foreach(var ${LIST_VARS})
            if (${arg} STREQUAL ${var})
                set(CURRENT_VAR ${var})
                set(skip_this_arg TRUE)
                break()
            endif()
        endforeach ()
        if (NOT skip_this_arg)
            set(${CURRENT_VAR} ${${CURRENT_VAR}} ${arg})
        endif()
    endforeach ()
endmacro()

macro(SET_TEST_DIR TEST_DIR)
    if (WIN32)
        if (NOT CMAKE_BUILD_TYPE)
            set(CMAKE_BUILD_TYPE Debug)
        endif()
        set(${TEST_DIR} ${EXECUTABLE_OUTPUT_PATH}/${CMAKE_BUILD_TYPE})
    else()
        set(${TEST_DIR} ${CMAKE_CURRENT_BINARY_DIR})
    endif()
endmacro()

macro(MATIO_TEST_READ NAME REFERENCE PROG_NAME)
    PARSE_TEST_ARGUMENTS("DEPENDS" "DEFAULT" ${ARGN})
    set(PROG_ARGS "${DEFAULT}")
    SEPARATE_ARGUMENTS(ARGS UNIX_COMMAND "${PROG_ARGS}")
    SET_TEST_DIR(TEST_DIR)
    set(EXECUTABLE ${TEST_DIR}/${PROG_NAME}${CMAKE_EXECUTABLE_SUFFIX})
    set(OUTPUT ${NAME}.out)
    add_test(${NAME} ${EXECUTABLE} ${PROG_ARGS} -o ${CMAKE_CURRENT_BINARY_DIR}/${OUTPUT}) # To perform memcheck tests
    if (DEPENDS)
        SET_TESTS_PROPERTIES(${NAME} PROPERTIES DEPENDS "${DEPENDS}")
    endif()
    add_test(${NAME}-COMPARISON
             ${CMAKE_COMMAND} -D TEST_DIR:STRING=${TEST_DIR}
                              -D TEST_OUTPUT:STRING=${OUTPUT}
                              -D TEST_REFERENCE_DIR:STRING=${MATIO_SOURCE_DIR}/test/results
                              -D TEST_RESULT:STRING=${REFERENCE}
                              -P ${MATIO_SOURCE_DIR}/test/runTest.cmake) # To compare output to reference file
    #SET_TESTS_PROPERTIES(${NAME} PROPERTIES DEPENDS ${EXECUTABLE})

    #  Add a dependency to the MATIO-matlab test so that COMPARISON tests are run after matlab ones (so they do not
    #  cleanup the files too early).

    if (MATLAB)
        string(REPLACE "MATIO-" "MATIO-matlab" MATDEPENDS ${NAME})
        set_tests_properties(${NAME}-COMPARISON PROPERTIES DEPENDS ${MATDEPENDS})
    endif()
    set_tests_properties(${NAME}-COMPARISON PROPERTIES
                         DEPENDS ${NAME}
                         PASS_REGULAR_EXPRESSION "Success")
endmacro()

set(MATIO_FILES)
macro(MATIO_TEST_WRITE NAME FILENAME PROG_NAME)
    PARSE_TEST_ARGUMENTS("DEPENDS" "DEFAULT" ${ARGN})
    set(PROG_ARGS "${DEFAULT}")
    SEPARATE_ARGUMENTS(ARGS UNIX_COMMAND "${PROG_ARGS}")
    SET_TEST_DIR(TEST_DIR)
    set(EXECUTABLE ${TEST_DIR}/${PROG_NAME}${CMAKE_EXECUTABLE_SUFFIX})
    set(MATIO_FILES ${MATIO_FILES} ${FILENAME} PARENT_SCOPE)
    add_test(${NAME} ${EXECUTABLE} ${PROG_ARGS} -o ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME}) # To perform memcheck tests
endmacro()

macro(MATIO_TEST_MATLAB_READ NAME FILE TEST_TYPE CLASS)
    if (MATLAB)
        PARSE_TEST_ARGUMENTS("DEPENDS" "DEFAULT" ${ARGN})
        SET_TEST_DIR(TEST_DIR)
        string(REPLACE "-" "_" MSBNAME ${FILE})
        string(REPLACE ".mat" "" MSBNAME ${MSBNAME})
        string(REPLACE "." "_" MSBNAME ${MSBNAME})
        set(FILENAME ${TEST_DIR}/${FILE})
        set(TYPE ${CLASS})
        configure_file(${CMAKE_CURRENT_SOURCE_DIR}/matlab_${TEST_TYPE}.cmake.in ${CMAKE_CURRENT_BINARY_DIR}/matlab/${MSBNAME}.m @ONLY)
        add_test(NAME ${NAME} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/matlab COMMAND ${MATLAB} -nosplash -nojvm -r "${MSBNAME};exit")
        if (DEPENDS)
            set_tests_properties(${NAME} PROPERTIES DEPENDS ${DEPENDS})
        endif()
        set_tests_properties(${NAME} PROPERTIES PASS_REGULAR_EXPRESSION "PASSED")
    endif()
endmacro()

#MATIO_TEST(TEST_SNPRINTF test_snprintf)

#   TODO: add dump tests

set(v4_vars var1 var11 var24)
set(vars)
foreach(i RANGE 1 69)
    set(vars ${vars} "var${i}")
endforeach()
set(vars ${vars} "var90" "var91" "var92")
set(compressed_vars ${vars})
set(uncompressed_vars ${vars})

set(HDFTESTS)
if (MAT73)
    set(HDFTESTS hdf)
    set(hdf_vars ${vars})
    set(special_vars var23 var27 var52 var66)
endif()

foreach(vers v4 compressed uncompressed ${HDFTESTS})
    foreach(endian le be)
        foreach(var ${${vers}_vars})
            set(MODIFIER)
            if (${vers} STREQUAL hdf)
                list(FIND special_vars ${var} special)
                if (NOT ${special} EQUAL -1)
                    set(MODIFIER -hdf)
                endif()
            endif()
            set(testname read-${vers}-${endian}-${var})
            set(input ${CMAKE_CURRENT_SOURCE_DIR}/datasets/matio_test_cases_${vers}_${endian}.mat)
            set(reference read-${var}${MODIFIER}.out)
            MATIO_TEST_READ(MATIO-${testname} ${reference} test_mat readvar ${input} ${var})
        endforeach()
    endforeach()
endforeach()

set(MATIO_WRITE_TESTS_NUMERIC
     write_2d_numeric write_complex_2d_numeric write_struct_2d_numeric
     write_struct_complex_2d_numeric write_cell_2d_numeric write_cell_complex_2d_numeric)

set(VERSIONS 5)
if (MAT73)
    set(VERSIONS ${VERSIONS} 7.3)
endif()

set(write_char_vars             a)
set(write_empty_2d_numeric_vars empty)
set(write_empty_struct_vars     var1 var2 var3 var4)
set(write_empty_cell_vars       var1 var2)
set(write_sparse_vars           sparse_matrix)
set(write_complex_sparse_vars   sparse_matrix)
set(writeinf_vars               d)
set(writenan_vars               d)
set(writenull_vars              d_null cd_null char_null struct_null struct_empty_with_fields struct_null_fields cell_null cell_null_cells)
set(writeslab_vars              d f i)

#   Tests that behave differently across versions.

set(writenull_special_vars          d_null cd_null char_null struct_null_fields cell_null_cells)
set(write_empty_struct_special_vars var4)

set(MATIO_EMPTY_TESTS  write_empty_2d_numeric write_empty_struct write_empty_cell)
set(MATIO_SPARSE_TESTS write_sparse write_complex_sparse)
#set(MATIO_OTHER_TESTS writeinf writenan writenull writeslab)
#   Invalidate writeslab tests which fail with segfault and cannot be xfailed.

set(MATIO_OTHER_TESTS writeinf writenull)
foreach (version ${VERSIONS})
    foreach(type write_char ${MATIO_SPARSE_TESTS} ${MATIO_EMPTY_TESTS} ${MATIO_OTHER_TESTS})
        set(testname ${type}-${version})
        set(filename test_${testname}.mat)
        MATIO_TEST_WRITE(MATIO-${testname} ${filename} test_mat -v ${version} ${type})
        MATIO_TEST_MATLAB_READ(MATIO-matlab-${testname} ${filename} ${type} ${class} DEPENDS MATIO-${testname})
        foreach (var ${${type}_vars})
            set(MODIFIER)
            if (${version} STREQUAL "7.3")
                list(FIND ${type}_special_vars ${var} special)
                if (NOT ${special} EQUAL -1)
                    set(MODIFIER -73)
                endif()
            endif()
            set(reference readvar-${type}${MODIFIER}-${var}.out)
            if (WIN32 AND ${type} STREQUAL "writeinf")
                set(reference readvar-${type}${MODIFIER}-${var}-win.out)
            endif()
            MATIO_TEST_READ(MATIO-readvar-${testname}-${var} ${reference} test_mat readvar ${filename} ${var} DEPENDS MATIO-${testname})
        endforeach()
        if (${version} STREQUAL "5")
            set(testname ${testname}-compressed)
            set(filename test_${testname}.mat)
            MATIO_TEST_WRITE(MATIO-${testname} ${filename} test_mat -v ${version} -z ${type} -o ${filename})
            MATIO_TEST_MATLAB_READ(MATIO-matlab-${testname} ${filename} ${type} NONE DEPENDS MATIO-${testname})
            foreach (var ${${type}_vars})
                set(reference readvar-${type}${MODIFIER}-${var}.out)
                if (WIN32 AND ${type} STREQUAL "writeinf")
                    set(reference readvar-${type}${MODIFIER}-${var}-win.out)
                endif()
                MATIO_TEST_READ(MATIO-readvar-${testname}-${var} ${reference} test_mat readvar ${filename} ${var} DEPENDS MATIO-${testname})
            endforeach()
        endif()
    endforeach()
    foreach(type ${MATIO_WRITE_TESTS_NUMERIC})
        foreach(class double single int64 uint64 int32 uint32 int16 uint16 int8 uint8)
            set(testname ${type}-${class}-${version})
            set(filename test_${testname}.mat)
            MATIO_TEST_WRITE(MATIO-${testname} ${filename} test_mat -c ${class} -v ${version} ${type})
            set(reference ${type}-${class}.out)
            MATIO_TEST_MATLAB_READ(MATIO-matlab-${testname} ${filename} ${type} ${class} DEPENDS MATIO-${testname})
            MATIO_TEST_READ(MATIO-readvar-${testname} ${reference} test_mat readvar ${filename} a DEPENDS MATIO-${testname})
            if (${version} STREQUAL "5")
                set(testname ${testname}-compressed)
                set(filename test_${testname}.mat)
                MATIO_TEST_WRITE(MATIO-${testname} ${filename} test_mat -c ${class} -v ${version} -z ${type} -o ${filename})
                MATIO_TEST_MATLAB_READ(MATIO-matlab-${testname} ${filename} ${type} ${class} DEPENDS MATIO-${testname})
                MATIO_TEST_READ(MATIO-readvar-${testname} ${reference} test_mat readvar ${filename} a DEPENDS MATIO-${testname})
            endif()
        endforeach()
    endforeach()
endforeach()

# See comment on writeslab above.
#SET_TESTS_PROPERTIES(
#    MATIO-readvar-writeslab-7.3-i-COMPARISON
#    MATIO-readvar-writeslab-7.3-f-COMPARISON MATIO-readvar-writeslab-7.3-d-COMPARISON
#    MATIO-readvar-writenan-5-d-COMPARISON MATIO-readvar-writenan-5-compressed-d-COMPARISON MATIO-readvar-writenan-7.3-d-COMPARISON
#    PROPERTIES WILL_FAIL TRUE)

#SET_TESTS_PROPERTIES(
#    MATIO-readvar-writenan-5-d-COMPARISON MATIO-readvar-writenan-5-compressed-d-COMPARISON
#    PROPERTIES WILL_FAIL TRUE)
#
#if (MAT73)
#    SET_TESTS_PROPERTIES(
#    MATIO-readvar-writenan-7.3-d-COMPARISON
#    PROPERTIES WILL_FAIL TRUE)
#endif()

# See comment on writeslab above.
#if (MATLAB)
#    SET_TESTS_PROPERTIES( MATIO-matlab-writeslab-7.3 PROPERTIES WILL_FAIL TRUE)
#endif()

set(MATIO_CELL_TESTS cell_api_set cell_api_getlinear cell_api_getcells)
set(MATIO_STRUCT_TESTS struct_api_create struct_api_setfield struct_api_getfieldnames struct_api_addfield struct_api_getlinear struct_api_get)
foreach(type ${MATIO_CELL_TESTS} ${MATIO_STRUCT_TESTS})
    set(reference ${type}.out)
    MATIO_TEST_READ(MATIO-${type} ${reference} test_mat ${type})
endforeach()

#foreach(arg ${MATIO_OTHER_FILES})
#    set(testname ${arg})
#    set(filename test_${testname}.mat)
#    if (${arg} STREQUAL "writenull")
#        set(filename test_write_null.mat)
#    endif()
#    set(MATIO_FILES ${MATIO_FILES} ${filename})
##    MATIO_TEST(MATIO-${testname} ${filename} Null.out test_mat ${arg})
#endforeach()

set(MATIO_IND_TESTS ind2sub sub2ind)
foreach(arg ${MATIO_IND_TESTS})
    set(TEST_REFERENCE ${arg}.out)
    MATIO_TEST_READ(MATIO-${arg} ${TEST_REFERENCE} test_mat ${arg})
endforeach()

#set(DATASETS d f i64 ui64 i32 i16 i8 str)
#foreach(file ${MATIO_FILES})
#    MATIO_TEST(MATIO-copy-${file} copy_${file} Copy.out test_mat copy ${file} -o copy_${file})
#    foreach (var ${DATASETS})
#        add_test(MATIO-delete-${file} test_mat delete ${file} ${var})
#    endforeach()
#endforeach()

#set(MATIO_WRITESLAB_VARS d f i)
#foreach(var ${MATIO_WRITESLAB_VARS})
#    MATIO_TEST(MATIO-readslab-${var} "" test_readslab_${var}.out test_mat readslab test_mat_writeslab.mat ${var})
#endforeach()

#foreach(arg write_struct_2d_numeric write_struct_complex_2d_numeric)
#    set(VERSIONS 4 5)
#    if (MAT73)
#        set(VERSIONS ${VERSIONS} 7.3)
#    endif()
#    foreach(field 1 2)
#        foreach(class double single int64 uint64 int32 uint32 int16 uint16 int8 uint8)
#            foreach (version ${VERSIONS})
#                set(testname ${arg}-${class}-${version})
#                set(filename test_${testname}.mat)
#                set(testname getstructfield-${testname})
#                MATIO_TEST(MATIO-${testname} "" ${testname}.out test_mat getstructfield ${filename} a field${field})
#                if (${version} STREQUAL "5")
#                    set(testname ${arg}-${class}-${version}-compressed)
#                    set(filename test_${testname}.mat)
#                    set(testname getstructfield-${testname})
#                    MATIO_TEST(MATIO-${testname} "" ${testname}.out test_mat getstructfield ${filename} a field${field})
#                endif()
#            endforeach()
#        endforeach()
#    endforeach()
#endforeach()

# Add more tests for these.

set(MATIO_READ_TESTS readvar4 readslab4 slab3)

# Set tests that are expected to fail (TO BE CORRECTED).

if (ENABLE_FORTRAN)
    include_directories(${MATIO_SOURCE_DIR}/src/fortran ${MATIO_BINARY_DIR}/src/fortran)
    add_executable(test_matf test_matf.f90)
    target_link_libraries(test_matf fmatio matio m)
    # TESTS
endif()
