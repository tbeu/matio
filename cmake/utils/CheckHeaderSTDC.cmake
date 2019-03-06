#
# - Check if the system has the ANSI C files
# CHECK_HEADER_STDC
#
# The following variables may be set before calling this macro to
# modify the way the check is run:
#
#  CMAKE_REQUIRED_FLAGS = string of compile command line flags
#  CMAKE_REQUIRED_DEFINITIONS = list of macros to define (-DFOO=bar)
#  CMAKE_REQUIRED_INCLUDES = list of include directories
# Copyright (c) 2009, Michihiro NAKAJIMA
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.


macro (CHECK_HEADER_STDC)
    if (NOT DEFINED STDC_HEADERS)
    if (CMAKE_REQUIRED_INCLUDES)
        set(CHECK_HEADER_STDC_C_INCLUDE_DIRS "-DINCLUDE_DIRECTORIES=${CMAKE_REQUIRED_INCLUDES}")
    else()
        set(CHECK_HEADER_STDC_C_INCLUDE_DIRS)
    endif()
    set(MACRO_CHECK_HEADER_STDC_FLAGS ${CMAKE_REQUIRED_FLAGS})

    message(STATUS "Cheking for ANSI C header files")
    try_run(CHECK_HEADER_STDC_result
      CHECK_HEADER_STDC_compile_result
      ${CMAKE_BINARY_DIR}
      ${CMAKE_CURRENT_SOURCE_DIR}/cmake/utils/CheckHeaderSTDC.c
      COMPILE_DEFINITIONS ${CMAKE_REQUIRED_DEFINITIONS}
      CMAKE_FLAGS
      -DCOMPILE_DEFINITIONS:STRING=${MACRO_CHECK_HEADER_STDC_FLAGS}
      "${CHECK_HEADER_STDC_C_INCLUDE_DIRS}"
      OUTPUT_VARIABLE OUTPUT)

    if (CHECK_HEADER_STDC_compile_result AND CHECK_HEADER_STDC_result EQUAL 0)
      find_path(CHECK_HEADER_STDC_path "string.h")
      if (CHECK_HEADER_STDC_path)
        file(STRINGS "${CHECK_HEADER_STDC_path}/string.h" CHECK_HEADER_STDC_result REGEX "[^a-zA-Z_]memchr[^a-zA-Z_]")
    if (CHECK_HEADER_STDC_result)
          file(STRINGS "${CHECK_HEADER_STDC_path}/stdlib.h" CHECK_HEADER_STDC_result REGEX "[^a-zA-Z_]free[^a-zA-Z_]")
    endif (CHECK_HEADER_STDC_result)
      endif (CHECK_HEADER_STDC_path)
    endif()

    if(CHECK_HEADER_STDC_result)
      message(STATUS "Cheking for ANSI C header files - found")
      set(STDC_HEADERS 1 CACHE INTERNAL "Have ANSI C headers")
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
        "Determining if the include file ${INCLUDE} "
        "exists passed with the following output:\n"
        "${OUTPUT}\n\n")
    else()
      message(STATUS "Cheking for ANSI C header files - not found")
      set(STDC_HEADERS "" CACHE INTERNAL "Have ANSI C headers")
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
        "Determining if the include file ${INCLUDE} "
        "exists failed with the following output:\n"
        "${OUTPUT}\n\n")
    endif()
  endif()
endmacro()
