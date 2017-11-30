set(PACKAGE_VERSION_MAJOR "1")
set(PACKAGE_VERSION_MINOR "5")
set(PACKAGE_VERSION_PATCH "2")
set(VERSION ${PACKAGE_VERSION_MAJOR}.${PACKAGE_VERSION_MINOR}.${PACKAGE_VERSION_PATCH})

set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake;${CMAKE_MODULE_PATH}")

# Options

option(ENABLE_FORTRAN "Enable fortran interface to mat library")
set(EXTRA_LANGAGE)
if (ENABLE_FORTRAN)
    set(EXTRA_LANGAGE Fortran)
    set(MATIO_FORTRAN TRUE)
endif()

#   CXX is added just for GenerateExportHeader that uses the
#   CXX compiler to check or visibilityy. It can disappear
#   as soon as this cmake bug is corrected.

project(MATIO C CXX ${EXTRA_LANGAGE})

cmake_minimum_required(VERSION 2.8)

include(FindLibrary)
include(DefaultFileVersion)
include(GenerateConfigFile)

option(USE_SYSTEM_ZLIB "Use installed zlib if possible" ON)
mark_as_advanced(USE_SYSTEM_ZLIB)
option(MAT73 "Matlab 7.3 support" ON)

unset(OPT)
if (NOT MAT73)
    set(OPT OPTIONAL)
endif()

#set(FIND_PACKAGE_DEBUG 1)
Dependencies(matio ZLIB ${OPT})
#message("[[Found: ZLIB: ${ZLIB_INCLUDE_DIRS} HDF5: ${HDF5_INCLUDE_DIRS}]]")

set(HAVE_ZLIB 1)
set(Z_PREFIX)

# Tell Hdf5 to disable its own zlib detection (and to use this version).

set(H5_ZLIB_HEADER zlib.h)

if (MAT73)
    find_package(HDF5 REQUIRED)
    message("Found HDF5 headers ${HDF5_INCLUDE_DIRS}, libraries ${HDF5_LIBRARIES}")
    include(HDF5Version)
    if (HDF5_VERSION_STRING VERSION_LESS "1.8.9")
        message(SEND_ERROR "hdf5 library must be 1.8.9 or higher. Using internal copy.")
    endif()
    if (NOT BUILD_SHARED_LIBS)
        set(TARGET_SUFFIX -static)
    endif()
    set(HAVE_HDF5 1)
    set(HDF5_LIBS ${HDF5_LIBRARIES})
endif()

# It may seem that ${CMAKE_ARCH_TRIPLET} might appear in a future cmake release.
# For now, do for the best.

set(MATIO_PLATFORM ${CMAKE_SYSTEM_PROCESSOR}-unknown-${CMAKE_SYSTEM_NAME})

# Options

option(ENABLE_PROFILE "Turn on profile")
if (ENABLE_PROFILE)
endif()

option(EXTENDED_SP "Enable extended sparse matrix data types not supported in Matlab" ON)
if (EXTENDED_SP)
    set(EXTENDED_SPARSE 1)
endif()

include (Documentation optional)
if (BUILD_DOCUMENTATION)
    add_subdirectory(documentation)
endif()

# Check headers

#   Public header

# Provide stdint.h for windows, if it is not already there.
# MSVC starts to include stddint.h only from MSVC10 !!!
# Append after in order to use the system includes if they are provided.

if (WIN32 AND NOT CYGWIN_INSTALL_PATH)
    set(CMAKE_REQUIRED_INCLUDES ${INTTYPES_INCLUDES})
    include_directories(AFTER SYSTEM ${INTTYPES_INCLUDES})
endif()

include(CheckIncludeFile)
include(CheckHeaderSTDC)

check_include_file(inttypes.h HAVE_INTTYPES_H)
check_include_file(stdint.h   HAVE_STDINT_H)
check_include_file(strings.h  HAVE_STRINGS_H)
check_include_file(ctype.h    HAVE_CTYPE_H)
check_include_file(stdlib.h   HAVE_STDLIB_H)
check_include_file(string.h   HAVE_STRING_H)
#CHECK_INCLUDE_FILE(malloc.h   HAVE_MALLOC_H)
check_header_stdc()

# type size

include(CheckTypeSize)
set(TYPES double float long int short char size_t)
foreach(TYPE ${TYPES})
    string(TOUPPER ${TYPE} TYPE_UPPER)
    check_type_size(${TYPE} SIZEOF_${TYPE_UPPER})
endforeach()
check_type_size("long long" LONG_LONG)
set(HAVE_LONG_LONG ${HAVE_${LONG_LONG}})
set(SIZEOF_LONG_LONG ${LONG_LONG})
check_type_size("long double" LONG_DOUBLE)
set(HAVE_LONG_DOUBLE ${HAVE_${LONG_DOUBLE}})
check_type_size(void* SIZEOF_VOID_P)

set(TYPES uint8_t uint16_t uint32_t uint64_t int8_t int16_t int32_t int64_t)
foreach(TYPE ${TYPES})
    string(TOUPPER ${TYPE} TYPE_UPPER)
    check_type_size(${TYPE} ${TYPE_UPPER})
    set(HAVE_MAT_${TYPE_UPPER} ${HAVE_${TYPE_UPPER}})
    set(SIZEOF_${TYPE_UPPER} ${${TYPE_UPPER}})
    if (NOT ${${TYPE_UPPER}})
        message(FATAL_ERROR "Unknown type ${TYPE}")
    endif()
endforeach()

# Check functions

include(CheckFunctionExists)
check_function_exists(vsnprintf  HAVE_VSNPRINTF)
check_function_exists(snprintf   HAVE_SNPRINTF)
check_function_exists(vasprintf  HAVE_VASPRINTF)
check_function_exists(asprintf   HAVE_ASPRINTF)
check_function_exists(strcasecmp HAVE_STRCASECMP)
check_function_exists(getopt     HAVE_GETOPT)

include(VA_COPY)
VA_COPY()

if (WIN32)
    if (NOT LIBRARY_OUTPUT_PATH)
        set(LIBRARY_OUTPUT_PATH    "${CMAKE_CURRENT_BINARY_DIR}")
    endif()
    if (NOT EXECUTABLE_OUTPUT_PATH)
        set(EXECUTABLE_OUTPUT_PATH "${CMAKE_CURRENT_BINARY_DIR}")
    endif()
endif()

if (NOT HAVE_GETOPT)
    add_subdirectory(getopt)
    include_directories(${CMAKE_CURRENT_SOURCE_DIR}/getopt)
    set(GETOPT_LIB gnu)
endif()

#   Probably a little kludgy...

set(LIBM)
if (UNIX)
    set(LIBM m)
endif()

include_directories(${CMAKE_CURRENT_BINARY_DIR}/src)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/src)
add_subdirectory(src)
add_subdirectory(tools)

# pkg-config

set(prefix ${CMAKE_INSTALL_PREFIX})
set(exec_prefix "\${prefix}")
set(libdir "\${exec_prefix}/lib")
set(includedir "\${prefix}/include")
set(ZLIB_LIBS ${ZLIB_LIBRARIES})

configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/matio.pc.in"
    "${CMAKE_CURRENT_BINARY_DIR}/matio.pc"
    IMMEDIATE @ONLY)

GenerateConfigFile(matio LIBRARIES matio)

#INSTALL(EXPORT matio DESTINATION )

# Testing

option(MATIO_BUILD_TESTING "Build matio tests" ON)
if (MATIO_BUILD_TESTING)
    if (IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/test/datasets)
        include(CTest)
        enable_testing()
        add_subdirectory(test)
    endif()
endif()

include(Uninstall)
include(Packaging)
