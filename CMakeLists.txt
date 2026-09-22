cmake_minimum_required(VERSION 3.7...3.31)

if(POLICY CMP0074)
    cmake_policy(SET CMP0074 NEW)
endif()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

project(matio
    VERSION 1.6.0
    LANGUAGES C CXX
)

set(CMAKE_CXX_STANDARD 11)

string(TOUPPER ${PROJECT_NAME} PROJECT_NAME_UPPER)
set(matio_PLATFORM ${CMAKE_SYSTEM_PROCESSOR}-pc-${CMAKE_SYSTEM_NAME})
set(matio_PACKAGE "${PROJECT_NAME}")
set(matio_PACKAGE_NAME "${PROJECT_NAME_UPPER}")
set(matio_PACKAGE_BUGREPORT "t-beu@users.sourceforge.net")
set(matio_PACKAGE_STRING "${PROJECT_NAME_UPPER} ${matio_VERSION}")
set(matio_PACKAGE_TARNAME "${PROJECT_NAME}")
set(matio_PACKAGE_URL "https://sourceforge.net/projects/matio")
set(matio_LIB_VERSIONINFO "15:0:1")

# Remove this block after bumping CMake to v3.21.0
# PROJECT_IS_TOP_LEVEL is defined then by default
if(CMAKE_VERSION VERSION_LESS 3.21.0)
  if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
    set(PROJECT_IS_TOP_LEVEL ON)
    set(matio_IS_TOP_LEVEL ON)
  else()
    set(PROJECT_IS_TOP_LEVEL OFF)
    set(matio_IS_TOP_LEVEL OFF)
  endif()
endif()

#Remove this block after bumping to 3.26
if(PROJECT_IS_TOP_LEVEL)
    set_property(GLOBAL PROPERTY USE_FOLDERS ON)
endif()

# Installation options
include(GNUInstallDirs)

include(cmake/options.cmake)
include(cmake/conan.cmake)
include(cmake/thirdParties.cmake)
include(cmake/staticAnalyzers.cmake)
include(cmake/compilerOptions.cmake)
include(cmake/src.cmake)
include(cmake/getopt.cmake)
include(cmake/tools.cmake)
include(cmake/test.cmake)
