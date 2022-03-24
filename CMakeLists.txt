cmake_minimum_required(VERSION 3.7)

if(POLICY CMP0074)
    cmake_policy(SET CMP0074 NEW)
endif()

project(matio
    VERSION 1.5.22
    LANGUAGES C CXX
)

string(TOUPPER ${PROJECT_NAME} PROJECT_NAME_UPPER)
set(matio_PLATFORM ${CMAKE_SYSTEM_PROCESSOR}-pc-${CMAKE_SYSTEM_NAME})
set(matio_PACKAGE "${PROJECT_NAME}")
set(matio_PACKAGE_NAME "${PROJECT_NAME_UPPER}")
set(matio_PACKAGE_BUGREPORT "t-beu@users.sourceforge.net")
set(matio_PACKAGE_STRING "${PROJECT_NAME_UPPER} ${matio_VERSION}")
set(matio_PACKAGE_TARNAME "${PROJECT_NAME}")
set(matio_PACKAGE_URL "https://sourceforge.net/projects/matio")
set(matio_LIB_VERSIONINFO "11:4:0")

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Installation options
include(GNUInstallDirs)

include(cmake/options.cmake)
include(cmake/conan.cmake)
include(cmake/thirdParties.cmake)
include(cmake/compilerOptions.cmake)
include(cmake/getopt.cmake)
include(cmake/src.cmake)
include(cmake/tools.cmake)
include(cmake/test.cmake)
