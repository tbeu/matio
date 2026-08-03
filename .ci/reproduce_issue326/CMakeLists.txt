# Reproduction for https://github.com/tbeu/matio/issues/326
#
# Issue #326 occurs when matio is built as a subproject of a parent CMake
# project that also builds json-c (as in wissem01chiha/slxio). json-c's
# CMakeLists.txt appends -D_GNU_SOURCE to CMAKE_REQUIRED_DEFINITIONS and then
# runs check_symbol_exists(vasprintf stdio.h HAVE_VASPRINTF). With _GNU_SOURCE
# defined, mingw-w64's stdio.h declares vasprintf (redirected to
# __mingw_vasprintf in libmingwex), so the check stores HAVE_VASPRINTF=1 in the
# shared top-level CMake cache.
#
# matio's own check_symbol_exists(vasprintf stdio.h HAVE_VASPRINTF) in
# cmake/compilerOptions.cmake then reuses that cached value instead of
# re-running the check, so matio builds snprintf/snprintf.c with
# HAVE_VASPRINTF=1 but without _GNU_SOURCE. Since mingw-w64 does not declare
# vasprintf without _GNU_SOURCE, the build fails with
#   snprintf/snprintf.c:1453:11: error: implicit declaration of function 'vasprintf'
#
# This project reproduces the exact setup by placing a json-c-like sibling
# subdirectory next to matio.
cmake_minimum_required(VERSION 3.15)

project(reproduce_issue326 C CXX)

add_subdirectory(jsonc_sim)
add_subdirectory(../.. matio)
