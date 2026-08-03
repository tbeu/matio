# Mimics the relevant part of json-c's CMakeLists.txt that triggers
# https://github.com/tbeu/matio/issues/326
#
# See json-c: list(APPEND CMAKE_REQUIRED_DEFINITIONS -D_GNU_SOURCE) followed by
# check_symbol_exists(vasprintf stdio.h HAVE_VASPRINTF).
project(jsonc_sim C)

list(APPEND CMAKE_REQUIRED_DEFINITIONS -D_GNU_SOURCE)

include(CheckSymbolExists)
check_symbol_exists(vasprintf stdio.h HAVE_VASPRINTF)

add_library(jsonc_sim INTERFACE)
