option(BUILD_SHARED_LIBS "Build shared libs" ON)
mark_as_advanced(BUILD_SHARED_LIBS)

set(LIB_TYPE STATIC)
if (BUILD_SHARED_LIBS)
    set(LIB_TYPE SHARED)
endif()

string(COMPARE NOTEQUAL "${BUILD_SHARED_STATUS}" "" BUILD_SHARED_STATUS_NOT_EMPTY)
if (BUILD_SHARED_STATUS_NOT_EMPTY)
    string(COMPARE NOTEQUAL "${BUILD_SHARED_STATUS}" "${BUILD_SHARED_LIBS}" RESET)
endif()

# Store in cache previous value of BUILD_SHARED_LIBS
set(BUILD_SHARED_STATUS "${BUILD_SHARED_LIBS}" CACHE INTERNAL "Previous shared status" FORCE)

function(MATIO_FIND_LIBRARY VAR)
    if (${RESET})
        set(${VAR} NOTFOUND CACHE STRING "" FORCE)
    endif()
    find_library(${VAR} ${ARGN})
    mark_as_advanced(${VAR})
endfunction()
