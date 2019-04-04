
include(cmake/utils/VA_COPY.cmake)
VA_COPY()

include(cmake/utils/CheckHeaderSTDC.cmake)
check_header_stdc()

# Make the variables HAVE_MAT_UINT8_T, etc...
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
