# Check functions
include(CheckSymbolExists)
check_symbol_exists(vsnprintf  stdio.h  HAVE_VSNPRINTF)
check_symbol_exists(snprintf   stdio.h  HAVE_SNPRINTF)
check_symbol_exists(vasprintf  stdio.h  HAVE_VASPRINTF)
check_symbol_exists(asprintf   stdio.h  HAVE_ASPRINTF)
check_symbol_exists(strcasecmp "strings.h;string.h" HAVE_STRCASECMP)
check_symbol_exists(getopt     unistd.h HAVE_GETOPT)
check_symbol_exists(va_copy    stdarg.h HAVE_VA_COPY)
check_symbol_exists(__va_copy  stdarg.h HAVE___VA_COPY)
check_symbol_exists(localeconv locale.h HAVE_LOCALECONV)
check_symbol_exists(fseeko     stdio.h  HAVE_FSEEKO)
check_symbol_exists(ftello     stdio.h  HAVE_FTELLO)
check_symbol_exists(fseeko64   stdio.h  HAVE_FSEEKO64)
check_symbol_exists(ftello64   stdio.h  HAVE_FTELLO64)
check_symbol_exists(_fseeki64  stdio.h  HAVE__FSEEKI64)
check_symbol_exists(_ftelli64  stdio.h  HAVE__FTELLI64)

include(CheckIncludeFile)
check_include_file(inttypes.h HAVE_INTTYPES_H)
check_include_file(stdint.h   HAVE_STDINT_H)
check_include_file(intsafe.h  HAVE_INTSAFE_H)
check_include_file(strings.h  HAVE_STRINGS_H)
check_include_file(ctype.h    HAVE_CTYPE_H)
check_include_file(stdlib.h   HAVE_STDLIB_H)
check_include_file(string.h   HAVE_STRING_H)
check_include_file(stdarg.h   HAVE_STDARG_H)
check_include_file(memory.h   HAVE_MEMORY_H)
check_include_file(unistd.h   HAVE_UNISTD_H)
check_include_file(sys/stat.h HAVE_SYS_STAT_H)
check_include_file(varargs.h  HAVE_VARARGS_H)
check_include_file(locale.h   HAVE_LOCALE_H)
check_include_file(dlfcn.h    HAVE_DLFCN_H)
set(MATIO_HAVE_STDINT_H ${HAVE_STDINT_H})
set(MATIO_HAVE_INTTYPES_H ${HAVE_INTTYPES_H})
if(NOT MATIO_HAVE_STDINT_H AND MSVC)
    set(STDINT_MSVC 1)
endif()

# Check C types
include(CheckTypeSize)
check_type_size(char          SIZEOF_CHAR)
check_type_size(double        SIZEOF_DOUBLE)
check_type_size(float         SIZEOF_FLOAT)
check_type_size(int           SIZEOF_INT)
check_type_size(long          SIZEOF_LONG)
check_type_size("long long"   SIZEOF_LONG_LONG)
check_type_size(short         SIZEOF_SHORT)
check_type_size(size_t        SIZEOF_SIZE_T)
check_type_size(intmax_t      INTMAX_T)
check_type_size(uintmax_t     UINTMAX_T)
check_type_size(uintptr_t     UINTPTR_T)
check_type_size(ptrdiff_t     PTRDIFF_T)
check_type_size("long double" LONG_DOUBLE)
check_type_size("long long int" LONG_LONG_INT)
check_type_size("unsigned long long int" UNSIGNED_LONG_LONG_INT)
set(SIZEOF_VOID_P ${CMAKE_SIZEOF_VOID_P})

# Make the variables HAVE_MAT_UINT8_T, etc...
set(TYPES uint8_t uint16_t uint32_t uint64_t int8_t int16_t int32_t int64_t)
foreach(TYPE ${TYPES})
    check_type_size(${TYPE} "SIZEOF_${TYPE}")
    set(_mat_${TYPE} ${TYPE})
    string(TOUPPER ${TYPE} TYPE_UPPER)
    set(HAVE_MAT_${TYPE_UPPER} ${HAVE_SIZEOF_${TYPE}})
    if(STDINT_MSVC)
        set(HAVE_MAT_${TYPE_UPPER} 1)
    endif()
endforeach()

include(CheckLibraryExists)
check_library_exists(m pow "" HAVE_LIBM)

include(CheckCSourceCompiles)
set(TEST_CODE_DECIMAL_POINT "
    #include <locale.h>
    int main() { struct lconv l; l.decimal_point; return 0; }
    "
)
check_c_source_compiles("${TEST_CODE_DECIMAL_POINT}" HAVE_STRUCT_LCONV_DECIMAL_POINT)

set(TEST_CODE_THOUSANDS_SEP "
    #include <locale.h>
    int main(){ struct lconv l; l.thousands_sep; return 0;}
    "
)
check_c_source_compiles("${TEST_CODE_THOUSANDS_SEP}" HAVE_STRUCT_LCONV_THOUSANDS_SEP)

set(USE_GNU_LINK_FLAGS 0)
set(USE_LLVM_MACOS_LINK_FLAGS 0)
set(REQUIRE_EXPLICIT_LIBC_LINK 0)
if(NOT MSVC)
    # OpenBSD apparently requires an explicit -lc if -Wl,--no-undefined
    # is used, but that is NOT required on other platforms, and on even
    # other platforms (e.g. MinGW) -lc might not even exist at all.
    # Therefore detect if -lc works and if it is required, and omit it
    # if it doesn't.
    # Just running check_linker_flag() doesn't help here, because we
    # actually need to reference a symbol from libc to cause an error.
    # Use -shared in CMAKE_REQUIRED_LINK_OPTIONS because how symbols
    # (esp. undefined ones) are resolved during linking can differ
    # between shared libraries and executables.
    set(TEST_SRC_LINK_NO_UNDEFINED "
#include <stdlib.h>
int main() { int* foo = (int*) malloc(sizeof(int)); free(foo); return 0; }
")

    set(CMAKE_REQUIRED_FLAGS "-fPIC")
    set(CMAKE_REQUIRED_LINK_OPTIONS "-shared;-Wl,--no-undefined")
    check_c_source_compiles("${TEST_SRC_LINK_NO_UNDEFINED}" HAVE_LINK_NO_UNDEFINED_IMPLICIT_LIBC)
    set(CMAKE_REQUIRED_LINK_OPTIONS "-shared;-Wl,--no-undefined;-lc")
    check_c_source_compiles("${TEST_SRC_LINK_NO_UNDEFINED}" HAVE_LINK_NO_UNDEFINED_EXPLICIT_LIBC)
    # Reuse the same source code for other linker flag tests (This was
    # previously guarded by CMake version >= 3.17 using
    # check_linker_flag, but this variant should work for even older
    # CMake versions.)
    set(CMAKE_REQUIRED_LINK_OPTIONS "-shared;-Wl,--retain-symbols-file,${PROJECT_SOURCE_DIR}/src/matio.sym")
    check_c_source_compiles("${TEST_SRC_LINK_NO_UNDEFINED}" HAVE_LINK_RETAIN_SYMBOLS_FILE)
    set(CMAKE_REQUIRED_LINK_OPTIONS "-shared;-Wl,-undefined,error")
    check_c_source_compiles("${TEST_SRC_LINK_NO_UNDEFINED}" HAVE_LINK_UNDEFINED_ERROR)
    set(CMAKE_REQUIRED_FLAGS "")
    set(CMAKE_REQUIRED_LINK_OPTIONS "")

    if((HAVE_LINK_NO_UNDEFINED_IMPLICIT_LIBC OR HAVE_LINK_NO_UNDEFINED_EXPLICIT_LIBC) AND HAVE_LINK_RETAIN_SYMBOLS_FILE)
        message(VERBOSE "Using GNU-style linker flags")
        set(USE_GNU_LINK_FLAGS 1)
    elseif(APPLE AND HAVE_LINK_UNDEFINED_ERROR)
        message(VERBOSE "Using Apple-clang-style linker flags")
        set(USE_LLVM_MACOS_LINK_FLAGS 1)
    endif()

    if(NOT HAVE_LINK_NO_UNDEFINED_IMPLICIT_LIBC AND HAVE_LINK_NO_UNDEFINED_EXPLICIT_LIBC)
        message(VERBOSE "The usage of -Wl,--no-undefined requires the explicit usage of -lc on this platform.")
        set(REQUIRE_EXPLICIT_LIBC_LINK 1)
    endif()
endif()
