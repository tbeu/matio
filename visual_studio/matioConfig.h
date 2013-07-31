/* Debug enabled */
#undef DEBUG

/* Extended sparse matrix data types */
#define EXTENDED_SPARSE 1

/* Define to dummy `main' function (if any) required to link to the Fortran
   libraries. */
#undef FC_DUMMY_MAIN

/* Define if F77 and FC dummy `main' functions are identical. */
#undef FC_DUMMY_MAIN_EQ_F77

/* Define to a macro mangling the given C identifier (in lower and upper
   case), which must not contain underscores, for linking with Fortran. */
#undef FC_FUNC

/* As FC_FUNC, but for C identifiers containing underscores. */
#undef FC_FUNC_

/* Have asprintf */
#undef HAVE_ASPRINTF

/* Define to 1 if you have the <dlfcn.h> header file. */
#undef HAVE_DLFCN_H

/* Define to 1 if you have the <getopt.h> header file. */
#undef HAVE_GETOPT_H

/* Define to 1 if you have the `getopt_long_only' function. */
#undef HAVE_GETOPT_LONG_ONLY

/* Define to 1 if you have the `m' library (-lm). */
#undef HAVE_LIBM

/* Have MAT int16 */
#define HAVE_MAT_INT16_T 1

/* Have MAT int32 */
#define HAVE_MAT_INT32_T 1

/* Have MAT int64 */
#define HAVE_MAT_INT64_T 1

/* Have MAT int8 */
#define HAVE_MAT_INT8_T 1

/* Have MAT int16 */
#define HAVE_MAT_UINT16_T 1

/* Have MAT int32 */
#define HAVE_MAT_UINT32_T 1

/* Have MAT int64 */
#define HAVE_MAT_UINT64_T 1

/* Have MAT int8 */
#define HAVE_MAT_UINT8_T 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Have snprintf */
#undef HAVE_SNPRINTF

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#undef HAVE_STRINGS_H

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#undef HAVE_SYS_STAT_H

/* Define to 1 if you have the <sys/types.h> header file. */
#undef HAVE_SYS_TYPES_H

/* Define to 1 if you have the <unistd.h> header file. */
#undef HAVE_UNISTD_H

/* Have vasprintf */
#undef HAVE_VASPRINTF

/* Have va_copy */
#undef HAVE_VA_COPY

/* Have vsnprintf */
#undef HAVE_VSNPRINTF

/* Have va_copy */
#undef HAVE___VA_COPY

/* OS is Linux */
#undef LINUX

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#undef LT_OBJDIR

/* Platform */
#if defined(_WIN32)
#   define MATIO_PLATFORM "i686-pc-windows"
#elif defined(_WIN64)
#   define MATIO_PLATFORM "x86_64-pc-windows"
#endif

/* Debug disabled */
#undef NODEBUG

/* Name of package */
#define PACKAGE "matio"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "chulbe2lsu@users.sourceforge.net"

/* Define to the full name of this package. */
#define PACKAGE_NAME "MATIO"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "MATIO 1.5.2"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "matio"

/* Define to the home page for this package. */
#define PACKAGE_URL "http://sourceforge.net/projects/matio"

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.5.2"

/* The size of `char', as computed by sizeof. */
#define SIZEOF_CHAR 1

/* The size of `double', as computed by sizeof. */
#define SIZEOF_DOUBLE 8

/* The size of `float', as computed by sizeof. */
#define SIZEOF_FLOAT 4

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 4

/* The size of `long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG 8

/* The size of `short', as computed by sizeof. */
#define SIZEOF_SHORT 2

#if defined(_WIN32)
    /* The size of `void *', as computed by sizeof. */
#   define SIZEOF_VOID_P 4
    /* The size of `size_t', as computed by sizeof. */
#    define SIZEOF_SIZE_T 4
#elif defined(_WIN64)
    /* The size of `void *', as computed by sizeof. */
#   define SIZEOF_VOID_P 8
    /* The size of `size_t', as computed by sizeof. */
#    define SIZEOF_SIZE_T 8
#endif

/* Define to 1 if you have the ANSI C header files. */
#undef STDC_HEADERS

/* Define to 1 if you have the ctype.h header file. */
#define HAVE_CTYPE_H 1

/* Version number of package */
#define VERSION "1.5.1"

/* Z prefix */
#undef Z_PREFIX
