dnl  configure.ac
dnl
dnl  NOTE:  Process this file with GNU autoconf to produce a configure script.
dnl
dnl Copyright (C) 2005-2016   Christopher C. Hulbert
dnl
dnl All rights reserved.
dnl
dnl Redistribution and use in source and binary forms, with or without
dnl modification, are permitted provided that the following conditions are met:
dnl
dnl    1. Redistributions of source code must retain the above copyright notice,
dnl       this list of conditions and the following disclaimer.
dnl
dnl    2. Redistributions in binary form must reproduce the above copyright
dnl       notice, this list of conditions and the following disclaimer in the
dnl       documentation and/or other materials provided with the distribution.
dnl
dnl THIS SOFTWARE IS PROVIDED BY CHRISTOPHER C. HULBERT ``AS IS'' AND ANY
dnl EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
dnl WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
dnl DISCLAIMED. IN NO EVENT SHALL CHRISTOPHER C. HULBERT OR CONTRIBUTORS BE
dnl LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
dnl CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
dnl SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
dnl INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
dnl CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
dnl ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
dnl POSSIBILITY OF SUCH DAMAGE.

AC_INIT([MATIO],[1.5.9],[chulbe2lsu@users.sourceforge.net],[],
        [http://sourceforge.net/projects/matio])
AC_CONFIG_SRCDIR([src/matio.h])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_MACRO_DIR([config])
AM_INIT_AUTOMAKE([foreign 1.8 no-dependencies])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES])
AM_MAINTAINER_MODE
AC_PREREQ(2.60)
AC_CANONICAL_HOST
AC_DEFINE_UNQUOTED([MATIO_PLATFORM],"${host}",[Platform])
AC_DEFINE_UNQUOTED([MATIO_MAJOR_VERSION],1,[Matio major version number])
AC_DEFINE_UNQUOTED([MATIO_MINOR_VERSION],5,[Matio minor version number])
AC_DEFINE_UNQUOTED([MATIO_RELEASE_LEVEL],9,[Matio release level number])
AC_DEFINE_UNQUOTED([MATIO_VERSION],159,[Matio version number])
PACKAGE_RELEASE_DATE="22 October 2016"
AC_DEFINE_UNQUOTED([MATIO_VERSION_STR],"1.5.9",[Matio version number])
AC_SUBST(PACKAGE_RELEASE_DATE)

AC_ARG_VAR([FCLDFLAGS],[Fortran compiler-specific flags at link time])
AC_ARG_VAR([LT_CFLAGS],[C compiler flags passed to libtool in compile mode])
AC_ARG_VAR([LT_LDFLAGS],[Flags passed to libtool in link mode])

dnl 
dnl  Build programs, C compiler, F77 compiler, make, install, etc.
dnl
AC_PROG_CC([pgcc icc gcc cc])

AC_ARG_ENABLE(coverage,
[  --enable-coverage       Enable coverage testing],
[CFLAGS="$CFLAGS -fprofile-arcs -ftest-coverage"])

dnl
dnl Check whether to build in debug mode
dnl
AC_ARG_ENABLE(debug,AS_HELP_STRING([--enable-debug=yes],[Turn on debugging]),
debug=$enableval,debug=no)
AC_MSG_CHECKING(debug mode)
if test "$debug" = "no"
then
    AC_MSG_RESULT(no)
    AC_DEFINE_UNQUOTED([NODEBUG],[],[Debug disabled])
else
    AC_MSG_RESULT(yes)
    AC_DEFINE_UNQUOTED([DEBUG],[],[Debug enabled])
    debug=yes
fi

dnl
dnl Check whether to build in profile mode
dnl
AC_ARG_ENABLE(profile,AS_HELP_STRING([--enable-profile=yes],[Turn on profile]),
profile=$enableval,profile=no)
AC_MSG_CHECKING(profile mode)
if test "$profile" = "no"
then
    AC_MSG_RESULT(no)
    AC_DEFINE_UNQUOTED([NODEBUG],[],[Debug disabled])
else
    AC_MSG_RESULT(yes)
    AC_DEFINE_UNQUOTED([DEBUG],[],[Debug enabled])
    profile=yes
fi

dnl
dnl Check for user-defined suffix to the library/include directories for
dnl installation and search. i.e. --with-libdir-suffix=64 looks in lib64 and
dnl --with-libdir-suffix=/x86_64 would look in lib/x86_64
dnl
MATIO_CHECK_LIBDIR_SUFFIX

dnl
dnl Set library directories to include libdir suffix
dnl
acl_libdirstem="lib$with_libdir_suffix"

dnl
dnl If debugging is enabled, add debugging options to the flags
dnl
if test "$debug" = "yes"
then
    CFLAGS="$CFLAGS $DEBUG_CFLAGS"
    FCFLAGS="$FCFLAGS $DEBUG_FCFLAGS"
else
    CFLAGS="$REL_CFLAGS $CFLAGS"
    FCFLAGS="$REL_FCFLAGS $FCFLAGS"
fi

dnl
dnl If profiling is enabled, add profile options to the flags
dnl
if test "$profile" = "yes"
then
    CFLAGS="$PROFILE_CFLAGS $CFLAGS"
    FCFLAGS="$PROFILE_FCFLAGS $FCFLAGS"
fi

AC_ENABLE_SHARED
AC_ENABLE_STATIC
AC_PROG_LIBTOOL

AC_PROG_MAKE_SET
AC_PROG_INSTALL

AC_PROG_GREP

dnl 
dnl  Libraries
dnl

AC_CHECK_LIB([m],[pow])

dnl
dnl  Headers
dnl

AC_CONFIG_HEADERS([src/matioConfig.h src/matio_pubconf.h])
AC_STDC_HEADERS
AC_CHECK_HEADERS_ONCE([stdarg.h])
AC_CHECK_HEADERS_ONCE([varargs.h])
AC_CHECK_HEADER([inttypes.h],[AC_DEFINE_UNQUOTED([MATIO_HAVE_INTTYPES_H],[1],[Matio has access to inttypes.h])])
AC_CHECK_HEADER([stdint.h],[AC_DEFINE_UNQUOTED([MATIO_HAVE_STDINT_H],[1],[Matio has access to stdint.h])])
AC_CHECK_HEADER([strings.h],[AC_DEFINE_UNQUOTED([HAVE_STRINGS_H],[1],[Matio has access to strings.h])])
AC_CHECK_HEADER([unistd.h],[AC_DEFINE_UNQUOTED([HAVE_UNISTD_H],[1],[Matio has access to unistd.h])])
AC_CHECK_HEADERS([locale.h stddef.h])
AC_CHECK_MEMBERS([struct lconv.decimal_point, struct lconv.thousands_sep], [], [], [#include <locale.h>])

AC_CHECK_TYPES([ptrdiff_t])
AC_CHECK_FUNCS([localeconv])

MATIO_CHECK_GETOPT_LONG

AC_CHECK_SIZEOF([double])
AC_CHECK_SIZEOF([float])
AC_CHECK_SIZEOF([long long])
AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([short])
AC_CHECK_SIZEOF([char])
AC_CHECK_SIZEOF([void *])
AC_CHECK_SIZEOF([size_t])

dnl
dnl Substitute size of int, size_t, and void * to matio_t.inc
SIZEOF_INT="$ac_cv_sizeof_int"
SIZEOF_SIZE_T="$ac_cv_sizeof_size_t"
SIZEOF_VOID_P="$ac_cv_sizeof_void_p"
AC_SUBST(SIZEOF_INT)
AC_SUBST(SIZEOF_SIZE_T)
AC_SUBST(SIZEOF_VOID_P)

CHECK_MATIO_INT64_T
CHECK_MATIO_UINT64_T
CHECK_MATIO_INT32_T
CHECK_MATIO_UINT32_T
CHECK_MATIO_INT16_T
CHECK_MATIO_UINT16_T
CHECK_MATIO_INT8_T
CHECK_MATIO_UINT8_T

if test "x$ac_have_mat_uint64_t" = "xyes"; then
    HAVE_UINT64=1
else
    HAVE_UINT64=0
fi
if test "x$ac_have_mat_int64_t" = "xyes"; then
    HAVE_INT64=1
else
    HAVE_INT64=0
fi
AC_SUBST([HAVE_UINT64],[$HAVE_UINT64])
AC_SUBST([HAVE_INT64], [$HAVE_INT64])

AC_TYPE_LONG_DOUBLE
AC_TYPE_LONG_LONG_INT
AC_TYPE_UNSIGNED_LONG_LONG_INT
AC_TYPE_SIZE_T
AC_TYPE_INTMAX_T
AC_TYPE_UINTMAX_T
AC_TYPE_UINTPTR_T
AC_CHECK_VA_COPY

AC_CACHE_CHECK([for vsnprintf()],matio_cv_have_vsnprintf,[
        AC_LINK_IFELSE([AC_LANG_SOURCE([[
        #include <stdlib.h>
        #include <stdarg.h>
        #include <stdio.h>
        int main() {
          va_list ap;
          vsnprintf(NULL,0,"%d",ap);
          return 0;
        }]])],
        [matio_cv_have_vsnprintf=yes],
        [matio_cv_have_vsnprintf=no])
])

AC_CACHE_CHECK([for snprintf()],matio_cv_have_snprintf,[
        AC_LINK_IFELSE([AC_LANG_SOURCE([[
        #include <stdlib.h>
        #include <stdarg.h>
        #include <stdio.h>
        int main() {
          snprintf(NULL,0,"%d",0);
          return 0;
        }]])],
        [matio_cv_have_snprintf=yes],
        [matio_cv_have_snprintf=no])
])

AC_CACHE_CHECK([for vasprintf()],matio_cv_have_vasprintf,[
        AC_LINK_IFELSE([AC_LANG_SOURCE([[
        #include <stdlib.h>
        #include <stdarg.h>
        #include <stdio.h>
        int main() {
          va_list ap;
          vasprintf(NULL,"%d",ap);
          return 0;
        }]])],
        [matio_cv_have_vasprintf=yes],
        [matio_cv_have_vasprintf=no])
])

AC_CACHE_CHECK([for asprintf()],matio_cv_have_asprintf,[
        AC_LINK_IFELSE([AC_LANG_SOURCE([[
        #include <stdlib.h>
        #include <stdarg.h>
        #include <stdio.h>
        int main() {
          asprintf(NULL,0,"%d",0);
          return 0;
        }]])],
        [matio_cv_have_asprintf=yes],
        [matio_cv_have_asprintf=no])
])

if test "$matio_cv_have_vsnprintf" = "yes"
then
    AC_DEFINE_UNQUOTED([HAVE_VSNPRINTF],[1],[Define to 1 if you have a C99 compliant `vsnprintf' function.])
fi
if test "$matio_cv_have_snprintf" = "yes"
then
    AC_DEFINE_UNQUOTED([HAVE_SNPRINTF],[1],[Define to 1 if you have a C99 compliant `snprintf' function.])
fi
if test "$matio_cv_have_vasprintf" = "yes"
then
    AC_DEFINE_UNQUOTED([HAVE_VASPRINTF],[1],[Define to 1 if you have the `vasprintf' function.])
fi
if test "$matio_cv_have_asprintf" = "yes"
then
    AC_DEFINE_UNQUOTED([HAVE_ASPRINTF],[1],[Define to 1 if you have the `asprintf' function.])
fi

MATIO_CHECK_MATLAB

MATIO_CHECK_ZLIB

MATIO_CHECK_HDF5
dnl
dnl Check whether to enable MAT v7.3 files
dnl
AC_ARG_ENABLE(mat73,
    AS_HELP_STRING([--enable-mat73=yes],[Enable MAT v7.3 file support]),
mat73=$enableval,mat73=yes)
AC_MSG_CHECKING(enable MAT v7.3 file support)
if test "x$mat73" = "xno" -o "x$ac_have_hdf5" = "xno" -o "x$matio_hdf5_is_v18" = "xno"
then
    AC_MSG_RESULT([no])
    mat73=no
else
    AC_MSG_RESULT([yes])
    AC_DEFINE_UNQUOTED([MAT73],[1],[MAT v7.3 file support])
    mat73=yes
fi
AM_CONDITIONAL(MAT73, test "x$mat73" = "xyes")

MATIO_CHECK_DEFAULT_FILE_VERSION

dnl
dnl Check whether to enable extended sparse matrix data types
dnl
AC_ARG_ENABLE(extended-sparse,AS_HELP_STRING([--enable-extended-sparse=yes],[Enable extended sparse matrix data types not supported in Matlab]),
extended_sparse=$enableval,extended_sparse=yes)
AC_MSG_CHECKING(enable extended sparse types)
if test "$extended_sparse" = "no"
then
    extended_sparse=no
    AC_MSG_RESULT(no)
else
    extended_sparse=yes
    AC_MSG_RESULT(yes)
    AC_DEFINE_UNQUOTED([EXTENDED_SPARSE],[],[Extended sparse matrix data types])
fi
AM_CONDITIONAL(EXTENDED_SPARSE, test "x$extended_sparse" = "xyes")

linux="no"
winnt="no"
sun="no"
case $build_os in
*linux*)
    linux="yes"
    AC_DEFINE_UNQUOTED([LINUX],[],[OS is Linux]);;
*mingw*)
    winnt="yes"
    AC_DEFINE_UNQUOTED([WINNT],[],[OS is WinNT]);;
*cygwin*)
    winnt="yes"
    AC_DEFINE_UNQUOTED([WINNT],[],[OS is CygWin]);;
*sun*)
    sun="yes"
    AC_DEFINE_UNQUOTED([SUN],[],[OS is Sun]);;
*solaris*)
    sun="yes"
    AC_DEFINE_UNQUOTED([SUN],[],[OS is Solaris]);;
esac

AM_CONDITIONAL(LINUX, test "x$linux" = "xyes")
AM_CONDITIONAL(WINNT, test "x$winnt" = "xyes")
AM_CONDITIONAL(SUN,   test "x$sun"   = "xyes")

# Initialize the test suite.
AC_CONFIG_TESTDIR([test])
AC_SUBST([AUTOM4TE], [${AUTOM4TE=autom4te}])
AC_SUBST([AUTOTEST], ['$(AUTOM4TE) --language=Autotest'])

AC_CONFIG_FILES([Makefile \
                 matio.pc \
                 src/Makefile \
                 getopt/Makefile \
                 tools/Makefile \
                 test/Makefile \
                 documentation/Makefile])
AC_OUTPUT

AC_MSG_RESULT([])
AC_MSG_RESULT([   MATIO Configuration Summary       ])
AC_MSG_RESULT([==============================================================])
AC_MSG_RESULT([           C Compiler: $CC])
AC_MSG_RESULT([               CFLAGS: $CFLAGS])
AC_MSG_RESULT([     Shared Libraries: $enable_shared])
AC_MSG_RESULT([     Static Libraries: $enable_static])
AC_MSG_RESULT([  default MAT version: $file_ver])
AC_MSG_RESULT([])
AC_MSG_RESULT([Features --------------------------------------------])
AC_MSG_RESULT([  MAT v7.3 file support: $mat73])
AC_MSG_RESULT([Extended sparse support: $extended_sparse])
AC_MSG_RESULT([])
AC_MSG_RESULT([Packages --------------------------------------------])
AC_MSG_RESULT([                 zlib: $ZLIB_LIBS])
AC_MSG_RESULT([                 hdf5: $HDF5_LIBS])
AC_MSG_RESULT([               MATLAB: $MATLABEXE])
AC_MSG_RESULT([])
AC_MSG_RESULT([])
