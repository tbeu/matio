dnl  configure.ac
dnl
dnl  NOTE:  Process this file with GNU autoconf to produce a configure script.
dnl
dnl Copyright (C) 2005-2010   Christopher C. Hulbert
dnl
dnl  This library is free software; you can redistribute it and/or
dnl  modify it under the terms of the GNU Lesser General Public
dnl  License as published by the Free Software Foundation; either
dnl  version 2.1 of the License, or (at your option) any later version.
dnl
dnl  This library is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl  Lesser General Public License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public
dnl  License along with this library; if not, write to the Free Software
dnl  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

AC_INIT([MATIO],[1.5.0],[chulbe1@mac.com],[])
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
AC_DEFINE_UNQUOTED([MATIO_RELEASE_LEVEL],0,[Matio release level number])
PACKAGE_RELEASE_DATE="3 January 2008"
AC_SUBST(PACKAGE_RELEASE_DATE)

gl_INIT()

AC_ARG_VAR([FCLDFLAGS],[Fortran compiler-specific flags at link time])
AC_ARG_VAR([LT_CFLAGS],[C compiler flags passed to libtool in compile mode])
AC_ARG_VAR([LT_LDFLAGS],[Flags passed to libtool in link mode])

# Enable fortran interface
AC_ARG_ENABLE(fortran,
[  --enable-fortran            enable fortran interface to mat library],
  [if test "$enableval" = "no" ; then
    enable_fortran=no
  else
    enable_fortran=yes
  fi],
  enable_fortran=no)

dnl 
dnl  Build programs, C compiler, F77 compiler, make, install, etc.
dnl
AC_PROG_CC([pgcc icc gcc cc])

gl_EARLY()

if test "$enable_fortran" = "yes"
then
    AC_PROG_FC([pgf95 pgf90 ifort gfortran g95])
    AC_FC_WRAPPERS
    AC_CONFIG_FILES([src/fortran/matio_t.inc])
fi

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

dnl
dnl Try to avoid having libtool search for a fortran compiler
dnl
F77=$FC
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
AC_CHECK_HEADER([inttypes.h],[AC_DEFINE_UNQUOTED([MATIO_HAVE_INTTYPES_H],[1])])
AC_CHECK_HEADER([stdint.h],[AC_DEFINE_UNQUOTED([MATIO_HAVE_STDINT_H],[1])])
AC_CHECK_HEADER([strings.h],[AC_DEFINE_UNQUOTED([HAVE_STRINGS_H])])
AC_CHECK_HEADER([unistd.h],[AC_DEFINE_UNQUOTED([HAVE_UNISTD_H])])

MATIO_CHECK_GETOPT_LONG
gl_GETOPT()

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

AC_CACHE_CHECK([for va_copy()],matio_cv_have_va_copy,[
	AC_LINK_IFELSE([#include <stdarg.h>
	void f (int i, ...) {
	va_list args1, args2;
	va_start (args1, i);
	va_copy (args2, args1);
	if (va_arg (args2, int) != 42 || va_arg (args1, int) != 42)
	  exit (1);
	va_end (args1); va_end (args2);
	}
	int main() {
	  f (0, 42);
	  return 0;
	}],
	[matio_cv_have_va_copy=yes],
	[matio_cv_have_va_copy=no])
])
if test "$matio_cv_have_va_copy" = "no"
then
    AC_CACHE_CHECK([for __va_copy()],matio_cv_have___va_copy,[
        AC_LINK_IFELSE([#include <stdarg.h>
            void f (int i, ...) {
                va_list args1, args2;
                va_start (args1, i);
                __va_copy (args2, args1);
                if (va_arg (args2, int) != 42 || va_arg (args1, int) != 42)
                    exit (1);
                va_end (args1); va_end (args2);
            }
            int main() {
                f (0, 42);
                return 0;
            }],
	[matio_cv_have___va_copy=yes],
	[matio_cv_have___va_copy=no])
    ])
    if test "matio_cv_have___va_copy" = "yes"
    then
        AC_DEFINE_UNQUOTED([HAVE___VA_COPY],[],[Have va_copy])
    fi
else
    AC_DEFINE_UNQUOTED([HAVE_VA_COPY],[],[Have va_copy])
fi

AC_CACHE_CHECK([for vsnprintf()],matio_cv_have_vsnprintf,[
        AC_LINK_IFELSE([
        #include <stdlib.h>
        #include <stdarg.h>
        #include <stdio.h>
        int main() {
          va_list ap;
          vsnprintf(NULL,0,"%d",ap);
          return 0;
        }],
        [matio_cv_have_vsnprintf=yes],
        [matio_cv_have_vsnprintf=no])
])

AC_CACHE_CHECK([for snprintf()],matio_cv_have_snprintf,[
        AC_LINK_IFELSE([
        #include <stdlib.h>
        #include <stdarg.h>
        #include <stdio.h>
        int main() {
          snprintf(NULL,0,"%d",0);
          return 0;
        }],
        [matio_cv_have_snprintf=yes],
        [matio_cv_have_snprintf=no])
])

AC_CACHE_CHECK([for vasprintf()],matio_cv_have_vasprintf,[
        AC_LINK_IFELSE([
        #include <stdlib.h>
        #include <stdarg.h>
        #include <stdio.h>
        int main() {
          va_list ap;
          vasprintf(NULL,"%d",ap);
          return 0;
        }],
        [matio_cv_have_vasprintf=yes],
        [matio_cv_have_vasprintf=no])
])

AC_CACHE_CHECK([for asprintf()],matio_cv_have_asprintf,[
        AC_LINK_IFELSE([
        #include <stdlib.h>
        #include <stdarg.h>
        #include <stdio.h>
        int main() {
          asprintf(NULL,0,"%d",0);
          return 0;
        }],
        [matio_cv_have_asprintf=yes],
        [matio_cv_have_asprintf=no])
])

if test "$matio_cv_have_vsnprintf" = "yes"
then
    AC_DEFINE_UNQUOTED([HAVE_VSNPRINTF],[],[Have vsnprintf])
fi
if test "$matio_cv_have_snprintf" = "yes"
then
    AC_DEFINE_UNQUOTED([HAVE_SNPRINTF],[],[Have snprintf])
fi
if test "$matio_cv_have_vasprintf" = "yes"
then
    AC_DEFINE_UNQUOTED([HAVE_VASPRINTF],[],[Have vasprintf])
fi
if test "$matio_cv_have_asprintf" = "yes"
then
    AC_DEFINE_UNQUOTED([HAVE_ASPRINTF],[],[Have asprintf])
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
AC_MSG_CHECKING(enable enable MAT v7.3 file support)
if test "x$mat73" = "xno"
then
    AC_MSG_RESULT([no])
    mat73=no
elif test "x$ac_have_hdf5" = "xno"
then
    AC_MSG_RESULT([no])
    AC_DEFINE_UNQUOTED([MAT73],[0],[MAT v7.3 file support])
    mat73=no
else
    AC_MSG_RESULT([yes])
    AC_DEFINE_UNQUOTED([MAT73],[1],[MAT v7.3 file support])
    mat73=yes
fi
AM_CONDITIONAL(MAT73, test "x$mat73" = "xyes")

MATIO_CHECK_DEFAULT_FILE_VERSION

dnl
dnl Check whether to build in debug mode
dnl
AC_ARG_ENABLE(extended-sparse,AS_HELP_STRING([--enable-extended-sparse=yes],[Enable extended sparse matrix data types not supported in Matlab]),
extended_sparse=$enableval,extended_sparse=yes)
AC_MSG_CHECKING(enable extended sparse types)
if test "$extended_sparse" = "no"
then
    AC_MSG_RESULT(no)
else
    AC_MSG_RESULT(yes)
    AC_DEFINE_UNQUOTED([EXTENDED_SPARSE],[],[Extended sparse matrix data types])
fi

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

dnl
dnl Check for doxygen program
dnl
AC_PATH_PROG(DOXYGEN,[doxygen])
AC_SUBST([DOXYGEN])
if test -n "$ac_cv_path_DOXYGEN"
then
    docs_install_default=yes
else
    docs_install_default=no
fi

dnl
dnl Check whether to build documentation
dnl
AC_ARG_ENABLE(docs,AS_HELP_STRING([--enable-docs=yes],[install documentation]),
docs_install=$enableval,docs_install=$docs_install_default)
AC_MSG_CHECKING(install docs)
if test "x$docs_install" = "xno"
then
    AC_MSG_RESULT(no)
elif test -z "$ac_cv_path_DOXYGEN"
then
    AC_MSG_RESULT([no (doxygen program not found)])
    docs_install=no
else
    AC_MSG_RESULT(yes)
    docs_install=yes
fi  
AM_CONDITIONAL(DOCS_INSTALL, test "x$docs_install" = "xyes")

AM_CONDITIONAL(ENABLE_FORTRAN, test "$enable_fortran" = "yes" )

dnl
dnl Without this, the linker line is incorrect on platforms without a fortran
dnl compiler even if we are not using fortran sources
dnl
if test "x$enable_fortran" != "xyes"; then
  FCLINK='$(LINK)'
else
  FCLINK='$(LIBTOOL) $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=link $(FCLD) $(AM_FCFLAGS) $(FCFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@'
fi
AC_SUBST(FCLINK)

# Initialize the test suite.
AC_CONFIG_TESTDIR([test])
AC_SUBST([AUTOM4TE], [${AUTOM4TE=autom4te}])
AC_SUBST([AUTOTEST], ['$(AUTOM4TE) --language=Autotest'])

AC_CONFIG_FILES([Makefile \
                 matio.pc \
                 src/Makefile \
                 getopt/lib/Makefile \
                 tools/Makefile \
                 test/Makefile \
                 doxygen/Makefile \
                 doxygen/user_api/Makefile
                 doxygen/dev_api/Makefile])
AC_OUTPUT

AC_MSG_RESULT([])
AC_MSG_RESULT([   MATIO Configuration Summary       ])
AC_MSG_RESULT([==============================================================])
AC_MSG_RESULT([           C Compiler: $CC])
AC_MSG_RESULT([     Fortran Compiler: $FC])
AC_MSG_RESULT([               CFLAGS: $CFLAGS])
AC_MSG_RESULT([              FCFLAGS: $FCFLAGS])
AC_MSG_RESULT([            FCLDFLAGS: $FCLDFLAGS])
AC_MSG_RESULT([     Shared Libraries: $enable_shared])
AC_MSG_RESULT([     Static Libraries: $enable_static])
AC_MSG_RESULT([  default MAT version: $file_ver])
AC_MSG_RESULT([])
AC_MSG_RESULT([Features --------------------------------------------])
AC_MSG_RESULT([    Fortran Interface: $enable_fortran])
AC_MSG_RESULT([MAT v7.3 file support: $mat73])
AC_MSG_RESULT([        Documentation: $docs_install])
AC_MSG_RESULT([])
AC_MSG_RESULT([Packages --------------------------------------------])
AC_MSG_RESULT([                 zlib: $ZLIB_LIBS])
AC_MSG_RESULT([                 hdf5: $HDF5_LIBS])
AC_MSG_RESULT([               MATLAB: $MATLABEXE])
AC_MSG_RESULT([])
AC_MSG_RESULT([])
