dnl  conFigure.ac
dnl
dnl  NOTE:  Process this file with GNU autoconf to produce a configure script.

AC_INIT([MATIO],[1.3.2],[cch@isl-inc.com],[])
AC_CONFIG_SRCDIR([src/matio.h])
AC_CONFIG_AUX_DIR(config)
dnl AM_INIT_AUTOMAKE([-no-undefined])
AM_INIT_AUTOMAKE
AM_MAINTAINER_MODE
AC_PREREQ(2.59)
AC_CANONICAL_HOST
AC_DEFINE_UNQUOTED(MATIO_PLATFORM,"${host}")
AC_DEFINE_UNQUOTED(MATIO_MAJOR_VERSION,1)
AC_DEFINE_UNQUOTED(MATIO_MINOR_VERSION,3)
AC_DEFINE_UNQUOTED(MATIO_RELEASE_LEVEL,2)
PACKAGE_RELEASE_DATE="28 October 2006"
AC_SUBST(PACKAGE_RELEASE_DATE)

AC_ARG_VAR([FCLDFLAGS],[Fortran compiler-specific flags at link time])

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

case $CC in
    *pgcc*)
        . $srcdir/config/pgi-cflags
        ;;
    *icc*)
        . $srcdir/config/intel-cflags
        ;;
    *gcc*)
        . $srcdir/config/gnu-cflags
        ;;
    *cl | *cl.exe)
        . $srcdir/config/ms-cflags
        ;;
esac

if test "$enable_fortran" = "yes"
then
    AC_PROG_FC([pgf95 pgf90 ifort gfortran g95])

    case $FC in
        *pgf9*)
            . $srcdir/config/pgi-fflags
            ;;
        *ifort*)
            . $srcdir/config/intel-fflags
            ;;
        *gfortran* | *g95*)
            . $srcdir/config/gnu-fflags
            ;;
    esac

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
    AC_DEFINE_UNQUOTED(NODEBUG)
else
    AC_MSG_RESULT(yes)
    AC_DEFINE_UNQUOTED(DEBUG)
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
    AC_DEFINE_UNQUOTED(NODEBUG)
else
    AC_MSG_RESULT(yes)
    AC_DEFINE_UNQUOTED(DEBUG)
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
AC_DISABLE_SHARED
AC_ENABLE_STATIC
AC_PROG_LIBTOOL

AC_PROG_MAKE_SET
AC_PROG_INSTALL

dnl 
dnl  Libraries
dnl

AC_CHECK_LIB([m],[pow])

dnl
dnl  Headers
dnl

AC_CONFIG_HEADERS([src/matioConfig.h])
AC_STDC_HEADERS
AC_CHECK_HEADER([inttypes.h],[AC_DEFINE_UNQUOTED([HAVE_INTTYPES_H])])
AC_CHECK_HEADER([stdint.h],[AC_DEFINE_UNQUOTED([HAVE_STDINT_H])])
AC_CHECK_HEADER([strings.h],[AC_DEFINE_UNQUOTED([HAVE_STRINGS_H])])
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(void *)

dnl
dnl Substitute size of int and void * to matio_t.inc
SIZEOF_INT="$ac_cv_sizeof_int"
SIZEOF_VOID_P="$ac_cv_sizeof_void_p"
AC_SUBST(SIZEOF_INT)
AC_SUBST(SIZEOF_VOID_P)

CHECK_MATIO_INT64_T
CHECK_MATIO_UINT64_T
CHECK_MATIO_INT32_T
CHECK_MATIO_UINT32_T
CHECK_MATIO_INT16_T
CHECK_MATIO_UINT16_T
CHECK_MATIO_INT8_T
CHECK_MATIO_UINT8_T

AC_CACHE_CHECK([for va_copy()],have_va_copy,[
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
	[have_va_copy=yes],
	[have_va_copy=no])
])
if test "$have_va_copy" = "no"
then
    AC_CACHE_CHECK([for __va_copy()],have___va_copy,[
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
	[have___va_copy=yes],
	[have___va_copy=no])
    ])
    if test "have___va_copy" = "yes"
    then
        AC_DEFINE_UNQUOTED([HAVE___VA_COPY])
    fi
else
    AC_DEFINE_UNQUOTED([HAVE_VA_COPY])
fi

AC_CACHE_CHECK([for vsnprintf()],have_vsnprintf,[
        AC_LINK_IFELSE([
        #include <stdlib.h>
        #include <stdarg.h>
        #include <stdio.h>
        int main() {
          va_list ap;
          vsnprintf(NULL,0,"%d",ap);
          return 0;
        }],
        [have_vsnprintf=yes],
        [have_vsnprintf=no])
])

AC_CACHE_CHECK([for snprintf()],have_snprintf,[
        AC_LINK_IFELSE([
        #include <stdlib.h>
        #include <stdarg.h>
        #include <stdio.h>
        int main() {
          snprintf(NULL,0,"%d",0);
          return 0;
        }],
        [have_snprintf=yes],
        [have_snprintf=no])
])

AC_CACHE_CHECK([for vasprintf()],have_vasprintf,[
        AC_LINK_IFELSE([
        #include <stdlib.h>
        #include <stdarg.h>
        #include <stdio.h>
        int main() {
          va_list ap;
          vasprintf(NULL,"%d",ap);
          return 0;
        }],
        [have_vasprintf=yes],
        [have_vasprintf=no])
])

AC_CACHE_CHECK([for asprintf()],have_asprintf,[
        AC_LINK_IFELSE([
        #include <stdlib.h>
        #include <stdarg.h>
        #include <stdio.h>
        int main() {
          asprintf(NULL,0,"%d",0);
          return 0;
        }],
        [have_asprintf=yes],
        [have_asprintf=no])
])

if test "$have_vsnprintf" = "yes"
then
    AC_DEFINE_UNQUOTED([HAVE_VSNPRINTF])
fi
if test "$have_snprintf" = "yes"
then
    AC_DEFINE_UNQUOTED([HAVE_SNPRINTF])
fi
if test "$have_vasprintf" = "yes"
then
    AC_DEFINE_UNQUOTED([HAVE_VASPRINTF])
fi
if test "$have_asprintf" = "yes"
then
    AC_DEFINE_UNQUOTED([HAVE_ASPRINTF])
fi

MATIO_CHECK_ZLIB

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
    AC_DEFINE_UNQUOTED(EXTENDED_SPARSE)
fi

linux="no"
winnt="no"
sun="no"
case $build_os in
*linux*)
    linux="yes"
    AC_DEFINE_UNQUOTED(LINUX);;
*mingw*)
    winnt="yes"
    AC_DEFINE_UNQUOTED(WINNT);;
*cygwin*)
    winnt="yes"
    AC_DEFINE_UNQUOTED(WINNT);;
*sun*)
    sun="yes"
    AC_DEFINE_UNQUOTED(SUN);;
*solaris*)
    sun="yes"
    AC_DEFINE_UNQUOTED(SUN);;
esac

AM_CONDITIONAL(LINUX, test "x$linux" = "xyes")
AM_CONDITIONAL(WINNT, test "x$winnt" = "xyes")
AM_CONDITIONAL(SUN,   test "x$sun"   = "xyes")

dnl
dnl Check whether to build test programs
dnl
AC_ARG_ENABLE(test,AS_HELP_STRING([--enable-test=yes],[build test programs]),
build_test=$enableval,build_test=no)
AC_MSG_CHECKING(whether to build test programs)
if test "x$build_test" = "xno"
then
    AC_MSG_RESULT(no)
else
    AC_MSG_RESULT(yes)
    build_test=yes
fi  
AM_CONDITIONAL(BUILD_TEST, test "x$build_test" = "xyes")

dnl
dnl Check whether to build documentation
dnl
AC_ARG_ENABLE(docs,AS_HELP_STRING([--enable-docs=yes],[install documentation]),
docs_install=$enableval,docs_install=no)
AC_MSG_CHECKING(install docs)
if test "$docs_install" = "no"
then
    AC_MSG_RESULT(no)
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
  FCLINK='$(LIBTOOL) --mode=link $(FCLD) $(AM_FCFLAGS) $(FCFLAGS) $(AM_LDFLAGS) $(LDFLAGS) -o $@'
fi
AC_SUBST(FCLINK)

AC_CONFIG_FILES([Makefile \
                 matio.pc \
                 src/Makefile \
                 test/Makefile \
                 doxygen/Makefile])
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
AC_MSG_RESULT([])
AC_MSG_RESULT([Features --------------------------------------------])
AC_MSG_RESULT([    Fortran Interface: $enable_fortran])
AC_MSG_RESULT([        test programs: $build_test])
AC_MSG_RESULT([        Documentation: $docs_install])
AC_MSG_RESULT([])
AC_MSG_RESULT([Packages --------------------------------------------])
AC_MSG_RESULT([                 zlib: $ZLIB_LIBS])
AC_MSG_RESULT([])
AC_MSG_RESULT([])
