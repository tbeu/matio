AC_DEFUN([MATIO_CHECK_ZLIB],
[
AC_ARG_WITH(zlib,AS_HELP_STRING([--with-zlib=DIR],
            [Prefix where zlib Library is installed]))
if test "x$with_zlib" != "xno"
then
    saved_LIBS="$LIBS"
    saved_CFLAGS="$CFLAGS"

    AC_MSG_CHECKING([for zlib Library])

    if test "x$with_zlib" = "x" -o "x$with_zlib" = "xyes"
    then
        # Try system zlib
        ZLIB_LDOPTS="-lz"
        ZLIB_CFLAGS=""
    else
        ZLIB_LDOPTS="-L$with_zlib/$acl_libdirstem -lz"
        ZLIB_CFLAGS="-I$with_zlib/include"
    fi
    LIBS="$saved_LIBS $ZLIB_LDOPTS"
    CFLAGS="$saved_CFLAGS $ZLIB_CFLAGS"

    Z_PREFIX=0
    AC_TRY_LINK( [
#include <stdlib.h>
#include <zlib.h>
                  ],
[inflateCopy(NULL,NULL);], ac_have_zlib=yes, ac_have_zlib=no)

    if test "$ac_have_zlib" = "no"
    then
        # Try again with Z_PREFIX
        AC_TRY_LINK( [
            #include <stdlib.h>
            #define Z_PREFIX
            #include <zlib.h>
        ],[inflateCopy(NULL,NULL);],ac_have_zlib=yes,ac_have_zlib=no)
        Z_PREFIX=1
    fi

    LIBS="$saved_LIBS"
    CFLAGS="$saved_CFLAGS"

    if test "$ac_have_zlib" = "yes"
    then
        ZLIB_LIBS="$ZLIB_LDOPTS"
        AC_DEFINE_UNQUOTED([HAVE_ZLIB],[1],[Have zlib])
        AC_SUBST(ZLIB_LIBS)
        AC_SUBST(ZLIB_CFLAGS)
        if test "$Z_PREFIX" = "1"
        then
            AC_DEFINE_UNQUOTED([Z_PREFIX],[1],[Z prefix])
        fi
        AC_MSG_RESULT([$ZLIB_LIBS])
    else
        AC_MSG_NOTICE($ac_have_zlib)
    fi
else
    ac_have_zlib=no
fi
AM_CONDITIONAL(HAVE_ZLIB, test "$ac_have_zlib" = "yes" )
])
