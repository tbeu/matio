dnl Copyright (c) 2015-2026, The matio contributors
dnl Copyright (c) 2005-2014, Christopher C. Hulbert
dnl All rights reserved.
dnl
dnl SPDX-License-Identifier: BSD-2-Clause
AC_DEFUN([CHECK_MATIO_INT64_T],
[
    AC_MSG_CHECKING([for mat_int64_t])

    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ]], [[int64_t i = 0;]])],[ac_have_mat_int64_t=yes],[ac_have_mat_int64_t=no])

    if test "x$ac_have_mat_int64_t" = "xyes"
    then
        ac_have_mat_int64_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
        AC_DEFINE_UNQUOTED([_mat_int64_t],[int64_t],[int64 type])
        AC_MSG_RESULT([int64_t])
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT != 64
            #matio sizeof(signed char) not 64 bits
            #endif
        ]], [[signed char i = 0;]])],[ac_have_mat_int64_t=yes],[ac_have_mat_int64_t=no])

        if test "x$ac_have_mat_int64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_int64_t],[signed char],[int64 type])
            AC_MSG_RESULT([signed char])
        fi
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_SHORT != 64
            #matio sizeof(short) not 64 bits
            #endif
        ]], [[short i = 0;]])],[ac_have_mat_int64_t=yes],[ac_have_mat_int64_t=no])

        if test "x$ac_have_mat_int64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_int64_t],[short],[int64 type])
            AC_MSG_RESULT([short])
        fi
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_INT != 64
            #matio sizeof(int) not 64 bits
            #endif
        ]], [[int i = 0;]])],[ac_have_mat_int64_t=yes],[ac_have_mat_int64_t=no])

        if test "x$ac_have_mat_int64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_int64_t],[int],[int64 type])
            AC_MSG_RESULT([int])
        fi
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG != 64
            #matio sizeof(long) not 64 bits
            #endif
        ]], [[long i = 0;]])],[ac_have_mat_int64_t=yes],[ac_have_mat_int64_t=no])

        if test "x$ac_have_mat_int64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_int64_t],[long],[int64 type])
            AC_MSG_RESULT([long])
        fi
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #include <stdlib.h>
            #ifdef HAVE_INTTYPES_H
            #   include <inttypes.h>
            #endif
            #ifdef HAVE_STDINT_H
            #   include <stdint.h>
            #endif
            #include <limits.h>
            #if CHAR_BIT*SIZEOF_LONG_LONG != 64
            #matio sizeof(long long) not 64 bits
            #endif
        ]], [[long long i = 0;]])],[ac_have_mat_int64_t=yes],[ac_have_mat_int64_t=no])

        if test "x$ac_have_mat_int64_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT64_T],[],[Have MAT int64])
            AC_DEFINE_UNQUOTED([_mat_int64_t],[long long],[int64 type])
            AC_MSG_RESULT([long long])
        fi
    fi
    if test "x$ac_have_mat_int64_t" != "xyes"
    then
        AC_MSG_RESULT([])
    fi
])
