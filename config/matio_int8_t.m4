dnl Copyright (c) 2015-2026, The matio contributors
dnl Copyright (c) 2005-2014, Christopher C. Hulbert
dnl All rights reserved.
dnl
dnl SPDX-License-Identifier: BSD-2-Clause
AC_DEFUN([CHECK_MATIO_INT8_T],
[
    AC_MSG_CHECKING([for mat_int8_t])

    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ]], [[int8_t i = 0;]])],[ac_have_mat_int8_t=yes],[ac_have_mat_int8_t=no])

    if test "x$ac_have_mat_int8_t" = "xyes"
    then
        ac_have_mat_int8_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
        AC_DEFINE_UNQUOTED([_mat_int8_t],[int8_t],[int8 type])
        AC_MSG_RESULT([int8_t])
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
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
            #if CHAR_BIT != 8
            #matio sizeof(signed char) not 8 bits
            #endif
        ]], [[signed char i = 0;]])],[ac_have_mat_int8_t=yes],[ac_have_mat_int8_t=no])

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[signed char],[int8 type])
            AC_MSG_RESULT([signed char])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
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
            #if CHAR_BIT*SIZEOF_SHORT != 8
            #matio sizeof(short) not 8 bits
            #endif
        ]], [[short i = 0;]])],[ac_have_mat_int8_t=yes],[ac_have_mat_int8_t=no])

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[short],[int8 type])
            AC_MSG_RESULT([short])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
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
            #if CHAR_BIT*SIZEOF_INT != 8
            #matio sizeof(int) not 8 bits
            #endif
        ]], [[int i = 0;]])],[ac_have_mat_int8_t=yes],[ac_have_mat_int8_t=no])

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[int],[int8 type])
            AC_MSG_RESULT([int])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
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
            #if CHAR_BIT*SIZEOF_LONG != 8
            #matio sizeof(long) not 8 bits
            #endif
        ]], [[long i = 0;]])],[ac_have_mat_int8_t=yes],[ac_have_mat_int8_t=no])

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[long],[int8 type])
            AC_MSG_RESULT([long])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
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
            #if CHAR_BIT*SIZEOF_LONG_LONG != 8
            #matio sizeof(long long) not 8 bits
            #endif
        ]], [[long long i = 0;]])],[ac_have_mat_int8_t=yes],[ac_have_mat_int8_t=no])

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[long long],[int8 type])
            AC_MSG_RESULT([long long])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
    then
        AC_MSG_RESULT([])
    fi
])
