dnl Copyright (c) 2015-2026, The matio contributors
dnl Copyright (c) 2005-2014, Christopher C. Hulbert
dnl All rights reserved.
dnl
dnl SPDX-License-Identifier: BSD-2-Clause
AC_DEFUN([CHECK_MATIO_UINT16_T],
[
    AC_MSG_CHECKING([for mat_uint16_t])

    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ]], [[uint16_t i = 0;]])],[ac_have_mat_uint16_t=yes],[ac_have_mat_uint16_t=no])

    if test "x$ac_have_mat_uint16_t" = "xyes"
    then
        ac_have_mat_uint16_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
        AC_DEFINE_UNQUOTED([_mat_uint16_t],[uint16_t],[int16 type])
        AC_MSG_RESULT([uint16_t])
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
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
            #if CHAR_BIT != 16
            #matio sizeof(signed char) not 16 bits
            #endif
        ]], [[unsigned char i = 0;]])],[ac_have_mat_uint16_t=yes],[ac_have_mat_uint16_t=no])

        if test "x$ac_have_mat_uint16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned char],[int16 type])
            AC_MSG_RESULT([unsigned char])
        fi
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
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
            #if CHAR_BIT*SIZEOF_SHORT != 16
            #matio sizeof(short) not 16 bits
            #endif
        ]], [[unsigned short i = 0;]])],[ac_have_mat_uint16_t=yes],[ac_have_mat_uint16_t=no])

        if test "x$ac_have_mat_uint16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned short],[int16 type])
            AC_MSG_RESULT([unsigned short])
        fi
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
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
            #if CHAR_BIT*SIZEOF_INT != 16
            #matio sizeof(int) not 16 bits
            #endif
        ]], [[unsigned int i = 0;]])],[ac_have_mat_uint16_t=yes],[ac_have_mat_uint16_t=no])

        if test "x$ac_have_mat_uint16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned int],[int16 type])
            AC_MSG_RESULT([unsigned int])
        fi
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
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
            #if CHAR_BIT*SIZEOF_LONG != 16
            #matio sizeof(long) not 16 bits
            #endif
        ]], [[unsigned long i = 0;]])],[ac_have_mat_uint16_t=yes],[ac_have_mat_uint16_t=no])

        if test "x$ac_have_mat_uint16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned long],[int16 type])
            AC_MSG_RESULT([unsigned long])
        fi
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
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
            #if CHAR_BIT*SIZEOF_LONG_LONG != 16
            #matio sizeof(long long) not 16 bits
            #endif
        ]], [[unsigned long long i = 0;]])],[ac_have_mat_uint16_t=yes],[ac_have_mat_uint16_t=no])

        if test "x$ac_have_mat_uint16_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT16_T],[],[Have MAT int16])
            AC_DEFINE_UNQUOTED([_mat_uint16_t],[unsigned long long],[int16 type])
            AC_MSG_RESULT([unsigned long long])
        fi
    fi
    if test "x$ac_have_mat_uint16_t" != "xyes"
    then
        AC_MSG_RESULT([])
    fi
])
