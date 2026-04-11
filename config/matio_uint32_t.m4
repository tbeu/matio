dnl Copyright (c) 2015-2026, The matio contributors
dnl Copyright (c) 2005-2014, Christopher C. Hulbert
dnl All rights reserved.
dnl
dnl SPDX-License-Identifier: BSD-2-Clause
AC_DEFUN([CHECK_MATIO_UINT32_T],
[
    AC_MSG_CHECKING([for mat_uint32_t])

    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ]], [[uint32_t i = 0;]])],[ac_have_mat_uint32_t=yes],[ac_have_mat_uint32_t=no])

    if test "x$ac_have_mat_uint32_t" = "xyes"
    then
        ac_have_mat_uint32_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
        AC_DEFINE_UNQUOTED([_mat_uint32_t],[uint32_t],[int32 type])
        AC_MSG_RESULT([uint32_t])
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
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
            #if CHAR_BIT != 32
            #matio sizeof(signed char) not 32 bits
            #endif
        ]], [[unsigned char i = 0;]])],[ac_have_mat_uint32_t=yes],[ac_have_mat_uint32_t=no])

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned char],[int32 type])
            AC_MSG_RESULT([unsigned char])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
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
            #if CHAR_BIT*SIZEOF_SHORT != 32
            #matio sizeof(short) not 32 bits
            #endif
        ]], [[unsigned short i = 0;]])],[ac_have_mat_uint32_t=yes],[ac_have_mat_uint32_t=no])

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned short],[int32 type])
            AC_MSG_RESULT([unsigned short])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
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
            #if CHAR_BIT*SIZEOF_INT != 32
            #matio sizeof(int) not 32 bits
            #endif
        ]], [[unsigned int i = 0;]])],[ac_have_mat_uint32_t=yes],[ac_have_mat_uint32_t=no])

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned int],[int32 type])
            AC_MSG_RESULT([unsigned int])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
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
            #if CHAR_BIT*SIZEOF_LONG != 32
            #matio sizeof(long) not 32 bits
            #endif
        ]], [[unsigned long i = 0;]])],[ac_have_mat_uint32_t=yes],[ac_have_mat_uint32_t=no])

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned long],[int32type ])
            AC_MSG_RESULT([unsigned long])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
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
            #if CHAR_BIT*SIZEOF_LONG_LONG != 32
            #matio sizeof(long long) not 32 bits
            #endif
        ]], [[unsigned long long i = 0;]])],[ac_have_mat_uint32_t=yes],[ac_have_mat_uint32_t=no])

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_UINT32_T],[],[Have MAT int32])
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned long long],[int32 type])
            AC_MSG_RESULT([unsigned long long])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_MSG_RESULT([])
    fi
])
