dnl Copyright (C) 2005-2006   Christopher C. Hulbert
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
AC_DEFUN([CHECK_MATIO_UINT32_T],
[
    AC_MSG_CHECKING([for mat_uint32_t])

    AC_TRY_LINK(
    [
        #include <stdlib.h>
        #ifdef HAVE_INTTYPES_H
        #   include <inttypes.h>
        #endif
        #ifdef HAVE_STDINT_H
        #   include <stdint.h>
        #endif
    ],
    [uint32_t i = 0;],
    ac_have_mat_uint32_t=yes,ac_have_mat_uint32_t=no)

    if test "x$ac_have_mat_uint32_t" = "xyes"
    then
        ac_have_mat_uint32_t=yes
        AC_DEFINE_UNQUOTED(HAVE_MAT_UINT32_T)
        AC_DEFINE_UNQUOTED([_mat_uint32_t],[uint32_t])
        AC_MSG_RESULT([uint32_t])
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned char i = 0;],
        ac_have_mat_uint32_t=yes,ac_have_mat_uint32_t=no)

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED(HAVE_MAT_UINT32_T)
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned char])
            AC_MSG_RESULT([unsigned char])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned short i = 0;],
        ac_have_mat_uint32_t=yes,ac_have_mat_uint32_t=no)

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED(HAVE_MAT_UINT32_T)
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned short])
            AC_MSG_RESULT([unsigned short])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned int i = 0;],
        ac_have_mat_uint32_t=yes,ac_have_mat_uint32_t=no)

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED(HAVE_MAT_UINT32_T)
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned int])
            AC_MSG_RESULT([unsigned int])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned long i = 0;],
        ac_have_mat_uint32_t=yes,ac_have_mat_uint32_t=no)

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED(HAVE_MAT_UINT32_T)
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned long])
            AC_MSG_RESULT([unsigned long])
        fi
    fi
    if test "x$ac_have_mat_uint32_t" != "xyes"
    then
        AC_TRY_LINK(
        [
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
        ],
        [unsigned long long i = 0;],
        ac_have_mat_uint32_t=yes,ac_have_mat_uint32_t=no)

        if test "x$ac_have_mat_uint32_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED(HAVE_MAT_UINT32_T)
            AC_DEFINE_UNQUOTED([_mat_uint32_t],[unsigned long long])
            AC_MSG_RESULT([unsigned long long])
        fi
    fi
])
