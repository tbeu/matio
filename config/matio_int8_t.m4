dnl Copyright (C) 2005-2017   Christopher C. Hulbert
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
AC_DEFUN([CHECK_MATIO_INT8_T],
[
    AC_MSG_CHECKING([for mat_int8_t])

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
    [int8_t i = 0;],
    ac_have_mat_int8_t=yes,ac_have_mat_int8_t=no)

    if test "x$ac_have_mat_int8_t" = "xyes"
    then
        ac_have_mat_int8_t=yes
        AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
        AC_DEFINE_UNQUOTED([_mat_int8_t],[int8_t],[int8 type])
        AC_MSG_RESULT([int8_t])
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
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
            #if CHAR_BIT != 8
            #matio sizeof(signed char) not 8 bits
            #endif
        ],
        [signed char i = 0;],
        ac_have_mat_int8_t=yes,ac_have_mat_int8_t=no)

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[signed char],[int8 type])
            AC_MSG_RESULT([signed char])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
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
            #if CHAR_BIT*SIZEOF_SHORT != 8
            #matio sizeof(short) not 8 bits
            #endif
        ],
        [short i = 0;],
        ac_have_mat_int8_t=yes,ac_have_mat_int8_t=no)

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[short],[int8 type])
            AC_MSG_RESULT([short])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
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
            #if CHAR_BIT*SIZEOF_INT != 8
            #matio sizeof(int) not 8 bits
            #endif
        ],
        [int i = 0;],
        ac_have_mat_int8_t=yes,ac_have_mat_int8_t=no)

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[int],[int8 type])
            AC_MSG_RESULT([int])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
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
            #if CHAR_BIT*SIZEOF_LONG != 8
            #matio sizeof(long) not 8 bits
            #endif
        ],
        [long i = 0;],
        ac_have_mat_int8_t=yes,ac_have_mat_int8_t=no)

        if test "x$ac_have_mat_int8_t" = "xyes"
        then
            AC_DEFINE_UNQUOTED([HAVE_MAT_INT8_T],[],[Have MAT int8])
            AC_DEFINE_UNQUOTED([_mat_int8_t],[long],[int8 type])
            AC_MSG_RESULT([long])
        fi
    fi
    if test "x$ac_have_mat_int8_t" != "xyes"
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
            #if CHAR_BIT*SIZEOF_LONG_LONG != 8
            #matio sizeof(long long) not 8 bits
            #endif
        ],
        [long long i = 0;],
        ac_have_mat_int8_t=yes,ac_have_mat_int8_t=no)

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
