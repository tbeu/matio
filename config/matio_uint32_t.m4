dnl Copyright (c) 2015-2022, The matio contributors
dnl Copyright (c) 2005-2014, Christopher C. Hulbert
dnl All rights reserved.
dnl
dnl Redistribution and use in source and binary forms, with or without
dnl modification, are permitted provided that the following conditions are met:
dnl
dnl 1. Redistributions of source code must retain the above copyright notice, this
dnl    list of conditions and the following disclaimer.
dnl
dnl 2. Redistributions in binary form must reproduce the above copyright notice,
dnl    this list of conditions and the following disclaimer in the documentation
dnl    and/or other materials provided with the distribution.
dnl
dnl THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
dnl AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
dnl IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
dnl DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
dnl FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
dnl DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
dnl SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
dnl CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
dnl OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
dnl OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
