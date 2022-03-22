dnl Copyright (c) 2005-2021, Christopher C. Hulbert
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
AC_DEFUN([AC_TYPE_OFF64_T],
[
    AC_MSG_CHECKING([for off64_t])

    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #define _FILE_OFFSET_BITS 64
        #include <sys/types.h>
    ]], [[off64_t off = 42;]])],[ac_cv_type_off64_t=yes],[ac_cv_type_off64_t=no])
    if test $ac_cv_type_off64_t = yes;
    then
        AC_DEFINE(HAVE_OFF64_T, [], [whether off64_t is defined in sys/types.h using _LARGEFILE64_SOURCE])
    fi
    if test "x$ac_cv_type_off64_t" != "xyes"
    then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi
])
