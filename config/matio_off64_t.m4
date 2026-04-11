dnl Copyright (c) 2015-2026, The matio contributors
dnl Copyright (c) 2005-2014, Christopher C. Hulbert
dnl All rights reserved.
dnl
dnl SPDX-License-Identifier: BSD-2-Clause
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
