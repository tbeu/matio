dnl Copyright (c) 2015-2026, The matio contributors
dnl Copyright (c) 2010-2014, Christopher C. Hulbert
dnl All rights reserved.
dnl
dnl SPDX-License-Identifier: BSD-2-Clause
AC_DEFUN([MATIO_CHECK_LIBDIR_SUFFIX],
[
AC_ARG_WITH(libdir-suffix,
    AS_HELP_STRING([--with-libdir-suffix=suffix],[Suffix to apply to library directories]),,with_libdir_suffix=)
])
