dnl Copyright (c) 2015-2026, The matio contributors
dnl Copyright (c) 2008-2014, Christopher C. Hulbert
dnl All rights reserved.
dnl
dnl SPDX-License-Identifier: BSD-2-Clause
AC_DEFUN([MATIO_CHECK_DEFAULT_FILE_VERSION],
[
AC_ARG_WITH(default-file-ver,
  AS_HELP_STRING([--with-default-file-ver=version],
    [Default MAT file version (4,5,7.3)]),[],[with_default_file_ver=5])
  AC_MSG_CHECKING([for default MAT file version])
file_ver=
case "$with_default_file_ver" in
  "4")
    file_ver=MAT_FT_MAT4
    ;;
  "5")
    file_ver=MAT_FT_MAT5
    ;;
  "7.3")
    file_ver=MAT_FT_MAT73
    ;;
esac
if test "x$file_ver" != "x"; then
  AC_DEFINE_UNQUOTED([MAT_FT_DEFAULT],[$file_ver],[Default file format])
  AC_MSG_RESULT([$file_ver])
else
  AC_MSG_ERROR([Unrecognized MAT file version])
fi
])
