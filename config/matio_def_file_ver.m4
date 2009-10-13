dnl Copyright (C) 2008   Christopher C. Hulbert
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
  AC_DEFINE_UNQUOTED([MAT_FT_DEFAULT],[$file_ver])
  AC_MSG_RESULT([$file_ver])
else
  AC_MSG_ERROR([Unrecognized MAT file version])
fi
])
