dnl Copyright (C) 2008-2017   Christopher C. Hulbert
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
