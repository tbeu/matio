dnl Copyright (c) 2015-2022, The matio contributors
dnl Copyright (c) 2008-2014, Christopher C. Hulbert
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
