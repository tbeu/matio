dnl Copyright (C) 2010-2017   Christopher C. Hulbert
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
AC_DEFUN([MATIO_CHECK_MATLAB],
[
AC_ARG_WITH(matlab,
  AS_HELP_STRING([--with-matlab=DIR],[Directory with 'matlab' program]),
    MATLAB_DIR="$withval",MATLAB_DIR="")

if test -n "$MATLAB_DIR" -a -d "$MATLAB_DIR"; then
    MATLAB_PATH="$PATH_SEPARATOR$MATLAB_DIR"
fi
# Do not use MATLAB as the program name because it can cause the matlab
# script to faile
AC_PATH_PROG([MATLABEXE], [matlab], [], [$PATH$MATLAB_PATH])

AC_SUBST([MATLABEXE])
if test -n "$MATLABEXE"
then
    AC_MSG_RESULT([$MATLABEXE])
    matio_cv_have_matlab=yes
else
    AC_MSG_RESULT([no])
    matio_cv_have_matlab=no
fi
])
