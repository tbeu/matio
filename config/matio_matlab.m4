dnl Copyright (c) 2015-2022, The matio contributors
dnl Copyright (c) 2010-2014, Christopher C. Hulbert
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
AC_DEFUN([MATIO_CHECK_MATLAB],
[
AC_ARG_WITH(matlab,
  AS_HELP_STRING([--with-matlab=DIR],[Directory with 'matlab' program]),
    MATLAB_DIR="$withval",MATLAB_DIR="")

if test -n "$MATLAB_DIR" -a -d "$MATLAB_DIR"; then
    MATLAB_PATH="$PATH_SEPARATOR$MATLAB_DIR"
fi
# Do not use MATLAB as the program name because it can cause the matlab
# script to fail
AC_PATH_PROG([MATLABEXE], [matlab], [], [$PATH$MATLAB_PATH])

AC_SUBST([MATLABEXE])
if test -n "$MATLABEXE"
then
    matio_cv_have_matlab=yes
else
    matio_cv_have_matlab=no
fi
])
