dnl Copyright (c) 2015-2026, The matio contributors
dnl Copyright (c) 2010-2014, Christopher C. Hulbert
dnl All rights reserved.
dnl
dnl SPDX-License-Identifier: BSD-2-Clause
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
