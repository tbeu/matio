dnl Copyright (C) 2010   Christopher C. Hulbert
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
