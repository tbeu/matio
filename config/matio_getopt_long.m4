AC_DEFUN([MATIO_CHECK_GETOPT_LONG],
[

AC_MSG_CHECKING(for getopt_long)

AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
 #include <stdlib.h>
 #ifdef HAVE_UNISTD_H
 #include <unistd.h>
 #endif
 #define _GNU_SOURCE /* For getopt_long on GNU systems */
 #include <getopt.h>
]], [[opt = getopt_long(0,NULL,NULL,NULL,NULL);]])],[ac_have_getopt_long=yes],[ac_have_getopt_long=no])

if test "x$ac_have_getopt_long" = "xyes"
then
    AC_MSG_RESULT([system])
else
    AC_MSG_RESULT([internal])
fi
AM_CONDITIONAL(NEED_GETOPT, test "x$ac_have_getopt_long" = "xno" )
])
