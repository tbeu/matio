AC_DEFUN([MATIO_CHECK_HDF5],
[
AC_ARG_WITH(hdf5,AS_HELP_STRING([--with-hdf5=DIR],
            [Prefix where HDF5 library is installed]),
            HDF5_DIR=${withval},HDF5_DIR=)

ac_have_hdf5=no
if test -n "${HDF5_DIR}"
then
    AC_MSG_CHECKING(for HDF5 software)

    HDF5_LIBS="-L${HDF5_DIR}/$acl_libdirstem -lhdf5"
    HDF5_CFLAGS="-I${HDF5_DIR}/include"

    saved_CFLAGS="$CFLAGS"
    saved_LDFLAGS="$LDFLAGS"
    saved_LIBS="$LIBS"

    CFLAGS="$HDF5_CFLAGS $saved_CFLAGS"
    LDFLAGS="$saved_LDFLAGS"
    LIBS="$HDF5_LIBS $ZLIB_LIBS $saved_LIBS"

    AC_TRY_LINK([#include<stdio.h>
                 #include <stdlib.h>
                 #include <hdf5.h>],
                 [H5open()],
                 [ac_have_hdf5=yes],
                 [ac_have_hdf5=no])

    CFLAGS="$saved_CFLAGS"
    LDFLAGS="$saved_LDFLAGS"
    LIBS="$saved_LIBS"
    if test "x$ac_have_hdf5" = "xyes"
    then
        AC_MSG_RESULT($HDF5_LIBS)
        AC_DEFINE_UNQUOTED(HAVE_HDF5)
    else
        HDF5_LIBS=
        HDF5_CFLAGS=
        AC_MSG_RESULT([no])
    fi
else
    HDF5_LIBS=
    HDF5_CFLAGS=
fi
AC_SUBST(HDF5_LIBS)
AC_SUBST(HDF5_CFLAGS)
AM_CONDITIONAL(HAVE_HDF5, test "x$ac_have_hdf5" = "xyes" )
])
