dnl Copyright (C) 2009-2016   Christopher C. Hulbert
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
AC_DEFUN([MATIO_CHECK_HDF5_V18],
[
    AC_MSG_CHECKING([if HDF5 interface is >= v1.8])
    saved_CFLAGS="$CFLAGS"
    saved_LDFLAGS="$LDFLAGS"
    saved_LIBS="$LIBS"

    CFLAGS="$HDF5_CFLAGS $saved_CFLAGS"
    LDFLAGS="$saved_LDFLAGS"
    LIBS="$HDF5_LIBS $ZLIB_LIBS $saved_LIBS"

    AC_TRY_LINK([#include<stdio.h>
                 #include <stdlib.h>
                 #include <hdf5.h>],
                 [#if defined(H5Rdereference)
                  /* HDF5 1.10.0 */
                  #define H5RDEREFERENCE(obj_id, ref_type, _ref) H5Rdereference2((obj_id), H5P_DATASET_ACCESS_DEFAULT, (ref_type), (_ref))
                  #else
                  /* HDF5 prior to 1.10.0 */
                  #define H5RDEREFERENCE(obj_id, ref_type, _ref) H5Rdereference((obj_id), (ref_type), (_ref))
                  #endif
                  hid_t dset_id = H5Dcreate(0,NULL,0,0,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
                  hobj_ref_t ref_ids[1];
                  hid_t ref_id = H5RDEREFERENCE(dset_id,H5R_OBJECT,ref_ids);],
                 [matio_hdf5_is_v18=yes],
                 [matio_hdf5_is_v18=no])

    CFLAGS="$saved_CFLAGS"
    LDFLAGS="$saved_LDFLAGS"
    LIBS="$saved_LIBS"

    AC_MSG_RESULT([$matio_hdf5_is_v18])
])

AC_DEFUN([MATIO_CHECK_HDF5],
[
AC_ARG_WITH(hdf5,AS_HELP_STRING([--with-hdf5=DIR],
            [Prefix where HDF5 library is installed]),
            HDF5_DIR=${withval},HDF5_DIR=)

ac_have_hdf5=no
if test "x${HDF5_DIR}" != "xno"
then
    AC_MSG_CHECKING(for HDF5 software)

    if test "x$HDF5_DIR" != "x" -a "x$HDF5_DIR" != "xyes"
    then
        HDF5_CFLAGS="-I${HDF5_DIR}/include"
        if test "$acl_libdirstem" != "lib" -a -d "${HDF5_DIR}/$acl_libdirstem"
        then
            HDF5_LIBS="-L${HDF5_DIR}/$acl_libdirstem -lhdf5"
        else
            HDF5_LIBS="-L${HDF5_DIR}/lib -lhdf5"
        fi
    else
        HDF5_LIBS="-lhdf5"
    fi

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
    else
        HDF5_LIBS=
        HDF5_CFLAGS=
        AC_MSG_RESULT([no])
    fi
else
    HDF5_LIBS=
    HDF5_CFLAGS=
fi

if test "x$ac_have_hdf5" = "xyes"
then
    MATIO_CHECK_HDF5_V18
    if test "x$matio_hdf5_is_v18" = "xyes"; then
        AC_DEFINE_UNQUOTED([HAVE_HDF5],[1],[Have HDF5])
    else
        HDF5_LIBS=""
        HDF5_CFLAGS=""
    fi
fi

AC_SUBST(HDF5_LIBS)
AC_SUBST(HDF5_CFLAGS)
AM_CONDITIONAL(HAVE_HDF5, test "x$ac_have_hdf5" = "xyes" -a "x$matio_hdf5_is_v18" = "xyes" )
])
