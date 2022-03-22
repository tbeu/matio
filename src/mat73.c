/** @file mat73.c
 * Matlab MAT version 7.3 file functions
 * @ingroup MAT
 */
/*
 * Copyright (c) 2015-2022, The matio contributors
 * Copyright (c) 2005-2014, Christopher C. Hulbert
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "matio_private.h"
#if defined(MAT73) && MAT73
#include "mat73.h"
#endif
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
#if defined(_MSC_VER) || defined(__MINGW32__)
#define strdup _strdup
#endif

#if defined(MAT73) && MAT73
#if HAVE_HDF5

static const char *ClassNames[] = {"",       "cell",   "struct", "object", "char",    "sparse",
                                   "double", "single", "int8",   "uint8",  "int16",   "uint16",
                                   "int32",  "uint32", "int64",  "uint64", "function"};

struct ReadNextIterData
{
    mat_t *mat;
    matvar_t *matvar;
};

struct ReadGroupInfoIterData
{
    hsize_t nfields;
    matvar_t *matvar;
};

#if H5_VERSION_GE(1, 10, 0)
#define H5RDEREFERENCE(obj_id, ref_type, _ref) \
    H5Rdereference2((obj_id), H5P_DATASET_ACCESS_DEFAULT, (ref_type), (_ref))
#else
#define H5RDEREFERENCE(obj_id, ref_type, _ref) H5Rdereference((obj_id), (ref_type), (_ref))
#endif

#if H5_VERSION_GE(1, 12, 0)
#define H5O_INFO_T H5O_info2_t
#define H5OGET_INFO_BY_NAME(loc_id, name, oinfo, lapl_id) \
    H5Oget_info_by_name3((loc_id), (name), (oinfo), H5O_INFO_BASIC, (lapl_id));
#elif H5_VERSION_GE(1, 10, 3)
#define H5O_INFO_T H5O_info_t
#define H5OGET_INFO_BY_NAME(loc_id, name, oinfo, lapl_id) \
    H5Oget_info_by_name2((loc_id), (name), (oinfo), H5O_INFO_BASIC, (lapl_id));
#else
#define H5O_INFO_T H5O_info_t
#define H5OGET_INFO_BY_NAME(loc_id, name, oinfo, lapl_id) \
    H5Oget_info_by_name((loc_id), (name), (oinfo), (lapl_id));
#endif

#if !defined(MAX_RANK)
/* Maximal number of dimensions for stack allocated temporary dimension arrays */
#define MAX_RANK (3)
#endif

/*===========================================================================
 *  Private functions
 *===========================================================================
 */
static enum matio_classes ClassStr2ClassType(const char *name);
static enum matio_classes DataType2ClassType(enum matio_types type);
static hid_t ClassType2H5T(enum matio_classes class_type);
static hid_t DataType2H5T(enum matio_types data_type);
static hid_t SizeType2H5T(void);
static hid_t DataType(hid_t h5_type, int isComplex);
static void Mat_H5GetChunkSize(size_t rank, hsize_t *dims, hsize_t *chunk_dims);
static int Mat_H5ReadVarInfo(matvar_t *matvar, hid_t dset_id);
static size_t *Mat_H5ReadDims(hid_t dset_id, hsize_t *nelems, int *rank);
static int Mat_H5ReadFieldNames(matvar_t *matvar, hid_t dset_id, hsize_t *nfields);
static int Mat_H5ReadDatasetInfo(mat_t *mat, matvar_t *matvar, hid_t dset_id);
static int Mat_H5ReadGroupInfo(mat_t *mat, matvar_t *matvar, hid_t dset_id);
static int Mat_H5ReadNextReferenceInfo(hid_t ref_id, matvar_t *matvar, mat_t *mat);
static int Mat_H5ReadNextReferenceData(hid_t ref_id, matvar_t *matvar, mat_t *mat);
static int Mat_VarWriteEmpty(hid_t id, matvar_t *matvar, const char *name, const char *class_name);
static int Mat_VarWriteCell73(hid_t id, matvar_t *matvar, const char *name, hid_t *refs_id,
                              hsize_t *dims);
static int Mat_VarWriteChar73(hid_t id, matvar_t *matvar, const char *name, hsize_t *dims);
static int Mat_WriteEmptyVariable73(hid_t id, const char *name, hsize_t rank, size_t *dims);
static int Mat_VarWriteLogical73(hid_t id, matvar_t *matvar, const char *name, hsize_t *dims);
static int Mat_VarWriteNumeric73(hid_t id, matvar_t *matvar, const char *name, hsize_t *dims,
                                 hsize_t *max_dims);
static int Mat_VarWriteAppendNumeric73(hid_t id, matvar_t *matvar, const char *name, hsize_t *dims,
                                       int dim);
static int Mat_VarWriteSparse73(hid_t id, matvar_t *matvar, const char *name);
static int Mat_VarWriteStruct73(hid_t id, matvar_t *matvar, const char *name, hid_t *refs_id,
                                hsize_t *dims, hsize_t *max_dims);
static int Mat_VarWriteAppendStruct73(hid_t id, matvar_t *matvar, const char *name, hid_t *refs_id,
                                      hsize_t *dims, int dim);
static int Mat_VarWriteNext73(hid_t id, matvar_t *matvar, const char *name, hid_t *refs_id);
static int Mat_VarWriteAppendNext73(hid_t id, matvar_t *matvar, const char *name, hid_t *refs_id,
                                    int dim);
static int Mat_VarWriteNextType73(hid_t id, matvar_t *matvar, const char *name, hid_t *refs_id,
                                  hsize_t *dims);
static int Mat_VarWriteAppendNextType73(hid_t id, matvar_t *matvar, const char *name,
                                        hid_t *refs_id, hsize_t *dims, int dim);
static herr_t Mat_VarReadNextInfoIterate(hid_t id, const char *name, const H5L_info_t *info,
                                         void *op_data);
static herr_t Mat_H5ReadGroupInfoIterate(hid_t dset_id, const char *name, const H5L_info_t *info,
                                         void *op_data);
static int Mat_H5ReadData(hid_t dset_id, hid_t h5_type, hid_t mem_space, hid_t dset_space,
                          int isComplex, void *data);
static int Mat_H5WriteData(hid_t dset_id, hid_t h5_type, hid_t mem_space, hid_t dset_space,
                           int isComplex, void *data);
static int Mat_H5WriteAppendData(hid_t id, hid_t h5_type, int mrank, const char *name,
                                 const size_t *mdims, hsize_t *dims, int dim, int isComplex,
                                 void *data);
static int Mat_VarWriteRef(hid_t id, matvar_t *matvar, enum matio_compression compression,
                           hid_t *refs_id, hobj_ref_t *ref);

static enum matio_classes
ClassStr2ClassType(const char *name)
{
    enum matio_classes id = MAT_C_EMPTY;
    if ( NULL != name ) {
        int k;
        for ( k = 1; k < 17; k++ ) {
            if ( 0 == strcmp(name, ClassNames[k]) ) {
                id = (enum matio_classes)k;
                break;
            }
        }
    }

    return id;
}

static enum matio_classes
DataType2ClassType(enum matio_types type)
{
    switch ( type ) {
        case MAT_T_DOUBLE:
            return MAT_C_DOUBLE;
        case MAT_T_SINGLE:
            return MAT_C_SINGLE;
#ifdef HAVE_MAT_INT64_T
        case MAT_T_INT64:
            return MAT_C_INT64;
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_T_UINT64:
            return MAT_C_UINT64;
#endif
        case MAT_T_INT32:
            return MAT_C_INT32;
        case MAT_T_UINT32:
            return MAT_C_UINT32;
        case MAT_T_INT16:
            return MAT_C_INT16;
        case MAT_T_UINT16:
            return MAT_C_UINT16;
        case MAT_T_INT8:
            return MAT_C_INT8;
        case MAT_T_UINT8:
            return MAT_C_UINT8;
        default:
            return MAT_C_EMPTY;
    }
}

static hid_t
ClassType2H5T(enum matio_classes class_type)
{
    switch ( class_type ) {
        case MAT_C_DOUBLE:
            return H5T_NATIVE_DOUBLE;
        case MAT_C_SINGLE:
            return H5T_NATIVE_FLOAT;
        case MAT_C_INT64:
#if CHAR_BIT * SIZEOF_SHORT == 64
            return H5T_NATIVE_SHORT;
#elif CHAR_BIT * SIZEOF_INT == 64
            return H5T_NATIVE_INT;
#elif CHAR_BIT * SIZEOF_LONG == 64
            return H5T_NATIVE_LONG;
#elif CHAR_BIT * SIZEOF_LONG_LONG == 64
            return H5T_NATIVE_LLONG;
#endif
        case MAT_C_UINT64:
#if CHAR_BIT * SIZEOF_SHORT == 64
            return H5T_NATIVE_USHORT;
#elif CHAR_BIT * SIZEOF_INT == 64
            return H5T_NATIVE_UINT;
#elif CHAR_BIT * SIZEOF_LONG == 64
            return H5T_NATIVE_ULONG;
#elif CHAR_BIT * SIZEOF_LONG_LONG == 64
            return H5T_NATIVE_ULLONG;
#endif
        case MAT_C_INT32:
#if CHAR_BIT == 32
            return H5T_NATIVE_SCHAR;
#elif CHAR_BIT * SIZEOF_SHORT == 32
            return H5T_NATIVE_SHORT;
#elif CHAR_BIT * SIZEOF_INT == 32
            return H5T_NATIVE_INT;
#elif CHAR_BIT * SIZEOF_LONG == 32
            return H5T_NATIVE_LONG;
#elif CHAR_BIT * SIZEOF_LONG_LONG == 32
            return H5T_NATIVE_LLONG;
#endif
        case MAT_C_UINT32:
#if CHAR_BIT == 32
            return H5T_NATIVE_UCHAR;
#elif CHAR_BIT * SIZEOF_SHORT == 32
            return H5T_NATIVE_USHORT;
#elif CHAR_BIT * SIZEOF_INT == 32
            return H5T_NATIVE_UINT;
#elif CHAR_BIT * SIZEOF_LONG == 32
            return H5T_NATIVE_ULONG;
#elif CHAR_BIT * SIZEOF_LONG_LONG == 32
            return H5T_NATIVE_ULLONG;
#endif
        case MAT_C_INT16:
#if CHAR_BIT == 16
            return H5T_NATIVE_SCHAR;
#elif CHAR_BIT * SIZEOF_SHORT == 16
            return H5T_NATIVE_SHORT;
#elif CHAR_BIT * SIZEOF_INT == 16
            return H5T_NATIVE_INT;
#elif CHAR_BIT * SIZEOF_LONG == 16
            return H5T_NATIVE_LONG;
#elif CHAR_BIT * SIZEOF_LONG_LONG == 16
            return H5T_NATIVE_LLONG;
#endif
        case MAT_C_UINT16:
#if CHAR_BIT == 16
            return H5T_NATIVE_UCHAR;
#elif CHAR_BIT * SIZEOF_SHORT == 16
            return H5T_NATIVE_USHORT;
#elif CHAR_BIT * SIZEOF_INT == 16
            return H5T_NATIVE_UINT;
#elif CHAR_BIT * SIZEOF_LONG == 16
            return H5T_NATIVE_ULONG;
#elif CHAR_BIT * SIZEOF_LONG_LONG == 16
            return H5T_NATIVE_ULLONG;
#endif
        case MAT_C_INT8:
#if CHAR_BIT == 8
            return H5T_NATIVE_SCHAR;
#elif CHAR_BIT * SIZEOF_SHORT == 8
            return H5T_NATIVE_SHORT;
#elif CHAR_BIT * SIZEOF_INT == 8
            return H5T_NATIVE_INT;
#elif CHAR_BIT * SIZEOF_LONG == 8
            return H5T_NATIVE_LONG;
#elif CHAR_BIT * SIZEOF_LONG_LONG == 8
            return H5T_NATIVE_LLONG;
#endif
        case MAT_C_UINT8:
#if CHAR_BIT == 8
            return H5T_NATIVE_UCHAR;
#elif CHAR_BIT * SIZEOF_SHORT == 8
            return H5T_NATIVE_USHORT;
#elif CHAR_BIT * SIZEOF_INT == 8
            return H5T_NATIVE_UINT;
#elif CHAR_BIT * SIZEOF_LONG == 8
            return H5T_NATIVE_ULONG;
#elif CHAR_BIT * SIZEOF_LONG_LONG == 8
            return H5T_NATIVE_ULLONG;
#endif
        default:
            return -1;
    }
}

static hid_t
DataType2H5T(enum matio_types data_type)
{
    switch ( data_type ) {
        case MAT_T_DOUBLE:
            return H5T_NATIVE_DOUBLE;
        case MAT_T_SINGLE:
            return H5T_NATIVE_FLOAT;
        case MAT_T_INT64:
#if CHAR_BIT * SIZEOF_SHORT == 64
            return H5T_NATIVE_SHORT;
#elif CHAR_BIT * SIZEOF_INT == 64
            return H5T_NATIVE_INT;
#elif CHAR_BIT * SIZEOF_LONG == 64
            return H5T_NATIVE_LONG;
#elif CHAR_BIT * SIZEOF_LONG_LONG == 64
            return H5T_NATIVE_LLONG;
#endif
        case MAT_T_UINT64:
#if CHAR_BIT * SIZEOF_SHORT == 64
            return H5T_NATIVE_USHORT;
#elif CHAR_BIT * SIZEOF_INT == 64
            return H5T_NATIVE_UINT;
#elif CHAR_BIT * SIZEOF_LONG == 64
            return H5T_NATIVE_ULONG;
#elif CHAR_BIT * SIZEOF_LONG_LONG == 64
            return H5T_NATIVE_ULLONG;
#endif
        case MAT_T_INT32:
#if CHAR_BIT == 32
            return H5T_NATIVE_SCHAR;
#elif CHAR_BIT * SIZEOF_SHORT == 32
            return H5T_NATIVE_SHORT;
#elif CHAR_BIT * SIZEOF_INT == 32
            return H5T_NATIVE_INT;
#elif CHAR_BIT * SIZEOF_LONG == 32
            return H5T_NATIVE_LONG;
#elif CHAR_BIT * SIZEOF_LONG_LONG == 32
            return H5T_NATIVE_LLONG;
#endif
        case MAT_T_UINT32:
#if CHAR_BIT == 32
            return H5T_NATIVE_UCHAR;
#elif CHAR_BIT * SIZEOF_SHORT == 32
            return H5T_NATIVE_USHORT;
#elif CHAR_BIT * SIZEOF_INT == 32
            return H5T_NATIVE_UINT;
#elif CHAR_BIT * SIZEOF_LONG == 32
            return H5T_NATIVE_ULONG;
#elif CHAR_BIT * SIZEOF_LONG_LONG == 32
            return H5T_NATIVE_ULLONG;
#endif
        case MAT_T_INT16:
#if CHAR_BIT == 16
            return H5T_NATIVE_SCHAR;
#elif CHAR_BIT * SIZEOF_SHORT == 16
            return H5T_NATIVE_SHORT;
#elif CHAR_BIT * SIZEOF_INT == 16
            return H5T_NATIVE_INT;
#elif CHAR_BIT * SIZEOF_LONG == 16
            return H5T_NATIVE_LONG;
#elif CHAR_BIT * SIZEOF_LONG_LONG == 16
            return H5T_NATIVE_LLONG;
#endif
        case MAT_T_UINT16:
        case MAT_T_UTF16:
#if CHAR_BIT == 16
            return H5T_NATIVE_UCHAR;
#elif CHAR_BIT * SIZEOF_SHORT == 16
            return H5T_NATIVE_USHORT;
#elif CHAR_BIT * SIZEOF_INT == 16
            return H5T_NATIVE_UINT;
#elif CHAR_BIT * SIZEOF_LONG == 16
            return H5T_NATIVE_ULONG;
#elif CHAR_BIT * SIZEOF_LONG_LONG == 16
            return H5T_NATIVE_ULLONG;
#endif
        case MAT_T_INT8:
#if CHAR_BIT == 8
            return H5T_NATIVE_SCHAR;
#elif CHAR_BIT * SIZEOF_SHORT == 8
            return H5T_NATIVE_SHORT;
#elif CHAR_BIT * SIZEOF_INT == 8
            return H5T_NATIVE_INT;
#elif CHAR_BIT * SIZEOF_LONG == 8
            return H5T_NATIVE_LONG;
#elif CHAR_BIT * SIZEOF_LONG_LONG == 8
            return H5T_NATIVE_LLONG;
#endif
        case MAT_T_UINT8:
#if CHAR_BIT == 8
            return H5T_NATIVE_UCHAR;
#elif CHAR_BIT * SIZEOF_SHORT == 8
            return H5T_NATIVE_USHORT;
#elif CHAR_BIT * SIZEOF_INT == 8
            return H5T_NATIVE_UINT;
#elif CHAR_BIT * SIZEOF_LONG == 8
            return H5T_NATIVE_ULONG;
#elif CHAR_BIT * SIZEOF_LONG_LONG == 8
            return H5T_NATIVE_ULLONG;
#endif
        case MAT_T_UTF8:
            return H5T_NATIVE_CHAR;
        default:
            return -1;
    }
}

static hid_t
SizeType2H5T(void)
{
    if ( sizeof(size_t) == H5Tget_size(H5T_NATIVE_HSIZE) )
        return H5T_NATIVE_HSIZE;
    else if ( sizeof(size_t) == H5Tget_size(H5T_NATIVE_ULLONG) )
        return H5T_NATIVE_ULLONG;
    else if ( sizeof(size_t) == H5Tget_size(H5T_NATIVE_ULONG) )
        return H5T_NATIVE_ULONG;
    else if ( sizeof(size_t) == H5Tget_size(H5T_NATIVE_UINT) )
        return H5T_NATIVE_UINT;
    else if ( sizeof(size_t) == H5Tget_size(H5T_NATIVE_USHORT) )
        return H5T_NATIVE_USHORT;
    else
        return -1;
}

static hid_t
DataType(hid_t h5_type, int isComplex)
{
    hid_t h5_dtype;
    if ( isComplex ) {
        size_t h5_size = H5Tget_size(h5_type);
        h5_dtype = H5Tcreate(H5T_COMPOUND, 2 * h5_size);
        H5Tinsert(h5_dtype, "real", 0, h5_type);
        H5Tinsert(h5_dtype, "imag", h5_size, h5_type);
    } else {
        h5_dtype = H5Tcopy(h5_type);
    }
    return h5_dtype;
}

static void
Mat_H5GetChunkSize(size_t rank, hsize_t *dims, hsize_t *chunk_dims)
{
    hsize_t i, j, chunk_size = 1;

    for ( i = 0; i < rank; i++ ) {
        chunk_dims[i] = 1;
        for ( j = 4096 / chunk_size; j > 1; j >>= 1 ) {
            if ( dims[i] >= j ) {
                chunk_dims[i] = j;
                break;
            }
        }
        chunk_size *= chunk_dims[i];
    }
}

static int
Mat_H5ReadVarInfo(matvar_t *matvar, hid_t dset_id)
{
    hid_t attr_id, type_id;
    ssize_t name_len;
    int err = MATIO_E_NO_ERROR;

    /* Get the HDF5 name of the variable */
    name_len = H5Iget_name(dset_id, NULL, 0);
    if ( name_len > 0 ) {
        matvar->internal->hdf5_name = (char *)malloc(name_len + 1);
        (void)H5Iget_name(dset_id, matvar->internal->hdf5_name, name_len + 1);
    } else {
        /* Can not get an internal name, so leave the identifier open */
        matvar->internal->id = dset_id;
    }

    attr_id = H5Aopen_by_name(dset_id, ".", "MATLAB_class", H5P_DEFAULT, H5P_DEFAULT);
    type_id = H5Aget_type(attr_id);
    if ( H5T_STRING == H5Tget_class(type_id) ) {
        char *class_str = (char *)calloc(H5Tget_size(type_id) + 1, 1);
        if ( NULL != class_str ) {
            herr_t herr;
            hid_t class_id = H5Tcopy(H5T_C_S1);
            H5Tset_size(class_id, H5Tget_size(type_id));
            herr = H5Aread(attr_id, class_id, class_str);
            H5Tclose(class_id);
            if ( herr < 0 ) {
                free(class_str);
                H5Tclose(type_id);
                H5Aclose(attr_id);
                return MATIO_E_GENERIC_READ_ERROR;
            }
            matvar->class_type = ClassStr2ClassType(class_str);
            if ( MAT_C_EMPTY == matvar->class_type || MAT_C_CHAR == matvar->class_type ) {
                int int_decode = 0;
                if ( H5Aexists_by_name(dset_id, ".", "MATLAB_int_decode", H5P_DEFAULT) ) {
                    hid_t attr_id2 = H5Aopen_by_name(dset_id, ".", "MATLAB_int_decode", H5P_DEFAULT,
                                                     H5P_DEFAULT);
                    /* FIXME: Check that dataspace is scalar */
                    herr = H5Aread(attr_id2, H5T_NATIVE_INT, &int_decode);
                    H5Aclose(attr_id2);
                    if ( herr < 0 ) {
                        free(class_str);
                        H5Tclose(type_id);
                        H5Aclose(attr_id);
                        return MATIO_E_GENERIC_READ_ERROR;
                    }
                }
                switch ( int_decode ) {
                    case 2:
                        matvar->data_type = MAT_T_UINT16;
                        break;
                    case 1:
                        matvar->data_type = MAT_T_UINT8;
                        break;
                    case 4:
                        matvar->data_type = MAT_T_UINT32;
                        break;
                    default:
                        matvar->data_type = MAT_T_UNKNOWN;
                        break;
                }
                if ( MAT_C_EMPTY == matvar->class_type ) {
                    /* Check if this is a logical variable */
                    if ( 0 == strcmp(class_str, "logical") ) {
                        matvar->isLogical = MAT_F_LOGICAL;
                    }
                    matvar->class_type = DataType2ClassType(matvar->data_type);
                } else if ( MAT_T_UNKNOWN == matvar->data_type ) {
                    matvar->data_type = MAT_T_UINT16;
                }
            } else {
                matvar->data_type = ClassType2DataType(matvar->class_type);
            }
            free(class_str);
        } else {
            err = MATIO_E_OUT_OF_MEMORY;
        }
    }
    H5Tclose(type_id);
    H5Aclose(attr_id);

    if ( err ) {
        return err;
    }

    /* Check if the variable is global */
    if ( H5Aexists_by_name(dset_id, ".", "MATLAB_global", H5P_DEFAULT) ) {
        herr_t herr;
        attr_id = H5Aopen_by_name(dset_id, ".", "MATLAB_global", H5P_DEFAULT, H5P_DEFAULT);
        /* FIXME: Check that dataspace is scalar */
        herr = H5Aread(attr_id, H5T_NATIVE_INT, &matvar->isGlobal);
        H5Aclose(attr_id);
        if ( herr < 0 ) {
            return MATIO_E_GENERIC_READ_ERROR;
        }
    }

    return err;
}

static size_t *
Mat_H5ReadDims(hid_t dset_id, hsize_t *nelems, int *rank)
{
    hid_t space_id;
    size_t *perm_dims;

    *nelems = 0;
    space_id = H5Dget_space(dset_id);
    *rank = H5Sget_simple_extent_ndims(space_id);
    if ( 0 > *rank ) {
        *rank = 0;
        H5Sclose(space_id);
        return NULL;
    }
    perm_dims = (size_t *)malloc(*rank * sizeof(*perm_dims));
    if ( NULL != perm_dims ) {
        int err = 0;
        if ( MAX_RANK >= *rank ) {
            hsize_t dims[MAX_RANK];
            int k;
            size_t tmp = 1;
            (void)H5Sget_simple_extent_dims(space_id, dims, NULL);
            /* Permute dimensions */
            for ( k = 0; k < *rank; k++ ) {
                perm_dims[k] = (size_t)dims[*rank - k - 1];
                err |= Mul(&tmp, tmp, perm_dims[k]);
            }
            if ( err ) {
                Mat_Critical("Integer multiplication overflow");
                free(perm_dims);
                perm_dims = NULL;
                *rank = 0;
            }
            *nelems = (hsize_t)tmp;
            H5Sclose(space_id);
        } else {
            hsize_t *dims = (hsize_t *)malloc(*rank * sizeof(hsize_t));
            if ( NULL != dims ) {
                int k;
                size_t tmp = 1;
                (void)H5Sget_simple_extent_dims(space_id, dims, NULL);
                /* Permute dimensions */
                for ( k = 0; k < *rank; k++ ) {
                    perm_dims[k] = (size_t)dims[*rank - k - 1];
                    err |= Mul(&tmp, tmp, perm_dims[k]);
                }
                if ( err ) {
                    Mat_Critical("Integer multiplication overflow");
                    free(perm_dims);
                    perm_dims = NULL;
                    *rank = 0;
                }
                *nelems = (hsize_t)tmp;
                free(dims);
                H5Sclose(space_id);
            } else {
                free(perm_dims);
                perm_dims = NULL;
                *rank = 0;
                H5Sclose(space_id);
                Mat_Critical("Error allocating memory for dims");
            }
        }
    } else {
        *rank = 0;
        H5Sclose(space_id);
        Mat_Critical("Error allocating memory for matvar->dims");
    }

    return perm_dims;
}

static int
Mat_H5ReadFieldNames(matvar_t *matvar, hid_t dset_id, hsize_t *nfields)
{
    hsize_t i;
    hid_t field_id, attr_id, space_id;
    hvl_t *fieldnames_vl;
    herr_t herr;
    int err;

    attr_id = H5Aopen_by_name(dset_id, ".", "MATLAB_fields", H5P_DEFAULT, H5P_DEFAULT);
    space_id = H5Aget_space(attr_id);
    err = H5Sget_simple_extent_dims(space_id, nfields, NULL);
    if ( err < 0 ) {
        H5Sclose(space_id);
        H5Aclose(attr_id);
        return MATIO_E_GENERIC_READ_ERROR;
    } else {
        err = MATIO_E_NO_ERROR;
    }
    fieldnames_vl = (hvl_t *)calloc((size_t)(*nfields), sizeof(*fieldnames_vl));
    if ( fieldnames_vl == NULL ) {
        H5Sclose(space_id);
        H5Aclose(attr_id);
        return MATIO_E_OUT_OF_MEMORY;
    }
    field_id = H5Aget_type(attr_id);
    herr = H5Aread(attr_id, field_id, fieldnames_vl);
    if ( herr >= 0 ) {
        matvar->internal->num_fields = (unsigned int)*nfields;
        matvar->internal->fieldnames =
            (char **)calloc((size_t)(*nfields), sizeof(*matvar->internal->fieldnames));
        if ( matvar->internal->fieldnames != NULL ) {
            for ( i = 0; i < *nfields; i++ ) {
                matvar->internal->fieldnames[i] = (char *)calloc(fieldnames_vl[i].len + 1, 1);
                if ( matvar->internal->fieldnames[i] != NULL ) {
                    if ( fieldnames_vl[i].p != NULL ) {
                        memcpy(matvar->internal->fieldnames[i], fieldnames_vl[i].p,
                               fieldnames_vl[i].len);
                    }
                } else {
                    err = MATIO_E_OUT_OF_MEMORY;
                    break;
                }
            }
        } else {
            err = MATIO_E_OUT_OF_MEMORY;
        }
#if H5_VERSION_GE(1, 12, 0)
        H5Treclaim(field_id, space_id, H5P_DEFAULT, fieldnames_vl);
#else
        H5Dvlen_reclaim(field_id, space_id, H5P_DEFAULT, fieldnames_vl);
#endif
    } else {
        err = MATIO_E_GENERIC_READ_ERROR;
    }

    H5Sclose(space_id);
    H5Tclose(field_id);
    H5Aclose(attr_id);
    free(fieldnames_vl);

    return err;
}

static int
Mat_H5ReadDatasetInfo(mat_t *mat, matvar_t *matvar, hid_t dset_id)
{
    int err;
    hsize_t nelems;

    err = Mat_H5ReadVarInfo(matvar, dset_id);
    if ( err ) {
        return err;
    }

    matvar->dims = Mat_H5ReadDims(dset_id, &nelems, &matvar->rank);
    if ( NULL == matvar->dims ) {
        return MATIO_E_UNKNOWN_ERROR;
    }

    /* Check for attribute that indicates an empty array */
    if ( H5Aexists_by_name(dset_id, ".", "MATLAB_empty", H5P_DEFAULT) ) {
        int empty = 0;
        herr_t herr;
        hid_t attr_id = H5Aopen_by_name(dset_id, ".", "MATLAB_empty", H5P_DEFAULT, H5P_DEFAULT);
        /* FIXME: Check that dataspace is scalar */
        herr = H5Aread(attr_id, H5T_NATIVE_INT, &empty);
        H5Aclose(attr_id);
        if ( herr < 0 ) {
            err = MATIO_E_GENERIC_READ_ERROR;
        } else if ( empty ) {
            matvar->rank = (int)matvar->dims[0];
            free(matvar->dims);
            matvar->dims = (size_t *)calloc(matvar->rank, sizeof(*matvar->dims));
            if ( matvar->dims == NULL ) {
                err = MATIO_E_OUT_OF_MEMORY;
            } else {
                herr =
                    H5Dread(dset_id, SizeType2H5T(), H5S_ALL, H5S_ALL, H5P_DEFAULT, matvar->dims);
                if ( herr < 0 ) {
                    err = MATIO_E_GENERIC_READ_ERROR;
                } else {
                    size_t tmp = 1;
                    err = Mat_MulDims(matvar, &tmp);
                    nelems = (hsize_t)tmp;
                }
            }
        }
        if ( err ) {
            return err;
        }
    }

    /* Test if dataset type is compound and if so if it's complex */
    {
        hid_t type_id = H5Dget_type(dset_id);
        if ( H5T_COMPOUND == H5Tget_class(type_id) ) {
            /* FIXME: Any more checks? */
            matvar->isComplex = MAT_F_COMPLEX;
        }
        H5Tclose(type_id);
    }

    /* Test if dataset is deflated */
    {
        hid_t plist_id = H5Dget_create_plist(dset_id);
        if ( plist_id > 0 ) {
            const int nFilters = H5Pget_nfilters(plist_id);
            int i;
            for ( i = 0; i < nFilters; i++ ) {
                const H5Z_filter_t filterType =
                    H5Pget_filter2(plist_id, i, NULL, NULL, 0, 0, NULL, NULL);
                if ( H5Z_FILTER_DEFLATE == filterType ) {
                    matvar->compression = MAT_COMPRESSION_ZLIB;
                    break;
                }
            }
            H5Pclose(plist_id);
        }
    }

    /* If the dataset is a cell array read the info of the cells */
    if ( MAT_C_CELL == matvar->class_type ) {
        matvar_t **cells;

        matvar->data_size = sizeof(matvar_t **);
        err = Mul(&matvar->nbytes, nelems, matvar->data_size);
        if ( err ) {
            Mat_Critical("Integer multiplication overflow");
            return err;
        }
        matvar->data = calloc(matvar->nbytes, 1);
        if ( NULL == matvar->data ) {
            Mat_Critical("Couldn't allocate memory for the data");
            return MATIO_E_OUT_OF_MEMORY;
        }
        cells = (matvar_t **)matvar->data;

        if ( nelems ) {
            hobj_ref_t *ref_ids = (hobj_ref_t *)calloc(nelems, sizeof(*ref_ids));
            if ( ref_ids != NULL ) {
                size_t i;
                herr_t herr =
                    H5Dread(dset_id, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref_ids);
                if ( herr < 0 ) {
                    free(ref_ids);
                    return MATIO_E_GENERIC_READ_ERROR;
                }
                for ( i = 0; i < nelems; i++ ) {
                    hid_t ref_id;
                    cells[i] = Mat_VarCalloc();
                    cells[i]->internal->hdf5_ref = ref_ids[i];
                    /* Closing of ref_id is done in Mat_H5ReadNextReferenceInfo */
                    ref_id = H5RDEREFERENCE(dset_id, H5R_OBJECT, ref_ids + i);
                    if ( ref_id < 0 ) {
                        err = MATIO_E_GENERIC_READ_ERROR;
                    } else {
                        cells[i]->internal->id = ref_id;
                        err = Mat_H5ReadNextReferenceInfo(ref_id, cells[i], mat);
                    }
                    if ( err ) {
                        break;
                    }
                }
                free(ref_ids);
            } else {
                err = MATIO_E_OUT_OF_MEMORY;
            }
        }
    } else if ( MAT_C_STRUCT == matvar->class_type ) {
        /* Empty structures can be a dataset */

        /* Check if the structure defines its fields in MATLAB_fields */
        if ( H5Aexists_by_name(dset_id, ".", "MATLAB_fields", H5P_DEFAULT) ) {
            hsize_t nfields;
            err = Mat_H5ReadFieldNames(matvar, dset_id, &nfields);
        }
    }

    return err;
}

static int
Mat_H5ReadGroupInfo(mat_t *mat, matvar_t *matvar, hid_t dset_id)
{
    int fields_are_variables = 1;
    hsize_t nfields = 0, nelems;
    hid_t attr_id, field_id;
    matvar_t **fields;
    H5O_type_t obj_type;
    int err;

    err = Mat_H5ReadVarInfo(matvar, dset_id);
    if ( err < 0 ) {
        return err;
    }

    /* Check if the variable is sparse */
    if ( H5Aexists_by_name(dset_id, ".", "MATLAB_sparse", H5P_DEFAULT) ) {
        herr_t herr;
        hid_t sparse_dset_id;
        unsigned nrows = 0;

        attr_id = H5Aopen_by_name(dset_id, ".", "MATLAB_sparse", H5P_DEFAULT, H5P_DEFAULT);
        herr = H5Aread(attr_id, H5T_NATIVE_UINT, &nrows);
        H5Aclose(attr_id);
        if ( herr < 0 ) {
            return MATIO_E_GENERIC_READ_ERROR;
        }

        matvar->class_type = MAT_C_SPARSE;

        sparse_dset_id = H5Dopen(dset_id, "jc", H5P_DEFAULT);
        matvar->dims = Mat_H5ReadDims(sparse_dset_id, &nelems, &matvar->rank);
        H5Dclose(sparse_dset_id);
        if ( NULL != matvar->dims ) {
            if ( 1 == matvar->rank ) {
                size_t *dims = (size_t *)realloc(matvar->dims, 2 * sizeof(*matvar->dims));
                if ( NULL != dims ) {
                    matvar->rank = 2;
                    matvar->dims = dims;
                }
            }
            if ( 2 == matvar->rank ) {
                matvar->dims[1] = matvar->dims[0] - 1;
                matvar->dims[0] = nrows;
            }
        } else {
            return MATIO_E_UNKNOWN_ERROR;
        }

        /* Test if dataset type is compound and if so if it's complex */
        if ( H5Lexists(dset_id, "data", H5P_DEFAULT) ) {
            hid_t type_id;
            sparse_dset_id = H5Dopen(dset_id, "data", H5P_DEFAULT);
            type_id = H5Dget_type(sparse_dset_id);
            if ( H5T_COMPOUND == H5Tget_class(type_id) ) {
                /* FIXME: Any more checks? */
                matvar->isComplex = MAT_F_COMPLEX;
            }
            H5Tclose(type_id);
            H5Dclose(sparse_dset_id);
        }
        return MATIO_E_NO_ERROR;
    }

    if ( MAT_C_STRUCT != matvar->class_type ) {
        return MATIO_E_GENERIC_READ_ERROR;
    }

    /* Check if the structure defines its fields in MATLAB_fields */
    if ( H5Aexists_by_name(dset_id, ".", "MATLAB_fields", H5P_DEFAULT) ) {
        err = Mat_H5ReadFieldNames(matvar, dset_id, &nfields);
        if ( err ) {
            return err;
        }
    } else {
        herr_t herr;
        H5G_info_t group_info;
        matvar->internal->num_fields = 0;
        group_info.nlinks = 0;
        herr = H5Gget_info(dset_id, &group_info);
        if ( herr >= 0 && group_info.nlinks > 0 ) {
            struct ReadGroupInfoIterData group_data = {0, NULL};

            /* First iteration to retrieve number of relevant links */
            herr = H5Literate_by_name(dset_id, matvar->internal->hdf5_name, H5_INDEX_NAME,
                                      H5_ITER_NATIVE, NULL, Mat_H5ReadGroupInfoIterate,
                                      (void *)&group_data, H5P_DEFAULT);
            if ( herr > 0 && group_data.nfields > 0 ) {
                matvar->internal->fieldnames = (char **)calloc(
                    (size_t)(group_data.nfields), sizeof(*matvar->internal->fieldnames));
                group_data.nfields = 0;
                group_data.matvar = matvar;
                if ( matvar->internal->fieldnames != NULL ) {
                    /* Second iteration to fill fieldnames */
                    H5Literate_by_name(dset_id, matvar->internal->hdf5_name, H5_INDEX_NAME,
                                       H5_ITER_NATIVE, NULL, Mat_H5ReadGroupInfoIterate,
                                       (void *)&group_data, H5P_DEFAULT);
                }
                matvar->internal->num_fields = (unsigned)group_data.nfields;
                nfields = group_data.nfields;
            }
        }
    }

    if ( nfields > 0 ) {
        H5O_INFO_T object_info;
        object_info.type = H5O_TYPE_UNKNOWN;
        H5OGET_INFO_BY_NAME(dset_id, matvar->internal->fieldnames[0], &object_info, H5P_DEFAULT);
        obj_type = object_info.type;
    } else {
        obj_type = H5O_TYPE_UNKNOWN;
    }
    if ( obj_type == H5O_TYPE_DATASET ) {
        hid_t field_type_id;
        field_id = H5Dopen(dset_id, matvar->internal->fieldnames[0], H5P_DEFAULT);
        field_type_id = H5Dget_type(field_id);
        if ( H5T_REFERENCE == H5Tget_class(field_type_id) ) {
            /* Check if the field has the MATLAB_class attribute. If so, it
             * means the structure is a scalar. Otherwise, the dimensions of
             * the field dataset is the dimensions of the structure
             */
            if ( H5Aexists_by_name(field_id, ".", "MATLAB_class", H5P_DEFAULT) ) {
                matvar->rank = 2;
                matvar->dims = (size_t *)malloc(2 * sizeof(*matvar->dims));
                if ( NULL != matvar->dims ) {
                    matvar->dims[0] = 1;
                    matvar->dims[1] = 1;
                    nelems = 1;
                } else {
                    H5Tclose(field_type_id);
                    H5Dclose(field_id);
                    Mat_Critical("Error allocating memory for matvar->dims");
                    return MATIO_E_OUT_OF_MEMORY;
                }
            } else {
                matvar->dims = Mat_H5ReadDims(field_id, &nelems, &matvar->rank);
                if ( NULL != matvar->dims ) {
                    fields_are_variables = 0;
                } else {
                    H5Tclose(field_type_id);
                    H5Dclose(field_id);
                    return MATIO_E_UNKNOWN_ERROR;
                }
            }
        } else {
            /* Structure should be a scalar */
            matvar->rank = 2;
            matvar->dims = (size_t *)malloc(2 * sizeof(*matvar->dims));
            if ( NULL != matvar->dims ) {
                matvar->dims[0] = 1;
                matvar->dims[1] = 1;
                nelems = 1;
            } else {
                H5Tclose(field_type_id);
                H5Dclose(field_id);
                Mat_Critical("Error allocating memory for matvar->dims");
                return MATIO_E_UNKNOWN_ERROR;
            }
        }
        H5Tclose(field_type_id);
        H5Dclose(field_id);
    } else {
        /* Structure should be a scalar */
        matvar->rank = 2;
        matvar->dims = (size_t *)malloc(2 * sizeof(*matvar->dims));
        if ( NULL != matvar->dims ) {
            matvar->dims[0] = 1;
            matvar->dims[1] = 1;
            nelems = 1;
        } else {
            Mat_Critical("Error allocating memory for matvar->dims");
            return MATIO_E_OUT_OF_MEMORY;
        }
    }

    if ( nelems < 1 || nfields < 1 )
        return err;

    matvar->data_size = sizeof(*fields);
    {
        size_t nelems_x_nfields;
        err = Mul(&nelems_x_nfields, nelems, nfields);
        err |= Mul(&matvar->nbytes, nelems_x_nfields, matvar->data_size);
        if ( err ) {
            Mat_Critical("Integer multiplication overflow");
            matvar->nbytes = 0;
            return err;
        }
    }
    fields = (matvar_t **)calloc(matvar->nbytes, 1);
    matvar->data = fields;
    if ( NULL != fields ) {
        hsize_t k;
        for ( k = 0; k < nfields; k++ ) {
            H5O_INFO_T object_info;
            fields[k] = NULL;
            object_info.type = H5O_TYPE_UNKNOWN;
            H5OGET_INFO_BY_NAME(dset_id, matvar->internal->fieldnames[k], &object_info,
                                H5P_DEFAULT);
            if ( object_info.type == H5O_TYPE_DATASET ) {
                field_id = H5Dopen(dset_id, matvar->internal->fieldnames[k], H5P_DEFAULT);
                if ( !fields_are_variables ) {
                    hobj_ref_t *ref_ids = (hobj_ref_t *)calloc((size_t)nelems, sizeof(*ref_ids));
                    if ( ref_ids != NULL ) {
                        hsize_t l;
                        herr_t herr = H5Dread(field_id, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL,
                                              H5P_DEFAULT, ref_ids);
                        if ( herr < 0 ) {
                            err = MATIO_E_GENERIC_READ_ERROR;
                        } else {
                            for ( l = 0; l < nelems; l++ ) {
                                hid_t ref_id;
                                fields[l * nfields + k] = Mat_VarCalloc();
                                fields[l * nfields + k]->name =
                                    strdup(matvar->internal->fieldnames[k]);
                                fields[l * nfields + k]->internal->hdf5_ref = ref_ids[l];
                                /* Closing of ref_id is done in Mat_H5ReadNextReferenceInfo */
                                ref_id = H5RDEREFERENCE(field_id, H5R_OBJECT, ref_ids + l);
                                if ( ref_id < 0 ) {
                                    err = MATIO_E_GENERIC_READ_ERROR;
                                } else {
                                    fields[l * nfields + k]->internal->id = ref_id;
                                    err = Mat_H5ReadNextReferenceInfo(ref_id,
                                                                      fields[l * nfields + k], mat);
                                }
                                if ( err ) {
                                    break;
                                }
                            }
                        }
                        free(ref_ids);
                    } else {
                        err = MATIO_E_OUT_OF_MEMORY;
                    }
                } else {
                    fields[k] = Mat_VarCalloc();
                    fields[k]->name = strdup(matvar->internal->fieldnames[k]);
                    err = Mat_H5ReadDatasetInfo(mat, fields[k], field_id);
                }
                H5Dclose(field_id);
            } else if ( object_info.type == H5O_TYPE_GROUP ) {
                field_id = H5Gopen(dset_id, matvar->internal->fieldnames[k], H5P_DEFAULT);
                if ( -1 < field_id ) {
                    fields[k] = Mat_VarCalloc();
                    fields[k]->name = strdup(matvar->internal->fieldnames[k]);
                    err = Mat_H5ReadGroupInfo(mat, fields[k], field_id);
                    H5Gclose(field_id);
                }
            }
            if ( err ) {
                break;
            }
        }
    } else {
        err = MATIO_E_OUT_OF_MEMORY;
    }

    return err;
}

static herr_t
Mat_H5ReadGroupInfoIterate(hid_t dset_id, const char *name, const H5L_info_t *info, void *op_data)
{
    matvar_t *matvar;
    H5O_INFO_T object_info;
    struct ReadGroupInfoIterData *group_data;

    /* FIXME: follow symlinks, datatypes? */

    object_info.type = H5O_TYPE_UNKNOWN;
    H5OGET_INFO_BY_NAME(dset_id, name, &object_info, H5P_DEFAULT);
    if ( H5O_TYPE_DATASET != object_info.type && H5O_TYPE_GROUP != object_info.type )
        return 0;

    group_data = (struct ReadGroupInfoIterData *)op_data;
    if ( group_data == NULL )
        return -1;
    matvar = group_data->matvar;

    switch ( object_info.type ) {
        case H5O_TYPE_GROUP:
            /* Check that this is not the /#refs# group */
            if ( 0 == strcmp(name, "#refs#") )
                return 0;
            /* Fall through */
        case H5O_TYPE_DATASET:
            if ( matvar != NULL ) {
                matvar->internal->fieldnames[group_data->nfields] = strdup(name);
            }
            group_data->nfields++;
            break;
        default:
            /* Not possible to get here */
            break;
    }

    return 1;
}

static int
Mat_H5ReadNextReferenceInfo(hid_t ref_id, matvar_t *matvar, mat_t *mat)
{
    int err;
    if ( ref_id < 0 || matvar == NULL )
        return MATIO_E_NO_ERROR;

    switch ( H5Iget_type(ref_id) ) {
        case H5I_DATASET:
            err = Mat_H5ReadDatasetInfo(mat, matvar, ref_id);
            if ( matvar->internal->id != ref_id ) {
                /* Close dataset and increment count */
                H5Dclose(ref_id);
            }

            /*H5Dclose(ref_id);*/
            break;

        case H5I_GROUP:
            err = Mat_H5ReadGroupInfo(mat, matvar, ref_id);
            break;

        default:
            err = MATIO_E_NO_ERROR;
            break;
    }

    return err;
}

static int
Mat_H5ReadData(hid_t dset_id, hid_t h5_type, hid_t mem_space, hid_t dset_space, int isComplex,
               void *data)
{
    herr_t herr;

    if ( !isComplex ) {
        herr = H5Dread(dset_id, h5_type, mem_space, dset_space, H5P_DEFAULT, data);
        if ( herr < 0 ) {
            return MATIO_E_GENERIC_READ_ERROR;
        }
    } else {
        mat_complex_split_t *complex_data = (mat_complex_split_t *)data;
        hid_t h5_complex;
        size_t h5_size = H5Tget_size(h5_type);

        h5_complex = H5Tcreate(H5T_COMPOUND, h5_size);
        H5Tinsert(h5_complex, "real", 0, h5_type);
        herr = H5Dread(dset_id, h5_complex, mem_space, dset_space, H5P_DEFAULT, complex_data->Re);
        H5Tclose(h5_complex);
        if ( herr < 0 ) {
            return MATIO_E_GENERIC_READ_ERROR;
        }

        h5_complex = H5Tcreate(H5T_COMPOUND, h5_size);
        H5Tinsert(h5_complex, "imag", 0, h5_type);
        herr = H5Dread(dset_id, h5_complex, mem_space, dset_space, H5P_DEFAULT, complex_data->Im);
        H5Tclose(h5_complex);
        if ( herr < 0 ) {
            return MATIO_E_GENERIC_READ_ERROR;
        }
    }

    return MATIO_E_NO_ERROR;
}

static int
Mat_H5ReadNextReferenceData(hid_t ref_id, matvar_t *matvar, mat_t *mat)
{
    int err = MATIO_E_NO_ERROR;
    size_t nelems = 1;

    if ( ref_id < 0 || matvar == NULL )
        return err;

    /* If the datatype with references is a cell, we've already read info into
     * the variable data, so just loop over each cell element and call
     * Mat_H5ReadNextReferenceData on it.
     */
    if ( MAT_C_CELL == matvar->class_type ) {
        size_t i;
        matvar_t **cells;

        if ( NULL == matvar->data ) {
            return err;
        }
        err = Mat_MulDims(matvar, &nelems);
        if ( err ) {
            return err;
        }
        cells = (matvar_t **)matvar->data;
        for ( i = 0; i < nelems; i++ ) {
            if ( NULL != cells[i] ) {
                err = Mat_H5ReadNextReferenceData(cells[i]->internal->id, cells[i], mat);
            }
            if ( err ) {
                break;
            }
        }
        return err;
    }

    switch ( H5Iget_type(ref_id) ) {
        case H5I_DATASET: {
            hid_t data_type_id, dset_id;
            if ( MAT_C_CHAR == matvar->class_type ) {
                matvar->data_size = (int)Mat_SizeOf(matvar->data_type);
                data_type_id = DataType2H5T(matvar->data_type);
            } else if ( MAT_C_STRUCT == matvar->class_type ) {
                /* Empty structure array */
                break;
            } else {
                matvar->data_size = (int)Mat_SizeOfClass(matvar->class_type);
                data_type_id = ClassType2H5T(matvar->class_type);
            }

            err = Mat_MulDims(matvar, &nelems);
            err |= Mul(&matvar->nbytes, nelems, matvar->data_size);
            if ( err || matvar->nbytes < 1 ) {
                H5Dclose(ref_id);
                break;
            }

            dset_id = ref_id;

            if ( !matvar->isComplex ) {
                matvar->data = malloc(matvar->nbytes);
            } else {
                matvar->data = ComplexMalloc(matvar->nbytes);
            }
            if ( NULL != matvar->data ) {
                err = Mat_H5ReadData(dset_id, data_type_id, H5S_ALL, H5S_ALL, matvar->isComplex,
                                     matvar->data);
            }
            H5Dclose(dset_id);
            break;
        }
        case H5I_GROUP: {
            if ( MAT_C_SPARSE == matvar->class_type ) {
                err = Mat_VarRead73(mat, matvar);
            } else {
                matvar_t **fields;
                size_t i;

                if ( !matvar->nbytes || !matvar->data_size || NULL == matvar->data )
                    break;
                nelems = matvar->nbytes / matvar->data_size;
                fields = (matvar_t **)matvar->data;
                for ( i = 0; i < nelems; i++ ) {
                    if ( NULL != fields[i] && 0 < fields[i]->internal->hdf5_ref &&
                         -1 < fields[i]->internal->id ) {
                        /* Dataset of references */
                        err = Mat_H5ReadNextReferenceData(fields[i]->internal->id, fields[i], mat);
                    } else {
                        err = Mat_VarRead73(mat, fields[i]);
                    }
                    if ( err ) {
                        break;
                    }
                }
            }
            break;
        }
        default:
            break;
    }

    return err;
}

static int
Mat_H5WriteData(hid_t dset_id, hid_t h5_type, hid_t mem_space, hid_t dset_space, int isComplex,
                void *data)
{
    int err = MATIO_E_NO_ERROR;

    if ( !isComplex ) {
        if ( 0 > H5Dwrite(dset_id, h5_type, mem_space, dset_space, H5P_DEFAULT, data) )
            err = MATIO_E_GENERIC_WRITE_ERROR;
    } else {
        mat_complex_split_t *complex_data = (mat_complex_split_t *)data;
        hid_t h5_complex;
        size_t h5_size = H5Tget_size(h5_type);

        /* Write real part of dataset */
        h5_complex = H5Tcreate(H5T_COMPOUND, h5_size);
        H5Tinsert(h5_complex, "real", 0, h5_type);
        err = Mat_H5WriteData(dset_id, h5_complex, mem_space, dset_space, 0, complex_data->Re);
        H5Tclose(h5_complex);

        /* Write imaginary part of dataset */
        h5_complex = H5Tcreate(H5T_COMPOUND, h5_size);
        H5Tinsert(h5_complex, "imag", 0, h5_type);
        err += Mat_H5WriteData(dset_id, h5_complex, mem_space, dset_space, 0, complex_data->Im);
        H5Tclose(h5_complex);
    }

    return err;
}

static int
Mat_H5WriteAppendData(hid_t id, hid_t h5_type, int mrank, const char *name, const size_t *mdims,
                      hsize_t *dims, int dim, int isComplex, void *data)
{
    int err = MATIO_E_NO_ERROR;
    hid_t dset_id, space_id;
    int rank;

    if ( dim < 1 || dim > mrank )
        return MATIO_E_BAD_ARGUMENT;

    dset_id = H5Dopen(id, name, H5P_DEFAULT);
    space_id = H5Dget_space(dset_id);
    rank = H5Sget_simple_extent_ndims(space_id);
    if ( rank == mrank ) {
        hsize_t *size_offset_dims;
        size_offset_dims = (hsize_t *)malloc(rank * sizeof(*size_offset_dims));
        if ( NULL != size_offset_dims ) {
            hsize_t offset;
            hid_t mspace_id;
            int k;

            (void)H5Sget_simple_extent_dims(space_id, size_offset_dims, NULL);
            offset = size_offset_dims[rank - dim];
            size_offset_dims[rank - dim] += mdims[dim - 1];
            H5Dset_extent(dset_id, size_offset_dims);
            for ( k = 0; k < rank; k++ ) {
                size_offset_dims[k] = 0;
            }
            size_offset_dims[rank - dim] = offset;
            /* Need to reopen */
            H5Sclose(space_id);
            space_id = H5Dget_space(dset_id);
            H5Sselect_hyperslab(space_id, H5S_SELECT_SET, size_offset_dims, NULL, dims, NULL);
            free(size_offset_dims);
            mspace_id = H5Screate_simple(rank, dims, NULL);
            err = Mat_H5WriteData(dset_id, h5_type, mspace_id, space_id, isComplex, data);
            H5Sclose(mspace_id);
        } else {
            err = MATIO_E_OUT_OF_MEMORY;
        }
    } else {
        err = MATIO_E_GENERIC_WRITE_ERROR;
    }
    H5Sclose(space_id);
    H5Dclose(dset_id);

    return err;
}

static int
Mat_VarWriteRef(hid_t id, matvar_t *matvar, enum matio_compression compression, hid_t *refs_id,
                hobj_ref_t *ref)
{
    int err;
    herr_t herr;
    char obj_name[64];
    H5G_info_t group_info;

    group_info.nlinks = 0;
    herr = H5Gget_info(*refs_id, &group_info);
    if ( herr < 0 ) {
        err = MATIO_E_BAD_ARGUMENT;
    } else {
        sprintf(obj_name, "%llu", (unsigned long long)group_info.nlinks);
        if ( NULL != matvar )
            matvar->compression = compression;
        err = Mat_VarWriteNext73(*refs_id, matvar, obj_name, refs_id);
        sprintf(obj_name, "/#refs#/%llu", (unsigned long long)group_info.nlinks);
        H5Rcreate(ref, id, obj_name, H5R_OBJECT, -1);
    }
    return err;
}

static int
Mat_VarWriteEmpty(hid_t id, matvar_t *matvar, const char *name, const char *class_name)
{
    int err = MATIO_E_NO_ERROR;
    hsize_t rank = matvar->rank;
    unsigned empty = 1;
    hid_t mspace_id, dset_id, attr_type_id, aspace_id, attr_id;

    mspace_id = H5Screate_simple(1, &rank, NULL);
    dset_id =
        H5Dcreate(id, name, H5T_NATIVE_HSIZE, mspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    attr_type_id = H5Tcopy(H5T_C_S1);
    H5Tset_size(attr_type_id, strlen(class_name));
    aspace_id = H5Screate(H5S_SCALAR);
    attr_id = H5Acreate(dset_id, "MATLAB_class", attr_type_id, aspace_id, H5P_DEFAULT, H5P_DEFAULT);
    if ( 0 > H5Awrite(attr_id, attr_type_id, class_name) )
        err = MATIO_E_GENERIC_WRITE_ERROR;
    H5Sclose(aspace_id);
    H5Aclose(attr_id);
    H5Tclose(attr_type_id);

    if ( MATIO_E_NO_ERROR == err ) {
        if ( 0 == strcmp(class_name, "struct") ) {
            /* Write the fields attribute */
            hsize_t nfields = matvar->internal->num_fields;
            if ( nfields ) {
                hvl_t *fieldnames = (hvl_t *)malloc((size_t)nfields * sizeof(*fieldnames));
                if ( NULL != fieldnames ) {
                    hid_t str_type_id, fieldnames_id;
                    hsize_t k;

                    str_type_id = H5Tcopy(H5T_C_S1);
                    for ( k = 0; k < nfields; k++ ) {
                        fieldnames[k].len = strlen(matvar->internal->fieldnames[k]);
                        fieldnames[k].p = matvar->internal->fieldnames[k];
                    }
                    H5Tset_size(str_type_id, 1);
                    fieldnames_id = H5Tvlen_create(str_type_id);
                    aspace_id = H5Screate_simple(1, &nfields, NULL);
                    attr_id = H5Acreate(dset_id, "MATLAB_fields", fieldnames_id, aspace_id,
                                        H5P_DEFAULT, H5P_DEFAULT);
                    if ( 0 > H5Awrite(attr_id, fieldnames_id, fieldnames) )
                        err = MATIO_E_GENERIC_WRITE_ERROR;
                    H5Aclose(attr_id);
                    H5Sclose(aspace_id);
                    H5Tclose(fieldnames_id);
                    H5Tclose(str_type_id);
                    free(fieldnames);
                } else {
                    err = MATIO_E_OUT_OF_MEMORY;
                }
            }
        } else if ( 0 == strcmp(class_name, "logical") ) {
            /* Write the MATLAB_int_decode attribute */
            int int_decode = 1;
            aspace_id = H5Screate(H5S_SCALAR);
            attr_id = H5Acreate(dset_id, "MATLAB_int_decode", H5T_NATIVE_INT, aspace_id,
                                H5P_DEFAULT, H5P_DEFAULT);
            if ( 0 > H5Awrite(attr_id, H5T_NATIVE_INT, &int_decode) )
                err = MATIO_E_GENERIC_WRITE_ERROR;
            H5Sclose(aspace_id);
            H5Aclose(attr_id);
        }

        if ( MATIO_E_NO_ERROR == err ) {
            /* Write the empty attribute */
            aspace_id = H5Screate(H5S_SCALAR);
            attr_id = H5Acreate(dset_id, "MATLAB_empty", H5T_NATIVE_UINT, aspace_id, H5P_DEFAULT,
                                H5P_DEFAULT);
            if ( 0 > H5Awrite(attr_id, H5T_NATIVE_UINT, &empty) )
                err = MATIO_E_GENERIC_WRITE_ERROR;
            H5Sclose(aspace_id);
            H5Aclose(attr_id);
        }

        if ( MATIO_E_NO_ERROR == err ) {
            /* Write the dimensions as the data */
            if ( 0 >
                 H5Dwrite(dset_id, SizeType2H5T(), H5S_ALL, H5S_ALL, H5P_DEFAULT, matvar->dims) )
                err = MATIO_E_GENERIC_WRITE_ERROR;
        }
    }
    H5Dclose(dset_id);
    H5Sclose(mspace_id);

    return err;
}

/** @if mat_devman
 * @brief Writes a cell array matlab variable to the specified HDF id with the
 *        given name
 *
 * @ingroup mat_internal
 * @param id HDF id of the parent object
 * @param matvar pointer to the cell array variable
 * @param name Name of the HDF dataset
 * @param refs_id pointer to the id of the /#refs# group in HDF5
 * @param dims array of permuted dimensions
 * @retval 0 on success
 * @endif
 */
static int
Mat_VarWriteCell73(hid_t id, matvar_t *matvar, const char *name, hid_t *refs_id, hsize_t *dims)
{
    int k;
    hsize_t nelems = 1, l;
    matvar_t **cells;
    int err = MATIO_E_NO_ERROR;

    cells = (matvar_t **)matvar->data;
    for ( k = 0; k < matvar->rank; k++ )
        nelems *= dims[k];

    if ( 0 == nelems || NULL == matvar->data ) {
        err = Mat_VarWriteEmpty(id, matvar, name, ClassNames[matvar->class_type]);
    } else {
        if ( *refs_id < 0 ) {
            if ( H5Lexists(id, "/#refs#", H5P_DEFAULT) ) {
                *refs_id = H5Gopen(id, "/#refs#", H5P_DEFAULT);
            } else {
                *refs_id = H5Gcreate(id, "/#refs#", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
        }
        if ( *refs_id > -1 ) {
            hobj_ref_t *refs = (hobj_ref_t *)malloc((size_t)nelems * sizeof(*refs));
            if ( NULL != refs ) {
                hid_t mspace_id = H5Screate_simple(matvar->rank, dims, NULL);
                hid_t dset_id = H5Dcreate(id, name, H5T_STD_REF_OBJ, mspace_id, H5P_DEFAULT,
                                          H5P_DEFAULT, H5P_DEFAULT);

                for ( l = 0; l < nelems; l++ ) {
                    err = Mat_VarWriteRef(id, cells[l], matvar->compression, refs_id, refs + l);
                    if ( err )
                        break;
                }
                if ( MATIO_E_NO_ERROR == err ) {
                    err = Mat_H5WriteData(dset_id, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, 0, refs);

                    if ( MATIO_E_NO_ERROR == err ) {
                        hid_t attr_id, aspace_id;
                        hid_t str_type_id = H5Tcopy(H5T_C_S1);
                        H5Tset_size(str_type_id, 4);
                        aspace_id = H5Screate(H5S_SCALAR);
                        attr_id = H5Acreate(dset_id, "MATLAB_class", str_type_id, aspace_id,
                                            H5P_DEFAULT, H5P_DEFAULT);
                        if ( 0 > H5Awrite(attr_id, str_type_id, "cell") )
                            err = MATIO_E_GENERIC_WRITE_ERROR;
                        H5Aclose(attr_id);
                        H5Sclose(aspace_id);
                        H5Tclose(str_type_id);
                    }
                }
                H5Dclose(dset_id);
                free(refs);
                H5Sclose(mspace_id);
            } else {
                err = MATIO_E_OUT_OF_MEMORY;
            }
        } else {
            err = MATIO_E_OUTPUT_BAD_DATA;
        }
    }

    return err;
}

/** @if mat_devman
 * @brief Writes a character matlab variable to the specified HDF id with the
 *        given name
 *
 * @ingroup mat_internal
 * @param id HDF id of the parent object
 * @param matvar pointer to the character variable
 * @param name Name of the HDF dataset
 * @param dims array of permuted dimensions
 * @retval 0 on success
 * @endif
 */
static int
Mat_VarWriteChar73(hid_t id, matvar_t *matvar, const char *name, hsize_t *dims)
{
    int err = MATIO_E_NO_ERROR, k;
    hsize_t nelems = 1;
    mat_uint16_t *u16 = NULL;
    hid_t h5type;

    for ( k = 0; k < matvar->rank; k++ ) {
        nelems *= dims[k];
    }

    if ( 0 == nelems || NULL == matvar->data ) {
        err = Mat_VarWriteEmpty(id, matvar, name, ClassNames[matvar->class_type]);
    } else {
        int matlab_int_decode = 2;
        hid_t mspace_id, dset_id, attr_type_id, attr_id, aspace_id;

        mspace_id = H5Screate_simple(matvar->rank, dims, NULL);
        switch ( matvar->data_type ) {
            case MAT_T_UTF32:
            case MAT_T_INT32:
            case MAT_T_UINT32:
                /* Not sure matlab will actually handle this */
                dset_id = H5Dcreate(id, name, ClassType2H5T(MAT_C_UINT32), mspace_id, H5P_DEFAULT,
                                    H5P_DEFAULT, H5P_DEFAULT);
                break;
            case MAT_T_UTF16:
            case MAT_T_UTF8:
            case MAT_T_INT16:
            case MAT_T_UINT16:
            case MAT_T_INT8:
            case MAT_T_UINT8:
                dset_id = H5Dcreate(id, name, ClassType2H5T(MAT_C_UINT16), mspace_id, H5P_DEFAULT,
                                    H5P_DEFAULT, H5P_DEFAULT);
                break;
            default:
                H5Sclose(mspace_id);
                return MATIO_E_OUTPUT_BAD_DATA;
        }
        attr_type_id = H5Tcopy(H5T_C_S1);
        H5Tset_size(attr_type_id, strlen(ClassNames[matvar->class_type]));
        aspace_id = H5Screate(H5S_SCALAR);
        attr_id =
            H5Acreate(dset_id, "MATLAB_class", attr_type_id, aspace_id, H5P_DEFAULT, H5P_DEFAULT);
        if ( 0 > H5Awrite(attr_id, attr_type_id, ClassNames[matvar->class_type]) )
            err = MATIO_E_GENERIC_WRITE_ERROR;
        H5Aclose(attr_id);
        H5Tclose(attr_type_id);

        if ( MATIO_E_NO_ERROR == err ) {
            attr_type_id = H5Tcopy(H5T_NATIVE_INT);
            attr_id = H5Acreate(dset_id, "MATLAB_int_decode", attr_type_id, aspace_id, H5P_DEFAULT,
                                H5P_DEFAULT);
            if ( 0 > H5Awrite(attr_id, attr_type_id, &matlab_int_decode) )
                err = MATIO_E_GENERIC_WRITE_ERROR;
            H5Aclose(attr_id);
            H5Tclose(attr_type_id);
        }
        H5Sclose(aspace_id);

        h5type = DataType2H5T(matvar->data_type);
        if ( MATIO_E_NO_ERROR == err && matvar->data_type == MAT_T_UTF8 ) {
            /* Convert to UTF-16 */
            h5type = H5T_NATIVE_UINT16;
            u16 = (mat_uint16_t *)calloc(nelems, sizeof(mat_uint16_t));
            if ( u16 != NULL ) {
                mat_uint8_t *data = (mat_uint8_t *)matvar->data;
                size_t i, j = 0;
                for ( i = 0; i < matvar->nbytes; i++ ) {
                    const mat_uint8_t c = data[i];
                    if ( c <= 0x7F ) { /* ASCII */
                        u16[j] = (mat_uint16_t)c;
                    } else if ( c < 0xE0 && i + 1 < matvar->nbytes ) { /* Extended ASCII */
                        const mat_uint16_t _a = (mat_uint16_t)(c & 0x1F);
                        const mat_uint16_t _b = (mat_uint16_t)(data[i + 1] & 0x3F);
                        u16[j] = (_a << 6) | _b;
                        i = i + 1;
                    } else if ( (c & 0xF0) == 0xE0 && i + 2 < matvar->nbytes ) { /* BMP */
                        const mat_uint16_t _a = (mat_uint16_t)(c & 0xF);
                        const mat_uint16_t _b = (mat_uint16_t)(data[i + 1] & 0x3C) >> 2;
                        const mat_uint16_t _c = (mat_uint16_t)(data[i + 1] & 0x3);
                        const mat_uint16_t _d = (mat_uint16_t)(data[i + 2] & 0x30) >> 4;
                        const mat_uint16_t _e = (mat_uint16_t)(data[i + 2] & 0xF);
                        u16[j] = (_a << 12) | (_b << 8) | (_c << 6) | (_d << 4) | _e;
                        i = i + 2;
                    } else { /* Full UTF-8 */
                        err = MATIO_E_OPERATION_NOT_SUPPORTED;
                        break;
                    }
                    j++;
                }
            } else {
                err = MATIO_E_OUT_OF_MEMORY;
            }
        }

        if ( MATIO_E_NO_ERROR == err ) {
            void *data = matvar->data_type == MAT_T_UTF8 ? u16 : matvar->data;
            if ( 0 > H5Dwrite(dset_id, h5type, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void *)data) )
                err = MATIO_E_GENERIC_WRITE_ERROR;
        }
        free(u16);
        H5Dclose(dset_id);
        H5Sclose(mspace_id);
    }

    return err;
}

static int
Mat_WriteEmptyVariable73(hid_t id, const char *name, hsize_t rank, size_t *dims)
{
    int err = MATIO_E_NO_ERROR;
    unsigned empty = 1;
    hid_t mspace_id, dset_id;

    mspace_id = H5Screate_simple(1, &rank, NULL);
    dset_id =
        H5Dcreate(id, name, H5T_NATIVE_HSIZE, mspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if ( dset_id > -1 ) {
        hid_t attr_type_id, attr_id, aspace_id;

        attr_type_id = H5Tcopy(H5T_C_S1);
        H5Tset_size(attr_type_id, 6);
        aspace_id = H5Screate(H5S_SCALAR);
        attr_id =
            H5Acreate(dset_id, "MATLAB_class", attr_type_id, aspace_id, H5P_DEFAULT, H5P_DEFAULT);
        if ( 0 > H5Awrite(attr_id, attr_type_id, "double") )
            err = MATIO_E_GENERIC_WRITE_ERROR;
        H5Sclose(aspace_id);
        H5Aclose(attr_id);
        H5Tclose(attr_type_id);

        if ( MATIO_E_NO_ERROR == err ) {
            aspace_id = H5Screate(H5S_SCALAR);
            attr_id = H5Acreate(dset_id, "MATLAB_empty", H5T_NATIVE_UINT, aspace_id, H5P_DEFAULT,
                                H5P_DEFAULT);
            if ( 0 > H5Awrite(attr_id, H5T_NATIVE_UINT, &empty) )
                err = MATIO_E_GENERIC_WRITE_ERROR;
            H5Sclose(aspace_id);
            H5Aclose(attr_id);
        }

        if ( MATIO_E_NO_ERROR == err ) {
            /* Write the dimensions as the data */
            if ( 0 > H5Dwrite(dset_id, SizeType2H5T(), H5S_ALL, H5S_ALL, H5P_DEFAULT, dims) )
                err = MATIO_E_GENERIC_WRITE_ERROR;
        }
        H5Dclose(dset_id);
    } else {
        err = MATIO_E_OUTPUT_BAD_DATA;
    }
    H5Sclose(mspace_id);

    return err;
}

/** @if mat_devman
 * @brief Writes a logical matlab variable to the specified HDF id with the
 *        given name
 *
 * @ingroup mat_internal
 * @param id HDF id of the parent object
 * @param matvar pointer to the logical variable
 * @param name Name of the HDF dataset
 * @param dims array of permuted dimensions
 * @retval 0 on success
 * @endif
 */
static int
Mat_VarWriteLogical73(hid_t id, matvar_t *matvar, const char *name, hsize_t *dims)
{
    int err = MATIO_E_NO_ERROR, k;
    hsize_t nelems = 1;
    hid_t plist;

    for ( k = 0; k < matvar->rank; k++ ) {
        nelems *= dims[k];
    }

    if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
        plist = H5Pcreate(H5P_DATASET_CREATE);
        if ( MAX_RANK >= matvar->rank ) {
            hsize_t chunk_dims[MAX_RANK];
            Mat_H5GetChunkSize(matvar->rank, dims, chunk_dims);
            H5Pset_chunk(plist, matvar->rank, chunk_dims);
        } else {
            hsize_t *chunk_dims = (hsize_t *)malloc(matvar->rank * sizeof(hsize_t));
            if ( NULL != chunk_dims ) {
                Mat_H5GetChunkSize(matvar->rank, dims, chunk_dims);
                H5Pset_chunk(plist, matvar->rank, chunk_dims);
                free(chunk_dims);
            } else {
                H5Pclose(plist);
                return MATIO_E_OUT_OF_MEMORY;
            }
        }
        H5Pset_deflate(plist, 9);
    } else {
        plist = H5P_DEFAULT;
    }

    if ( 0 == nelems || NULL == matvar->data ) {
        err = Mat_VarWriteEmpty(id, matvar, name, "logical");
    } else {
        int int_decode = 1;
        hid_t mspace_id, dset_id, attr_type_id, attr_id, aspace_id;

        mspace_id = H5Screate_simple(matvar->rank, dims, NULL);
        /* Note that MATLAB only recognizes uint8 as logical */
        dset_id = H5Dcreate(id, name, ClassType2H5T(MAT_C_UINT8), mspace_id, H5P_DEFAULT, plist,
                            H5P_DEFAULT);
        attr_type_id = H5Tcopy(H5T_C_S1);
        H5Tset_size(attr_type_id, 7);
        aspace_id = H5Screate(H5S_SCALAR);
        attr_id =
            H5Acreate(dset_id, "MATLAB_class", attr_type_id, aspace_id, H5P_DEFAULT, H5P_DEFAULT);
        if ( 0 > H5Awrite(attr_id, attr_type_id, "logical") )
            err = MATIO_E_GENERIC_WRITE_ERROR;
        H5Sclose(aspace_id);
        H5Aclose(attr_id);
        H5Tclose(attr_type_id);

        if ( MATIO_E_NO_ERROR == err ) {
            /* Write the MATLAB_int_decode attribute */
            aspace_id = H5Screate(H5S_SCALAR);
            attr_id = H5Acreate(dset_id, "MATLAB_int_decode", H5T_NATIVE_INT, aspace_id,
                                H5P_DEFAULT, H5P_DEFAULT);
            if ( 0 > H5Awrite(attr_id, H5T_NATIVE_INT, &int_decode) )
                err = MATIO_E_GENERIC_WRITE_ERROR;
            H5Sclose(aspace_id);
            H5Aclose(attr_id);
        }

        if ( MATIO_E_NO_ERROR == err ) {
            if ( 0 > H5Dwrite(dset_id, DataType2H5T(matvar->data_type), H5S_ALL, H5S_ALL,
                              H5P_DEFAULT, matvar->data) )
                err = MATIO_E_GENERIC_WRITE_ERROR;
        }
        H5Dclose(dset_id);
        H5Sclose(mspace_id);
    }

    if ( H5P_DEFAULT != plist )
        H5Pclose(plist);

    return err;
}

/** @if mat_devman
 * @brief Writes a numeric matlab variable to the specified HDF id with the
 *        given name
 *
 * @ingroup mat_internal
 * @param id HDF id of the parent object
 * @param matvar pointer to the numeric variable
 * @param name Name of the HDF dataset
 * @param dims array of permuted dimensions
 * @param max_dims maximum dimensions
 * @retval 0 on success
 * @endif
 */
static int
Mat_VarWriteNumeric73(hid_t id, matvar_t *matvar, const char *name, hsize_t *dims,
                      hsize_t *max_dims)
{
    int err = MATIO_E_NO_ERROR, k;
    hsize_t nelems = 1;
    hid_t plist;

    for ( k = 0; k < matvar->rank; k++ ) {
        nelems *= dims[k];
    }

    if ( matvar->compression || NULL != max_dims ) {
        plist = H5Pcreate(H5P_DATASET_CREATE);
        if ( MAX_RANK >= matvar->rank ) {
            hsize_t chunk_dims[MAX_RANK];
            Mat_H5GetChunkSize(matvar->rank, dims, chunk_dims);
            H5Pset_chunk(plist, matvar->rank, chunk_dims);
        } else {
            hsize_t *chunk_dims = (hsize_t *)malloc(matvar->rank * sizeof(hsize_t));
            if ( NULL != chunk_dims ) {
                Mat_H5GetChunkSize(matvar->rank, dims, chunk_dims);
                H5Pset_chunk(plist, matvar->rank, chunk_dims);
                free(chunk_dims);
            } else {
                H5Pclose(plist);
                return MATIO_E_OUT_OF_MEMORY;
            }
        }
        if ( matvar->compression == MAT_COMPRESSION_ZLIB )
            H5Pset_deflate(plist, 9);
    } else {
        plist = H5P_DEFAULT;
    }

    if ( 0 == nelems || NULL == matvar->data ) {
        err = Mat_VarWriteEmpty(id, matvar, name, ClassNames[matvar->class_type]);
    } else {
        hid_t mspace_id, dset_id, attr_type_id, attr_id, aspace_id;
        hid_t h5_type = ClassType2H5T(matvar->class_type);
        hid_t h5_dtype = DataType(h5_type, matvar->isComplex);

        mspace_id = H5Screate_simple(matvar->rank, dims, max_dims);
        dset_id = H5Dcreate(id, name, h5_dtype, mspace_id, H5P_DEFAULT, plist, H5P_DEFAULT);
        attr_type_id = H5Tcopy(H5T_C_S1);
        H5Tset_size(attr_type_id, strlen(ClassNames[matvar->class_type]));
        aspace_id = H5Screate(H5S_SCALAR);
        attr_id =
            H5Acreate(dset_id, "MATLAB_class", attr_type_id, aspace_id, H5P_DEFAULT, H5P_DEFAULT);
        if ( 0 > H5Awrite(attr_id, attr_type_id, ClassNames[matvar->class_type]) )
            err = MATIO_E_GENERIC_WRITE_ERROR;
        H5Sclose(aspace_id);
        H5Aclose(attr_id);
        H5Tclose(attr_type_id);
        H5Tclose(h5_dtype);
        if ( MATIO_E_NO_ERROR == err ) {
            err = Mat_H5WriteData(dset_id, h5_type, H5S_ALL, H5S_ALL, matvar->isComplex,
                                  matvar->data);
        }
        H5Dclose(dset_id);
        H5Sclose(mspace_id);
    }

    if ( H5P_DEFAULT != plist )
        H5Pclose(plist);

    return err;
}

/** @if mat_devman
 * @brief Writes/appends a numeric matlab variable to the specified HDF id with the
 *        given name
 *
 * @ingroup mat_internal
 * @param id HDF id of the parent object
 * @param matvar pointer to the numeric variable
 * @param name Name of the HDF dataset
 * @param dims array of permuted dimensions
 * @param dim dimension to append data
 * @retval 0 on success
 * @endif
 */
static int
Mat_VarWriteAppendNumeric73(hid_t id, matvar_t *matvar, const char *name, hsize_t *dims, int dim)
{
    int err = MATIO_E_NO_ERROR, k;
    hsize_t nelems = 1;

    for ( k = 0; k < matvar->rank; k++ ) {
        nelems *= dims[k];
    }

    if ( 0 != nelems && NULL != matvar->data ) {
        if ( H5Lexists(id, matvar->name, H5P_DEFAULT) ) {
            err = Mat_H5WriteAppendData(id, ClassType2H5T(matvar->class_type), matvar->rank,
                                        matvar->name, matvar->dims, dims, dim, matvar->isComplex,
                                        matvar->data);
        } else {
            /* Create with unlimited number of dimensions */
            if ( MAX_RANK >= matvar->rank ) {
                hsize_t max_dims[MAX_RANK];
                for ( k = 0; k < matvar->rank; k++ ) {
                    max_dims[k] = H5S_UNLIMITED;
                }
                err = Mat_VarWriteNumeric73(id, matvar, name, dims, max_dims);
            } else {
                hsize_t *max_dims = (hsize_t *)malloc(matvar->rank * sizeof(hsize_t));
                if ( NULL != max_dims ) {
                    for ( k = 0; k < matvar->rank; k++ ) {
                        max_dims[k] = H5S_UNLIMITED;
                    }
                    err = Mat_VarWriteNumeric73(id, matvar, name, dims, max_dims);
                    free(max_dims);
                } else {
                    err = MATIO_E_OUT_OF_MEMORY;
                }
            }
        }
    } else {
        err = MATIO_E_OUTPUT_BAD_DATA;
    }

    return err;
}

/** @if mat_devman
 * @brief Writes a sparse matrix variable to the specified HDF id with the
 *        given name
 *
 * @ingroup mat_internal
 * @param id HDF id of the parent object
 * @param matvar pointer to the structure variable
 * @param name Name of the HDF dataset
 * @retval 0 on success
 * @endif
 */
static int
Mat_VarWriteSparse73(hid_t id, matvar_t *matvar, const char *name)
{
    int err = MATIO_E_NO_ERROR;
    hid_t sparse_id;

    sparse_id = H5Gcreate(id, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if ( sparse_id < 0 ) {
        Mat_Critical("Error creating group for sparse array %s", matvar->name);
        err = MATIO_E_OUTPUT_BAD_DATA;
    } else {
        hid_t size_type_id, h5_type, h5_dtype;
        hid_t mspace_id, dset_id, attr_type_id, attr_id, aspace_id;
        mat_sparse_t *sparse;
        hsize_t nir, njc, ndata;
        mat_uint64_t sparse_attr_value;
        enum matio_classes class_type;

        sparse = (mat_sparse_t *)matvar->data;
        class_type = DataType2ClassType(matvar->data_type);
        attr_type_id = H5Tcopy(H5T_C_S1);
        H5Tset_size(attr_type_id, matvar->isLogical ? 7 : strlen(ClassNames[class_type]));
        aspace_id = H5Screate(H5S_SCALAR);
        attr_id =
            H5Acreate(sparse_id, "MATLAB_class", attr_type_id, aspace_id, H5P_DEFAULT, H5P_DEFAULT);
        if ( 0 > H5Awrite(attr_id, attr_type_id,
                          matvar->isLogical ? "logical" : ClassNames[class_type]) )
            err = MATIO_E_GENERIC_WRITE_ERROR;
        H5Sclose(aspace_id);
        H5Aclose(attr_id);
        H5Tclose(attr_type_id);

        if ( MATIO_E_NO_ERROR == err ) {
            if ( matvar->isLogical ) {
                /* Write the MATLAB_int_decode attribute */
                int int_decode = 1;
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(sparse_id, "MATLAB_int_decode", H5T_NATIVE_INT, aspace_id,
                                    H5P_DEFAULT, H5P_DEFAULT);
                if ( 0 > H5Awrite(attr_id, H5T_NATIVE_INT, &int_decode) )
                    err = MATIO_E_GENERIC_WRITE_ERROR;
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
            }
        }

        if ( MATIO_E_NO_ERROR == err ) {
            sparse_attr_value = matvar->dims[0];
            size_type_id = ClassType2H5T(MAT_C_UINT64);
            aspace_id = H5Screate(H5S_SCALAR);
            attr_id = H5Acreate(sparse_id, "MATLAB_sparse", size_type_id, aspace_id, H5P_DEFAULT,
                                H5P_DEFAULT);
            if ( 0 > H5Awrite(attr_id, size_type_id, &sparse_attr_value) )
                err = MATIO_E_GENERIC_WRITE_ERROR;
            H5Sclose(aspace_id);
            H5Aclose(attr_id);
        }

        if ( MATIO_E_NO_ERROR == err ) {
            ndata = sparse->ndata;
            h5_type = DataType2H5T(matvar->data_type);
            h5_dtype = DataType(h5_type, matvar->isComplex);
            mspace_id = H5Screate_simple(1, &ndata, NULL);
            dset_id = H5Dcreate(sparse_id, "data", h5_dtype, mspace_id, H5P_DEFAULT, H5P_DEFAULT,
                                H5P_DEFAULT);
            H5Tclose(h5_dtype);
            err = Mat_H5WriteData(dset_id, h5_type, H5S_ALL, H5S_ALL, matvar->isComplex,
                                  sparse->data);
            H5Dclose(dset_id);
            H5Sclose(mspace_id);
        }

        if ( MATIO_E_NO_ERROR == err ) {
            nir = sparse->nir;
            mspace_id = H5Screate_simple(1, &nir, NULL);
            dset_id = H5Dcreate(sparse_id, "ir", size_type_id, mspace_id, H5P_DEFAULT, H5P_DEFAULT,
                                H5P_DEFAULT);
            err = Mat_H5WriteData(dset_id, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, 0, sparse->ir);
            H5Dclose(dset_id);
            H5Sclose(mspace_id);
        }

        if ( MATIO_E_NO_ERROR == err ) {
            njc = sparse->njc;
            mspace_id = H5Screate_simple(1, &njc, NULL);
            dset_id = H5Dcreate(sparse_id, "jc", size_type_id, mspace_id, H5P_DEFAULT, H5P_DEFAULT,
                                H5P_DEFAULT);
            err = Mat_H5WriteData(dset_id, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, 0, sparse->jc);
            H5Dclose(dset_id);
            H5Sclose(mspace_id);
        }
        H5Gclose(sparse_id);
    }

    return err;
}

/** @if mat_devman
 * @brief Writes a structure matlab variable to the specified HDF id with the
 *        given name
 *
 * @ingroup mat_internal
 * @param id HDF id of the parent object
 * @param matvar pointer to the structure variable
 * @param name Name of the HDF dataset
 * @param refs_id pointer to the id of the /#refs# group in HDF5
 * @param dims array of permuted dimensions
 * @param max_dims maximum dimensions
 * @retval 0 on success
 * @endif
 */
static int
Mat_VarWriteStruct73(hid_t id, matvar_t *matvar, const char *name, hid_t *refs_id, hsize_t *dims,
                     hsize_t *max_dims)
{
    int err;
    hsize_t nelems;

    {
        size_t tmp = 1;
        err = Mat_MulDims(matvar, &tmp);
        nelems = (hsize_t)tmp;
    }

    if ( err || 0 == nelems || NULL == matvar->data ) {
        err = Mat_VarWriteEmpty(id, matvar, name, ClassNames[matvar->class_type]);
    } else {
        hid_t struct_id = H5Gcreate(id, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if ( struct_id < 0 ) {
            Mat_Critical("Error creating group for struct %s", name);
            err = MATIO_E_OUTPUT_BAD_DATA;
        } else {
            hid_t attr_id, aspace_id;
            hid_t str_type_id;
            matvar_t **fields = (matvar_t **)matvar->data;
            hsize_t nfields = matvar->internal->num_fields, k;

            str_type_id = H5Tcopy(H5T_C_S1);
            H5Tset_size(str_type_id, 6);
            aspace_id = H5Screate(H5S_SCALAR);
            attr_id = H5Acreate(struct_id, "MATLAB_class", str_type_id, aspace_id, H5P_DEFAULT,
                                H5P_DEFAULT);
            if ( 0 > H5Awrite(attr_id, str_type_id, "struct") )
                err = MATIO_E_GENERIC_WRITE_ERROR;
            H5Aclose(attr_id);
            H5Sclose(aspace_id);

            /* Structure with no fields */
            if ( nfields == 0 ) {
                H5Gclose(struct_id);
                H5Tclose(str_type_id);
                return err;
            }

            if ( MATIO_E_NO_ERROR == err ) {
                hvl_t *fieldnames = (hvl_t *)malloc((size_t)nfields * sizeof(*fieldnames));
                if ( NULL != fieldnames ) {
                    hid_t fieldnames_id;
                    for ( k = 0; k < nfields; k++ ) {
                        fieldnames[k].len = strlen(matvar->internal->fieldnames[k]);
                        fieldnames[k].p = matvar->internal->fieldnames[k];
                    }
                    H5Tset_size(str_type_id, 1);
                    fieldnames_id = H5Tvlen_create(str_type_id);
                    aspace_id = H5Screate_simple(1, &nfields, NULL);
                    attr_id = H5Acreate(struct_id, "MATLAB_fields", fieldnames_id, aspace_id,
                                        H5P_DEFAULT, H5P_DEFAULT);
                    if ( 0 > H5Awrite(attr_id, fieldnames_id, fieldnames) )
                        err = MATIO_E_GENERIC_WRITE_ERROR;
                    H5Aclose(attr_id);
                    H5Sclose(aspace_id);
                    H5Tclose(fieldnames_id);
                    H5Tclose(str_type_id);
                    free(fieldnames);
                } else {
                    err = MATIO_E_OUT_OF_MEMORY;
                }
            }

            if ( MATIO_E_NO_ERROR == err ) {
                if ( 1 == nelems && NULL == max_dims ) {
                    for ( k = 0; k < nfields; k++ ) {
                        if ( NULL != fields[k] )
                            fields[k]->compression = matvar->compression;
                        err = Mat_VarWriteNext73(struct_id, fields[k],
                                                 matvar->internal->fieldnames[k], refs_id);
                    }
                } else {
                    if ( *refs_id < 0 ) {
                        if ( H5Lexists(id, "/#refs#", H5P_DEFAULT) ) {
                            *refs_id = H5Gopen(id, "/#refs#", H5P_DEFAULT);
                        } else {
                            *refs_id =
                                H5Gcreate(id, "/#refs#", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
                        }
                    }
                    if ( *refs_id > -1 ) {
                        hid_t plist = H5P_DEFAULT;
                        hobj_ref_t **refs = (hobj_ref_t **)calloc((size_t)nfields, sizeof(*refs));
                        if ( NULL != refs ) {
                            hsize_t l;
                            for ( l = 0; l < nfields; l++ ) {
                                refs[l] = (hobj_ref_t *)calloc((size_t)nelems, sizeof(*refs[l]));
                                if ( NULL == refs[l] ) {
                                    err = MATIO_E_OUT_OF_MEMORY;
                                    break;
                                }
                            }

                            if ( MATIO_E_NO_ERROR == err ) {
                                for ( k = 0; k < nelems; k++ ) {
                                    for ( l = 0; l < nfields; l++ ) {
                                        err = Mat_VarWriteRef(id, fields[k * nfields + l],
                                                              matvar->compression, refs_id,
                                                              refs[l] + k);
                                        if ( err )
                                            break;
                                    }
                                    if ( err )
                                        break;
                                }
                            }

                            if ( MATIO_E_NO_ERROR == err ) {
                                if ( NULL != max_dims ) {
                                    plist = H5Pcreate(H5P_DATASET_CREATE);
                                    if ( MAX_RANK >= matvar->rank ) {
                                        hsize_t chunk_dims[MAX_RANK];
                                        Mat_H5GetChunkSize(matvar->rank, dims, chunk_dims);
                                        H5Pset_chunk(plist, matvar->rank, chunk_dims);
                                    } else {
                                        hsize_t *chunk_dims =
                                            (hsize_t *)malloc(matvar->rank * sizeof(hsize_t));
                                        if ( NULL != chunk_dims ) {
                                            Mat_H5GetChunkSize(matvar->rank, dims, chunk_dims);
                                            H5Pset_chunk(plist, matvar->rank, chunk_dims);
                                            free(chunk_dims);
                                        } else {
                                            H5Pclose(plist);
                                            plist = H5P_DEFAULT;
                                            err = MATIO_E_OUT_OF_MEMORY;
                                        }
                                    }
                                } else {
                                    plist = H5P_DEFAULT;
                                }
                            }

                            if ( MATIO_E_NO_ERROR == err ) {
                                hid_t mspace_id = H5Screate_simple(matvar->rank, dims, max_dims);
                                for ( l = 0; l < nfields; l++ ) {
                                    hid_t dset_id = H5Dcreate(
                                        struct_id, matvar->internal->fieldnames[l], H5T_STD_REF_OBJ,
                                        mspace_id, H5P_DEFAULT, plist, H5P_DEFAULT);
                                    err = Mat_H5WriteData(dset_id, H5T_STD_REF_OBJ, H5S_ALL,
                                                          H5S_ALL, 0, refs[l]);
                                    H5Dclose(dset_id);
                                    if ( err )
                                        break;
                                }
                                H5Sclose(mspace_id);
                            }
                            for ( l = 0; l < nfields; l++ )
                                free(refs[l]);
                            free(refs);
                        } else {
                            err = MATIO_E_OUT_OF_MEMORY;
                        }
                        if ( H5P_DEFAULT != plist )
                            H5Pclose(plist);
                    } else {
                        err = MATIO_E_OUTPUT_BAD_DATA;
                    }
                }
            }
            H5Gclose(struct_id);
        }
    }

    return err;
}

static int
Mat_VarWriteAppendStruct73(hid_t id, matvar_t *matvar, const char *name, hid_t *refs_id,
                           hsize_t *dims, int dim)
{
    int err = MATIO_E_NO_ERROR;
    hsize_t nelems = 1;

    {
        int k;
        for ( k = 0; k < matvar->rank; k++ ) {
            nelems *= dims[k];
        }
    }

    if ( 0 != nelems && NULL != matvar->data ) {
        if ( H5Lexists(id, name, H5P_DEFAULT) ) {
            hobj_ref_t **refs;
            hsize_t nfields = matvar->internal->num_fields;
            matvar_t **fields = (matvar_t **)matvar->data;

            if ( *refs_id <= -1 )
                return MATIO_E_OUTPUT_BAD_DATA;

            refs = (hobj_ref_t **)calloc((size_t)nfields, sizeof(*refs));
            if ( NULL != refs ) {
                hsize_t l;
                for ( l = 0; l < nfields; l++ ) {
                    refs[l] = (hobj_ref_t *)calloc((size_t)nelems, sizeof(*refs[l]));
                    if ( NULL == refs[l] ) {
                        err = MATIO_E_OUT_OF_MEMORY;
                        break;
                    }
                }

                if ( MATIO_E_NO_ERROR == err ) {
                    hsize_t k;
                    for ( k = 0; k < nelems; k++ ) {
                        for ( l = 0; l < nfields; l++ ) {
                            err = Mat_VarWriteRef(id, fields[k * nfields + l], matvar->compression,
                                                  refs_id, refs[l] + k);
                            if ( err )
                                break;
                        }
                        if ( err )
                            break;
                    }
                }

                if ( MATIO_E_NO_ERROR == err ) {
                    hid_t struct_id = H5Gopen(id, name, H5P_DEFAULT);
                    for ( l = 0; l < nfields; l++ ) {
                        err = Mat_H5WriteAppendData(struct_id, H5T_STD_REF_OBJ, matvar->rank,
                                                    matvar->internal->fieldnames[l], matvar->dims,
                                                    dims, dim, 0, refs[l]);
                        if ( err )
                            break;
                    }
                    H5Gclose(struct_id);
                }
                for ( l = 0; l < nfields; l++ )
                    free(refs[l]);
                free(refs);
            }
        } else {
            /* Create with unlimited number of dimensions */
            if ( MAX_RANK >= matvar->rank ) {
                hsize_t max_dims[MAX_RANK];
                int k;
                for ( k = 0; k < matvar->rank; k++ ) {
                    max_dims[k] = H5S_UNLIMITED;
                }
                err = Mat_VarWriteStruct73(id, matvar, name, refs_id, dims, max_dims);
            } else {
                hsize_t *max_dims = (hsize_t *)malloc(matvar->rank * sizeof(hsize_t));
                if ( NULL != max_dims ) {
                    int k;
                    for ( k = 0; k < matvar->rank; k++ ) {
                        max_dims[k] = H5S_UNLIMITED;
                    }
                    err = Mat_VarWriteStruct73(id, matvar, name, refs_id, dims, max_dims);
                    free(max_dims);
                } else {
                    err = MATIO_E_OUT_OF_MEMORY;
                }
            }
        }
    }

    return err;
}

static int
Mat_VarWriteNext73(hid_t id, matvar_t *matvar, const char *name, hid_t *refs_id)
{
    int err;

    if ( NULL == matvar ) {
        size_t dims[2] = {0, 0};
        return Mat_WriteEmptyVariable73(id, name, 2, dims);
    }

    if ( MAX_RANK >= matvar->rank ) {
        hsize_t perm_dims[MAX_RANK];
        err = Mat_VarWriteNextType73(id, matvar, name, refs_id, perm_dims);
    } else {
        hsize_t *perm_dims = (hsize_t *)malloc(matvar->rank * sizeof(hsize_t));
        if ( NULL != perm_dims ) {
            err = Mat_VarWriteNextType73(id, matvar, name, refs_id, perm_dims);
            free(perm_dims);
        } else {
            err = MATIO_E_OUT_OF_MEMORY;
        }
    }

    return err;
}

static int
Mat_VarWriteAppendNext73(hid_t id, matvar_t *matvar, const char *name, hid_t *refs_id, int dim)
{
    int err;

    if ( MAX_RANK >= matvar->rank ) {
        hsize_t perm_dims[MAX_RANK];
        err = Mat_VarWriteAppendNextType73(id, matvar, name, refs_id, perm_dims, dim);
    } else {
        hsize_t *perm_dims = (hsize_t *)malloc(matvar->rank * sizeof(hsize_t));
        if ( NULL != perm_dims ) {
            err = Mat_VarWriteAppendNextType73(id, matvar, name, refs_id, perm_dims, dim);
            free(perm_dims);
        } else {
            err = MATIO_E_OUT_OF_MEMORY;
        }
    }

    return err;
}

static int
Mat_VarWriteNextType73(hid_t id, matvar_t *matvar, const char *name, hid_t *refs_id, hsize_t *dims)
{
    int err, k;

    /* Permute dimensions */
    for ( k = 0; k < matvar->rank; k++ ) {
        dims[k] = matvar->dims[matvar->rank - k - 1];
    }

    if ( matvar->isLogical && matvar->class_type != MAT_C_SPARSE ) {
        err = Mat_VarWriteLogical73(id, matvar, name, dims);
    } else {
        switch ( matvar->class_type ) {
            case MAT_C_DOUBLE:
            case MAT_C_SINGLE:
            case MAT_C_INT64:
            case MAT_C_UINT64:
            case MAT_C_INT32:
            case MAT_C_UINT32:
            case MAT_C_INT16:
            case MAT_C_UINT16:
            case MAT_C_INT8:
            case MAT_C_UINT8:
                err = Mat_VarWriteNumeric73(id, matvar, name, dims, NULL);
                break;
            case MAT_C_CHAR:
                err = Mat_VarWriteChar73(id, matvar, name, dims);
                break;
            case MAT_C_STRUCT:
                err = Mat_VarWriteStruct73(id, matvar, name, refs_id, dims, NULL);
                break;
            case MAT_C_CELL:
                err = Mat_VarWriteCell73(id, matvar, name, refs_id, dims);
                break;
            case MAT_C_SPARSE:
                err = Mat_VarWriteSparse73(id, matvar, name);
                break;
            case MAT_C_EMPTY:
                err = Mat_WriteEmptyVariable73(id, name, matvar->rank, matvar->dims);
                break;
            case MAT_C_FUNCTION:
            case MAT_C_OBJECT:
            case MAT_C_OPAQUE:
                err = MATIO_E_OPERATION_NOT_SUPPORTED;
                break;
            default:
                err = MATIO_E_OUTPUT_BAD_DATA;
                break;
        }
    }

    return err;
}

static int
Mat_VarWriteAppendNextType73(hid_t id, matvar_t *matvar, const char *name, hid_t *refs_id,
                             hsize_t *dims, int dim)
{
    int err, k;

    /* Permute dimensions */
    for ( k = 0; k < matvar->rank; k++ ) {
        dims[k] = matvar->dims[matvar->rank - k - 1];
    }

    if ( !matvar->isLogical ) {
        switch ( matvar->class_type ) {
            case MAT_C_DOUBLE:
            case MAT_C_SINGLE:
            case MAT_C_INT64:
            case MAT_C_UINT64:
            case MAT_C_INT32:
            case MAT_C_UINT32:
            case MAT_C_INT16:
            case MAT_C_UINT16:
            case MAT_C_INT8:
            case MAT_C_UINT8:
                err = Mat_VarWriteAppendNumeric73(id, matvar, name, dims, dim);
                break;
            case MAT_C_STRUCT:
                err = Mat_VarWriteAppendStruct73(id, matvar, name, refs_id, dims, dim);
                break;
            case MAT_C_EMPTY:
            case MAT_C_CHAR:
            case MAT_C_CELL:
            case MAT_C_SPARSE:
            case MAT_C_FUNCTION:
            case MAT_C_OBJECT:
            case MAT_C_OPAQUE:
                err = Mat_VarWriteNextType73(id, matvar, name, refs_id, dims);
                break;
            default:
                err = MATIO_E_OUTPUT_BAD_DATA;
                break;
        }
    } else {
        err = MATIO_E_OPERATION_NOT_SUPPORTED;
    }

    return err;
}

/** @if mat_devman
 * @brief Creates a new Matlab MAT version 7.3 file
 *
 * Tries to create a new Matlab MAT file with the given name and optional
 * header string.  If no header string is given, the default string
 * is used containing the software, version, and date in it.  If a header
 * string is given, at most the first 116 characters is written to the file.
 * The given header string need not be the full 116 characters, but MUST be
 * NULL terminated.
 * @ingroup mat_internal
 * @param matname Name of MAT file to create
 * @param hdr_str Optional header string, NULL to use default
 * @return A pointer to the MAT file or NULL if it failed.  This is not a
 * simple FILE * and should not be used as one.
 * @endif
 */
mat_t *
Mat_Create73(const char *matname, const char *hdr_str)
{
    FILE *fp = NULL;
    mat_int16_t endian = 0, version;
    mat_t *mat = NULL;
    size_t err;
    time_t t;
    hid_t plist_id, fid, plist_ap;

    plist_id = H5Pcreate(H5P_FILE_CREATE);
    H5Pset_userblock(plist_id, 512);
    plist_ap = H5Pcreate(H5P_FILE_ACCESS);
#if H5_VERSION_GE(1, 10, 2)
    H5Pset_libver_bounds(plist_ap, H5F_LIBVER_EARLIEST, H5F_LIBVER_V18);
#endif
    fid = H5Fcreate(matname, H5F_ACC_TRUNC, plist_id, plist_ap);
    H5Fclose(fid);
    H5Pclose(plist_id);

#if defined(_WIN32) && defined(_MSC_VER) && H5_VERSION_GE(1, 11, 6)
    {
        wchar_t *wname = utf82u(matname);
        if ( NULL != wname ) {
            fp = _wfopen(wname, L"r+b");
            free(wname);
        }
    }
#else
    fp = fopen(matname, "r+b");
#endif
    if ( !fp ) {
        H5Pclose(plist_ap);
        return NULL;
    }

    (void)fseeko(fp, 0, SEEK_SET);

    mat = (mat_t *)malloc(sizeof(*mat));
    if ( mat == NULL ) {
        fclose(fp);
        H5Pclose(plist_ap);
        return NULL;
    }

    mat->fp = NULL;
    mat->header = NULL;
    mat->subsys_offset = NULL;
    mat->filename = NULL;
    mat->version = 0;
    mat->byteswap = 0;
    mat->mode = 0;
    mat->bof = 128;
    mat->next_index = 0;
    mat->num_datasets = 0;
    mat->refs_id = -1;
    mat->dir = NULL;

    t = time(NULL);
    mat->filename = strdup(matname);
    mat->mode = MAT_ACC_RDWR;
    mat->byteswap = 0;
    mat->header = (char *)malloc(128 * sizeof(char));
    mat->subsys_offset = (char *)malloc(8 * sizeof(char));
    memset(mat->header, ' ', 128);
    if ( hdr_str == NULL ) {
        err = mat_snprintf(mat->header, 116,
                           "MATLAB 7.3 MAT-file, Platform: %s, "
                           "Created by: libmatio v%d.%d.%d on %s HDF5 schema 0.5",
                           MATIO_PLATFORM, MATIO_MAJOR_VERSION, MATIO_MINOR_VERSION,
                           MATIO_RELEASE_LEVEL, ctime(&t));
    } else {
        err = mat_snprintf(mat->header, 116, "%s", hdr_str);
    }
    if ( err >= 116 )
        mat->header[115] = '\0'; /* Just to make sure it's NULL terminated */
    memset(mat->subsys_offset, ' ', 8);
    mat->version = (int)0x0200;
    endian = 0x4d49;

    version = 0x0200;

    fwrite(mat->header, 1, 116, fp);
    fwrite(mat->subsys_offset, 1, 8, fp);
    fwrite(&version, 2, 1, fp);
    fwrite(&endian, 2, 1, fp);

    fclose(fp);

    fid = H5Fopen(matname, H5F_ACC_RDWR, plist_ap);
    H5Pclose(plist_ap);

    mat->fp = malloc(sizeof(hid_t));
    *(hid_t *)mat->fp = fid;

    return mat;
}

/** @if mat_devman
 * @brief Closes a MAT file
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @retval 0 on success
 * @endif
 */
int
Mat_Close73(mat_t *mat)
{
    int err = MATIO_E_NO_ERROR;
    if ( mat->refs_id > -1 )
        H5Gclose(mat->refs_id);
    if ( 0 > H5Fclose(*(hid_t *)mat->fp) )
        err = MATIO_E_FILESYSTEM_ERROR_ON_CLOSE;
    free(mat->fp);
    mat->fp = NULL;
    return err;
}

/** @if mat_devman
 * @brief Reads the MAT variable identified by matvar
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar MAT variable pointer
 * @retval 0 on success
 * @endif
 */
int
Mat_VarRead73(mat_t *mat, matvar_t *matvar)
{
    int err = MATIO_E_NO_ERROR;
    hid_t fid, dset_id, ref_id;

    if ( NULL == mat || NULL == matvar )
        return MATIO_E_BAD_ARGUMENT;
    else if ( NULL == matvar->internal->hdf5_name && 0 > matvar->internal->id )
        return MATIO_E_READ_VARIABLE_DOES_NOT_EXIST;

    fid = *(hid_t *)mat->fp;

    switch ( matvar->class_type ) {
        case MAT_C_DOUBLE:
        case MAT_C_SINGLE:
        case MAT_C_INT64:
        case MAT_C_UINT64:
        case MAT_C_INT32:
        case MAT_C_UINT32:
        case MAT_C_INT16:
        case MAT_C_UINT16:
        case MAT_C_INT8:
        case MAT_C_UINT8: {
            size_t nelems = 1;
            matvar->data_size = (int)Mat_SizeOfClass(matvar->class_type);
            err = Mat_MulDims(matvar, &nelems);
            if ( err ) {
                Mat_Critical("Integer multiplication overflow");
                return err;
            }
            err = Mul(&matvar->nbytes, nelems, matvar->data_size);
            if ( err ) {
                Mat_Critical("Integer multiplication overflow");
                return err;
            }

            if ( nelems < 1 )
                break;

            if ( NULL != matvar->internal->hdf5_name ) {
                ref_id = H5Dopen(fid, matvar->internal->hdf5_name, H5P_DEFAULT);
            } else {
                ref_id = matvar->internal->id;
                H5Iinc_ref(ref_id);
            }
            if ( 0 < matvar->internal->hdf5_ref ) {
                dset_id = H5RDEREFERENCE(ref_id, H5R_OBJECT, &matvar->internal->hdf5_ref);
            } else {
                dset_id = ref_id;
                H5Iinc_ref(dset_id);
            }

            if ( !matvar->isComplex ) {
                matvar->data = malloc(matvar->nbytes);
            } else {
                matvar->data = ComplexMalloc(matvar->nbytes);
            }
            if ( NULL != matvar->data ) {
                err = Mat_H5ReadData(dset_id, ClassType2H5T(matvar->class_type), H5S_ALL, H5S_ALL,
                                     matvar->isComplex, matvar->data);
            }
            H5Dclose(dset_id);
            H5Dclose(ref_id);
            break;
        }
        case MAT_C_CHAR: {
            size_t nelems = 1;
            matvar->data_size = (int)Mat_SizeOf(matvar->data_type);
            err = Mat_MulDims(matvar, &nelems);
            if ( err ) {
                Mat_Critical("Integer multiplication overflow");
                return err;
            }
            err = Mul(&matvar->nbytes, nelems, matvar->data_size);
            if ( err ) {
                Mat_Critical("Integer multiplication overflow");
                return err;
            }

            if ( NULL != matvar->internal->hdf5_name ) {
                dset_id = H5Dopen(fid, matvar->internal->hdf5_name, H5P_DEFAULT);
            } else {
                dset_id = matvar->internal->id;
                H5Iinc_ref(dset_id);
            }
            if ( matvar->nbytes > 0 ) {
                matvar->data = malloc(matvar->nbytes);
                if ( NULL != matvar->data ) {
                    herr_t herr = H5Dread(dset_id, DataType2H5T(matvar->data_type), H5S_ALL,
                                          H5S_ALL, H5P_DEFAULT, matvar->data);
                    if ( herr < 0 ) {
                        err = MATIO_E_GENERIC_READ_ERROR;
                    }
                } else {
                    err = MATIO_E_OUT_OF_MEMORY;
                }
            }
            H5Dclose(dset_id);
            break;
        }
        case MAT_C_STRUCT: {
            matvar_t **fields;
            size_t i, nelems_x_nfields, nelems = 1;

            if ( !matvar->internal->num_fields || NULL == matvar->data )
                break;

            err = Mat_MulDims(matvar, &nelems);
            if ( err ) {
                Mat_Critical("Integer multiplication overflow");
                return err;
            }
            err = Mul(&nelems_x_nfields, nelems, matvar->internal->num_fields);
            if ( err ) {
                Mat_Critical("Integer multiplication overflow");
                return err;
            }

            fields = (matvar_t **)matvar->data;
            for ( i = 0; i < nelems_x_nfields; i++ ) {
                if ( NULL != fields[i] && 0 < fields[i]->internal->hdf5_ref &&
                     -1 < fields[i]->internal->id ) {
                    /* Dataset of references */
                    err = Mat_H5ReadNextReferenceData(fields[i]->internal->id, fields[i], mat);
                } else {
                    err = Mat_VarRead73(mat, fields[i]);
                }
                if ( err ) {
                    break;
                }
            }
            break;
        }
        case MAT_C_CELL: {
            matvar_t **cells;
            size_t i, nelems;

            if ( NULL == matvar->data ) {
                Mat_Critical("Data is NULL for cell array %s", matvar->name);
                err = MATIO_E_FILE_FORMAT_VIOLATION;
                break;
            }
            nelems = matvar->nbytes / matvar->data_size;
            cells = (matvar_t **)matvar->data;
            for ( i = 0; i < nelems; i++ ) {
                if ( NULL != cells[i] ) {
                    err = Mat_H5ReadNextReferenceData(cells[i]->internal->id, cells[i], mat);
                }
                if ( err ) {
                    break;
                }
            }
            break;
        }
        case MAT_C_SPARSE: {
            hid_t sparse_dset_id;
            mat_sparse_t *sparse_data = (mat_sparse_t *)calloc(1, sizeof(*sparse_data));

            if ( NULL != matvar->internal->hdf5_name ) {
                dset_id = H5Gopen(fid, matvar->internal->hdf5_name, H5P_DEFAULT);
            } else {
                dset_id = matvar->internal->id;
                H5Iinc_ref(dset_id);
            }

            if ( H5Lexists(dset_id, "ir", H5P_DEFAULT) ) {
                size_t *dims;
                hsize_t nelems;
                int rank;

                sparse_dset_id = H5Dopen(dset_id, "ir", H5P_DEFAULT);
                dims = Mat_H5ReadDims(sparse_dset_id, &nelems, &rank);
                if ( NULL != dims ) {
                    size_t nbytes = 0;
                    sparse_data->nir = (mat_uint32_t)dims[0];
                    free(dims);
                    err = Mul(&nbytes, sparse_data->nir, sizeof(mat_uint32_t));
                    if ( err ) {
                        H5Dclose(sparse_dset_id);
                        H5Gclose(dset_id);
                        free(sparse_data);
                        Mat_Critical("Integer multiplication overflow");
                        return err;
                    }
                    sparse_data->ir = (mat_uint32_t *)malloc(nbytes);
                    if ( sparse_data->ir != NULL ) {
                        herr_t herr = H5Dread(sparse_dset_id, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL,
                                              H5P_DEFAULT, sparse_data->ir);
                        if ( herr < 0 ) {
                            err = MATIO_E_GENERIC_READ_ERROR;
                        }
                    } else {
                        err = MATIO_E_OUT_OF_MEMORY;
                    }
                } else {
                    err = MATIO_E_UNKNOWN_ERROR;
                }
                H5Dclose(sparse_dset_id);
                if ( err ) {
                    H5Gclose(dset_id);
                    if ( sparse_data->ir != NULL )
                        free(sparse_data->ir);
                    free(sparse_data);
                    return err;
                }
            }

            if ( H5Lexists(dset_id, "jc", H5P_DEFAULT) ) {
                size_t *dims;
                hsize_t nelems;
                int rank;

                sparse_dset_id = H5Dopen(dset_id, "jc", H5P_DEFAULT);
                dims = Mat_H5ReadDims(sparse_dset_id, &nelems, &rank);
                if ( NULL != dims ) {
                    size_t nbytes = 0;
                    sparse_data->njc = (mat_uint32_t)dims[0];
                    free(dims);
                    err = Mul(&nbytes, sparse_data->njc, sizeof(mat_uint32_t));
                    if ( err ) {
                        H5Dclose(sparse_dset_id);
                        H5Gclose(dset_id);
                        free(sparse_data);
                        Mat_Critical("Integer multiplication overflow");
                        return err;
                    }
                    sparse_data->jc = (mat_uint32_t *)malloc(nbytes);
                    if ( sparse_data->jc != NULL ) {
                        herr_t herr = H5Dread(sparse_dset_id, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL,
                                              H5P_DEFAULT, sparse_data->jc);
                        if ( herr < 0 ) {
                            err = MATIO_E_GENERIC_READ_ERROR;
                        }
                    } else {
                        err = MATIO_E_OUT_OF_MEMORY;
                    }
                } else {
                    err = MATIO_E_UNKNOWN_ERROR;
                }
                H5Dclose(sparse_dset_id);
                if ( err ) {
                    H5Gclose(dset_id);
                    if ( sparse_data->jc != NULL )
                        free(sparse_data->jc);
                    if ( sparse_data->ir != NULL )
                        free(sparse_data->ir);
                    free(sparse_data);
                    return err;
                }
            }

            if ( H5Lexists(dset_id, "data", H5P_DEFAULT) ) {
                size_t *dims;
                hsize_t nelems;
                int rank;

                sparse_dset_id = H5Dopen(dset_id, "data", H5P_DEFAULT);
                dims = Mat_H5ReadDims(sparse_dset_id, &nelems, &rank);
                if ( NULL != dims ) {
                    size_t ndata_bytes = 0;
                    sparse_data->nzmax = (mat_uint32_t)dims[0];
                    sparse_data->ndata = (mat_uint32_t)dims[0];
                    free(dims);
                    err = Mul(&ndata_bytes, sparse_data->nzmax, Mat_SizeOf(matvar->data_type));
                    if ( err ) {
                        H5Dclose(sparse_dset_id);
                        H5Gclose(dset_id);
                        free(sparse_data);
                        Mat_Critical("Integer multiplication overflow");
                        return err;
                    }
                    matvar->data_size = sizeof(mat_sparse_t);
                    matvar->nbytes = matvar->data_size;
                    if ( !matvar->isComplex ) {
                        sparse_data->data = malloc(ndata_bytes);
                    } else {
                        sparse_data->data = ComplexMalloc(ndata_bytes);
                    }
                    if ( NULL != sparse_data->data ) {
                        err =
                            Mat_H5ReadData(sparse_dset_id, DataType2H5T(matvar->data_type), H5S_ALL,
                                           H5S_ALL, matvar->isComplex, sparse_data->data);
                    } else {
                        err = MATIO_E_OUT_OF_MEMORY;
                    }
                }
                H5Dclose(sparse_dset_id);
            }
            H5Gclose(dset_id);
            matvar->data = sparse_data;
            break;
        }
        case MAT_C_EMPTY:
        case MAT_C_FUNCTION:
        case MAT_C_OBJECT:
        case MAT_C_OPAQUE:
            break;
        default:
            err = MATIO_E_FAIL_TO_IDENTIFY;
            break;
    }
    return err;
}

/** @if mat_devman
 * @brief Reads a slab of data from the mat variable @c matvar
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @param data pointer to store the read data in (must be of size
 *        edge[0]*...edge[rank-1]*Mat_SizeOfClass(matvar->class_type))
 * @param start index to start reading data in each dimension
 * @param stride write data every @c stride elements in each dimension
 * @param edge number of elements to read in each dimension
 * @retval 0 on success
 * @endif
 */
int
Mat_VarReadData73(mat_t *mat, matvar_t *matvar, void *data, int *start, int *stride, int *edge)
{
    int err = MATIO_E_NO_ERROR, k;
    hid_t fid, dset_id, ref_id, dset_space, mem_space;
    hsize_t *dset_start_stride_edge;
    hsize_t *dset_start, *dset_stride, *dset_edge;

    if ( NULL == mat || NULL == matvar || NULL == data || NULL == start || NULL == stride ||
         NULL == edge )
        return MATIO_E_BAD_ARGUMENT;
    else if ( NULL == matvar->internal->hdf5_name && 0 > matvar->internal->id )
        return MATIO_E_FAIL_TO_IDENTIFY;

    fid = *(hid_t *)mat->fp;

    dset_start_stride_edge = (hsize_t *)malloc(matvar->rank * 3 * sizeof(hsize_t));
    if ( NULL == dset_start_stride_edge ) {
        return MATIO_E_OUT_OF_MEMORY;
    }
    dset_start = &dset_start_stride_edge[0];
    dset_stride = &dset_start_stride_edge[matvar->rank];
    dset_edge = &dset_start_stride_edge[2 * matvar->rank];

    for ( k = 0; k < matvar->rank; k++ ) {
        dset_start[k] = start[matvar->rank - k - 1];
        dset_stride[k] = stride[matvar->rank - k - 1];
        dset_edge[k] = edge[matvar->rank - k - 1];
    }
    mem_space = H5Screate_simple(matvar->rank, dset_edge, NULL);

    switch ( matvar->class_type ) {
        case MAT_C_DOUBLE:
        case MAT_C_SINGLE:
        case MAT_C_INT64:
        case MAT_C_UINT64:
        case MAT_C_INT32:
        case MAT_C_UINT32:
        case MAT_C_INT16:
        case MAT_C_UINT16:
        case MAT_C_INT8:
        case MAT_C_UINT8:
            if ( NULL != matvar->internal->hdf5_name ) {
                ref_id = H5Dopen(fid, matvar->internal->hdf5_name, H5P_DEFAULT);
            } else {
                ref_id = matvar->internal->id;
                H5Iinc_ref(ref_id);
            }
            if ( 0 < matvar->internal->hdf5_ref ) {
                dset_id = H5RDEREFERENCE(ref_id, H5R_OBJECT, &matvar->internal->hdf5_ref);
            } else {
                dset_id = ref_id;
                H5Iinc_ref(dset_id);
            }

            dset_space = H5Dget_space(dset_id);
            H5Sselect_hyperslab(dset_space, H5S_SELECT_SET, dset_start, dset_stride, dset_edge,
                                NULL);
            err = Mat_H5ReadData(dset_id, ClassType2H5T(matvar->class_type), mem_space, dset_space,
                                 matvar->isComplex, data);
            H5Sclose(dset_space);
            H5Dclose(dset_id);
            H5Dclose(ref_id);
            break;
        default:
            err = MATIO_E_FAIL_TO_IDENTIFY;
            break;
    }
    H5Sclose(mem_space);
    free(dset_start_stride_edge);

    return err;
}

/** @if mat_devman
 * @brief Reads a subset of a MAT variable using a 1-D indexing
 *
 * Reads data from a MAT variable using a linear (1-D) indexing mode. The
 * variable must have been read by Mat_VarReadInfo.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @param data pointer to store the read data in (must be of size
 *        edge*Mat_SizeOfClass(matvar->class_type))
 * @param start starting index
 * @param stride stride of data
 * @param edge number of elements to read
 * @retval 0 on success
 * @endif
 */
int
Mat_VarReadDataLinear73(mat_t *mat, matvar_t *matvar, void *data, int start, int stride, int edge)
{
    int err = MATIO_E_NO_ERROR, k;
    hid_t fid, dset_id, dset_space, mem_space;
    hsize_t *points, dset_edge, *dimp;

    if ( NULL == mat || NULL == matvar || NULL == data )
        return MATIO_E_BAD_ARGUMENT;
    else if ( NULL == matvar->internal->hdf5_name && 0 > matvar->internal->id )
        return MATIO_E_FAIL_TO_IDENTIFY;

    fid = *(hid_t *)mat->fp;

    dset_edge = edge;
    mem_space = H5Screate_simple(1, &dset_edge, NULL);

    switch ( matvar->class_type ) {
        case MAT_C_DOUBLE:
        case MAT_C_SINGLE:
        case MAT_C_INT64:
        case MAT_C_UINT64:
        case MAT_C_INT32:
        case MAT_C_UINT32:
        case MAT_C_INT16:
        case MAT_C_UINT16:
        case MAT_C_INT8:
        case MAT_C_UINT8:
            points = (hsize_t *)malloc(matvar->rank * (size_t)dset_edge * sizeof(*points));
            if ( NULL == points ) {
                err = MATIO_E_OUT_OF_MEMORY;
                break;
            }
            dimp = (hsize_t *)malloc(matvar->rank * sizeof(hsize_t));
            if ( NULL == dimp ) {
                err = MATIO_E_OUT_OF_MEMORY;
                free(points);
                break;
            }
            dimp[0] = 1;
            for ( k = 1; k < matvar->rank; k++ )
                dimp[k] = dimp[k - 1] * matvar->dims[k - 1];
            for ( k = 0; k < edge; k++ ) {
                size_t l, coord;
                coord = (size_t)(start + k * stride);
                for ( l = matvar->rank; l--; ) {
                    size_t idx = (size_t)(coord / dimp[l]);
                    points[matvar->rank * (k + 1) - 1 - l] = idx;
                    coord -= idx * (size_t)dimp[l];
                }
            }
            free(dimp);

            if ( NULL != matvar->internal->hdf5_name ) {
                dset_id = H5Dopen(fid, matvar->internal->hdf5_name, H5P_DEFAULT);
            } else {
                dset_id = matvar->internal->id;
                H5Iinc_ref(dset_id);
            }
            dset_space = H5Dget_space(dset_id);
            H5Sselect_elements(dset_space, H5S_SELECT_SET, (size_t)dset_edge, points);
            free(points);
            err = Mat_H5ReadData(dset_id, ClassType2H5T(matvar->class_type), mem_space, dset_space,
                                 matvar->isComplex, data);
            H5Sclose(dset_space);
            H5Dclose(dset_id);
            break;
        default:
            err = MATIO_E_FAIL_TO_IDENTIFY;
            break;
    }
    H5Sclose(mem_space);

    return err;
}

/** @if mat_devman
 * @brief Reads the header information for the next MAT variable
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @return pointer to the MAT variable or NULL
 * @endif
 */
matvar_t *
Mat_VarReadNextInfo73(mat_t *mat)
{
    hid_t id;
    hsize_t idx;
    herr_t herr;
    struct ReadNextIterData mat_data;

    if ( mat == NULL )
        return NULL;

    if ( mat->next_index >= mat->num_datasets )
        return NULL;

    id = *(hid_t *)mat->fp;
    idx = (hsize_t)mat->next_index;
    mat_data.mat = mat;
    mat_data.matvar = NULL;
    herr = H5Literate(id, H5_INDEX_NAME, H5_ITER_NATIVE, &idx, Mat_VarReadNextInfoIterate,
                      (void *)&mat_data);
    if ( herr > 0 )
        mat->next_index = (size_t)idx;
    return mat_data.matvar;
}

static herr_t
Mat_VarReadNextInfoIterate(hid_t id, const char *name, const H5L_info_t *info, void *op_data)
{
    mat_t *mat;
    H5O_INFO_T object_info;
    struct ReadNextIterData *mat_data;

    /* FIXME: follow symlinks, datatypes? */

    /* Check that this is not the /#refs# or /"#subsystem#" group */
    if ( 0 == strcmp(name, "#refs#") || 0 == strcmp(name, "#subsystem#") )
        return 0;

    object_info.type = H5O_TYPE_UNKNOWN;
    H5OGET_INFO_BY_NAME(id, name, &object_info, H5P_DEFAULT);
    if ( H5O_TYPE_DATASET != object_info.type && H5O_TYPE_GROUP != object_info.type )
        return 0;

    mat_data = (struct ReadNextIterData *)op_data;
    if ( NULL == mat_data )
        return -1;
    mat = mat_data->mat;

    switch ( object_info.type ) {
        case H5O_TYPE_DATASET: {
            int err;
            hid_t dset_id;
            matvar_t *matvar = Mat_VarCalloc();
            if ( NULL == matvar )
                return -1;

            matvar->name = strdup(name);
            if ( NULL == matvar->name ) {
                Mat_VarFree(matvar);
                return -1;
            }

            dset_id = H5Dopen(id, name, H5P_DEFAULT);
            err = Mat_H5ReadDatasetInfo(mat, matvar, dset_id);
            if ( matvar->internal->id != dset_id ) {
                /* Close dataset and increment count */
                H5Dclose(dset_id);
            }
            if ( err ) {
                Mat_VarFree(matvar);
                return -1;
            }
            mat_data->matvar = matvar;
            break;
        }
        case H5O_TYPE_GROUP: {
            int err;
            hid_t dset_id;
            matvar_t *matvar = Mat_VarCalloc();
            if ( NULL == matvar )
                return -1;

            matvar->name = strdup(name);
            if ( NULL == matvar->name ) {
                Mat_VarFree(matvar);
                return -1;
            }

            dset_id = H5Gopen(id, name, H5P_DEFAULT);
            err = Mat_H5ReadGroupInfo(mat, matvar, dset_id);
            H5Gclose(dset_id);
            if ( err ) {
                Mat_VarFree(matvar);
                return -1;
            }
            mat_data->matvar = matvar;
            break;
        }
        default:
            break;
    }

    return 1;
}

/** @if mat_devman
 * @brief Writes a matlab variable to a version 7.3 matlab file
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @param compress option to compress the variable
 *        (only works for numeric types)
 * @retval 0 on success
 * @endif
 */
int
Mat_VarWrite73(mat_t *mat, matvar_t *matvar, int compress)
{
    hid_t id;

    if ( NULL == mat || NULL == matvar )
        return MATIO_E_BAD_ARGUMENT;

    matvar->compression = (enum matio_compression)compress;

    id = *(hid_t *)mat->fp;
    return Mat_VarWriteNext73(id, matvar, matvar->name, &(mat->refs_id));
}

/** @if mat_devman
 * @brief Writes/appends a matlab variable to a version 7.3 matlab file
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @param compress option to compress the variable
 *        (only works for numeric types)
 * @param dim dimension to append data
 *        (only works for numeric types)
 * @retval 0 on success
 * @endif
 */
int
Mat_VarWriteAppend73(mat_t *mat, matvar_t *matvar, int compress, int dim)
{
    hid_t id;

    if ( NULL == mat || NULL == matvar )
        return MATIO_E_BAD_ARGUMENT;

    matvar->compression = (enum matio_compression)compress;

    id = *(hid_t *)mat->fp;
    return Mat_VarWriteAppendNext73(id, matvar, matvar->name, &(mat->refs_id), dim);
}

#endif
#endif
