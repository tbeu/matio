/** @file mat5.c
 * Matlab MAT version 5 file functions
 * @ingroup MAT
 */
/*
 * Copyright (C) 2005-2010   Christopher C. Hulbert
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
#include "hdf5.h"
#include "matio_private.h"
#include "mat73.h"

static hsize_t perm_dims[10];
static hsize_t dims1[2] = {1,1};

static const char *Mat_class_names[] = {
    "",
    "cell",
    "struct",
    "object",
    "char",
    "sparse",
    "double",
    "single",
    "int8",
    "uint8",
    "int16",
    "uint16",
    "int32",
    "uint32",
    "int64",
    "uint64",
    "function"
};

/*===========================================================================
 *  Private functions
 *===========================================================================
 */
static enum matio_classes Mat_class_str_to_id(const char *name);
static hid_t Mat_class_type_to_hid_t(enum matio_classes class_type);
static hid_t Mat_data_type_to_hid_t(enum matio_types data_type);
static hid_t Mat_dims_type_to_hid_t(void);
static void  Mat_H5ReadDatasetInfo(mat_t *mat,matvar_t *matvar,hid_t dset_id);
static void  Mat_H5ReadGroupInfo(mat_t *mat,matvar_t *matvar,hid_t dset_id);
static void  Mat_H5ReadNextReferenceInfo(hid_t ref_id,matvar_t *matvar,mat_t *mat);
static void  Mat_H5ReadNextReferenceData(hid_t ref_id,matvar_t *matvar,mat_t *mat);
static int   Mat_WriteNextStructField73(hid_t id,matvar_t *matvar,const char *name);
static int   Mat_WriteNextCellField73(hid_t id,matvar_t *matvar,const char *name);

static enum matio_classes
Mat_class_str_to_id(const char *name)
{
    enum matio_classes id = MAT_C_EMPTY;
    if ( NULL != name ) {
        int k;
        for ( k = 1; k < 17; k++ ) {
            if ( !strcmp(name,Mat_class_names[k]) ) {
                id = (enum matio_classes)k;
                break;
            }
        }
    }
    return id;
}

static enum matio_types
Mat_ClassToType73(enum matio_classes class_type)
{
    enum matio_types type;
    switch ( class_type ) {
        case MAT_C_DOUBLE:
            type = MAT_T_DOUBLE;
            break;
        case MAT_C_SINGLE:
            type = MAT_T_SINGLE;
            break;
        case MAT_C_INT64:
            type = MAT_T_INT64;
            break;
        case MAT_C_UINT64:
            type = MAT_T_UINT64;
            break;
        case MAT_C_INT32:
            type = MAT_T_INT32;
            break;
        case MAT_C_UINT32:
            type = MAT_T_UINT32;
            break;
        case MAT_C_INT16:
            type = MAT_T_INT16;
            break;
        case MAT_C_UINT16:
            type = MAT_T_UINT16;
            break;
        case MAT_C_INT8:
            type = MAT_T_INT8;
            break;
        case MAT_C_UINT8:
            type = MAT_T_UINT8;
            break;
        case MAT_C_CELL:
            type = MAT_T_CELL;
            break;
        case MAT_C_STRUCT:
            type = MAT_T_STRUCT;
            break;
        default:
            type = MAT_T_UNKNOWN;
            break;
    }

    return type;
}

static hid_t
Mat_class_type_to_hid_t(enum matio_classes class_type)
{
    switch ( class_type ) {
        case MAT_C_DOUBLE:
            return H5T_NATIVE_DOUBLE;
        case MAT_C_SINGLE:
            return H5T_NATIVE_FLOAT;
        case MAT_C_INT64:
#       if CHAR_BIT*SIZEOF_SHORT == 64
            return H5T_NATIVE_SHORT;
#       elif CHAR_BIT*SIZEOF_INT == 64
            return H5T_NATIVE_INT;
#       elif CHAR_BIT*SIZEOF_LONG == 64
            return H5T_NATIVE_LONG;
#       elif CHAR_BIT*SIZEOF_LONG_LONG == 64
            return H5T_NATIVE_LLONG;
#       endif
        case MAT_C_UINT64:
#       if CHAR_BIT*SIZEOF_SHORT == 64
            return H5T_NATIVE_USHORT;
#       elif CHAR_BIT*SIZEOF_INT == 64
            return H5T_NATIVE_UINT;
#       elif CHAR_BIT*SIZEOF_LONG == 64
            return H5T_NATIVE_ULONG;
#       elif CHAR_BIT*SIZEOF_LONG_LONG == 64
            return H5T_NATIVE_ULLONG;
#       endif
        case MAT_C_INT32:
#       if CHAR_BIT == 32
            return H5T_NATIVE_SCHAR;
#       elif CHAR_BIT*SIZEOF_SHORT == 32
            return H5T_NATIVE_SHORT;
#       elif CHAR_BIT*SIZEOF_INT == 32
            return H5T_NATIVE_INT;
#       elif CHAR_BIT*SIZEOF_LONG == 32
            return H5T_NATIVE_LONG;
#       elif CHAR_BIT*SIZEOF_LONG_LONG == 32
            return H5T_NATIVE_LLONG;
#       endif
        case MAT_C_UINT32:
#       if CHAR_BIT == 32
            return H5T_NATIVE_UCHAR;
#       elif CHAR_BIT*SIZEOF_SHORT == 32
            return H5T_NATIVE_USHORT;
#       elif CHAR_BIT*SIZEOF_INT == 32
            return H5T_NATIVE_UINT;
#       elif CHAR_BIT*SIZEOF_LONG == 32
            return H5T_NATIVE_ULONG;
#       elif CHAR_BIT*SIZEOF_LONG_LONG == 32
            return H5T_NATIVE_ULLONG;
#       endif
        case MAT_C_INT16:
#       if CHAR_BIT == 16
            return H5T_NATIVE_SCHAR;
#       elif CHAR_BIT*SIZEOF_SHORT == 16
            return H5T_NATIVE_SHORT;
#       elif CHAR_BIT*SIZEOF_INT == 16
            return H5T_NATIVE_INT;
#       elif CHAR_BIT*SIZEOF_LONG == 16
            return H5T_NATIVE_LONG;
#       elif CHAR_BIT*SIZEOF_LONG_LONG == 16
            return H5T_NATIVE_LLONG;
#       endif
        case MAT_C_UINT16:
#       if CHAR_BIT == 16
            return H5T_NATIVE_UCHAR;
#       elif CHAR_BIT*SIZEOF_SHORT == 16
            return H5T_NATIVE_USHORT;
#       elif CHAR_BIT*SIZEOF_INT == 16
            return H5T_NATIVE_UINT;
#       elif CHAR_BIT*SIZEOF_LONG == 16
            return H5T_NATIVE_ULONG;
#       elif CHAR_BIT*SIZEOF_LONG_LONG == 16
            return H5T_NATIVE_ULLONG;
#       endif
        case MAT_C_INT8:
#       if CHAR_BIT == 8
            return H5T_NATIVE_SCHAR;
#       elif CHAR_BIT*SIZEOF_SHORT == 8
            return H5T_NATIVE_SHORT;
#       elif CHAR_BIT*SIZEOF_INT == 8
            return H5T_NATIVE_INT;
#       elif CHAR_BIT*SIZEOF_LONG == 8
            return H5T_NATIVE_LONG;
#       elif CHAR_BIT*SIZEOF_LONG_LONG == 8
            return H5T_NATIVE_LLONG;
#       endif
        case MAT_C_UINT8:
#       if CHAR_BIT == 8
            return H5T_NATIVE_UCHAR;
#       elif CHAR_BIT*SIZEOF_SHORT == 8
            return H5T_NATIVE_USHORT;
#       elif CHAR_BIT*SIZEOF_INT == 8
            return H5T_NATIVE_UINT;
#       elif CHAR_BIT*SIZEOF_LONG == 8
            return H5T_NATIVE_ULONG;
#       elif CHAR_BIT*SIZEOF_LONG_LONG == 8
            return H5T_NATIVE_ULLONG;
#       endif
       default:
           return -1;
    }
}

static hid_t
Mat_data_type_to_hid_t(enum matio_types data_type)
{
    switch ( data_type ) {
        case MAT_T_DOUBLE:
            return H5T_NATIVE_DOUBLE;
        case MAT_T_SINGLE:
            return H5T_NATIVE_FLOAT;
        case MAT_T_INT64:
#       if CHAR_BIT*SIZEOF_SHORT == 64
            return H5T_NATIVE_SHORT;
#       elif CHAR_BIT*SIZEOF_INT == 64
            return H5T_NATIVE_INT;
#       elif CHAR_BIT*SIZEOF_LONG == 64
            return H5T_NATIVE_LONG;
#       elif CHAR_BIT*SIZEOF_LONG_LONG == 64
            return H5T_NATIVE_LLONG;
#       endif
        case MAT_T_UINT64:
#       if CHAR_BIT*SIZEOF_SHORT == 64
            return H5T_NATIVE_USHORT;
#       elif CHAR_BIT*SIZEOF_INT == 64
            return H5T_NATIVE_UINT;
#       elif CHAR_BIT*SIZEOF_LONG == 64
            return H5T_NATIVE_ULONG;
#       elif CHAR_BIT*SIZEOF_LONG_LONG == 64
            return H5T_NATIVE_ULLONG;
#       endif
        case MAT_T_INT32:
#       if CHAR_BIT == 32
            return H5T_NATIVE_SCHAR;
#       elif CHAR_BIT*SIZEOF_SHORT == 32
            return H5T_NATIVE_SHORT;
#       elif CHAR_BIT*SIZEOF_INT == 32
            return H5T_NATIVE_INT;
#       elif CHAR_BIT*SIZEOF_LONG == 32
            return H5T_NATIVE_LONG;
#       elif CHAR_BIT*SIZEOF_LONG_LONG == 32
            return H5T_NATIVE_LLONG;
#       endif
        case MAT_T_UINT32:
#       if CHAR_BIT == 32
            return H5T_NATIVE_UCHAR;
#       elif CHAR_BIT*SIZEOF_SHORT == 32
            return H5T_NATIVE_USHORT;
#       elif CHAR_BIT*SIZEOF_INT == 32
            return H5T_NATIVE_UINT;
#       elif CHAR_BIT*SIZEOF_LONG == 32
            return H5T_NATIVE_ULONG;
#       elif CHAR_BIT*SIZEOF_LONG_LONG == 32
            return H5T_NATIVE_ULLONG;
#       endif
        case MAT_T_INT16:
#       if CHAR_BIT == 16
            return H5T_NATIVE_SCHAR;
#       elif CHAR_BIT*SIZEOF_SHORT == 16
            return H5T_NATIVE_SHORT;
#       elif CHAR_BIT*SIZEOF_INT == 16
            return H5T_NATIVE_INT;
#       elif CHAR_BIT*SIZEOF_LONG == 16
            return H5T_NATIVE_LONG;
#       elif CHAR_BIT*SIZEOF_LONG_LONG == 16
            return H5T_NATIVE_LLONG;
#       endif
        case MAT_T_UINT16:
#       if CHAR_BIT == 16
            return H5T_NATIVE_UCHAR;
#       elif CHAR_BIT*SIZEOF_SHORT == 16
            return H5T_NATIVE_USHORT;
#       elif CHAR_BIT*SIZEOF_INT == 16
            return H5T_NATIVE_UINT;
#       elif CHAR_BIT*SIZEOF_LONG == 16
            return H5T_NATIVE_ULONG;
#       elif CHAR_BIT*SIZEOF_LONG_LONG == 16
            return H5T_NATIVE_ULLONG;
#       endif
        case MAT_T_INT8:
#       if CHAR_BIT == 8
            return H5T_NATIVE_SCHAR;
#       elif CHAR_BIT*SIZEOF_SHORT == 8
            return H5T_NATIVE_SHORT;
#       elif CHAR_BIT*SIZEOF_INT == 8
            return H5T_NATIVE_INT;
#       elif CHAR_BIT*SIZEOF_LONG == 8
            return H5T_NATIVE_LONG;
#       elif CHAR_BIT*SIZEOF_LONG_LONG == 8
            return H5T_NATIVE_LLONG;
#       endif
        case MAT_T_UINT8:
#       if CHAR_BIT == 8
            return H5T_NATIVE_UCHAR;
#       elif CHAR_BIT*SIZEOF_SHORT == 8
            return H5T_NATIVE_USHORT;
#       elif CHAR_BIT*SIZEOF_INT == 8
            return H5T_NATIVE_UINT;
#       elif CHAR_BIT*SIZEOF_LONG == 8
            return H5T_NATIVE_ULONG;
#       elif CHAR_BIT*SIZEOF_LONG_LONG == 8
            return H5T_NATIVE_ULLONG;
#       endif
       default:
           return -1;
    }
}

static hid_t
Mat_dims_type_to_hid_t(void)
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

static void
Mat_complex_interleaved_to_split(void *data,struct ComplexSplit *complex_data,
    enum matio_types type,size_t numel)
{
    switch ( type ) {
        case MAT_T_DOUBLE:
        {
            double *rp      = complex_data->Re;
            double *ip      = complex_data->Im;
            double *tmp_ptr = data;
            size_t  k;
            for ( k = 0; k < numel; k++ ) {
                *rp++ = *tmp_ptr++;
                *ip++ = *tmp_ptr++;
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float *rp      = complex_data->Re;
            float *ip      = complex_data->Im;
            float *tmp_ptr = data;
            size_t  k;
            for ( k = 0; k < numel; k++ ) {
                *rp++ = *tmp_ptr++;
                *ip++ = *tmp_ptr++;
            }
            break;
        }
#ifdef HAVE_MAT_INT64_T
        case MAT_T_INT64:
        {
            mat_int64_t *rp      = complex_data->Re;
            mat_int64_t *ip      = complex_data->Im;
            mat_int64_t *tmp_ptr = data;
            size_t  k;
            for ( k = 0; k < numel; k++ ) {
                *rp++ = *tmp_ptr++;
                *ip++ = *tmp_ptr++;
            }
            break;
        }
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_T_UINT64:
        {
            mat_uint64_t *rp      = complex_data->Re;
            mat_uint64_t *ip      = complex_data->Im;
            mat_uint64_t *tmp_ptr = data;
            size_t  k;
            for ( k = 0; k < numel; k++ ) {
                *rp++ = *tmp_ptr++;
                *ip++ = *tmp_ptr++;
            }
            break;
        }
#endif
        case MAT_T_INT32:
        {
            mat_int32_t *rp      = complex_data->Re;
            mat_int32_t *ip      = complex_data->Im;
            mat_int32_t *tmp_ptr = data;
            size_t  k;
            for ( k = 0; k < numel; k++ ) {
                *rp++ = *tmp_ptr++;
                *ip++ = *tmp_ptr++;
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t *rp      = complex_data->Re;
            mat_uint32_t *ip      = complex_data->Im;
            mat_uint32_t *tmp_ptr = data;
            size_t  k;
            for ( k = 0; k < numel; k++ ) {
                *rp++ = *tmp_ptr++;
                *ip++ = *tmp_ptr++;
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t *rp      = complex_data->Re;
            mat_int16_t *ip      = complex_data->Im;
            mat_int16_t *tmp_ptr = data;
            size_t  k;
            for ( k = 0; k < numel; k++ ) {
                *rp++ = *tmp_ptr++;
                *ip++ = *tmp_ptr++;
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t *rp      = complex_data->Re;
            mat_uint16_t *ip      = complex_data->Im;
            mat_uint16_t *tmp_ptr = data;
            size_t  k;
            for ( k = 0; k < numel; k++ ) {
                *rp++ = *tmp_ptr++;
                *ip++ = *tmp_ptr++;
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t *rp      = complex_data->Re;
            mat_int8_t *ip      = complex_data->Im;
            mat_int8_t *tmp_ptr = data;
            size_t  k;
            for ( k = 0; k < numel; k++ ) {
                *rp++ = *tmp_ptr++;
                *ip++ = *tmp_ptr++;
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t *rp      = complex_data->Re;
            mat_uint8_t *ip      = complex_data->Im;
            mat_uint8_t *tmp_ptr = data;
            size_t  k;
            for ( k = 0; k < numel; k++ ) {
                *rp++ = *tmp_ptr++;
                *ip++ = *tmp_ptr++;
            }
            break;
        }
    } /* switch class_type */
}

static void
Mat_H5ReadDatasetInfo(mat_t *mat,matvar_t *matvar,hid_t dset_id)
{
    ssize_t  name_len;
    /* FIXME */
    hsize_t  dims[10];
    hid_t   attr_id,type_id,space_id;
    H5E_auto_t efunc;
    void       *client_data;

#if 0
    matvar->fp = mat;
    name_len = H5Gget_objname_by_idx(fid,mat->next_index,NULL,0);
    matvar->name = malloc(1+name_len);
    if ( matvar->name ) {
        name_len = H5Gget_objname_by_idx(fid,mat->next_index,
                                         matvar->name,1+name_len);
        matvar->name[name_len] = '\0';
    }
    dset_id = H5Dopen(fid,matvar->name);
#endif

    /* Get the HDF5 name of the variable */
    name_len = H5Iget_name(dset_id,NULL,0);
    if ( name_len > 0 ) {
        matvar->internal->hdf5_name = malloc(name_len+1);
        (void)H5Iget_name(dset_id,matvar->internal->hdf5_name,name_len+1);
    } else {
        /* Can not get an internal name, so leave the identifier open */
        matvar->internal->id = dset_id;
    }

    space_id     = H5Dget_space(dset_id);
    matvar->rank = H5Sget_simple_extent_ndims(space_id);
    matvar->dims = malloc(matvar->rank*sizeof(*matvar->dims));
    if ( NULL != matvar->dims ) {
        int k;
        H5Sget_simple_extent_dims(space_id,dims,NULL);
        for ( k = 0; k < matvar->rank; k++ )
            matvar->dims[k] = dims[matvar->rank - k - 1];
    }
    H5Sclose(space_id);

    attr_id = H5Aopen_name(dset_id,"MATLAB_class");
    type_id  = H5Aget_type(attr_id);
    if ( H5T_STRING == H5Tget_class(type_id) ) {
        char *class_str = calloc(H5Tget_size(type_id)+1,1);
        if ( NULL != class_str ) {
            hid_t class_id = H5Tcopy(H5T_C_S1);
            H5Tset_size(class_id,H5Tget_size(type_id));
            H5Aread(attr_id,class_id,class_str);
            H5Tclose(class_id);
            matvar->class_type = Mat_class_str_to_id(class_str);
            matvar->data_type  = Mat_ClassToType73(matvar->class_type);
            free(class_str);
        }
    }
    H5Tclose(type_id);
    H5Aclose(attr_id);

    /* Turn off error printing so testing for attributes doesn't print
     * error stacks
     */
    H5Eget_auto(&efunc,&client_data);
    H5Eset_auto((H5E_auto_t)0,NULL);

    attr_id = H5Aopen_name(dset_id,"MATLAB_global");
    /* FIXME: Check that dataspace is scalar */
    if ( -1 < attr_id ) {
        H5Aread(attr_id,H5T_NATIVE_INT,&matvar->isGlobal);
        H5Aclose(attr_id);
    }

    /* Check for attribute that indicates an empty array */
    attr_id = H5Aopen_name(dset_id,"MATLAB_empty");
    /* FIXME: Check that dataspace is scalar */
    if ( -1 < attr_id ) {
        int empty = 0;
        H5Aread(attr_id,H5T_NATIVE_INT,&empty);
        H5Aclose(attr_id);
        if ( empty ) {
            matvar->rank = matvar->dims[0];
            matvar->dims = calloc(matvar->rank,sizeof(*matvar->dims));
            H5Dread(dset_id,Mat_dims_type_to_hid_t(),H5S_ALL,H5S_ALL,
                    H5P_DEFAULT,matvar->dims);
        }
    }

    H5Eset_auto(efunc,client_data);

    /* Test if dataset type is compound and if so if it's complex */
    type_id = H5Dget_type(dset_id);
    if ( H5T_COMPOUND == H5Tget_class(type_id) ) {
        /* FIXME: Any more checks? */
        matvar->isComplex = MAT_F_COMPLEX;
    }
    H5Tclose(type_id);
}

static void
Mat_H5ReadGroupInfo(mat_t *mat,matvar_t *matvar,hid_t dset_id)
{
    ssize_t  name_len;
    int      k;
    char    **fieldnames = NULL;
    /* FIXME */
    hsize_t  dims[10],nfields=0,numel;
    hid_t   attr_id,type_id,space_id,field_id,field_type_id;
    matvar_t **fields;
    H5E_auto_t efunc;
    void       *client_data;

#if 0
    matvar->fp = mat;
    name_len = H5Gget_objname_by_idx(fid,mat->next_index,NULL,0);
    matvar->name = malloc(1+name_len);
    if ( matvar->name ) {
        name_len = H5Gget_objname_by_idx(fid,mat->next_index,
                                         matvar->name,1+name_len);
        matvar->name[name_len] = '\0';
    }
    dset_id = H5Gopen(fid,matvar->name);
#endif

    /* Get the HDF5 name of the variable */
    name_len = H5Iget_name(dset_id,NULL,0);
    if ( name_len > 0 ) {
        matvar->internal->hdf5_name = malloc(name_len+1);
        (void)H5Iget_name(dset_id,matvar->internal->hdf5_name,name_len+1);
    } else {
        /* Can not get an internal name, so leave the identifier open */
        matvar->internal->id = dset_id;
    }

    attr_id = H5Aopen_name(dset_id,"MATLAB_class");
    type_id  = H5Aget_type(attr_id);
    if ( H5T_STRING == H5Tget_class(type_id) ) {
        char *class_str = calloc(H5Tget_size(type_id)+1,1);
        if ( NULL != class_str ) {
            hid_t class_id = H5Tcopy(H5T_C_S1);
            H5Tset_size(class_id,H5Tget_size(type_id));
            H5Aread(attr_id,class_id,class_str);
            H5Tclose(class_id);
            matvar->class_type = Mat_class_str_to_id(class_str);
            matvar->data_type  = Mat_ClassToType73(matvar->class_type);
            free(class_str);
        }
    }
    H5Tclose(type_id);
    H5Aclose(attr_id);

    /* Turn off error printing so testing for attributes doesn't print
     * error stacks
     */
    H5Eget_auto(&efunc,&client_data);
    H5Eset_auto((H5E_auto_t)0,NULL);

    /* Check if the variable is global */
    attr_id = H5Aopen_name(dset_id,"MATLAB_global");
    /* FIXME: Check that dataspace is scalar */
    if ( -1 < attr_id ) {
        H5Aread(attr_id,H5T_NATIVE_INT,&matvar->isGlobal);
        H5Aclose(attr_id);
    }

    /* Check if the variable is sparse */
    attr_id = H5Aopen_name(dset_id,"MATLAB_sparse");
    if ( -1 < attr_id ) {
        hid_t sparse_dset_id;
        unsigned nrows = 0;

        H5Eset_auto(efunc,client_data);

        H5Aread(attr_id,H5T_NATIVE_UINT,&nrows);
        H5Aclose(attr_id);

        matvar->class_type = MAT_C_SPARSE;

        matvar->rank = 2;
        matvar->dims = malloc(matvar->rank*sizeof(*matvar->dims));
        matvar->dims[0] = nrows;

        sparse_dset_id = H5Dopen(dset_id,"jc");
        if ( -1 < sparse_dset_id ) {
            space_id = H5Dget_space(sparse_dset_id);
            (void)H5Sget_simple_extent_dims(space_id,dims,NULL);
            matvar->dims[1] = dims[0] - 1;
        }

        /* Test if dataset type is compound and if so if it's complex */
        sparse_dset_id = H5Dopen(dset_id,"data");
        if ( -1 < sparse_dset_id ) {
            type_id = H5Dget_type(sparse_dset_id);
            if ( H5T_COMPOUND == H5Tget_class(type_id) ) {
                /* FIXME: Any more checks? */
                matvar->isComplex = MAT_F_COMPLEX;
            }
            H5Tclose(type_id);
        }
        return;
    }

    /* Check if the structure defines its fields in MATLAB_fields */
    attr_id = H5Aopen_name(dset_id,"MATLAB_fields");
    if ( -1 < attr_id ) {
        int field_length;
        hvl_t     *fieldnames_vl;
        space_id = H5Aget_space(attr_id);
        (void)H5Sget_simple_extent_dims(space_id,&nfields,NULL);
        field_id = H5Aget_type(attr_id);
        fieldnames_vl = malloc(nfields*sizeof(*fieldnames_vl));
        H5Aread(attr_id,field_id,fieldnames_vl);

        fieldnames = malloc(nfields*sizeof(*fieldnames));
        for ( k = 0; k < nfields; k++ ) {
            fieldnames[k] = calloc(fieldnames_vl[k].len+1,1);
            memcpy(fieldnames[k],fieldnames_vl[k].p,
                   fieldnames_vl[k].len);
        }

        H5Sclose(space_id);
        H5Tclose(field_id);
        H5Aclose(attr_id);
        free(fieldnames_vl);
    } else {
        hsize_t next_index = 0,num_objs  = 0;
        H5Gget_num_objs(dset_id,&num_objs);
        fieldnames = calloc(num_objs,sizeof(*fieldnames));
        /* FIXME: follow symlinks, datatypes? */
        while ( next_index < num_objs ) {
            if ( H5G_DATASET == H5Gget_objtype_by_idx(dset_id,next_index) ) {
                int len;
                len = H5Gget_objname_by_idx(dset_id,next_index,NULL,0);
                fieldnames[nfields] = calloc(len+1,sizeof(*fieldnames));
                H5Gget_objname_by_idx(dset_id,next_index,fieldnames[nfields],len+1);
                nfields++;
            } else if ( H5G_GROUP == H5Gget_objtype_by_idx(dset_id,next_index) ) {
                /* Check that this is not the /#refs# group */
                char name[128] = {0,};
                (void)H5Gget_objname_by_idx(dset_id,next_index,name,127);
                if ( strcmp(name,"#refs#") ) {
                    int len;
                    len = H5Gget_objname_by_idx(dset_id,next_index,NULL,0);
                    fieldnames[nfields] = calloc(len+1,1);
                    H5Gget_objname_by_idx(dset_id,next_index,fieldnames[nfields],len+1);
                    nfields++;
                }
            }
            next_index++;
        }
    }

    if ( -1 < (field_id = H5Dopen(dset_id,fieldnames[0])) ) {
        field_type_id = H5Dget_type(field_id);
        if ( H5T_REFERENCE == H5Tget_class(field_type_id) ) {
            space_id        = H5Dget_space(field_id);
            matvar->rank    = H5Sget_simple_extent_ndims(space_id);
            matvar->dims   = malloc(matvar->rank*sizeof(*matvar->dims));
            (void)H5Sget_simple_extent_dims(space_id,dims,NULL);
            numel = 1;
            for ( k = 0; k < matvar->rank; k++ ) {
                matvar->dims[k] = dims[matvar->rank - k - 1];
                numel *= matvar->dims[k];
            }
            H5Sclose(space_id);
        } else {
            /* Structure should be a scalar */
            matvar->rank    = 2;
            matvar->dims    = malloc(2*sizeof(*matvar->dims));
            matvar->dims[0] = 1;
            matvar->dims[1] = 1;
            numel = 1;
        }
        H5Tclose(field_type_id);
        H5Dclose(field_id);
    } else {
        /* Structure should be a scalar */
        numel = 1;
        matvar->rank    = 2;
        matvar->dims    = malloc(2*sizeof(*matvar->dims));
        matvar->dims[0] = 1;
        matvar->dims[1] = 1;
    }

    if ( !nfields ) {
        H5Eset_auto(efunc,client_data);
        return;
    }

    fields = malloc(nfields*numel*sizeof(*fields));
    matvar->data = fields;
    matvar->data_size = sizeof(*fields);
    matvar->nbytes    = nfields*numel*matvar->data_size;
    if ( NULL != fields ) {
        for ( k = 0; k < nfields; k++ ) {
            int l;
            fields[k] = NULL;
            if ( -1 < (field_id = H5Dopen(dset_id,fieldnames[k])) ) {
                field_type_id = H5Dget_type(field_id);
                switch ( H5Tget_class(field_type_id) ) {
                    case H5T_REFERENCE:
                    {
                        hobj_ref_t *ref_ids = malloc(numel*sizeof(*ref_ids));
                        H5Dread(field_id,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,
                                H5P_DEFAULT,ref_ids);
                        for ( l = 0; l < numel; l++ ) {
                            hid_t ref_id;
                            fields[l*nfields+k] = Mat_VarCalloc();
                            fields[l*nfields+k]->name = strdup(fieldnames[k]);
                            fields[l*nfields+k]->internal->hdf5_ref=ref_ids[l];
                            /* Get the HDF5 name of the variable */
                            name_len = H5Iget_name(field_id,NULL,0);
                            if ( name_len > 0 ) {
                                fields[l*nfields+k]->internal->hdf5_name = 
                                    malloc(name_len+1);
                                (void)H5Iget_name(field_id,
                                    fields[l*nfields+k]->internal->hdf5_name,
                                    name_len+1);
                            }
                            /* Closing of ref_id is done in
                             * Mat_H5ReadNextReferenceInfo
                             */
                            ref_id = H5Rdereference(field_id,H5R_OBJECT,
                                                    ref_ids+l);
                            fields[l*nfields+k]->internal->id=ref_id;
                            Mat_H5ReadNextReferenceInfo(ref_id,fields[l*nfields+k],mat);
                        }
                        free(ref_ids);
                        break;
                    }
                    case H5T_INTEGER:
                    case H5T_FLOAT:
                    case H5T_COMPOUND:
                    {
                        fields[k] = Mat_VarCalloc();
                        fields[k]->internal->fp   = mat;
                        fields[k]->name = strdup(fieldnames[k]);
                        Mat_H5ReadDatasetInfo(mat,fields[k],field_id);
                        break;
                    }
                }
                H5Dclose(field_id);
            } else if ( -1 < (field_id=H5Gopen(dset_id,fieldnames[k])) ) {
                fields[k] = Mat_VarCalloc();
                fields[k]->internal->fp   = mat;
                fields[k]->name = strdup(fieldnames[k]);
                Mat_H5ReadGroupInfo(mat,fields[k],field_id);
                H5Gclose(field_id);
            }
        }
    }

    H5Eset_auto(efunc,client_data);
}

static void
Mat_H5ReadNextReferenceInfo(hid_t ref_id,matvar_t *matvar,mat_t *mat)
{
    hid_t       gid;
    H5E_auto_t  efunc;
    void       *client_data;

    if( ref_id < 0 || matvar == NULL)
        return;

    switch ( H5Iget_type(ref_id) ) {
        case H5I_DATASET:
        {
            ssize_t  name_len;
            /* FIXME */
            hsize_t  dims[10];
            hid_t   attr_id,type_id,dset_id,space_id;

            //matvar->fp = mat;
            dset_id = ref_id;

#if 0
            /* Get the HDF5 name of the variable */
            name_len = H5Iget_name(dset_id,NULL,0);
            matvar->hdf5_name = malloc(name_len+1);
            (void)H5Iget_name(dset_id,matvar->hdf5_name,name_len);
            printf("%s\n",matvar->hdf5_name);
#endif

            /* Get the rank and dimensions of the data */
            space_id = H5Dget_space(dset_id);
            matvar->rank = H5Sget_simple_extent_ndims(space_id);
            matvar->dims = malloc(matvar->rank*sizeof(*matvar->dims));
            if ( NULL != matvar->dims ) {
                int k;
                H5Sget_simple_extent_dims(space_id,dims,NULL);
                for ( k = 0; k < matvar->rank; k++ )
                    matvar->dims[k] = dims[matvar->rank - k - 1];
            }
            H5Sclose(space_id);

            attr_id = H5Aopen_name(dset_id,"MATLAB_class");
            type_id  = H5Aget_type(attr_id);
            if ( H5T_STRING == H5Tget_class(type_id) ) {
                char *class_str = calloc(H5Tget_size(type_id)+1,1);
                if ( NULL != class_str ) {
                    hid_t class_id = H5Tcopy(H5T_C_S1);
                    H5Tset_size(class_id,H5Tget_size(type_id));
                    H5Aread(attr_id,class_id,class_str);
                    H5Tclose(class_id);
                    matvar->class_type = Mat_class_str_to_id(class_str);
                    matvar->data_type  = Mat_ClassToType73(matvar->class_type);
                    free(class_str);
                }
            }
            H5Tclose(type_id);
            H5Aclose(attr_id);

            /* Turn off error printing so testing for attributes doesn't print
             * error stacks
             */
            H5Eget_auto(&efunc,&client_data);
            H5Eset_auto((H5E_auto_t)0,NULL);

            attr_id = H5Aopen_name(dset_id,"MATLAB_global");
            /* FIXME: Check that dataspace is scalar */
            if ( -1 < attr_id ) {
                H5Aread(attr_id,H5T_NATIVE_INT,&matvar->isGlobal);
                H5Aclose(attr_id);
            }

            /* Check for attribute that indicates an empty array */
            attr_id = H5Aopen_name(dset_id,"MATLAB_empty");
            /* FIXME: Check that dataspace is scalar */
            if ( -1 < attr_id ) {
                int empty = 0;
                H5Aread(attr_id,H5T_NATIVE_INT,&empty);
                H5Aclose(attr_id);
                if ( empty ) {
                    matvar->rank = matvar->dims[0];
                    matvar->dims = calloc(matvar->rank,sizeof(*matvar->dims));
                    H5Dread(dset_id,Mat_dims_type_to_hid_t(),H5S_ALL,H5S_ALL,
                            H5P_DEFAULT,matvar->dims);
                }
            }

            /* Test if dataset type is compound and if so if it's complex */
            type_id = H5Dget_type(dset_id);
            if ( H5T_COMPOUND == H5Tget_class(type_id) ) {
                /* FIXME: Any more checks? */
                matvar->isComplex = MAT_F_COMPLEX;
            }
            H5Tclose(type_id);

            /* If the dataset is a cell array read theinfo of the cells */
            if ( MAT_C_CELL == matvar->class_type ) {
                matvar_t **cells;
                int i,ncells = 1;
                hid_t field_id,ref_id,field_type_id;
                hobj_ref_t *ref_ids;

                for ( i = 0; i < matvar->rank; i++ )
                    ncells *= matvar->dims[i];
                matvar->data_size = sizeof(matvar_t**);
                matvar->nbytes    = ncells*matvar->data_size;
                matvar->data      = malloc(matvar->nbytes);
                cells  = matvar->data;

                ref_ids = malloc(ncells*sizeof(*ref_ids));
                H5Dread(dset_id,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                        ref_ids);
                for ( i = 0; i < ncells; i++ ) {
                    hid_t ref_id;
                    cells[i] = Mat_VarCalloc();
                    cells[i]->internal->hdf5_ref = ref_ids[i];
                    /* Closing of ref_id is done in Mat_H5ReadNextReferenceInfo */
                    ref_id = H5Rdereference(dset_id,H5R_OBJECT,ref_ids+i);
                    cells[i]->internal->id=ref_id;
                    cells[i]->internal->fp=matvar->internal->fp;
                    Mat_H5ReadNextReferenceInfo(ref_id,cells[i],mat);
                }
                free(ref_ids);
            }

            if ( matvar->internal->id != dset_id ) {
                /* Close dataset and increment count */
                H5Dclose(dset_id);
            }

            H5Eset_auto(efunc,client_data);
            /*H5Dclose(dset_id);*/
            break;
        }
        case H5I_GROUP:
        {
            ssize_t  name_len;
            int      k;
            char    **fieldnames = NULL;
            /* FIXME */
            hsize_t  dims[10],nfields,numel;
            hid_t   attr_id,type_id,dset_id,space_id,field_id,field_type_id;
            matvar_t **fields;

            dset_id = ref_id;

            /* Get the HDF5 name of the variable */
            name_len = H5Iget_name(dset_id,NULL,0);
            if ( name_len > 0 ) {
                matvar->internal->hdf5_name = malloc(name_len+1);
                (void)H5Iget_name(dset_id,matvar->internal->hdf5_name,name_len+1);
            } else {
                /* Can not get an internal name, so leave the identifier open */
                matvar->internal->id = dset_id;
            }

            Mat_H5ReadGroupInfo(mat,matvar,dset_id);

            H5Eset_auto(efunc,client_data);
            /*H5Gclose(dset_id);*/
            break;
        }
        default:
            break;
    }
    return;
}

static void
Mat_H5ReadNextReferenceData(hid_t ref_id,matvar_t *matvar,mat_t *mat)
{
    int k;
    size_t numel;
    hid_t fid,dset_id;

    if( ref_id < 0 || matvar == NULL)
        return;

    /* If the datatype with references is a cell, we've already read info into
     * the variable data, so just loop over each cell element and call
     * Mat_H5ReadNextReferenceData on it.
     */
    if ( MAT_C_CELL == matvar->class_type ) {
        matvar_t **cells = matvar->data;
        numel = 1;
        for ( k = 0; k < matvar->rank; k++ )
            numel *= matvar->dims[k];
        for ( k = 0; k < numel; k++ )
            Mat_H5ReadNextReferenceData(cells[k]->internal->id,cells[k],mat);
        return;
    }

    switch ( H5Iget_type(ref_id) ) {
        case H5I_DATASET:
        {
            hid_t data_type_id;
            numel = 1;
            for ( k = 0; k < matvar->rank; k++ )
                numel *= matvar->dims[k];

            if ( MAT_C_CHAR == matvar->class_type ) {
                matvar->data_type = MAT_T_UINT8;
                matvar->data_size = Mat_SizeOf(MAT_T_UINT8);
                data_type_id      = Mat_data_type_to_hid_t(MAT_T_UINT8);
            } else {
                matvar->data_size = Mat_SizeOfClass(matvar->class_type);
                data_type_id      = Mat_class_type_to_hid_t(matvar->class_type);
            }
            matvar->nbytes    = numel*matvar->data_size;

            if ( matvar->nbytes < 1 ) {
                H5Dclose(ref_id);
                break;
            }

            dset_id = ref_id;

            if ( !matvar->isComplex ) {
                matvar->data      = malloc(matvar->nbytes);
                if ( NULL != matvar->data ) {
                    H5Dread(dset_id,data_type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                            matvar->data);
                }
            } else {
                struct ComplexSplit *complex_data;
                hid_t h5_complex_base,h5_complex;

                complex_data     = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);

                h5_complex_base = data_type_id;
                h5_complex      = H5Tcreate(H5T_COMPOUND,
                                            H5Tget_size(h5_complex_base));
                H5Tinsert(h5_complex,"real",0,h5_complex_base);
                H5Dread(dset_id,h5_complex,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                        complex_data->Re);
                H5Tclose(h5_complex);

                h5_complex      = H5Tcreate(H5T_COMPOUND,
                                            H5Tget_size(h5_complex_base));
                H5Tinsert(h5_complex,"imag",0,h5_complex_base);
                H5Dread(dset_id,h5_complex,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                        complex_data->Im);
                H5Tclose(h5_complex);
                matvar->data = complex_data;
            }
            H5Dclose(dset_id);
            break;
        }
        case H5I_GROUP:
        {
            matvar_t **fields;
            int i,nfields = 0;
            hid_t field_id,ref_id,field_type_id;

            if ( MAT_C_SPARSE == matvar->class_type ) {
                Mat_VarRead73(mat,matvar);
            } else {
                if ( !matvar->nbytes || !matvar->data_size || NULL == matvar->data )
                    break;
                nfields = matvar->nbytes / matvar->data_size;
                fields  = matvar->data;
                for ( i = 0; i < nfields; i++ ) {
                    if (  0 < fields[i]->internal->hdf5_ref &&
                         -1 < fields[i]->internal->id ) {
                        /* Dataset of references */
                        Mat_H5ReadNextReferenceData(fields[i]->internal->id,fields[i],mat);
                    } else {
                        Mat_VarRead73(mat,fields[i]);
                    }
                }
            }
            break;
        }
        default:
            break;
    }
    return;
}

static int
Mat_WriteNextStructField73(hid_t id,matvar_t *matvar,const char *name)
{
    unsigned long k,numel;
    hid_t mspace_id,dset_id,attr_type_id,attr_id,aspace_id;

    if ( NULL == matvar )
        return -1;

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
            numel = 1;
            for ( k = 0; k < matvar->rank; k++ ) {
                perm_dims[k] = matvar->dims[matvar->rank-k-1];
                numel *= perm_dims[k];
            }

            if ( 0 == numel || NULL == matvar->data ) {
                hsize_t rank = matvar->rank;
                unsigned empty = 1;
                mspace_id = H5Screate_simple(1,&rank,NULL);
                dset_id = H5Dcreate(id,name,H5T_NATIVE_HSIZE,mspace_id,
                                    H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                /* Write the empty attribute */
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_empty",H5T_NATIVE_UINT,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,H5T_NATIVE_UINT,&empty);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                /* Write the dimensions as the data */
                H5Dwrite(dset_id,Mat_dims_type_to_hid_t(),H5S_ALL,H5S_ALL,
                         H5P_DEFAULT,matvar->dims);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            } else if ( matvar->isComplex ) {
                hid_t h5_complex,h5_complex_base;

                h5_complex_base = Mat_class_type_to_hid_t(matvar->class_type);
                h5_complex      = H5Tcreate(H5T_COMPOUND,
                                      2*H5Tget_size(h5_complex_base));
                H5Tinsert(h5_complex,"real",0,h5_complex_base);
                H5Tinsert(h5_complex,"imag",H5Tget_size(h5_complex_base),
                          h5_complex_base);
                mspace_id = H5Screate_simple(matvar->rank,perm_dims,NULL);
                dset_id = H5Dcreate(id,name,h5_complex,mspace_id,H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                H5Tclose(h5_complex);

                /* Write real part of dataset */
                h5_complex = H5Tcreate(H5T_COMPOUND,
                                       H5Tget_size(h5_complex_base));
                H5Tinsert(h5_complex,"real",0,h5_complex_base);
                H5Dwrite(dset_id,h5_complex,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                         ((struct ComplexSplit*)matvar->data)->Re);
                H5Tclose(h5_complex);

                /* Write imaginary part of dataset */
                h5_complex      = H5Tcreate(H5T_COMPOUND,
                                      H5Tget_size(h5_complex_base));
                H5Tinsert(h5_complex,"imag",0,h5_complex_base);
                H5Dwrite(dset_id,h5_complex,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                         ((struct ComplexSplit*)matvar->data)->Im);
                H5Tclose(h5_complex);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            } else { /* matvar->isComplex */
                mspace_id = H5Screate_simple(matvar->rank,perm_dims,NULL);
                dset_id = H5Dcreate(id,name,
                    Mat_class_type_to_hid_t(matvar->class_type),mspace_id,
                    H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                H5Dwrite(dset_id,Mat_data_type_to_hid_t(matvar->data_type),
                    H5S_ALL,H5S_ALL,H5P_DEFAULT,matvar->data);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            }
            break;
        case MAT_C_CHAR:
        {
            if ( 0 == numel || NULL == matvar->data ) {
                hsize_t rank = matvar->rank;
                unsigned empty = 1;
                mspace_id = H5Screate_simple(1,&rank,NULL);
                dset_id = H5Dcreate(id,name,H5T_NATIVE_HSIZE,mspace_id,
                                    H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                /* Write the empty attribute */
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_empty",H5T_NATIVE_UINT,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,H5T_NATIVE_UINT,&empty);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                /* Write the dimensions as the data */
                H5Dwrite(dset_id,Mat_dims_type_to_hid_t(),H5S_ALL,H5S_ALL,
                         H5P_DEFAULT,matvar->dims);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            } else {
                int matlab_int_decode = 2;
                for ( k = 0; k < matvar->rank; k++ )
                    perm_dims[k] = matvar->dims[matvar->rank-k-1];

                mspace_id = H5Screate_simple(matvar->rank,perm_dims,NULL);
                switch ( matvar->data_type ) {
                    case MAT_T_UTF32:
                    case MAT_T_INT32:
                    case MAT_T_UINT32:
                        /* Not sure matlab will actually handle this */
                        dset_id = H5Dcreate(id,name,
                            Mat_class_type_to_hid_t(MAT_C_UINT32),mspace_id,
                            H5P_DEFAULT);
                        break;
                    case MAT_T_UTF16:
                    case MAT_T_UTF8:
                    case MAT_T_INT16:
                    case MAT_T_UINT16:
                    case MAT_T_INT8:
                    case MAT_T_UINT8:
                        dset_id = H5Dcreate(id,name,
                            Mat_class_type_to_hid_t(MAT_C_UINT16),mspace_id,
                            H5P_DEFAULT);
                        break;
                }
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,Mat_class_names[matvar->class_type]);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);

                attr_type_id = H5Tcopy(H5T_NATIVE_INT);
                attr_id = H5Acreate(dset_id,"MATLAB_int_decode",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,&matlab_int_decode);
                H5Tclose(attr_type_id);
                H5Sclose(aspace_id);

                H5Dwrite(dset_id,Mat_data_type_to_hid_t(matvar->data_type),
                    H5S_ALL,H5S_ALL,H5P_DEFAULT,matvar->data);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            }
            break;
        }
        case MAT_C_STRUCT:
        {
            hid_t struct_id,str_type_id,fieldnames_id;
            hsize_t    nfields,nmemb;
            matvar_t **fields;
            hvl_t     *fieldnames;
            char       id_name[128] = {'\0',};
            int        is_ref;

            nmemb = matvar->dims[0];
            for ( k = 1; k < matvar->rank; k++ )
                nmemb *= matvar->dims[k];
 
            if ( 0 == nmemb || NULL == matvar->data ) {
                hsize_t rank = matvar->rank;
                unsigned empty = 1;
                mspace_id = H5Screate_simple(1,&rank,NULL);
                dset_id = H5Dcreate(id,name,H5T_NATIVE_HSIZE,mspace_id,
                                    H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                /* Write the empty attribute */
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_empty",H5T_NATIVE_UINT,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,H5T_NATIVE_UINT,&empty);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);

                nfields = matvar->nbytes / (matvar->data_size);
                if ( nfields ) {
                    str_type_id = H5Tcopy(H5T_C_S1);
                    fieldnames = malloc(nfields*sizeof(*fieldnames));
                    fields     = matvar->data;
                    for ( k = 0; k < nfields; k++ ) {
                        fieldnames[k].len = strlen(fields[k]->name);
                        fieldnames[k].p   = fields[k]->name;
                    }
                    H5Tset_size(str_type_id,1);
                    fieldnames_id = H5Tvlen_create(str_type_id);
                    aspace_id     = H5Screate_simple(1,&nfields,NULL);
                    attr_id = H5Acreate(dset_id,"MATLAB_fields",fieldnames_id,
                                        aspace_id,H5P_DEFAULT);
                    H5Awrite(attr_id,fieldnames_id,fieldnames);
                    H5Aclose(attr_id);
                    H5Sclose(aspace_id);
                    H5Tclose(fieldnames_id);
                    H5Tclose(str_type_id);
                    free(fieldnames);
                }

                /* Write the dimensions as the data */
                H5Dwrite(dset_id,Mat_dims_type_to_hid_t(),H5S_ALL,H5S_ALL,
                         H5P_DEFAULT,matvar->dims);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            } else {
                (void)H5Iget_name(id,id_name,127);
                is_ref = !strcmp(id_name,"/#refs#");
                struct_id = H5Gcreate(id,name,0);
                if ( struct_id < 0 ) {
                    Mat_Critical("Error creating group for struct %s",name);
                } else {
                    str_type_id = H5Tcopy(H5T_C_S1);
                    H5Tset_size(str_type_id,7);
                    aspace_id = H5Screate(H5S_SCALAR);
                    attr_id = H5Acreate(struct_id,"MATLAB_class",str_type_id,
                                        aspace_id,H5P_DEFAULT);
                    H5Awrite(attr_id,str_type_id,"struct");
                    H5Aclose(attr_id);

                    nfields = matvar->nbytes / (nmemb*matvar->data_size);

                    /* Structure with no fields */
                    if ( nfields == 0 ) {
                        H5Gclose(struct_id);
                        break;
                    }

                    fieldnames = malloc(nfields*sizeof(*fieldnames));
                    fields     = matvar->data;
                    for ( k = 0; k < nfields; k++ ) {
                        fieldnames[k].len = strlen(fields[k]->name);
                        fieldnames[k].p   = fields[k]->name;
                    }
                    H5Tset_size(str_type_id,1);
                    fieldnames_id = H5Tvlen_create(str_type_id);
                    aspace_id     = H5Screate_simple(1,&nfields,NULL);
                    attr_id = H5Acreate(struct_id,"MATLAB_fields",fieldnames_id,
                                        aspace_id,H5P_DEFAULT);
                    H5Awrite(attr_id,fieldnames_id,fieldnames);
                    H5Aclose(attr_id);
                    H5Sclose(aspace_id);
                    H5Tclose(fieldnames_id);
                    H5Tclose(str_type_id);
                    free(fieldnames);

                    if ( 1 == nmemb ) {
                        for ( k = 0; k < nmemb*nfields; k++ )
                            Mat_WriteNextStructField73(struct_id,fields[k],
                                fields[k]->name);
                    } else {
                        hid_t refs_id;

                        if (is_ref) {
                            refs_id = id;
                        } else {
                            if ((refs_id=H5Gopen(id,"/#refs#") < 0 ))
                                refs_id = H5Gcreate(id,"/#refs#",0);
                        }
                        if ( refs_id > -1 ) {
                            char name[64];
                            hobj_ref_t **refs;
                            hsize_t      num_obj;
                            int l;

                            refs = malloc(nfields*sizeof(*refs));
                            for ( l = 0; l < nfields; l++ )
                                refs[l] = malloc(nmemb*sizeof(*refs[l]));

                            for ( k = 0; k < nmemb; k++ ) {
                                for ( l = 0; l < nfields; l++ ) {
                                    (void)H5Gget_num_objs(refs_id,&num_obj);
                                    sprintf(name,"%lu",num_obj);
                                    Mat_WriteNextStructField73(refs_id,
                                        fields[k*nfields+l],name);
                                    sprintf(name,"/#refs#/%lu",num_obj);
                                    H5Rcreate(refs[l]+k,id,name,
                                              H5R_OBJECT,-1);
                                }
                            }
                            for ( k = 0; k < matvar->rank; k++ )
                                perm_dims[k] = matvar->dims[matvar->rank-k-1];

                            mspace_id=H5Screate_simple(matvar->rank,perm_dims,NULL);
                            for ( l = 0; l < nfields; l++ ) {
                                dset_id = H5Dcreate(struct_id,
                                    fields[l]->name,H5T_STD_REF_OBJ,mspace_id,
                                    H5P_DEFAULT);
                                H5Dwrite(dset_id,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,
                                    H5P_DEFAULT,refs[l]);
                                H5Dclose(dset_id);
                                free(refs[l]);
                            }
                            free(refs);
                            H5Sclose(mspace_id);
                            if ( !is_ref )
                                H5Gclose(refs_id);
                        }
                    }
                }
                H5Gclose(struct_id);
            }
            break;
        }
        case MAT_C_CELL:
        {
            hid_t str_type_id;
            hsize_t    nmemb;
            matvar_t **cells;
            hid_t refs_id;
            H5E_auto_t efunc;
            void       *client_data;
            int        is_ref;
            char       id_name[128] = {'\0',};

            cells = matvar->data;
            nmemb = matvar->dims[0];
            for ( k = 1; k < matvar->rank; k++ )
                nmemb *= matvar->dims[k];

            if ( 0 == nmemb || NULL == matvar->data ) {
                hsize_t rank = matvar->rank;
                unsigned empty = 1;
                mspace_id = H5Screate_simple(1,&rank,NULL);
                dset_id = H5Dcreate(id,name,H5T_NATIVE_HSIZE,mspace_id,
                                    H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                /* Write the empty attribute */
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_empty",H5T_NATIVE_UINT,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,H5T_NATIVE_UINT,&empty);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                /* Write the dimensions as the data */
                H5Dwrite(dset_id,Mat_dims_type_to_hid_t(),H5S_ALL,H5S_ALL,
                         H5P_DEFAULT,matvar->dims);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            } else {
                (void)H5Iget_name(id,id_name,127);
                is_ref = !strcmp(id_name,"/#refs#");
                if (is_ref) {
                    refs_id = id;
                } else {
                    /* Turn off error-checking so we don't get messages if opening
                     * group /#refs# fails
                     */
                    H5Eget_auto(&efunc,&client_data);
                    H5Eset_auto((H5E_auto_t)0,NULL);
                    if ((refs_id=H5Gopen(id,"/#refs#") < 0 ))
                        refs_id = H5Gcreate(id,"/#refs#",0);
                    H5Eset_auto(efunc,client_data);
                }
                
                if ( refs_id > -1 ) {
                    char        obj_name[64];
                    hobj_ref_t *refs;
                    hsize_t     num_obj;

                    refs = malloc(nmemb*sizeof(*refs));
                    mspace_id=H5Screate_simple(matvar->rank,perm_dims,NULL);
                    dset_id = H5Dcreate(id,name,H5T_STD_REF_OBJ,mspace_id,
                                        H5P_DEFAULT);

                    for ( k = 0; k < nmemb; k++ ) {
                        (void)H5Gget_num_objs(refs_id,&num_obj);
                        sprintf(obj_name,"%lu",num_obj);
                        Mat_WriteNextCellField73(refs_id,cells[k],obj_name);
                        sprintf(obj_name,"/#refs#/%lu",num_obj);
                        H5Rcreate(refs+k,id,obj_name,H5R_OBJECT,-1);
                    }
                    for ( k = 0; k < matvar->rank; k++ )
                        perm_dims[k] = matvar->dims[matvar->rank-k-1];

                    H5Dwrite(dset_id,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,
                            H5P_DEFAULT,refs);

                    str_type_id = H5Tcopy(H5T_C_S1);
                    H5Tset_size(str_type_id,7);
                    aspace_id = H5Screate(H5S_SCALAR);
                    attr_id = H5Acreate(dset_id,"MATLAB_class",str_type_id,
                                    aspace_id,H5P_DEFAULT);
                    H5Awrite(attr_id,str_type_id,"cell");
                    H5Aclose(attr_id);
                    H5Sclose(aspace_id);
                    H5Tclose(str_type_id);
                    H5Dclose(dset_id);
                    free(refs);
                    H5Sclose(mspace_id);
                    if ( !is_ref )
                        H5Gclose(refs_id);
                }
            }
            break;
        }
    }
    return 0;
}

static int
Mat_WriteNextCellField73(hid_t id,matvar_t *matvar,const char *name)
{
    unsigned long k,numel;
    hid_t mspace_id,dset_id,attr_type_id,attr_id,aspace_id;

    if ( NULL == matvar )
        return -1;

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
            numel = 1;
            for ( k = 0; k < matvar->rank; k++ ) {
                perm_dims[k] = matvar->dims[matvar->rank-k-1];
                numel *= perm_dims[k];
            }

            if ( 0 == numel || NULL == matvar->data ) {
                hsize_t rank = matvar->rank;
                unsigned empty = 1;
                mspace_id = H5Screate_simple(1,&rank,NULL);
                dset_id = H5Dcreate(id,name,H5T_NATIVE_HSIZE,mspace_id,
                                    H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                /* Write the empty attribute */
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_empty",H5T_NATIVE_UINT,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,H5T_NATIVE_UINT,&empty);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                /* Write the dimensions as the data */
                H5Dwrite(dset_id,Mat_dims_type_to_hid_t(),H5S_ALL,H5S_ALL,
                         H5P_DEFAULT,matvar->dims);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            } else if ( matvar->isComplex ) {
                hid_t h5_complex,h5_complex_base;

                h5_complex_base = Mat_class_type_to_hid_t(matvar->class_type);
                h5_complex      = H5Tcreate(H5T_COMPOUND,
                                      2*H5Tget_size(h5_complex_base));
                H5Tinsert(h5_complex,"real",0,h5_complex_base);
                H5Tinsert(h5_complex,"imag",H5Tget_size(h5_complex_base),
                          h5_complex_base);
                mspace_id = H5Screate_simple(matvar->rank,perm_dims,NULL);
                dset_id = H5Dcreate(id,name,h5_complex,mspace_id,H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                H5Tclose(h5_complex);

                /* Write real part of dataset */
                h5_complex = H5Tcreate(H5T_COMPOUND,
                                       H5Tget_size(h5_complex_base));
                H5Tinsert(h5_complex,"real",0,h5_complex_base);
                H5Dwrite(dset_id,h5_complex,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                         ((struct ComplexSplit*)matvar->data)->Re);
                H5Tclose(h5_complex);

                /* Write imaginary part of dataset */
                h5_complex      = H5Tcreate(H5T_COMPOUND,
                                      H5Tget_size(h5_complex_base));
                H5Tinsert(h5_complex,"imag",0,h5_complex_base);
                H5Dwrite(dset_id,h5_complex,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                         ((struct ComplexSplit*)matvar->data)->Im);
                H5Tclose(h5_complex);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            } else { /* matvar->isComplex */
                mspace_id = H5Screate_simple(matvar->rank,perm_dims,NULL);
                dset_id = H5Dcreate(id,name,
                    Mat_class_type_to_hid_t(matvar->class_type),mspace_id,
                    H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                H5Dwrite(dset_id,Mat_data_type_to_hid_t(matvar->data_type),
                    H5S_ALL,H5S_ALL,H5P_DEFAULT,matvar->data);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            }
            break;
        case MAT_C_CHAR:
        {
            if ( 0 == numel || NULL == matvar->data ) {
                hsize_t rank = matvar->rank;
                unsigned empty = 1;
                mspace_id = H5Screate_simple(1,&rank,NULL);
                dset_id = H5Dcreate(id,name,H5T_NATIVE_HSIZE,mspace_id,
                                    H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                /* Write the empty attribute */
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_empty",H5T_NATIVE_UINT,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,H5T_NATIVE_UINT,&empty);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                /* Write the dimensions as the data */
                H5Dwrite(dset_id,Mat_dims_type_to_hid_t(),H5S_ALL,H5S_ALL,
                         H5P_DEFAULT,matvar->dims);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            } else {
                int matlab_int_decode = 2;
                for ( k = 0; k < matvar->rank; k++ )
                    perm_dims[k] = matvar->dims[matvar->rank-k-1];

                mspace_id = H5Screate_simple(matvar->rank,perm_dims,NULL);
                switch ( matvar->data_type ) {
                    case MAT_T_UTF32:
                    case MAT_T_INT32:
                    case MAT_T_UINT32:
                        /* Not sure matlab will actually handle this */
                        dset_id = H5Dcreate(id,name,
                            Mat_class_type_to_hid_t(MAT_C_UINT32),mspace_id,
                            H5P_DEFAULT);
                        break;
                    case MAT_T_UTF16:
                    case MAT_T_UTF8:
                    case MAT_T_INT16:
                    case MAT_T_UINT16:
                    case MAT_T_INT8:
                    case MAT_T_UINT8:
                        dset_id = H5Dcreate(id,name,
                            Mat_class_type_to_hid_t(MAT_C_UINT16),mspace_id,
                            H5P_DEFAULT);
                        break;
                }
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,Mat_class_names[matvar->class_type]);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);

                attr_type_id = H5Tcopy(H5T_NATIVE_INT);
                attr_id = H5Acreate(dset_id,"MATLAB_int_decode",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,&matlab_int_decode);
                H5Tclose(attr_type_id);
                H5Sclose(aspace_id);

                H5Dwrite(dset_id,Mat_data_type_to_hid_t(matvar->data_type),
                    H5S_ALL,H5S_ALL,H5P_DEFAULT,matvar->data);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            }
            break;
        }
        case MAT_C_STRUCT:
        {
            hid_t struct_id,str_type_id,fieldnames_id;
            hsize_t    nfields,nmemb;
            matvar_t **fields;
            hvl_t     *fieldnames;
            char       id_name[128] = {'\0',};
            int        is_ref;

            nmemb = matvar->dims[0];
            for ( k = 1; k < matvar->rank; k++ )
                nmemb *= matvar->dims[k];
 
            if ( 0 == nmemb || NULL == matvar->data ) {
                hsize_t rank = matvar->rank;
                unsigned empty = 1;
                mspace_id = H5Screate_simple(1,&rank,NULL);
                dset_id = H5Dcreate(id,name,H5T_NATIVE_HSIZE,mspace_id,
                                    H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                /* Write the empty attribute */
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_empty",H5T_NATIVE_UINT,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,H5T_NATIVE_UINT,&empty);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);

                nfields = matvar->nbytes / (matvar->data_size);
                if ( nfields ) {
                    str_type_id = H5Tcopy(H5T_C_S1);
                    fieldnames = malloc(nfields*sizeof(*fieldnames));
                    fields     = matvar->data;
                    for ( k = 0; k < nfields; k++ ) {
                        fieldnames[k].len = strlen(fields[k]->name);
                        fieldnames[k].p   = fields[k]->name;
                    }
                    H5Tset_size(str_type_id,1);
                    fieldnames_id = H5Tvlen_create(str_type_id);
                    aspace_id     = H5Screate_simple(1,&nfields,NULL);
                    attr_id = H5Acreate(dset_id,"MATLAB_fields",fieldnames_id,
                                        aspace_id,H5P_DEFAULT);
                    H5Awrite(attr_id,fieldnames_id,fieldnames);
                    H5Aclose(attr_id);
                    H5Sclose(aspace_id);
                    H5Tclose(fieldnames_id);
                    H5Tclose(str_type_id);
                    free(fieldnames);
                }

                /* Write the dimensions as the data */
                H5Dwrite(dset_id,Mat_dims_type_to_hid_t(),H5S_ALL,H5S_ALL,
                         H5P_DEFAULT,matvar->dims);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            } else {
                (void)H5Iget_name(id,id_name,127);
                is_ref = !strcmp(id_name,"/#refs#");
                struct_id = H5Gcreate(id,name,0);
                if ( struct_id < 0 ) {
                    Mat_Critical("Error creating group for struct %s",name);
                } else {
                    str_type_id = H5Tcopy(H5T_C_S1);
                    H5Tset_size(str_type_id,7);
                    aspace_id = H5Screate(H5S_SCALAR);
                    attr_id = H5Acreate(struct_id,"MATLAB_class",str_type_id,
                                        aspace_id,H5P_DEFAULT);
                    H5Awrite(attr_id,str_type_id,"struct");
                    H5Aclose(attr_id);

                    nfields = matvar->nbytes / (nmemb*matvar->data_size);

                    /* Structure with no fields */
                    if ( nfields == 0 ) {
                        H5Gclose(struct_id);
                        break;
                    }


                    fieldnames = malloc(nfields*sizeof(*fieldnames));
                    fields     = matvar->data;
                    for ( k = 0; k < nfields; k++ ) {
                        fieldnames[k].len = strlen(fields[k]->name);
                        fieldnames[k].p   = fields[k]->name;
                    }
                    H5Tset_size(str_type_id,1);
                    fieldnames_id = H5Tvlen_create(str_type_id);
                    aspace_id     = H5Screate_simple(1,&nfields,NULL);
                    attr_id = H5Acreate(struct_id,"MATLAB_fields",fieldnames_id,
                                        aspace_id,H5P_DEFAULT);
                    H5Awrite(attr_id,fieldnames_id,fieldnames);
                    H5Aclose(attr_id);
                    H5Sclose(aspace_id);
                    H5Tclose(fieldnames_id);
                    H5Tclose(str_type_id);
                    free(fieldnames);

                    if ( 1 == nmemb ) {
                        for ( k = 0; k < nmemb*nfields; k++ )
                            Mat_WriteNextStructField73(struct_id,fields[k],
                                fields[k]->name);
                    } else {
                        hid_t refs_id;

                        if (is_ref) {
                            refs_id = id;
                        } else {
                            if ((refs_id=H5Gopen(id,"/#refs#") < 0 ))
                                refs_id = H5Gcreate(id,"/#refs#",0);
                        }
                        if ( refs_id > -1 ) {
                            char obj_name[64];
                            hobj_ref_t **refs;
                            hsize_t      num_obj;
                            int l;

                            refs = malloc(nfields*sizeof(*refs));
                            for ( l = 0; l < nfields; l++ )
                                refs[l] = malloc(nmemb*sizeof(*refs[l]));

                            for ( k = 0; k < nmemb; k++ ) {
                                for ( l = 0; l < nfields; l++ ) {
                                    (void)H5Gget_num_objs(refs_id,&num_obj);
                                    sprintf(obj_name,"%lu",num_obj);
                                    Mat_WriteNextStructField73(refs_id,
                                        fields[k*nfields+l],obj_name);
                                    sprintf(obj_name,"/#refs#/%lu",num_obj);
                                    H5Rcreate(refs[l]+k,id,obj_name,
                                              H5R_OBJECT,-1);
                                }
                            }
                            for ( k = 0; k < matvar->rank; k++ )
                                perm_dims[k] = matvar->dims[matvar->rank-k-1];

                            mspace_id=H5Screate_simple(matvar->rank,perm_dims,NULL);
                            for ( l = 0; l < nfields; l++ ) {
                                dset_id = H5Dcreate(struct_id,
                                    fields[l]->name,H5T_STD_REF_OBJ,mspace_id,
                                    H5P_DEFAULT);
                                H5Dwrite(dset_id,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,
                                    H5P_DEFAULT,refs[l]);
                                H5Dclose(dset_id);
                                free(refs[l]);
                            }
                            free(refs);
                            H5Sclose(mspace_id);
                            if ( !is_ref )
                                H5Gclose(refs_id);
                        }
                    }
                }
                H5Gclose(struct_id);
            }
            break;
        }
        case MAT_C_CELL:
        {
            hid_t str_type_id;
            hsize_t    nmemb;
            matvar_t **cells;
            hid_t refs_id;
            H5E_auto_t efunc;
            void       *client_data;
            int        is_ref;
            char       id_name[128] = {'\0',};

            cells = matvar->data;
            nmemb = matvar->dims[0];
            for ( k = 1; k < matvar->rank; k++ )
                nmemb *= matvar->dims[k];

            if ( 0 == nmemb || NULL == matvar->data ) {
                hsize_t rank = matvar->rank;
                unsigned empty = 1;
                mspace_id = H5Screate_simple(1,&rank,NULL);
                dset_id = H5Dcreate(id,name,H5T_NATIVE_HSIZE,mspace_id,
                                    H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                /* Write the empty attribute */
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_empty",H5T_NATIVE_UINT,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,H5T_NATIVE_UINT,&empty);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                /* Write the dimensions as the data */
                H5Dwrite(dset_id,Mat_dims_type_to_hid_t(),H5S_ALL,H5S_ALL,
                         H5P_DEFAULT,matvar->dims);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            } else {
                (void)H5Iget_name(id,id_name,127);
                is_ref = !strcmp(id_name,"/#refs#");
                if (is_ref) {
                    refs_id = id;
                } else {
                    /* Turn off error-checking so we don't get messages if opening
                     * group /#refs# fails
                     */
                    H5Eget_auto(&efunc,&client_data);
                    H5Eset_auto((H5E_auto_t)0,NULL);
                    if ((refs_id=H5Gopen(id,"/#refs#") < 0 ))
                        refs_id = H5Gcreate(id,"/#refs#",0);
                    H5Eset_auto(efunc,client_data);
                }
                
                if ( refs_id > -1 ) {
                    char        obj_name[64];
                    hobj_ref_t *refs;
                    hsize_t     num_obj;

                    refs = malloc(nmemb*sizeof(*refs));
                    mspace_id=H5Screate_simple(matvar->rank,perm_dims,NULL);
                    dset_id = H5Dcreate(id,name,H5T_STD_REF_OBJ,mspace_id,
                                        H5P_DEFAULT);

                    for ( k = 0; k < nmemb; k++ ) {
                        (void)H5Gget_num_objs(refs_id,&num_obj);
                        sprintf(obj_name,"%lu",num_obj);
                        Mat_WriteNextCellField73(refs_id,cells[k],obj_name);
                        sprintf(obj_name,"/#refs#/%lu",num_obj);
                        H5Rcreate(refs+k,id,obj_name,H5R_OBJECT,-1);
                    }
                    for ( k = 0; k < matvar->rank; k++ )
                        perm_dims[k] = matvar->dims[matvar->rank-k-1];

                    H5Dwrite(dset_id,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,
                            H5P_DEFAULT,refs);

                    str_type_id = H5Tcopy(H5T_C_S1);
                    H5Tset_size(str_type_id,7);
                    aspace_id = H5Screate(H5S_SCALAR);
                    attr_id = H5Acreate(dset_id,"MATLAB_class",str_type_id,
                                    aspace_id,H5P_DEFAULT);
                    H5Awrite(attr_id,str_type_id,"cell");
                    H5Aclose(attr_id);
                    H5Sclose(aspace_id);
                    H5Tclose(str_type_id);
                    H5Dclose(dset_id);
                    free(refs);
                    H5Sclose(mspace_id);
                    if ( !is_ref )
                        H5Gclose(refs_id);
                }
            }
            break;
        }
    }
    return 0;
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
Mat_Create73(const char *matname,const char *hdr_str)
{
    FILE *fp = NULL;
    mat_int16_t endian = 0, version;
    mat_t *mat = NULL;
    size_t err;
    time_t t;
    hid_t plist_id,fid;

    plist_id = H5Pcreate(H5P_FILE_CREATE);
    H5Pset_userblock(plist_id,512);
    fid = H5Fcreate(matname,H5F_ACC_TRUNC,plist_id,H5P_DEFAULT);
    H5Fclose(fid);
    H5Pclose(plist_id);

    fp = fopen(matname,"r+b");
    if ( !fp )
        return NULL;

    fseek(fp,0,SEEK_SET);

    mat = malloc(sizeof(*mat));
    if ( !mat ) {
        fclose(fp);
        return NULL;
    }

    mat->fp               = NULL;
    mat->header           = NULL;
    mat->subsys_offset    = NULL;
    mat->filename         = NULL;
    mat->version          = 0;
    mat->byteswap         = 0;
    mat->mode             = 0;
    mat->bof              = 0;
    mat->next_index       = 0;

    t = time(NULL);
    mat->filename = strdup_printf("%s",matname);
    mat->mode     = MAT_ACC_RDWR;
    mat->byteswap = 0;
    mat->header   = calloc(1,128);
    mat->subsys_offset = calloc(1,16);
    memset(mat->header,' ',128);
    if ( hdr_str == NULL ) {
        err = mat_snprintf(mat->header,116,"MATLAB 7.0 MAT-file, Platform: %s,"
                "Created by libmatio v%d.%d.%d on %s HDF5 schema 0.5",
                MATIO_PLATFORM,MATIO_MAJOR_VERSION,MATIO_MINOR_VERSION,
                MATIO_RELEASE_LEVEL,ctime(&t));
        mat->header[115] = '\0';    /* Just to make sure it's NULL terminated */    } else {
        err = mat_snprintf(mat->header,116,"%s",hdr_str);
    }
    mat->header[err] = ' ';
    mat_snprintf(mat->subsys_offset,15,"            ");
    mat->version = (int)0x0200;
    endian = 0x4d49;

    version = 0x0200;

    err = fwrite(mat->header,1,116,fp);
    err = fwrite(mat->subsys_offset,1,8,fp);
    err = fwrite(&version,2,1,fp);
    err = fwrite(&endian,2,1,fp);

    fclose(fp);

    fid = H5Fopen(matname,H5F_ACC_RDWR,H5P_DEFAULT);

    mat->fp = malloc(sizeof(hid_t));
    *(hid_t*)mat->fp = fid;

    return mat;
}

/** @if mat_devman
 * @brief Reads the MAT variable identified by matvar
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar MAT variable pointer
 * @endif
 */
void
Mat_VarRead73(mat_t *mat,matvar_t *matvar)
{
    int k;
    size_t numel;
    hid_t fid,dset_id;

    if ( NULL == mat || NULL == matvar )
        return;
    else if (NULL == matvar->internal->hdf5_name && 0 > matvar->internal->id)
        return;

    fid = *(hid_t*)mat->fp;

    switch (matvar->class_type) {
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
            numel = 1;
            for ( k = 0; k < matvar->rank; k++ )
                numel *= matvar->dims[k];
            matvar->data_size = Mat_SizeOfClass(matvar->class_type);
            matvar->nbytes    = numel*matvar->data_size;

            if ( NULL != matvar->internal->hdf5_name ) {
                dset_id = H5Dopen(fid,matvar->internal->hdf5_name);
            } else {
                dset_id = matvar->internal->id;
                H5Iinc_ref(dset_id);
            }

            if ( !matvar->isComplex ) {
                matvar->data      = malloc(matvar->nbytes);
                if ( NULL != matvar->data ) {
                    H5Dread(dset_id,Mat_class_type_to_hid_t(matvar->class_type),
                            H5S_ALL,H5S_ALL,H5P_DEFAULT,matvar->data);
                }
            } else {
                struct ComplexSplit *complex_data;
                hid_t h5_complex_base,h5_complex;

                complex_data     = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);

                h5_complex_base = Mat_class_type_to_hid_t(matvar->class_type);
                h5_complex      = H5Tcreate(H5T_COMPOUND,
                                            H5Tget_size(h5_complex_base));
                H5Tinsert(h5_complex,"real",0,h5_complex_base);
                H5Dread(dset_id,h5_complex,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                        complex_data->Re);
                H5Tclose(h5_complex);

                h5_complex      = H5Tcreate(H5T_COMPOUND,
                                            H5Tget_size(h5_complex_base));
                H5Tinsert(h5_complex,"imag",0,h5_complex_base);
                H5Dread(dset_id,h5_complex,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                        complex_data->Im);
                H5Tclose(h5_complex);
                matvar->data = complex_data;
            }
            H5Dclose(dset_id);
            break;
        case MAT_C_CHAR:
            numel = 1;
            for ( k = 0; k < matvar->rank; k++ )
                numel *= matvar->dims[k];
            matvar->data_type = MAT_T_UINT8;
            matvar->data_size = 1;
            matvar->nbytes    = numel*matvar->data_size;

            if ( NULL != matvar->internal->hdf5_name ) {
                dset_id = H5Dopen(fid,matvar->internal->hdf5_name);
            } else {
                dset_id = matvar->internal->id;
                H5Iinc_ref(dset_id);
            }
            matvar->data = malloc(matvar->nbytes);
            if ( NULL != matvar->data ) {
                H5Dread(dset_id,Mat_data_type_to_hid_t(matvar->data_type),
                        H5S_ALL,H5S_ALL,H5P_DEFAULT,matvar->data);
            }
            break;
        case MAT_C_STRUCT:
        {
            matvar_t **fields;
            int i,nfields = 0;
            hid_t field_id,ref_id,field_type_id;

            if ( !matvar->nbytes || !matvar->data_size || NULL == matvar->data )
                break;
            nfields = matvar->nbytes / matvar->data_size;
            fields  = matvar->data;
            for ( i = 0; i < nfields; i++ ) {
                if (  0 < fields[i]->internal->hdf5_ref &&
                     -1 < fields[i]->internal->id ) {
                    /* Dataset of references */
                    Mat_H5ReadNextReferenceData(fields[i]->internal->id,fields[i],mat);
                } else {
                    Mat_VarRead73(mat,fields[i]);
                }
            }
            break;
        }
        case MAT_C_CELL:
        {
            matvar_t **cells;
            int i,ncells = 0;
            hid_t field_id,ref_id,field_type_id;

            if ( NULL != matvar->internal->hdf5_name ) {
                dset_id = H5Dopen(fid,matvar->internal->hdf5_name);
            } else {
                dset_id = matvar->internal->id;
                H5Iinc_ref(dset_id);
            }

            ncells = matvar->nbytes / matvar->data_size;
            cells  = matvar->data;

            for ( i = 0; i < ncells; i++ )
                Mat_H5ReadNextReferenceData(cells[i]->internal->id,cells[i],mat);
            break;
        }
        case MAT_C_SPARSE:
        {
            hid_t sparse_dset_id, space_id;
            hsize_t dims[2] = {0,};
            struct sparse_t *sparse_data = calloc(1,sizeof(*sparse_data));

            if ( NULL != matvar->internal->hdf5_name ) {
                dset_id = H5Gopen(fid,matvar->internal->hdf5_name);
            } else {
                dset_id = matvar->internal->id;
                H5Iinc_ref(dset_id);
            }

            sparse_dset_id = H5Dopen(dset_id,"ir");
            if ( -1 < sparse_dset_id ) {
                space_id = H5Dget_space(sparse_dset_id);
                H5Sget_simple_extent_dims(space_id,dims,NULL);
                sparse_data->nir = dims[0];
                sparse_data->ir = malloc(sparse_data->nir*
                                         sizeof(*sparse_data->ir));
                H5Dread(sparse_dset_id,H5T_NATIVE_INT,
                        H5S_ALL,H5S_ALL,H5P_DEFAULT,sparse_data->ir);
                H5Sclose(space_id);
                H5Dclose(sparse_dset_id);
            }

            sparse_dset_id = H5Dopen(dset_id,"jc");
            if ( -1 < sparse_dset_id ) {
                space_id = H5Dget_space(sparse_dset_id);
                H5Sget_simple_extent_dims(space_id,dims,NULL);
                sparse_data->njc = dims[0];
                sparse_data->jc = malloc(sparse_data->njc*
                                         sizeof(*sparse_data->jc));
                H5Dread(sparse_dset_id,H5T_NATIVE_INT,
                        H5S_ALL,H5S_ALL,H5P_DEFAULT,sparse_data->jc);
                H5Sclose(space_id);
                H5Dclose(sparse_dset_id);
            }

            sparse_dset_id = H5Dopen(dset_id,"data");
            if ( -1 < sparse_dset_id ) {
                size_t ndata_bytes;
                space_id = H5Dget_space(sparse_dset_id);
                H5Sget_simple_extent_dims(space_id,dims,NULL);
                sparse_data->nzmax = dims[0];
                sparse_data->ndata = dims[0];
                matvar->data_size  = sizeof(struct sparse_t);
                matvar->nbytes     = matvar->data_size;

                ndata_bytes = sparse_data->nzmax*Mat_SizeOf(matvar->data_type);
                if ( !matvar->isComplex ) {
                    sparse_data->data  = malloc(ndata_bytes);
                    if ( NULL != sparse_data->data ) {
                        H5Dread(sparse_dset_id,
                                Mat_data_type_to_hid_t(matvar->data_type),
                                H5S_ALL,H5S_ALL,H5P_DEFAULT,sparse_data->data);
                    }
                } else {
                    struct ComplexSplit *complex_data;
                    hid_t h5_complex_base,h5_complex;

                    complex_data     = malloc(sizeof(*complex_data));
                    complex_data->Re = malloc(ndata_bytes);
                    complex_data->Im = malloc(ndata_bytes);

                    h5_complex_base = Mat_data_type_to_hid_t(matvar->data_type);
                    h5_complex      = H5Tcreate(H5T_COMPOUND,
                                                H5Tget_size(h5_complex_base));
                    H5Tinsert(h5_complex,"real",0,h5_complex_base);
                    H5Dread(sparse_dset_id,h5_complex,H5S_ALL,H5S_ALL,
                            H5P_DEFAULT,complex_data->Re);
                    H5Tclose(h5_complex);

                    h5_complex      = H5Tcreate(H5T_COMPOUND,
                                                H5Tget_size(h5_complex_base));
                    H5Tinsert(h5_complex,"imag",0,h5_complex_base);
                    H5Dread(sparse_dset_id,h5_complex,H5S_ALL,H5S_ALL,
                            H5P_DEFAULT,complex_data->Im);
                    H5Tclose(h5_complex);
                    sparse_data->data = complex_data;
                }
                H5Sclose(space_id);
                H5Dclose(sparse_dset_id);
            }
            matvar->data = sparse_data;
            break;
        }
    }
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
Mat_VarReadNextInfo73( mat_t *mat )
{
    hid_t       fid,gid;
    hsize_t     num_objs;
    H5E_auto_t  efunc;
    void       *client_data;
    matvar_t   *matvar;

    if( mat == NULL )
        return NULL;

    fid = *(hid_t*)mat->fp;
    H5Gget_num_objs(fid,&num_objs);
    /* FIXME: follow symlinks, datatypes? */
    while ( mat->next_index < num_objs ) {
        if ( H5G_DATASET == H5Gget_objtype_by_idx(fid,mat->next_index) ) {
            break;
        } else if ( H5G_GROUP == H5Gget_objtype_by_idx(fid,mat->next_index) ) {
            /* Check that this is not the /#refs# group */
            char name[128] = {0,};
            (void)H5Gget_objname_by_idx(fid,mat->next_index,name,127);
            if ( strcmp(name,"#refs#") )
                break;
            else
                mat->next_index++;
        } else {
            mat->next_index++;
        }
    }

    if ( mat->next_index >= num_objs )
        return NULL;
    else if ( NULL == (matvar = Mat_VarCalloc()) )
        return NULL;

    switch ( H5Gget_objtype_by_idx(fid,mat->next_index) ) {
        case H5G_DATASET:
        {
            ssize_t  name_len;
            /* FIXME */
            hsize_t  dims[10];
            hid_t   attr_id,type_id,dset_id,space_id;

            matvar->internal->fp = mat;
            name_len = H5Gget_objname_by_idx(fid,mat->next_index,NULL,0);
            matvar->name = malloc(1+name_len);
            if ( matvar->name ) {
                name_len = H5Gget_objname_by_idx(fid,mat->next_index,
                                                 matvar->name,1+name_len);
                matvar->name[name_len] = '\0';
            }
            dset_id = H5Dopen(fid,matvar->name);

            /* Get the HDF5 name of the variable */
            name_len = H5Iget_name(dset_id,NULL,0);
            if ( name_len > 0 ) {
                matvar->internal->hdf5_name = malloc(name_len+1);
                (void)H5Iget_name(dset_id,matvar->internal->hdf5_name,
                                  name_len+1);
            } else {
                /* Can not get an internal name, so leave the identifier open */
                matvar->internal->id = dset_id;
            }

            space_id = H5Dget_space(dset_id);
            matvar->rank = H5Sget_simple_extent_ndims(space_id);
            matvar->dims = malloc(matvar->rank*sizeof(*matvar->dims));
            if ( NULL != matvar->dims ) {
                int k;
                H5Sget_simple_extent_dims(space_id,dims,NULL);
                for ( k = 0; k < matvar->rank; k++ )
                    matvar->dims[k] = dims[matvar->rank - k - 1];
            }
            H5Sclose(space_id);

            attr_id = H5Aopen_name(dset_id,"MATLAB_class");
            type_id  = H5Aget_type(attr_id);
            if ( H5T_STRING == H5Tget_class(type_id) ) {
                char *class_str = calloc(H5Tget_size(type_id)+1,1);
                if ( NULL != class_str ) {
                    hid_t class_id = H5Tcopy(H5T_C_S1);
                    H5Tset_size(class_id,H5Tget_size(type_id));
                    H5Aread(attr_id,class_id,class_str);
                    H5Tclose(class_id);
                    matvar->class_type = Mat_class_str_to_id(class_str);
                    matvar->data_type  = Mat_ClassToType73(matvar->class_type);
                    free(class_str);
                }
            }
            H5Tclose(type_id);
            H5Aclose(attr_id);

            /* Turn off error printing so testing for attributes doesn't print
             * error stacks
             */
            H5Eget_auto(&efunc,&client_data);
            H5Eset_auto((H5E_auto_t)0,NULL);

            attr_id = H5Aopen_name(dset_id,"MATLAB_global");
            /* FIXME: Check that dataspace is scalar */
            if ( -1 < attr_id ) {
                H5Aread(attr_id,H5T_NATIVE_INT,&matvar->isGlobal);
                H5Aclose(attr_id);
            }

            /* Check for attribute that indicates an empty array */
            attr_id = H5Aopen_name(dset_id,"MATLAB_empty");
            /* FIXME: Check that dataspace is scalar */
            if ( -1 < attr_id ) {
                int empty = 0;
                H5Aread(attr_id,H5T_NATIVE_INT,&empty);
                H5Aclose(attr_id);
                if ( empty ) {
                    matvar->rank = matvar->dims[0];
                    matvar->dims = calloc(matvar->rank,sizeof(*matvar->dims));
                    H5Dread(dset_id,Mat_dims_type_to_hid_t(),H5S_ALL,H5S_ALL,
                            H5P_DEFAULT,matvar->dims);
                }
            }

            H5Eset_auto(efunc,client_data);

            /* Test if dataset type is compound and if so if it's complex */
            type_id = H5Dget_type(dset_id);
            if ( H5T_COMPOUND == H5Tget_class(type_id) ) {
                /* FIXME: Any more checks? */
                matvar->isComplex = MAT_F_COMPLEX;
            }
            H5Tclose(type_id);

            /* If the dataset is a cell array read theinfo of the cells */
            if ( MAT_C_CELL == matvar->class_type ) {
                matvar_t **cells;
                int i,ncells = 1;
                hid_t field_id,ref_id,field_type_id;
                hobj_ref_t *ref_ids;

                for ( i = 0; i < matvar->rank; i++ )
                    ncells *= matvar->dims[i];
                matvar->data_size = sizeof(matvar_t**);
                matvar->nbytes    = ncells*matvar->data_size;
                matvar->data      = malloc(matvar->nbytes);
                cells  = matvar->data;

                ref_ids = malloc(ncells*sizeof(*ref_ids));
                H5Dread(dset_id,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                        ref_ids);
                for ( i = 0; i < ncells; i++ ) {
                    hid_t ref_id;
                    cells[i] = Mat_VarCalloc();
                    cells[i]->internal->hdf5_ref = ref_ids[i];
                    /* Closing of ref_id is done in Mat_H5ReadNextReferenceInfo */
                    ref_id = H5Rdereference(dset_id,H5R_OBJECT,ref_ids+i);
                    cells[i]->internal->id=ref_id;
                    cells[i]->internal->fp=matvar->internal->fp;
                    Mat_H5ReadNextReferenceInfo(ref_id,cells[i],mat);
                }
                free(ref_ids);
            } else if ( MAT_C_STRUCT == matvar->class_type ) {
                /* Empty structures can be a dataset */

                /* Turn off error printing so testing for attributes doesn't print
                 * error stacks
                 */
                H5Eget_auto(&efunc,&client_data);
                H5Eset_auto((H5E_auto_t)0,NULL);
                /* Check if the structure defines its fields in MATLAB_fields */
                attr_id = H5Aopen_name(dset_id,"MATLAB_fields");
                if ( -1 < attr_id ) {
                    int field_length, i;
                    hid_t      field_id;
                    hsize_t    nfields;
                    hvl_t     *fieldnames_vl;
                    matvar_t **fields;

                    space_id = H5Aget_space(attr_id);
                    (void)H5Sget_simple_extent_dims(space_id,&nfields,NULL);
                    field_id = H5Aget_type(attr_id);
                    fieldnames_vl = malloc(nfields*sizeof(*fieldnames_vl));
                    H5Aread(attr_id,field_id,fieldnames_vl);

                    fields = calloc(nfields,sizeof(*fields));
                    for ( i = 0; i < nfields; i++ ) {
                        fields[i] = Mat_VarCalloc();
                        fields[i]->name = calloc(fieldnames_vl[i].len+1,1);
                        memcpy(fields[i]->name,fieldnames_vl[i].p,
                               fieldnames_vl[i].len);
                    }

                    matvar->data = fields;
                    matvar->data_size = sizeof(matvar_t*);
                    matvar->nbytes    = nfields*matvar->data_size;

                    H5Sclose(space_id);
                    H5Tclose(field_id);
                    H5Aclose(attr_id);
                    free(fieldnames_vl);
                }
                H5Eset_auto(efunc,client_data);
            }

            if ( matvar->internal->id != dset_id ) {
                /* Close dataset and increment count */
                H5Dclose(dset_id);
            }
            mat->next_index++;
            break;
        }
        case H5G_GROUP:
        {
            ssize_t  name_len;
            int      k;
            char    **fieldnames = NULL;
            /* FIXME */
            hsize_t  dims[10],nfields,numel;
            hid_t   attr_id,type_id,dset_id,space_id,field_id,field_type_id;
            matvar_t **fields;

            matvar->internal->fp = mat;
            name_len = H5Gget_objname_by_idx(fid,mat->next_index,NULL,0);
            matvar->name = malloc(1+name_len);
            if ( matvar->name ) {
                name_len = H5Gget_objname_by_idx(fid,mat->next_index,
                                                 matvar->name,1+name_len);
                matvar->name[name_len] = '\0';
            }
            dset_id = H5Gopen(fid,matvar->name);

            Mat_H5ReadGroupInfo(mat,matvar,dset_id);
            H5Gclose(dset_id);
            mat->next_index++;
            break;
        }
        default:
            break;
    }
    return matvar;
}

/** @if mat_devman
 * @brief Writes a matlab variable to a version 7.3 matlab file
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @param compress option to compress the variable
 *                 (only works for numeric types)
 * @retval 0 on success
 * @endif
 */
int
Mat_VarWrite73(mat_t *mat,matvar_t *matvar,int compress)
{
    unsigned long k,numel;
    hid_t mspace_id,dset_id,attr_type_id,attr_id,aspace_id;

    if ( NULL == mat || NULL == matvar )
        return -1;

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
            numel = 1;
            for ( k = 0; k < matvar->rank; k++ ) {
                perm_dims[k] = matvar->dims[matvar->rank-k-1];
                numel *= perm_dims[k];
            }

            if ( 0 == numel || NULL == matvar->data ) {
                hsize_t rank = matvar->rank;
                unsigned empty = 1;
                mspace_id = H5Screate_simple(1,&rank,NULL);
                dset_id = H5Dcreate(*(hid_t*)mat->fp,matvar->name,
                              H5T_NATIVE_HSIZE,mspace_id,H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                /* Write the empty attribute */
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_empty",H5T_NATIVE_UINT,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,H5T_NATIVE_UINT,&empty);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                /* Write the dimensions as the data */
                H5Dwrite(dset_id,Mat_dims_type_to_hid_t(),H5S_ALL,H5S_ALL,
                         H5P_DEFAULT,matvar->dims);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            } else if ( matvar->isComplex ) {
                hid_t h5_complex,h5_complex_base;

                h5_complex_base = Mat_class_type_to_hid_t(matvar->class_type);
                h5_complex      = H5Tcreate(H5T_COMPOUND,
                                      2*H5Tget_size(h5_complex_base));
                H5Tinsert(h5_complex,"real",0,h5_complex_base);
                H5Tinsert(h5_complex,"imag",H5Tget_size(h5_complex_base),
                          h5_complex_base);
                mspace_id = H5Screate_simple(matvar->rank,perm_dims,NULL);
                dset_id = H5Dcreate(*(hid_t*)mat->fp,matvar->name,
                                    h5_complex,mspace_id,H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                H5Tclose(h5_complex);

                /* Write real part of dataset */
                h5_complex = H5Tcreate(H5T_COMPOUND,
                                       H5Tget_size(h5_complex_base));
                H5Tinsert(h5_complex,"real",0,h5_complex_base);
                H5Dwrite(dset_id,h5_complex,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                         ((struct ComplexSplit*)matvar->data)->Re);
                H5Tclose(h5_complex);

                /* Write imaginary part of dataset */
                h5_complex      = H5Tcreate(H5T_COMPOUND,
                                      H5Tget_size(h5_complex_base));
                H5Tinsert(h5_complex,"imag",0,h5_complex_base);
                H5Dwrite(dset_id,h5_complex,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                         ((struct ComplexSplit*)matvar->data)->Im);
                H5Tclose(h5_complex);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            } else { /* matvar->isComplex */
                mspace_id = H5Screate_simple(matvar->rank,perm_dims,NULL);
                dset_id = H5Dcreate(*(hid_t*)mat->fp,matvar->name,
                    Mat_class_type_to_hid_t(matvar->class_type),mspace_id,
                    H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                H5Dwrite(dset_id,Mat_data_type_to_hid_t(matvar->data_type),
                    H5S_ALL,H5S_ALL,H5P_DEFAULT,matvar->data);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            }
            break;
        case MAT_C_CHAR:
        {
            numel = 1;
            for ( k = 0; k < matvar->rank; k++ ) {
                perm_dims[k] = matvar->dims[matvar->rank-k-1];
                numel *= perm_dims[k];
            }
            if ( 0 == numel || NULL == matvar->data ) {
                hsize_t rank = matvar->rank;
                unsigned empty = 1;
                mspace_id = H5Screate_simple(1,&rank,NULL);
                dset_id = H5Dcreate(*(hid_t*)mat->fp,matvar->name,
                              H5T_NATIVE_HSIZE,mspace_id,H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                /* Write the empty attribute */
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_empty",H5T_NATIVE_UINT,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,H5T_NATIVE_UINT,&empty);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                /* Write the dimensions as the data */
                H5Dwrite(dset_id,Mat_dims_type_to_hid_t(),H5S_ALL,H5S_ALL,
                         H5P_DEFAULT,matvar->dims);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            } else {
                int matlab_int_decode = 2;

                mspace_id = H5Screate_simple(matvar->rank,perm_dims,NULL);
                switch ( matvar->data_type ) {
                    case MAT_T_UTF32:
                    case MAT_T_INT32:
                    case MAT_T_UINT32:
                        /* Not sure matlab will actually handle this */
                        dset_id = H5Dcreate(*(hid_t*)mat->fp,matvar->name,
                            Mat_class_type_to_hid_t(MAT_C_UINT32),mspace_id,
                            H5P_DEFAULT);
                        break;
                    case MAT_T_UTF16:
                    case MAT_T_UTF8:
                    case MAT_T_INT16:
                    case MAT_T_UINT16:
                    case MAT_T_INT8:
                    case MAT_T_UINT8:
                        dset_id = H5Dcreate(*(hid_t*)mat->fp,matvar->name,
                            Mat_class_type_to_hid_t(MAT_C_UINT16),mspace_id,
                            H5P_DEFAULT);
                        break;
                }
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,Mat_class_names[matvar->class_type]);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);

                attr_type_id = H5Tcopy(H5T_NATIVE_INT);
                attr_id = H5Acreate(dset_id,"MATLAB_int_decode",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,&matlab_int_decode);
                H5Tclose(attr_type_id);
                H5Sclose(aspace_id);

                H5Dwrite(dset_id,Mat_data_type_to_hid_t(matvar->data_type),
                    H5S_ALL,H5S_ALL,H5P_DEFAULT,matvar->data);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            }
            break;
        }
        case MAT_C_STRUCT:
        {
            hid_t struct_id,str_type_id,fieldnames_id;
            hsize_t    nfields,nmemb;
            matvar_t **fields;
            hvl_t     *fieldnames;

            nmemb = matvar->dims[0];
            for ( k = 1; k < matvar->rank; k++ )
                nmemb *= matvar->dims[k];
 
            if ( 0 == nmemb || NULL == matvar->data ) {
                hsize_t rank = matvar->rank;
                unsigned empty = 1;
                mspace_id = H5Screate_simple(1,&rank,NULL);
                dset_id = H5Dcreate(*(hid_t*)mat->fp,matvar->name,
                              H5T_NATIVE_HSIZE,mspace_id,H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                /* Write the empty attribute */
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_empty",H5T_NATIVE_UINT,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,H5T_NATIVE_UINT,&empty);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);

                nfields = matvar->nbytes / (matvar->data_size);
                if ( nfields ) {
                    str_type_id = H5Tcopy(H5T_C_S1);
                    fieldnames = malloc(nfields*sizeof(*fieldnames));
                    fields     = matvar->data;
                    for ( k = 0; k < nfields; k++ ) {
                        fieldnames[k].len = strlen(fields[k]->name);
                        fieldnames[k].p   = fields[k]->name;
                    }
                    H5Tset_size(str_type_id,1);
                    fieldnames_id = H5Tvlen_create(str_type_id);
                    aspace_id     = H5Screate_simple(1,&nfields,NULL);
                    attr_id = H5Acreate(dset_id,"MATLAB_fields",fieldnames_id,
                                        aspace_id,H5P_DEFAULT);
                    H5Awrite(attr_id,fieldnames_id,fieldnames);
                    H5Aclose(attr_id);
                    H5Sclose(aspace_id);
                    H5Tclose(fieldnames_id);
                    H5Tclose(str_type_id);
                    free(fieldnames);
                }

                /* Write the dimensions as the data */
                H5Dwrite(dset_id,Mat_dims_type_to_hid_t(),H5S_ALL,H5S_ALL,
                         H5P_DEFAULT,matvar->dims);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            } else {
                struct_id = H5Gcreate(*(hid_t*)mat->fp,matvar->name,0);
                if ( struct_id < 0 ) {
                    Mat_Critical("Error creating group for struct %s",matvar->name);
                } else {
                    str_type_id = H5Tcopy(H5T_C_S1);
                    H5Tset_size(str_type_id,7);
                    aspace_id = H5Screate(H5S_SCALAR);
                    attr_id = H5Acreate(struct_id,"MATLAB_class",str_type_id,
                                        aspace_id,H5P_DEFAULT);
                    H5Awrite(attr_id,str_type_id,"struct");
                    H5Aclose(attr_id);

                    nfields = matvar->nbytes / (nmemb*matvar->data_size);

                    /* Structure with no fields */
                    if ( nfields == 0 ) {
                        H5Gclose(struct_id);
                        break;
                    }

                    fieldnames = malloc(nfields*sizeof(*fieldnames));
                    fields     = matvar->data;
                    for ( k = 0; k < nfields; k++ ) {
                        fieldnames[k].len = strlen(fields[k]->name);
                        fieldnames[k].p   = fields[k]->name;
                    }
                    H5Tset_size(str_type_id,1);
                    fieldnames_id = H5Tvlen_create(str_type_id);
                    aspace_id     = H5Screate_simple(1,&nfields,NULL);
                    attr_id = H5Acreate(struct_id,"MATLAB_fields",fieldnames_id,
                                        aspace_id,H5P_DEFAULT);
                    H5Awrite(attr_id,fieldnames_id,fieldnames);
                    H5Aclose(attr_id);
                    H5Sclose(aspace_id);
                    H5Tclose(fieldnames_id);
                    H5Tclose(str_type_id);
                    free(fieldnames);

                    if ( 1 == nmemb ) {
                        for ( k = 0; k < nmemb*nfields; k++ )
                            Mat_WriteNextStructField73(struct_id,fields[k],
                                fields[k]->name);
                    } else {
                        hid_t refs_id;
                        H5E_auto_t  efunc;
                        void       *client_data;
                        /* Silence errors if /#refs does not exist */
                        H5Eget_auto(&efunc,&client_data);
                        H5Eset_auto((H5E_auto_t)0,NULL);
                        if ((refs_id=H5Gopen(*(hid_t*)mat->fp,"/#refs#") < 0 )) {
                            refs_id = H5Gcreate(*(hid_t*)mat->fp,"/#refs#",0);
                        }
                        H5Eset_auto(efunc,client_data);
                    
                        if ( refs_id > -1 ) {
                            char name[64];
                            hobj_ref_t **refs;
                            hsize_t      num_obj;
                            int l;

                            refs = malloc(nfields*sizeof(*refs));
                            for ( l = 0; l < nfields; l++ )
                                refs[l] = malloc(nmemb*sizeof(*refs[l]));

                            for ( k = 0; k < nmemb; k++ ) {
                                for ( l = 0; l < nfields; l++ ) {
                                    (void)H5Gget_num_objs(refs_id,&num_obj);
                                    sprintf(name,"%lu",num_obj);
                                    Mat_WriteNextStructField73(refs_id,
                                        fields[k*nfields+l],name);
                                    sprintf(name,"/#refs#/%lu",num_obj);
                                    H5Rcreate(refs[l]+k,*(hid_t*)mat->fp,name,
                                              H5R_OBJECT,-1);
                                }
                            }
                            for ( k = 0; k < matvar->rank; k++ )
                                perm_dims[k] = matvar->dims[matvar->rank-k-1];

                            mspace_id=H5Screate_simple(matvar->rank,perm_dims,NULL);
                            for ( l = 0; l < nfields; l++ ) {
                                dset_id = H5Dcreate(struct_id,
                                    fields[l]->name,H5T_STD_REF_OBJ,mspace_id,
                                    H5P_DEFAULT);
                                H5Dwrite(dset_id,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,
                                    H5P_DEFAULT,refs[l]);
                                H5Dclose(dset_id);
                                free(refs[l]);
                            }
                            free(refs);
                            H5Sclose(mspace_id);
                            H5Gclose(refs_id);
                        }
                    }
                    H5Gclose(struct_id);
                }
            }
            break;
        }
        case MAT_C_CELL:
        {
            hid_t str_type_id;
            hsize_t    nmemb;
            matvar_t **cells;
            hid_t refs_id;
            H5E_auto_t efunc;
            void       *client_data;

            cells = matvar->data;
            nmemb = matvar->dims[0];
            for ( k = 1; k < matvar->rank; k++ )
                nmemb *= matvar->dims[k];

            if ( 0 == nmemb || NULL == matvar->data ) {
                hsize_t rank = matvar->rank;
                unsigned empty = 1;
                mspace_id = H5Screate_simple(1,&rank,NULL);
                dset_id = H5Dcreate(*(hid_t*)mat->fp,matvar->name,
                              H5T_NATIVE_HSIZE,mspace_id,H5P_DEFAULT);
                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);
                /* Write the empty attribute */
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(dset_id,"MATLAB_empty",H5T_NATIVE_UINT,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,H5T_NATIVE_UINT,&empty);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                /* Write the dimensions as the data */
                H5Dwrite(dset_id,Mat_dims_type_to_hid_t(),H5S_ALL,H5S_ALL,
                         H5P_DEFAULT,matvar->dims);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            } else {
                /* Turn off error-checking so we don't get messages if opening
                 * group /#refs# fails
                 */
                H5Eget_auto(&efunc,&client_data);
                H5Eset_auto((H5E_auto_t)0,NULL);
                if ((refs_id=H5Gopen(*(hid_t*)mat->fp,"/#refs#") < 0 ))
                    refs_id = H5Gcreate(*(hid_t*)mat->fp,"/#refs#",0);
                H5Eset_auto(efunc,client_data);
                
                if ( refs_id > -1 ) {
                    char        name[64];
                    hobj_ref_t *refs;
                    hsize_t     num_obj;

                    refs = malloc(nmemb*sizeof(*refs));

                    for ( k = 0; k < nmemb; k++ ) {
                        (void)H5Gget_num_objs(refs_id,&num_obj);
                        sprintf(name,"%lu",num_obj);
                        Mat_WriteNextCellField73(refs_id,cells[k],name);
                        sprintf(name,"/#refs#/%lu",num_obj);
                        H5Rcreate(refs+k,*(hid_t*)mat->fp,name,H5R_OBJECT,-1);
                    }
                    for ( k = 0; k < matvar->rank; k++ )
                        perm_dims[k] = matvar->dims[matvar->rank-k-1];

                    mspace_id=H5Screate_simple(matvar->rank,perm_dims,NULL);
                    dset_id = H5Dcreate(*(hid_t*)mat->fp,matvar->name,
                        H5T_STD_REF_OBJ,mspace_id,H5P_DEFAULT);
                    H5Dwrite(dset_id,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,
                        H5P_DEFAULT,refs);

                    str_type_id = H5Tcopy(H5T_C_S1);
                    H5Tset_size(str_type_id,7);
                    aspace_id = H5Screate(H5S_SCALAR);
                    attr_id = H5Acreate(dset_id,"MATLAB_class",str_type_id,
                                    aspace_id,H5P_DEFAULT);
                    H5Awrite(attr_id,str_type_id,"cell");
                    H5Aclose(attr_id);
                    H5Sclose(aspace_id);
                    H5Tclose(str_type_id);
                    H5Dclose(dset_id);
                    free(refs);
                    H5Sclose(mspace_id);
                    H5Gclose(refs_id);
                }
            }
            break;
        }
        case MAT_C_SPARSE:
        {
            hid_t str_type_id, sparse_id;
            hsize_t    nmemb;
            matvar_t **cells;
            hid_t refs_id;
            H5E_auto_t efunc;
            void       *client_data;

            nmemb = matvar->dims[0];
            for ( k = 1; k < matvar->rank; k++ )
                nmemb *= matvar->dims[k];

            sparse_id = H5Gcreate(*(hid_t*)mat->fp,matvar->name,0);
            if ( sparse_id < 0 ) {
                Mat_Critical("Error creating group for sparse array %s",
                    matvar->name);
            } else {
                hid_t size_type_id;
                sparse_t *sparse;
                hsize_t rank, nir, njc, ndata;
                mat_uint64_t sparse_attr_value;

                sparse = matvar->data;
                rank = matvar->rank;

                attr_type_id = H5Tcopy(H5T_C_S1);
                H5Tset_size(attr_type_id,
                            strlen(Mat_class_names[matvar->class_type])+1);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(sparse_id,"MATLAB_class",attr_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,attr_type_id,
                         Mat_class_names[matvar->class_type]);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);
                H5Tclose(attr_type_id);

                sparse_attr_value = matvar->dims[0];
                size_type_id = Mat_class_type_to_hid_t(MAT_C_UINT64);
                aspace_id = H5Screate(H5S_SCALAR);
                attr_id = H5Acreate(sparse_id,"MATLAB_sparse",size_type_id,
                                    aspace_id,H5P_DEFAULT);
                H5Awrite(attr_id,size_type_id,&sparse_attr_value);
                H5Sclose(aspace_id);
                H5Aclose(attr_id);

                ndata = sparse->ndata;
                mspace_id = H5Screate_simple(1,&ndata,NULL);
                if ( matvar->isComplex ) {
                    hid_t h5_complex,h5_complex_base;
                    void *buf;
                    struct ComplexSplit *complex_data;

                    complex_data = sparse->data;

                    /* Create dataset datatype as compound with real and
                     * imaginary fields
                     */
                    h5_complex_base = Mat_data_type_to_hid_t(matvar->data_type);
                    h5_complex      = H5Tcreate(H5T_COMPOUND,
                                          2*H5Tget_size(h5_complex_base));
                    H5Tinsert(h5_complex,"real",0,h5_complex_base);
                    H5Tinsert(h5_complex,"imag",H5Tget_size(h5_complex_base),
                              h5_complex_base);

                    /* Create dataset */
                    perm_dims[0] = ndata;
                    mspace_id = H5Screate_simple(1,perm_dims,NULL);
                    dset_id = H5Dcreate(sparse_id,"data",h5_complex,mspace_id,
                                  H5P_DEFAULT);
                    H5Tclose(h5_complex);

                    /* Write real part of dataset */
                    h5_complex = H5Tcreate(H5T_COMPOUND,
                                           H5Tget_size(h5_complex_base));
                    H5Tinsert(h5_complex,"real",0,h5_complex_base);
                    H5Dwrite(dset_id,h5_complex,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                             complex_data->Re);
                    H5Tclose(h5_complex);

                    /* Write imaginary part of dataset */
                    h5_complex      = H5Tcreate(H5T_COMPOUND,
                                          H5Tget_size(h5_complex_base));
                    H5Tinsert(h5_complex,"imag",0,h5_complex_base);
                    H5Dwrite(dset_id,h5_complex,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                             complex_data->Im);
                    H5Tclose(h5_complex);
                    H5Dclose(dset_id);
                    H5Sclose(mspace_id);
                } else { /* if ( matvar->isComplex ) */
                    dset_id = H5Dcreate(sparse_id,"data",
                        H5T_NATIVE_DOUBLE,mspace_id,H5P_DEFAULT);
                    H5Dwrite(dset_id,H5T_NATIVE_DOUBLE,H5S_ALL,H5S_ALL,
                             H5P_DEFAULT,sparse->data);
                    H5Dclose(dset_id);
                    H5Sclose(mspace_id);
                }

                nir = sparse->nir;
                mspace_id = H5Screate_simple(1,&nir,NULL);
                dset_id = H5Dcreate(sparse_id,"ir",size_type_id,mspace_id,
                                    H5P_DEFAULT);
                H5Dwrite(dset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                         sparse->ir);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);

                njc = sparse->njc;
                mspace_id = H5Screate_simple(1,&njc,NULL);
                dset_id = H5Dcreate(sparse_id,"jc",size_type_id,mspace_id,
                                    H5P_DEFAULT);
                H5Dwrite(dset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,
                         sparse->jc);
                H5Dclose(dset_id);
                H5Sclose(mspace_id);
            }
            break;
        }
    }
    return 0;
}
