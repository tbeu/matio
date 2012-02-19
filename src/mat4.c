/** @file mat4.c
 * Matlab MAT version 4 file functions
 * @ingroup MAT
 */
/*
 * Copyright (C) 2005-2011   Christopher C. Hulbert
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *    1. Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY CHRISTOPHER C. HULBERT ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL CHRISTOPHER C. HULBERT OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "matio_private.h"
#include "mat4.h"

/** @if mat_devman
 * @brief Reads the data of a version 4 MAT file variable
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar MAT variable pointer to read the data
 * @endif
 */
void
Read4(mat_t *mat,matvar_t *matvar)
{
    unsigned int N;
    if ( fseek(mat->fp,matvar->internal->datapos,SEEK_SET) )
        return;

    N = matvar->dims[0]*matvar->dims[1];
    switch ( matvar->class_type ) {
        case MAT_C_DOUBLE:
            matvar->data_size = sizeof(double);
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data;

                matvar->nbytes   = N*sizeof(double);
                complex_data     = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                matvar->data     = complex_data;
                if ( complex_data != NULL &&
                    complex_data->Re != NULL && complex_data->Im != NULL ) {
                    ReadDoubleData(mat, complex_data->Re, matvar->data_type, N);
                    ReadDoubleData(mat, complex_data->Im, matvar->data_type, N);
                }
            } else {
                matvar->nbytes = N*sizeof(double);
                matvar->data   = malloc(matvar->nbytes);
                if ( matvar->data != NULL )
                    ReadDoubleData(mat, matvar->data, matvar->data_type, N);
            }
            /* Update data type to match format of matvar->data */
            matvar->data_type = MAT_T_DOUBLE;
            break;
        case MAT_C_CHAR:
            matvar->data_size = 1;
            matvar->nbytes = N;
            matvar->data = malloc(matvar->nbytes);
            if ( NULL == matvar->data )
                Mat_Critical("Memory allocation failure");
            else
                ReadUInt8Data(mat,matvar->data,matvar->data_type,N);
            matvar->data_type = MAT_T_UINT8;
            break;
        default:
            Mat_Critical("MAT V4 data type error");
            return;
    }

    return;
}

/** @if mat_devman
 * @brief Reads a slab of data from a version 4 MAT file for the @c matvar variable
 *
 * @ingroup mat_internal
 * @param mat Version 4 MAT file pointer
 * @param matvar pointer to the mat variable
 * @param data pointer to store the read data in (must be of size
 *             edge[0]*...edge[rank-1]*Mat_SizeOfClass(matvar->class_type))
 * @param start index to start reading data in each dimension
 * @param stride write data every @c stride elements in each dimension
 * @param edge number of elements to read in each dimension
 * @retval 0 on success
 * @endif
 */
int
ReadData4(mat_t *mat,matvar_t *matvar,void *data,
      int *start,int *stride,int *edge)
{
    int err = 0;
    enum matio_classes class_type = MAT_C_EMPTY;

    fseek(mat->fp,matvar->internal->datapos,SEEK_SET);

    switch( matvar->data_type ) {
        case MAT_T_DOUBLE:
            class_type = MAT_C_DOUBLE;
            break;
        case MAT_T_SINGLE:
            class_type = MAT_C_SINGLE;
            break;
        case MAT_T_INT32:
            class_type = MAT_C_INT32;
            break;
        case MAT_T_INT16:
            class_type = MAT_C_INT16;
            break;
        case MAT_T_UINT16:
            class_type = MAT_C_UINT16;
            break;
        case MAT_T_UINT8:
            class_type = MAT_C_UINT8;
            break;
    }

    if ( matvar->rank == 2 ) {
        if ( stride[0]*(edge[0]-1)+start[0]+1 > matvar->dims[0] )
            err = 1;
        else if ( stride[1]*(edge[1]-1)+start[1]+1 > matvar->dims[1] )
            err = 1;
        if ( matvar->isComplex ) {
            mat_complex_split_t *cdata = data;
            long nbytes = edge[0]*edge[1]*Mat_SizeOf(matvar->data_type);

            ReadDataSlab2(mat,cdata->Re,class_type,matvar->data_type,
                    matvar->dims,start,stride,edge);
            fseek(mat->fp,matvar->internal->datapos+nbytes,SEEK_SET);
            ReadDataSlab2(mat,cdata->Im,class_type,
                matvar->data_type,matvar->dims,start,stride,edge);
        } else {
            ReadDataSlab2(mat,data,class_type,matvar->data_type,
                    matvar->dims,start,stride,edge);
        }
    } else {
        if ( matvar->isComplex ) {
            int i;
            mat_complex_split_t *cdata = data;
            long nbytes = Mat_SizeOf(matvar->data_type);

            for ( i = 0; i < matvar->rank; i++ )
                nbytes *= edge[i];

            ReadDataSlabN(mat,cdata->Re,class_type,matvar->data_type,
                matvar->rank,matvar->dims,start,stride,edge);
            fseek(mat->fp,matvar->internal->datapos+nbytes,SEEK_SET);
            ReadDataSlab2(mat,cdata->Im,class_type,
                matvar->data_type,matvar->dims,start,stride,edge);
        } else {
            ReadDataSlabN(mat,data,class_type,matvar->data_type,
                matvar->rank,matvar->dims,start,stride,edge);
        }
    }
    return err;
}

/** @if mat_devman
 * @brief Reads the header information for the next MAT variable in a version 4 MAT file
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @retuen pointer to the MAT variable or NULL
 * @endif
 */
matvar_t *
Mat_VarReadNextInfo4(mat_t *mat)
{
    int       tmp,M,O,data_type,class_type;
    long      nBytes;
    size_t    err;
    matvar_t *matvar = NULL;

    if ( mat == NULL || mat->fp == NULL )
        return NULL;
    else if ( NULL == (matvar = Mat_VarCalloc()) )
        return NULL;

    matvar->internal->fp   = mat;
    matvar->internal->fpos = ftell(mat->fp);

    err = fread(&tmp,sizeof(int),1,mat->fp);
    if ( !err ) {
        free(matvar);
        return NULL;
    }

    /* See if MOPT may need byteswapping */
    if ( tmp < 0 || tmp > 4052 ) {
        if ( Mat_int32Swap(&tmp) > 4052 ) {
            Mat_VarFree(matvar);
            return NULL;
        }
    }

    M = floor(tmp / 1000.0);
    tmp -= M*1000;
    O = floor(tmp / 100.0);
    tmp -= O*100;
    data_type = floor(tmp / 10.0);
    tmp -= data_type*10;
    class_type = floor(tmp);

    mat->byteswap = (M == 1) ? 1 : 0;
    /* Convert the V4 data type */
    switch ( data_type ) {
        case 0:
            matvar->data_type = MAT_T_DOUBLE;
            break;
        case 1:
            matvar->data_type = MAT_T_SINGLE;
            break;
        case 2:
            matvar->data_type = MAT_T_INT32;
            break;
        case 3:
            matvar->data_type = MAT_T_INT16;
            break;
        case 4:
            matvar->data_type = MAT_T_UINT16;
            break;
        case 5:
            matvar->data_type = MAT_T_UINT8;
            break;
        default:
            matvar->data_type = MAT_T_UNKNOWN;
            break;
    }
    switch ( class_type ) {
        case 0:
            matvar->class_type = MAT_C_DOUBLE;
            break;
        case 1:
            matvar->class_type = MAT_C_CHAR;
            break;
        case 2:
            matvar->class_type = MAT_C_SPARSE;
            break;
    }
    matvar->rank = 2;
    /* FIXME: Check allocation */
    matvar->dims = malloc(2*sizeof(*matvar->dims));
    err = fread(&tmp,sizeof(int),1,mat->fp);
    if ( mat->byteswap )
        Mat_int32Swap(&tmp);
    matvar->dims[0] = tmp;
    if ( !err ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    err = fread(&tmp,sizeof(int),1,mat->fp);
    if ( mat->byteswap )
        Mat_int32Swap(&tmp);
    matvar->dims[1] = tmp;
    if ( !err ) {
        Mat_VarFree(matvar);
        return NULL;
    }

    err = fread(&(matvar->isComplex),sizeof(int),1,mat->fp);
    if ( !err ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    err = fread(&tmp,sizeof(int),1,mat->fp);
    if ( !err ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    if ( mat->byteswap )
        Mat_int32Swap(&tmp);
    /* FIXME: Check allocation */
    matvar->name = malloc(tmp);
    err = fread(matvar->name,1,tmp,mat->fp);
    if ( !err ) {
        Mat_VarFree(matvar);
        return NULL;
    }

    matvar->internal->datapos = ftell(mat->fp);
    nBytes = matvar->dims[0]*matvar->dims[1]*Mat_SizeOf(matvar->data_type);
    if ( matvar->isComplex )
        nBytes *= 2;
    fseek(mat->fp,nBytes,SEEK_CUR);

    return matvar;
}
