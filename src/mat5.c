/** @file mat5.c
 * Matlab MAT version 5 file functions
 * @ingroup MAT
 */
/*
 * Copyright (C) 2005-2006   Christopher C. Hulbert
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

/* FIXME: Implement Unicode support */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
#include "matio.h"
#include "mat5.h"
#include "matio_private.h"

static const char *class_type_desc[16] = {"Undefined","Cell Array","Structure",
       "Object","Character Array","Sparse Array","Double Precision Array",
       "Single Precision Array", "8-bit, signed Integer Array",
       "8-bit, Unsigned Integer Array","16-bit, signed Integer Array",
       "16-bit, unsigned Integer Array","32-bit, signed Integer Array",
       "32-bit, unsigned Integer Array","Matlab Array","Compressed Data"};
static const char *data_type_desc[23] = {"Unknown","8-bit, signed integer",
       "8-bit, unsigned integer","16-bit, signed integer",
       "16-bit, unsigned integer","32-bit, signed integer",
       "32-bit, unsigned integer","IEEE 754 single-precision","RESERVED",
       "IEEE 754 double-precision","RESERVED","RESERVED",
       "64-bit, signed integer","64-bit, unsigned integer", "Matlab Array",
       "Compressed Data","Unicode UTF-8 Encoded Character Data",
       "Unicode UTF-16 Encoded Character Data",
       "Unicode UTF-32 Encoded Character Data","","String","Cell Array",
       "Structure"};

/*
 * -------------------------------------------------------------
 *   Private Functions
 * -------------------------------------------------------------
 */

#if 0
/*----------- Functions to read data into type-specified arrays -----------*/
static int ReadDoubleData(mat_t *mat,double  *data,int data_type,int len);
static int ReadSingleData(mat_t *mat,float   *data,int data_type,int len);
static int ReadInt32Data (mat_t *mat,mat_int32_t *data,int data_type,int len);
static int ReadInt16Data (mat_t *mat,mat_int16_t *data,int data_type,int len);
static int ReadInt8Data  (mat_t *mat,mat_int8_t  *data,int data_type,int len);
static int ReadCompressedDoubleData(mat_t *mat,z_stream *z,double  *data,
               int data_type,int len);
static int ReadCompressedSingleData(mat_t *mat,z_stream *z,float   *data,
               int data_type,int len);
static int ReadCompressedInt32Data (mat_t *mat,z_stream *z,mat_int32_t *data,
               int data_type,int len);
static int ReadCompressedInt16Data (mat_t *mat,z_stream *z,mat_int16_t *data,
               int data_type,int len);
static int ReadCompressedInt8Data  (mat_t *mat,z_stream *z,mat_int8_t  *data,
               int data_type,int len);
/*-------------------------------------------------------------------------*/

/*----------- Functions to do partial I/O (slabs) ------------------------*/
static int ReadDataSlabN(mat_t *mat,void *data,int class_type,
               int data_type,int rank,int *dims,int *start,int *stride,
               int *edge);
static int ReadDataSlab2(mat_t *mat,void *data,int class_type,
               int data_type,int *dims,int *start,int *stride,int *edge);
static int ReadCompressedDataSlabN(mat_t *mat,z_stream *z,void *data,
               int class_type,int data_type,int rank,int *dims,int *start,
               int *stride,int *edge);
static int ReadCompressedDataSlab2(mat_t *mat,z_stream *z,void *data,
               int class_type,int data_type,int *dims,int *start,int *stride,
               int *edge);
/*-------------------------------------------------------------------------*/

static int WriteCellArrayField( mat_t *mat, matvar_t *matvar, 
                                int compress );
static int WriteStructField( mat_t *mat, matvar_t *matvar, 
                             int compress );
static int ReadNextStructField( mat_t *mat, matvar_t *matvar );
static int ReadNextCell( mat_t *mat, matvar_t *matvar );
static int WriteEmptyCharData(mat_t *mat, int N, int data_type);
#endif

/*
 *-------------------------------------------------------------------
 *
 *-------------------------------------------------------------------
 */

size_t
GetMatrixMaxBufSize(matvar_t *matvar)
{
    size_t nBytes = 0,len;
    size_t tag_size = 8, array_flags_size = 8;
    int    nmemb = 1, i;

    if ( matvar == NULL )
        return nBytes;

    /* Add the Array Flags tag and space to the number of bytes */
    nBytes += tag_size + array_flags_size;

    /* Get size of variable name, pad it to an 8 byte block, and add it to nBytes */
    if ( NULL != matvar->name )
        len = strlen(matvar->name);
    else
        len=8;

    if ( len <= 4 ) {
        nBytes += tag_size;
    } else {
        if ( len % 8 )
            len = len + (8 - len % 8);
        nBytes += tag_size + len;
    }

    /* Add rank and dimensions, padded to an 8 byte block */
    for ( i = 0, len = 0; i < matvar->rank; i++ )
        nmemb *= matvar->dims[i];
    if ( matvar->rank % 2 )
        nBytes += tag_size + matvar->rank*4 + 4;
    else
        nBytes += tag_size + matvar->rank*4;

    if ( matvar->class_type == MAT_C_STRUCT ) {
        matvar_t **fields = matvar->data;
        int i, nfields;
        size_t maxlen = 0;

        nfields = matvar->nbytes / (nmemb*matvar->data_size);
        for ( i = 0; i < nfields; i++ ) {
            if ( NULL != fields[i]->name && strlen(fields[i]->name) > maxlen )
                maxlen = strlen(fields[i]->name);
        }
        maxlen++;
        while ( nfields*maxlen % 8 != 0 )
            maxlen++;

        nBytes += tag_size + maxlen*nfields;

        /* FIXME: Add bytes for the fieldnames */
        if ( NULL != fields && nfields > 0 ) {
            for ( i = 0; i < nfields*nmemb; i++ )
                nBytes += GetMatrixMaxBufSize(fields[i]);
        }
    } else if ( matvar->class_type == MAT_C_CELL ) {
        matvar_t **cells = matvar->data;
        int i, ncells = matvar->nbytes / matvar->data_size;

        if ( NULL != cells && ncells > 0 ) {
            for ( i = 0; i < ncells; i++ )
                nBytes += GetMatrixMaxBufSize(cells[i]);
        }
    } else if ( matvar->class_type == MAT_C_SPARSE ) {
        sparse_t *sparse = matvar->data;

        nBytes += tag_size + sparse->njc*sizeof(mat_int32_t) +
                  tag_size + sparse->nir*sizeof(mat_int32_t) +
                  tag_size + sparse->ndata*Mat_SizeOf(matvar->data_type);
        if ( matvar->isComplex )
            nBytes += tag_size + sparse->ndata*Mat_SizeOf(matvar->data_type);
    } else {
        nBytes += tag_size + nmemb*Mat_SizeOf(matvar->data_type);
        if ( matvar->isComplex )
            nBytes += tag_size + nmemb*Mat_SizeOf(matvar->data_type);
    }
    
    return nBytes;
}

int
WriteCharData(mat_t *mat, void *data, int N,int data_type)
{
    int nBytes = 0, bytesread = 0, i;
    mat_int8_t pad1 = 0;

    switch ( data_type ) {
        case MAT_T_UINT16:
        {
            nBytes = N*2;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            fwrite(data,2,N,mat->fp);
            if ( nBytes % 8 )
                for ( i = nBytes % 8; i < 8; i++ )
                    fwrite(&pad1,1,1,mat->fp);
            break;
        }
        case MAT_T_INT8:
        case MAT_T_UINT8:
        {
            mat_uint8_t *ptr;
            mat_uint16_t c;

            /* Matlab can't read MAT_C_CHAR as uint8, needs uint16 */
            nBytes = N*2;
            data_type = MAT_T_UINT16;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            ptr = data;
            for ( i = 0; i < N; i++ ) {
                c = (mat_uint16_t)*(char *)ptr;
                fwrite(&c,2,1,mat->fp);
                ptr++;
            }
            if ( nBytes % 8 )
                for ( i = nBytes % 8; i < 8; i++ )
                    fwrite(&pad1,1,1,mat->fp);
            break;
        }
        case MAT_T_UTF8:
        {
            mat_uint8_t *ptr;

            nBytes = N;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            ptr = data;
            fwrite(ptr,1,nBytes,mat->fp);
            if ( nBytes % 8 )
                for ( i = nBytes % 8; i < 8; i++ )
                    fwrite(&pad1,1,1,mat->fp);
            break;
        }
    }
    bytesread+=nBytes;
    return bytesread;
}

int
WriteEmptyCharData(mat_t *mat, int N, int data_type)
{
    int nBytes = 0, bytesread = 0, i;
    mat_int8_t pad1 = 0;

    switch ( data_type ) {
        case MAT_T_UINT8: /* Matlab MAT_C_CHAR needs uint16 */
        case MAT_T_INT8:  /* Matlab MAT_C_CHAR needs uint16 */
            data_type = MAT_T_UINT16;
        case MAT_T_UINT16:
        {
            mat_uint16_t u16 = 0;
            nBytes = N*sizeof(mat_uint16_t);
            fwrite(&data_type,sizeof(mat_int32_t),1,mat->fp);
            fwrite(&nBytes,sizeof(mat_int32_t),1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&u16,sizeof(mat_uint16_t),1,mat->fp);
            if ( nBytes % 8 )
                for ( i = nBytes % 8; i < 8; i++ )
                    fwrite(&pad1,1,1,mat->fp);
            break;
        }
        case MAT_T_UTF8:
        {
            mat_uint8_t u8 = 0;
            nBytes = N;
            fwrite(&data_type,sizeof(mat_int32_t),1,mat->fp);
            fwrite(&nBytes,sizeof(mat_int32_t),1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&u8,sizeof(mat_uint8_t),1,mat->fp);
            if ( nBytes % 8 )
                for ( i = nBytes % 8; i < 8; i++ )
                    fwrite(&pad1,1,1,mat->fp);
            break;
        }
    }
    bytesread+=nBytes;
    return bytesread;
}

/*
 * Writes the data tags and empty data to the file to save space for the
 * variable when the actual data is written
 */
int
WriteEmptyData(mat_t *mat,int N,int data_type)
{
    int nBytes = 0, data_size, i;

    if ( (mat == NULL) || (mat->fp == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d = 0.0;

            data_size = sizeof(double);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&d,data_size,1,mat->fp);
            break;
        }
        case MAT_T_SINGLE:
        {
            float f = 0.0;

            data_size = sizeof(float);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&f,data_size,1,mat->fp);
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8 = 0;

            data_size = sizeof(mat_int8_t);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&i8,data_size,1,mat->fp);
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8 = 0;

            data_size = sizeof(mat_uint8_t);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&ui8,data_size,1,mat->fp);
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16 = 0;

            data_size = sizeof(mat_int16_t);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&i16,data_size,1,mat->fp);
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16 = 0;

            data_size = sizeof(mat_uint16_t);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&ui16,data_size,1,mat->fp);
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32 = 0;

            data_size = sizeof(mat_int32_t);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&i32,data_size,1,mat->fp);
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32 = 0;

            data_size = sizeof(mat_uint32_t);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&ui32,data_size,1,mat->fp);
            break;
        }
        default:
            nBytes = 0;
    }
    return nBytes;
}

#if defined(HAVE_ZLIB)
int
WriteCompressedEmptyData(mat_t *mat,z_stream *z,int N,int data_type)
{
    int nBytes = 0, data_size, i, err, byteswritten = 0;

    if ( (mat == NULL) || (mat->fp == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            mat_uint32_t uncomp_buf[32] = {0,};
            mat_uint32_t comp_buf[32] = {0,};
            double data_uncomp_buf[4] = {0.0,};

            data_size = sizeof(double);
            nBytes = N*data_size;
            uncomp_buf[0] = data_type;
            uncomp_buf[1] = 0;
            z->next_out  = comp_buf;
            z->next_in   = uncomp_buf;
            z->avail_out = 32*sizeof(*comp_buf);
            z->avail_in  = 8;
            err = deflate(z,Z_NO_FLUSH);
            byteswritten += fwrite(comp_buf,1,32*sizeof(*comp_buf)-z->avail_out,mat->fp);
            for ( i = 0; i < N; i++ ) {
                z->next_out  = comp_buf;
                z->next_in   = data_uncomp_buf;
                z->avail_out = 32*sizeof(*comp_buf);
                z->avail_in  = 8;
                err = deflate(z,Z_NO_FLUSH);
                byteswritten += fwrite(comp_buf,32*sizeof(*comp_buf)-z->avail_out,1,mat->fp);
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f = 0.0;

            data_size = sizeof(float);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&f,data_size,1,mat->fp);
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8 = 0;

            data_size = sizeof(mat_int8_t);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&i8,data_size,1,mat->fp);
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8 = 0;

            data_size = sizeof(mat_uint8_t);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&ui8,data_size,1,mat->fp);
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16 = 0;

            data_size = sizeof(mat_int16_t);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&i16,data_size,1,mat->fp);
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16 = 0;

            data_size = sizeof(mat_uint16_t);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&ui16,data_size,1,mat->fp);
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32 = 0;

            data_size = sizeof(mat_int32_t);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&i32,data_size,1,mat->fp);
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32 = 0;

            data_size = sizeof(mat_uint32_t);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&ui32,data_size,1,mat->fp);
            break;
        }
        default:
            nBytes = 0;
    }
    return byteswritten;
}
#endif

int
WriteDataSlab2(mat_t *mat,void *data,int data_type,int *dims,int *start,
              int *stride,int *edge)
{
    int nBytes = 0, data_size, i, j;
    long pos, row_stride, col_stride;

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) ||
         (start == NULL) || (stride == NULL) || (edge    == NULL) ) {
        return 0;
    }

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double *ptr;

            data_size = sizeof(double);
            ptr = (double *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;

            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    fwrite(ptr++,data_size,1,mat->fp);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float *ptr;

            data_size = sizeof(float);
            ptr = (float *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;

            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    fwrite(ptr++,data_size,1,mat->fp);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t *ptr;

            data_size = sizeof(mat_int32_t);
            ptr = (mat_int32_t *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;

            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    fwrite(ptr++,data_size,1,mat->fp);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t *ptr;

            data_size = sizeof(mat_uint32_t);
            ptr = (mat_uint32_t *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;

            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    fwrite(ptr++,data_size,1,mat->fp);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t *ptr;

            data_size = sizeof(mat_int16_t);
            ptr = (mat_int16_t *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;

            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    fwrite(ptr++,data_size,1,mat->fp);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t *ptr;

            data_size = sizeof(mat_uint16_t);
            ptr = (mat_uint16_t *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;

            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    fwrite(ptr++,data_size,1,mat->fp);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t *ptr;

            data_size = sizeof(mat_int8_t);
            ptr = (mat_int8_t *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;

            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    fwrite(ptr++,data_size,1,mat->fp);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t *ptr;

            data_size = sizeof(mat_uint8_t);
            ptr = (mat_uint8_t *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;

            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    fwrite(ptr++,data_size,1,mat->fp);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        default:
            nBytes = 0;
    }
    return nBytes;
}

int
WriteCharDataSlab2(mat_t *mat,void *data,int data_type,int *dims,int *start,
              int *stride,int *edge)
{
    int nBytes = 0, data_size, i, j;
    long pos, row_stride, col_stride;

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) ||
         (start == NULL) || (stride == NULL) || (edge    == NULL) ) {
        return 0;
    }

    switch ( data_type ) {
        case MAT_T_UINT16:
        {
            mat_uint16_t *ptr;

            data_size = sizeof(mat_uint16_t);
            ptr = data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;

            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    fwrite(ptr++,data_size,1,mat->fp);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        case MAT_T_INT8:
        case MAT_T_UINT8:
        {
            /* Matlab can't read MAT_C_CHAR as uint8, needs uint16 */
            mat_uint8_t *ptr;
            mat_uint16_t c;

            data_size = sizeof(mat_uint16_t);
            ptr = data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;

            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++,ptr++ ) {
                    c = *ptr;
                    fwrite(&c,data_size,1,mat->fp);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        case MAT_T_UTF8:
        {
            mat_uint8_t *ptr;

            data_size = sizeof(mat_uint8_t);
            ptr = data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;

            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++,ptr++ ) {
                    fwrite(ptr,data_size,1,mat->fp);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        default:
            nBytes = 0;
    }
    return nBytes;
}

/* Writes the data buffer to the file */
int
WriteData(mat_t *mat,void *data,int N,int data_type)
{
    int nBytes = 0, data_size;

    if ((mat == NULL) || (mat->fp == NULL) || (data == NULL && N > 0))
        return 0;

    data_size = Mat_SizeOf(data_type);
    nBytes    = N*data_size;
    fwrite(&data_type,4,1,mat->fp);
    fwrite(&nBytes,4,1,mat->fp);
    fwrite(data,data_size,N,mat->fp);

    return nBytes;
}

#if defined(HAVE_ZLIB)
/* Compresses the data buffer and writes it to the file */
int
WriteCompressedData(mat_t *mat,z_stream *z,void *data,int N,int data_type)
{
    int nBytes = 0, data_size, data_tag[2], err, byteswritten = 0;
    int buf_size = 1024;
    mat_uint8_t   buf[1024], pad[8] = {0,};

    if ((mat == NULL) || (data == NULL) || (mat->fp == NULL))
        return 0;

    data_size = Mat_SizeOf(data_type);

    data_tag[0]  = data_type;
    data_tag[1]  = data_size*N;
    z->next_in   = data_tag;
    z->avail_in  = 8;
    z->next_out  = buf;
    z->avail_out = buf_size;
    err = deflate(z,Z_NO_FLUSH);
    byteswritten += fwrite(buf,1,buf_size-z->avail_out,mat->fp);
    z->next_in   = data;
    z->avail_in  = N*data_size;
    do {
        z->next_out  = buf;
        z->avail_out = buf_size;
        err = deflate(z,Z_NO_FLUSH);
        byteswritten += fwrite(buf,1,buf_size-z->avail_out,mat->fp);
    } while ( z->avail_out == 0 );
    /* Add/Compress padding to pad to 8-byte boundary */
    if ( N*data_size % 8 ) {
        z->next_in   = pad;
        z->avail_in  = 8 - (N*data_size % 8);
        z->next_out  = buf;
        z->avail_out = buf_size;
        err = deflate(z,Z_NO_FLUSH);
        byteswritten += fwrite(buf,1,buf_size-z->avail_out,mat->fp);
    }
    nBytes = byteswritten;
    return nBytes;
}
#endif

/*
 * Reads the next struct fields (fieldname length,names,data headers for all
 * the fields
 */
int
ReadNextCell( mat_t *mat, matvar_t *matvar )
{
    int ncells, bytesread = 0, i, err;
    matvar_t **cells = NULL;

    if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
        mat_uint32_t uncomp_buf[16] = {0,};
        int      nbytes;
        mat_uint32_t array_flags; 

        ncells = 1;
        for ( i = 0; i < matvar->rank; i++ )
            ncells *= matvar->dims[i];
        matvar->data_size = sizeof(matvar_t *);
        matvar->nbytes    = ncells*matvar->data_size;
        matvar->data = malloc(matvar->nbytes);
        if ( !matvar->data )
            return bytesread;
        cells = matvar->data;
        for ( i = 0; i < ncells; i++ ) {
            cells[i] = Mat_VarCalloc();
            if ( NULL == cells[i] ) {
                Mat_Critical("Couldn't allocate memory for cell %d", i);
                continue;
            }

            cells[i]->fpos = ftell(mat->fp)-matvar->z->avail_in;

            /* Read variable tag for cell */
            bytesread += InflateVarTag(mat,matvar,uncomp_buf);
            if ( mat->byteswap ) {
                (void)uint32Swap(uncomp_buf);
                (void)uint32Swap(uncomp_buf+1);
            }
            nbytes = uncomp_buf[1];
            if ( uncomp_buf[0] != MAT_T_MATRIX ) {
                Mat_Critical("cells[%d], Uncompressed type not MAT_T_MATRIX",i);
                Mat_VarFree(cells[i]);
                cells[i] = NULL;
            }
            cells[i]->compression = 1;
            bytesread += InflateArrayFlags(mat,matvar,uncomp_buf);
            nbytes -= 16;
            if ( mat->byteswap ) {
                (void)uint32Swap(uncomp_buf);
                (void)uint32Swap(uncomp_buf+1);
                (void)uint32Swap(uncomp_buf+2);
                (void)uint32Swap(uncomp_buf+3);
            }
            /* Array Flags */
            if ( uncomp_buf[0] == MAT_T_UINT32 ) {
               array_flags = uncomp_buf[2];
               cells[i]->class_type  = (array_flags & MAT_F_CLASS_T);
               cells[i]->isComplex   = (array_flags & MAT_F_COMPLEX);
               cells[i]->isGlobal    = (array_flags & MAT_F_GLOBAL);
               cells[i]->isLogical   = (array_flags & MAT_F_LOGICAL);
               if ( cells[i]->class_type == MAT_C_SPARSE ) {
                   /* Need to find a more appropriate place to store nzmax */
                   cells[i]->nbytes      = uncomp_buf[3];
               }
            } else {
                Mat_Critical("Expected MAT_T_UINT32 for Array Tags, got %d",
                               uncomp_buf[0]);
                bytesread+=InflateSkip(mat,matvar->z,nbytes);
            }
            bytesread += InflateDimensions(mat,matvar,uncomp_buf);
            nbytes -= 8;
            if ( mat->byteswap ) {
                (void)uint32Swap(uncomp_buf);
                (void)uint32Swap(uncomp_buf+1);
            }
            /* Rank and Dimension */
            if ( uncomp_buf[0] == MAT_T_INT32 ) {
                int j = 0;

                cells[i]->rank = uncomp_buf[1];
                nbytes -= cells[i]->rank;
                cells[i]->rank /= 4;
                cells[i]->dims = malloc(cells[i]->rank*sizeof(int));
                if ( mat->byteswap ) {
                    for ( j = 0; j < cells[i]->rank; j++ )
                        cells[i]->dims[j] = uint32Swap(uncomp_buf+2+j);
                } else {
                    for ( j = 0; j < cells[i]->rank; j++ )
                        cells[i]->dims[j] = uncomp_buf[2+j];
                }
                if ( cells[i]->rank % 2 != 0 )
                    nbytes -= 4;
            }
            bytesread += InflateVarNameTag(mat,matvar,uncomp_buf);
            nbytes -= 8;
            cells[i]->z = calloc(1,sizeof(z_stream));
            err = inflateCopy(cells[i]->z,matvar->z);
            if ( err != Z_OK )
                Mat_Critical("inflateCopy returned error %d",err);
            cells[i]->datapos = ftell(mat->fp)-matvar->z->avail_in;
            if ( cells[i]->class_type == MAT_C_STRUCT )
                bytesread+=ReadNextStructField(mat,cells[i]);
            else if ( cells[i]->class_type == MAT_C_CELL )
                bytesread+=ReadNextCell(mat,cells[i]);
            fseek(mat->fp,cells[i]->datapos,SEEK_SET);
            bytesread+=InflateSkip(mat,matvar->z,nbytes);
        }
#else
        Mat_Critical("Not compiled with zlib support");
#endif

    } else {
        int ncells;
        mat_uint32_t buf[16];
        int      nbytes,nBytes;
        mat_uint32_t array_flags; 

        ncells = 1;
        for ( i = 0; i < matvar->rank; i++ )
            ncells *= matvar->dims[i];
        matvar->data_size = sizeof(matvar_t *);
        matvar->nbytes    = ncells*matvar->data_size;
        matvar->data = malloc(matvar->nbytes);
        if ( !matvar->data ) {
            Mat_Critical("Couldn't allocate memory for %s->data",matvar->name);
            return bytesread;
        }
        cells = (matvar_t **)matvar->data;
        for ( i = 0; i < ncells; i++ ) {
            cells[i] = Mat_VarCalloc();
            if ( !cells[i] ) {
                Mat_Critical("Couldn't allocate memory for cell %d", i);
                continue;
            }

            cells[i]->fpos = ftell(mat->fp);

            /* Read variable tag for cell */
            bytesread += fread(buf,4,2,mat->fp);
            if ( mat->byteswap ) {
                (void)uint32Swap(buf);
                (void)uint32Swap(buf+1);
            }
            nBytes = buf[1];
            if ( buf[0] != MAT_T_MATRIX ) {
                Mat_Critical("cells[%d] not MAT_T_MATRIX, fpos = %ld",i,ftell(mat->fp));
                Mat_VarFree(cells[i]);
                cells[i] = NULL;
                continue;
            }
            cells[i]->compression = 0;
#if defined(HAVE_ZLIB)
            cells[i]->z = NULL;
#endif

            /* Read Array Flags and The Dimensions Tag */
            bytesread  += fread(buf,4,6,mat->fp);
            if ( mat->byteswap ) {
                (void)uint32Swap(buf);
                (void)uint32Swap(buf+1);
                (void)uint32Swap(buf+2);
                (void)uint32Swap(buf+3);
                (void)uint32Swap(buf+4);
                (void)uint32Swap(buf+5);
            }
            nBytes-=24;
            /* Array Flags */
            if ( buf[0] == MAT_T_UINT32 ) {
               array_flags = buf[2];
               cells[i]->class_type  = (array_flags & MAT_F_CLASS_T);
               cells[i]->isComplex   = (array_flags & MAT_F_COMPLEX);
               cells[i]->isGlobal    = (array_flags & MAT_F_GLOBAL);
               cells[i]->isLogical   = (array_flags & MAT_F_LOGICAL);
               if ( cells[i]->class_type == MAT_C_SPARSE ) {
                   /* Need to find a more appropriate place to store nzmax */
                   cells[i]->nbytes      = buf[3];
               }
            }
            /* Rank and Dimension */
            if ( buf[4] == MAT_T_INT32 ) {
                int j;
                nbytes = buf[5];
                nBytes-=nbytes;

                cells[i]->rank = nbytes / 4;
                cells[i]->dims = malloc(cells[i]->rank*sizeof(int));

                /* Assumes rank <= 16 */
                if ( cells[i]->rank % 2 != 0 ) {
                    bytesread+=fread(buf,4,cells[i]->rank+1,mat->fp);
                    nBytes-=4;
                } else
                    bytesread+=fread(buf,4,cells[i]->rank,mat->fp);

                if ( mat->byteswap ) {
                    for ( j = 0; j < cells[i]->rank; j++ )
                        cells[i]->dims[j] = uint32Swap(buf+j);
                } else {
                    for ( j = 0; j < cells[i]->rank; j++ )
                        cells[i]->dims[j] = buf[j];
                }
            }
            /* Variable Name Tag */
            bytesread+=fread(buf,1,8,mat->fp);
            nBytes-=8;
            cells[i]->datapos = ftell(mat->fp);
            if ( cells[i]->class_type == MAT_C_STRUCT )
                bytesread+=ReadNextStructField(mat,cells[i]);
            if ( cells[i]->class_type == MAT_C_CELL )
                bytesread+=ReadNextCell(mat,cells[i]);
            fseek(mat->fp,cells[i]->datapos+nBytes,SEEK_SET);
        }
    }

    return bytesread;
}

/*
 * Reads the next struct fields (fieldname length,names,data headers for all
 * the fields
 */
int
ReadNextStructField( mat_t *mat, matvar_t *matvar )
{
    int fieldname_size,nfields, bytesread = 0, i, err;
    matvar_t **fields = NULL;

    if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
        char    *ptr;
        mat_uint32_t uncomp_buf[16] = {0,};
        int      nbytes, j, nmemb = 1;
        mat_uint32_t array_flags; 

        for ( i = 0; i < matvar->rank; i++ )
            nmemb *= matvar->dims[i];

        /* Inflate Field name length */
        bytesread += InflateFieldNameLength(mat,matvar,uncomp_buf);
        if ( mat->byteswap ) {
            (void)uint32Swap(uncomp_buf);
            (void)uint32Swap(uncomp_buf+1);
        }
        if ( (uncomp_buf[0] & 0x0000ffff) == MAT_T_INT32 ) {
            fieldname_size = uncomp_buf[1];
        } else {
            Mat_Warning("Error getting fieldname size");
            return bytesread;
        }

        bytesread += InflateFieldNamesTag(mat,matvar,uncomp_buf);
        if ( mat->byteswap ) {
            (void)uint32Swap(uncomp_buf);
            (void)uint32Swap(uncomp_buf+1);
        }
        nfields = uncomp_buf[1];
        nfields = nfields / fieldname_size;
        matvar->data_size = sizeof(matvar_t *);
        matvar->nbytes    = nmemb*nfields*matvar->data_size;
        matvar->data      = malloc(matvar->nbytes);
        if ( !matvar->data )
            return 1;
        fields = matvar->data;
        if ( nfields*fieldname_size % 8 != 0 )
            i = 8-(nfields*fieldname_size % 8);
        else
            i = 0;
        ptr = malloc(nfields*fieldname_size+i);
        bytesread += InflateFieldNames(mat,matvar,ptr,nfields,fieldname_size,i);
        for ( i = 0; i < nfields; i++ ) {
            fields[i]       = calloc(1,sizeof(matvar_t));
            fields[i]->name = malloc(fieldname_size);
            memcpy(fields[i]->name,ptr+i*fieldname_size,fieldname_size);
            fields[i]->name[fieldname_size-1] = '\0';
        }
        for ( i = 1; i < nmemb; i++ ) {
            for ( j = 0; j < nfields; j++ ) {
                fields[i*nfields+j] = calloc(1,sizeof(matvar_t));
                fields[i*nfields+j]->name = strdup_printf("%s",fields[j]->name);
            }
        }

        for ( i = 0; i < nmemb*nfields; i++ ) {
            fields[i]->fpos = ftell(mat->fp)-matvar->z->avail_in;
            /* Read variable tag for struct field */
            bytesread += InflateVarTag(mat,matvar,uncomp_buf);
            if ( mat->byteswap ) {
                (void)uint32Swap(uncomp_buf);
                (void)uint32Swap(uncomp_buf+1);
            }
            nbytes = uncomp_buf[1];
            if ( uncomp_buf[0] != MAT_T_MATRIX ) {
                Mat_Critical("fields[%d], Uncompressed type not MAT_T_MATRIX",i);
                Mat_VarFree(fields[i]);
                fields[i] = NULL;
                continue;
            } else if ( nbytes == 0 ) {
                fields[i]->rank = 0;
                continue;
            }
            fields[i]->compression = COMPRESSION_ZLIB;
            bytesread += InflateArrayFlags(mat,matvar,uncomp_buf);
            nbytes -= 16;
            if ( mat->byteswap ) {
                (void)uint32Swap(uncomp_buf);
                (void)uint32Swap(uncomp_buf+1);
                (void)uint32Swap(uncomp_buf+2);
                (void)uint32Swap(uncomp_buf+3);
            }
            /* Array Flags */
            if ( uncomp_buf[0] == MAT_T_UINT32 ) {
               array_flags = uncomp_buf[2];
               fields[i]->class_type  = (array_flags & MAT_F_CLASS_T);
               fields[i]->isComplex   = (array_flags & MAT_F_COMPLEX);
               fields[i]->isGlobal    = (array_flags & MAT_F_GLOBAL);
               fields[i]->isLogical   = (array_flags & MAT_F_LOGICAL);
               if ( fields[i]->class_type == MAT_C_SPARSE ) {
                   /* Need to find a more appropriate place to store nzmax */
                   fields[i]->nbytes      = uncomp_buf[3];
               }
            } else {
                Mat_Critical("Expected MAT_T_UINT32 for Array Tags, got %d",
                    uncomp_buf[0]);
                bytesread+=InflateSkip(mat,matvar->z,nbytes);
            }
            bytesread += InflateDimensions(mat,matvar,uncomp_buf);
            nbytes -= 8;
            if ( mat->byteswap ) {
                (void)uint32Swap(uncomp_buf);
                (void)uint32Swap(uncomp_buf+1);
            }
            /* Rank and Dimension */
            if ( uncomp_buf[0] == MAT_T_INT32 ) {
                int j = 0;

                fields[i]->rank = uncomp_buf[1];
                nbytes -= fields[i]->rank;
                fields[i]->rank /= 4;
                fields[i]->dims = malloc(fields[i]->rank*sizeof(int));
                if ( mat->byteswap ) {
                    for ( j = 0; j < fields[i]->rank; j++ )
                        fields[i]->dims[j] = uint32Swap(uncomp_buf+2+j);
                } else {
                    for ( j = 0; j < fields[i]->rank; j++ )
                        fields[i]->dims[j] = uncomp_buf[2+j];
                }
                if ( fields[i]->rank % 2 != 0 )
                    nbytes -= 4;
            }
            bytesread += InflateVarNameTag(mat,matvar,uncomp_buf);
            nbytes -= 8;
            fields[i]->z = calloc(1,sizeof(z_stream));
            err = inflateCopy(fields[i]->z,matvar->z);
            if ( err != Z_OK ) {
                Mat_Critical("inflateCopy returned error %d",err);
            }
            fields[i]->datapos = ftell(mat->fp)-matvar->z->avail_in;
            if ( fields[i]->class_type == MAT_C_STRUCT )
                bytesread+=ReadNextStructField(mat,fields[i]);
            else if ( fields[i]->class_type == MAT_C_CELL )
                bytesread+=ReadNextCell(mat,fields[i]);
            fseek(mat->fp,fields[i]->datapos,SEEK_SET);
            bytesread+=InflateSkip(mat,matvar->z,nbytes);
        }
        free(ptr);
#else
        Mat_Critical("Not compiled with zlib support");
#endif
    } else {
        int fieldname_size,nfields;
        mat_uint32_t buf[16] = {0,};
        int      nbytes,nBytes,nmemb=1,j;
        mat_uint32_t array_flags; 

        for ( i = 0; i < matvar->rank; i++ )
            nmemb *= matvar->dims[i];

        bytesread+=fread(buf,4,2,mat->fp);
        if ( mat->byteswap ) {
            (void)uint32Swap(buf);
            (void)uint32Swap(buf+1);
        }
        if ( (buf[0] & 0x0000ffff) == MAT_T_INT32 ) {
            fieldname_size = buf[1];
        } else {
            Mat_Warning("Error getting fieldname size");
            return bytesread;
        }
        bytesread+=fread(buf,4,2,mat->fp);
        if ( mat->byteswap ) {
            (void)uint32Swap(buf);
            (void)uint32Swap(buf+1);
        }
        nfields = buf[1];
        nfields = nfields / fieldname_size;
        matvar->data_size = sizeof(matvar_t *);
        matvar->nbytes    = nmemb*nfields*matvar->data_size;
        matvar->data = malloc(matvar->nbytes);
        if ( !matvar->data )
            return bytesread;
        fields = (matvar_t **)matvar->data;
        for ( i = 0; i < nfields; i++ ) {
            fields[i] = calloc(1,sizeof(matvar_t));
            fields[i]->name = malloc(fieldname_size);
            bytesread+=fread(fields[i]->name,1,fieldname_size,mat->fp);
            fields[i]->name[fieldname_size-1] = '\0';
        }
        for ( i = 1; i < nmemb; i++ ) {
            for ( j = 0; j < nfields; j++ ) {
                fields[i*nfields+j] = calloc(1,sizeof(matvar_t));
                fields[i*nfields+j]->name = strdup_printf("%s",fields[j]->name);
            }
        }
        if ( (nfields*fieldname_size) % 8 ) {
            fseek(mat->fp,8-((nfields*fieldname_size) % 8),SEEK_CUR);
            bytesread+=8-((nfields*fieldname_size) % 8);
        }
        for ( i = 0; i < nmemb*nfields; i++ ) {

            fields[i]->fpos = ftell(mat->fp);

            /* Read variable tag for struct field */
            bytesread += fread(buf,4,2,mat->fp);
            if ( mat->byteswap ) {
                (void)uint32Swap(buf);
                (void)uint32Swap(buf+1);
            }
            nBytes = buf[1];
            if ( buf[0] != MAT_T_MATRIX ) {
                Mat_Critical("fields[%d] not MAT_T_MATRIX, fpos = %ld",i,ftell(mat->fp));
                Mat_VarFree(fields[i]);
                fields[i] = NULL;
                return bytesread;
            } else if ( nBytes == 0 ) {
                fields[i]->rank = 0;
                continue;
            }
            fields[i]->compression = 0;
#if defined(HAVE_ZLIB)
            fields[i]->z = NULL;
#endif

            /* Read Array Flags and The Dimensions Tag */
            bytesread  += fread(buf,4,6,mat->fp);
            if ( mat->byteswap ) {
                (void)uint32Swap(buf);
                (void)uint32Swap(buf+1);
                (void)uint32Swap(buf+2);
                (void)uint32Swap(buf+3);
                (void)uint32Swap(buf+4);
                (void)uint32Swap(buf+5);
            }
            nBytes-=24;
            /* Array Flags */
            if ( buf[0] == MAT_T_UINT32 ) {
               array_flags = buf[2];
               fields[i]->class_type  = (array_flags & MAT_F_CLASS_T);
               fields[i]->isComplex   = (array_flags & MAT_F_COMPLEX);
               fields[i]->isGlobal    = (array_flags & MAT_F_GLOBAL);
               fields[i]->isLogical   = (array_flags & MAT_F_LOGICAL);
               if ( fields[i]->class_type == MAT_C_SPARSE ) {
                   /* Need to find a more appropriate place to store nzmax */
                   fields[i]->nbytes      = buf[3];
               }
            }
            /* Rank and Dimension */
            if ( buf[4] == MAT_T_INT32 ) {
                int j;

                nbytes = buf[5];
                nBytes-=nbytes;

                fields[i]->rank = nbytes / 4;
                fields[i]->dims = malloc(fields[i]->rank*sizeof(int));

                /* Assumes rank <= 16 */
                if ( fields[i]->rank % 2 != 0 ) {
                    bytesread+=fread(buf,4,fields[i]->rank+1,mat->fp);
                    nBytes-=4;
                } else
                    bytesread+=fread(buf,4,fields[i]->rank,mat->fp);

                if ( mat->byteswap ) {
                    for ( j = 0; j < fields[i]->rank; j++ )
                        fields[i]->dims[j] = uint32Swap(buf+j);
                } else {
                    for ( j = 0; j < fields[i]->rank; j++ )
                        fields[i]->dims[j] = buf[j];
                }
            }
            /* Variable Name Tag */
            bytesread+=fread(buf,1,8,mat->fp);
            nBytes-=8;
            fields[i]->datapos = ftell(mat->fp);
            if ( fields[i]->class_type == MAT_C_STRUCT )
                bytesread+=ReadNextStructField(mat,fields[i]);
            else if ( fields[i]->class_type == MAT_C_CELL )
                bytesread+=ReadNextCell(mat,fields[i]);
            fseek(mat->fp,fields[i]->datapos+nBytes,SEEK_SET);
        }
    }

    return bytesread;
}

/*
 * Reads the next struct fields (fieldname length,names,data headers for all
 * the fields
 */
int
ReadNextFunctionHandle(mat_t *mat, matvar_t *matvar)
{
    int nfunctions = 1, bytesread = 0, i;
    matvar_t **functions = NULL;

    for ( i = 0; i < matvar->rank; i++ )
        nfunctions *= matvar->dims[i];

    matvar->data = malloc(nfunctions*sizeof(matvar_t *));
    if ( matvar->data != NULL ) {
        matvar->data_size = sizeof(matvar_t *);
        matvar->nbytes    = nfunctions*matvar->data_size;
        functions = matvar->data;
        for ( i = 0 ; i < nfunctions; i++ )
            functions[i] = Mat_VarReadNextInfo(mat);
    } else {
        bytesread = 0;
        matvar->data_size = 0;
        matvar->nbytes    = 0;
    }

    return bytesread;
}

/*
 * ----------------------------------------
 *  Write routines for struct/cell fields
 * ----------------------------------------
 */
int
WriteCellArrayFieldInfo(mat_t *mat,matvar_t *matvar,int compress )
{
    mat_uint32_t array_flags = 0x0; 
    mat_int16_t  array_name_type = MAT_T_INT8;
    int      array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int      array_flags_size = 8, pad4 = 0, matrix_type = MAT_T_MATRIX;
    mat_int8_t   pad1 = 0;
    int      nBytes, i, nmemb = 1;
    long     start = 0, end = 0;

    if ((matvar == NULL) || (mat == NULL))
        return 0;

#if 0
    nBytes = GetMatrixMaxBufSize(matvar);
#endif

    fwrite(&matrix_type,4,1,mat->fp);
    fwrite(&pad4,4,1,mat->fp);
    start = ftell(mat->fp);

    /* Array Flags */
    if ( matvar->rank > 1 && ( matvar->dims[0] > 1 || matvar->dims[1] > 1 ) &&
         matvar->class_type == MAT_C_INT32 ) {
        array_flags = MAT_C_DOUBLE & MAT_F_CLASS_T;
    } else {
        array_flags = matvar->class_type & MAT_F_CLASS_T;
    }
    if ( matvar->isComplex )
        array_flags |= MAT_F_COMPLEX;
    if ( matvar->isGlobal )
        array_flags |= MAT_F_GLOBAL;
    if ( matvar->isLogical )
        array_flags |= MAT_F_LOGICAL;

    if ( mat->byteswap )
        array_flags = int32Swap((mat_int32_t*)&array_flags);
    fwrite(&array_flags_type,4,1,mat->fp);
    fwrite(&array_flags_size,4,1,mat->fp);
    fwrite(&array_flags,4,1,mat->fp);
    fwrite(&pad4,4,1,mat->fp);
    /* Rank and Dimension */
    nBytes = matvar->rank * 4;
    fwrite(&dims_array_type,4,1,mat->fp);
    fwrite(&nBytes,4,1,mat->fp);
    for ( i = 0; i < matvar->rank; i++ ) {
        mat_int32_t dim;
        dim = matvar->dims[i];
        nmemb *= dim;
        fwrite(&dim,4,1,mat->fp);
    }
    if ( matvar->rank % 2 != 0 )
        fwrite(&pad4,4,1,mat->fp);
    /* Name of variable */
    if ( !matvar->name ) {
        fwrite(&array_name_type,2,1,mat->fp);
        fwrite(&pad1,1,1,mat->fp);
        fwrite(&pad1,1,1,mat->fp);
        fwrite(&pad4,4,1,mat->fp);
    } else if ( strlen(matvar->name) <= 4 ) {
        mat_int16_t array_name_len = (mat_int16_t)strlen(matvar->name);
        mat_int8_t  pad1 = 0;
        fwrite(&array_name_type,2,1,mat->fp);
        fwrite(&array_name_len,2,1,mat->fp);
        fwrite(matvar->name,1,array_name_len,mat->fp);
        for ( i = array_name_len; i < 4; i++ )
            fwrite(&pad1,1,1,mat->fp);
    } else {
        mat_int32_t array_name_len = (mat_int32_t)strlen(matvar->name);
        mat_int8_t  pad1 = 0;

        fwrite(&array_name_type,2,1,mat->fp);
        fwrite(&pad1,1,1,mat->fp);
        fwrite(&pad1,1,1,mat->fp);
        fwrite(&array_name_len,4,1,mat->fp);
        fwrite(matvar->name,1,array_name_len,mat->fp);
        if ( array_name_len % 8 )
            for ( i = array_name_len % 8; i < 8; i++ )
                fwrite(&pad1,1,1,mat->fp);
    }

    matvar->datapos = ftell(mat->fp);
    switch ( matvar->class_type ) {
        case MAT_C_DOUBLE:
        case MAT_C_SINGLE:
        case MAT_C_INT32:
        case MAT_C_UINT32:
        case MAT_C_INT16:
        case MAT_C_UINT16:
        case MAT_C_INT8:
        case MAT_C_UINT8:
            nBytes = WriteEmptyData(mat,nmemb,matvar->data_type);
            if ( nBytes % 8 )
                for ( i = nBytes % 8; i < 8; i++ )
                    fwrite(&pad1,1,1,mat->fp);
            if ( matvar->isComplex ) {
                nBytes = WriteEmptyData(mat,nmemb,matvar->data_type);
                if ( nBytes % 8 )
                    for ( i = nBytes % 8; i < 8; i++ )
                        fwrite(&pad1,1,1,mat->fp);
            }
            break;
        case MAT_C_CHAR:
        {
            WriteEmptyCharData(mat,nmemb,matvar->data_type);
            break;
        }
        case MAT_C_CELL:
        {
            int nfields = matvar->nbytes / matvar->data_size;
            matvar_t **fields = (matvar_t **)matvar->data;

            for ( i = 0; i < nfields; i++ )
                WriteCellArrayFieldInfo(mat,fields[i],compress);
            break;
        }
        /* FIXME: Structures */
    }
    end = ftell(mat->fp);
    nBytes = (int)(end-start);
    fseek(mat->fp,(long)-(nBytes+4),SEEK_CUR);
    fwrite(&nBytes,4,1,mat->fp);
    fseek(mat->fp,end,SEEK_SET);
    return 0;
}

int
WriteCellArrayField(mat_t *mat,matvar_t *matvar,int compress )
{
    mat_uint32_t array_flags = 0x0; 
    mat_int16_t  array_name_type = MAT_T_INT8,fieldname_type = MAT_T_INT32,fieldname_data_size=4;
    int      array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int      array_flags_size = 8, pad4 = 0, matrix_type = MAT_T_MATRIX;
    mat_int8_t   pad1 = 0;
    int      nBytes, i, nmemb = 1, nzmax = 0;
    long     start = 0, end = 0;

    if ((matvar == NULL) || (mat == NULL))
        return 0;

#if 0
    nBytes = GetMatrixMaxBufSize(matvar);
#endif

    fwrite(&matrix_type,4,1,mat->fp);
    fwrite(&pad4,4,1,mat->fp);
    start = ftell(mat->fp);

    /* Array Flags */
    if ( matvar->rank > 1 && ( matvar->dims[0] > 1 || matvar->dims[1] > 1 ) &&
         matvar->class_type == MAT_C_INT32 ) {
        array_flags = MAT_C_DOUBLE & MAT_F_CLASS_T;
    } else {
        array_flags = matvar->class_type & MAT_F_CLASS_T;
    }
    if ( matvar->isComplex )
        array_flags |= MAT_F_COMPLEX;
    if ( matvar->isGlobal )
        array_flags |= MAT_F_GLOBAL;
    if ( matvar->isLogical )
        array_flags |= MAT_F_LOGICAL;
    if ( matvar->class_type == MAT_C_SPARSE )
        nzmax = ((sparse_t *)matvar->data)->nzmax;

    if ( mat->byteswap )
        array_flags = int32Swap((mat_int32_t*)&array_flags);
    fwrite(&array_flags_type,4,1,mat->fp);
    fwrite(&array_flags_size,4,1,mat->fp);
    fwrite(&array_flags,4,1,mat->fp);
    fwrite(&nzmax,4,1,mat->fp);
    /* Rank and Dimension */
    nBytes = matvar->rank * 4;
    fwrite(&dims_array_type,4,1,mat->fp);
    fwrite(&nBytes,4,1,mat->fp);
    for ( i = 0; i < matvar->rank; i++ ) {
        mat_int32_t dim;
        dim = matvar->dims[i];
        nmemb *= dim;
        fwrite(&dim,4,1,mat->fp);
    }
    if ( matvar->rank % 2 != 0 )
        fwrite(&pad4,4,1,mat->fp);
    /* Name of variable */
    if ( !matvar->name ) {
        fwrite(&array_name_type,2,1,mat->fp);
        fwrite(&pad1,1,1,mat->fp);
        fwrite(&pad1,1,1,mat->fp);
        fwrite(&pad4,4,1,mat->fp);
    } else if ( strlen(matvar->name) <= 4 ) {
        mat_int16_t array_name_len = (mat_int16_t)strlen(matvar->name);
        mat_int8_t  pad1 = 0;
        fwrite(&array_name_type,2,1,mat->fp);
        fwrite(&array_name_len,2,1,mat->fp);
        fwrite(matvar->name,1,array_name_len,mat->fp);
        for ( i = array_name_len; i < 4; i++ )
            fwrite(&pad1,1,1,mat->fp);
    } else {
        mat_int32_t array_name_len = (mat_int32_t)strlen(matvar->name);
        mat_int8_t  pad1 = 0;

        fwrite(&array_name_type,2,1,mat->fp);
        fwrite(&pad1,1,1,mat->fp);
        fwrite(&pad1,1,1,mat->fp);
        fwrite(&array_name_len,4,1,mat->fp);
        fwrite(matvar->name,1,array_name_len,mat->fp);
        if ( array_name_len % 8 )
            for ( i = array_name_len % 8; i < 8; i++ )
                fwrite(&pad1,1,1,mat->fp);
    }

    switch ( matvar->class_type ) {
        case MAT_C_DOUBLE:
        case MAT_C_SINGLE:
        case MAT_C_INT32:
        case MAT_C_UINT32:
        case MAT_C_INT16:
        case MAT_C_UINT16:
        case MAT_C_INT8:
        case MAT_C_UINT8:
        {
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data = matvar->data;


                nBytes=WriteData(mat,complex_data->Re,nmemb,matvar->data_type);
                if ( nBytes % 8 )
                    for ( i = nBytes % 8; i < 8; i++ )
                        fwrite(&pad1,1,1,mat->fp);
                nBytes=WriteData(mat,complex_data->Im,nmemb,matvar->data_type);
                if ( nBytes % 8 )
                    for ( i = nBytes % 8; i < 8; i++ )
                        fwrite(&pad1,1,1,mat->fp);
            } else {
                nBytes = WriteData(mat,matvar->data,nmemb,matvar->data_type);
                if ( nBytes % 8 )
                    for ( i = nBytes % 8; i < 8; i++ )
                        fwrite(&pad1,1,1,mat->fp);
            }
            break;
        }
        case MAT_C_CHAR:
            WriteCharData(mat,matvar->data,nmemb,matvar->data_type);
            break;
        case MAT_C_CELL:
        {
            int nfields = matvar->nbytes / matvar->data_size;
            matvar_t **fields = (matvar_t **)matvar->data;

            for ( i = 0; i < nfields; i++ )
                WriteCellArrayField(mat,fields[i],compress);
            break;
        }
        case MAT_C_STRUCT:
        {
            char **fieldnames, *padzero;
            int    fieldname_size, nfields;
            size_t maxlen = 0;
            matvar_t **fields = (matvar_t **)matvar->data;
            unsigned fieldname;

            nfields = matvar->nbytes / (nmemb*matvar->data_size);
            fieldnames = malloc(nfields*sizeof(char *));
            for ( i = 0; i < nfields; i++ ) {
                fieldnames[i] = fields[i]->name;
                if ( strlen(fieldnames[i]) > maxlen )
                    maxlen = strlen(fieldnames[i]);
            }
            maxlen++;
            fieldname_size = maxlen;
            while ( nfields*fieldname_size % 8 != 0 )
                fieldname_size++;
#if 0
            fwrite(&fieldname_type,2,1,mat->fp);
            fwrite(&fieldname_data_size,2,1,mat->fp);
#else
            fieldname = (fieldname_data_size<<16) | fieldname_type;
            fwrite(&fieldname,4,1,mat->fp);
#endif
            fwrite(&fieldname_size,4,1,mat->fp);
            fwrite(&array_name_type,2,1,mat->fp);
            fwrite(&pad1,1,1,mat->fp);
            fwrite(&pad1,1,1,mat->fp);
            nBytes = nfields*fieldname_size;
            fwrite(&nBytes,4,1,mat->fp);
            padzero = calloc(fieldname_size,1);
            for ( i = 0; i < nfields; i++ ) {
                fwrite(fieldnames[i],1,strlen(fieldnames[i]),mat->fp);
                fwrite(padzero,1,fieldname_size-strlen(fieldnames[i]),mat->fp);
            }
            free(fieldnames);
            for ( i = 0; i < nmemb*nfields; i++ )
                WriteStructField(mat,fields[i]);
            break;
        }
        case MAT_C_SPARSE:
        {
            sparse_t *sparse = matvar->data;

            nBytes = WriteData(mat,sparse->ir,sparse->nir,MAT_T_INT32);
            if ( nBytes % 8 )
                for ( i = nBytes % 8; i < 8; i++ )
                    fwrite(&pad1,1,1,mat->fp);
            nBytes = WriteData(mat,sparse->jc,sparse->njc,MAT_T_INT32);
            if ( nBytes % 8 )
                for ( i = nBytes % 8; i < 8; i++ )
                    fwrite(&pad1,1,1,mat->fp);
            nBytes = WriteData(mat,sparse->data,sparse->ndata,matvar->data_type);
            if ( nBytes % 8 )
                for ( i = nBytes % 8; i < 8; i++ )
                    fwrite(&pad1,1,1,mat->fp);
        }
    }
    end = ftell(mat->fp);
    nBytes = (int)(end-start);
    fseek(mat->fp,(long)-(nBytes+4),SEEK_CUR);
    fwrite(&nBytes,4,1,mat->fp);
    fseek(mat->fp,end,SEEK_SET);
    return 0;
}

int
WriteStructField(mat_t *mat,matvar_t *matvar)
{
    mat_uint32_t array_flags = 0x0; 
    mat_int16_t  fieldname_type = MAT_T_INT32,fieldname_data_size=4;
    mat_int32_t  array_name_type = MAT_T_INT8;
    int      array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int      array_flags_size = 8, pad4 = 0, matrix_type = MAT_T_MATRIX;
    mat_int8_t   pad1 = 0;
    int      nBytes, i, nmemb = 1, nzmax = 0;
    long     start = 0, end = 0;

    if ( (matvar == NULL) || ( mat == NULL ))
        return 1;

    fwrite(&matrix_type,4,1,mat->fp);
    fwrite(&pad4,4,1,mat->fp);
    start = ftell(mat->fp);

    /* Array Flags */
    array_flags = matvar->class_type & MAT_F_CLASS_T;
    if ( matvar->isComplex )
        array_flags |= MAT_F_COMPLEX;
    if ( matvar->isGlobal )
        array_flags |= MAT_F_GLOBAL;
    if ( matvar->isLogical )
        array_flags |= MAT_F_LOGICAL;
    if ( matvar->class_type == MAT_C_SPARSE )
        nzmax = ((sparse_t *)matvar->data)->nzmax;

    if ( mat->byteswap )
        array_flags = int32Swap((mat_int32_t*)&array_flags);
    fwrite(&array_flags_type,4,1,mat->fp);
    fwrite(&array_flags_size,4,1,mat->fp);
    fwrite(&array_flags,4,1,mat->fp);
    fwrite(&nzmax,4,1,mat->fp);
    /* Rank and Dimension */
    nBytes = matvar->rank * 4;
    fwrite(&dims_array_type,4,1,mat->fp);
    fwrite(&nBytes,4,1,mat->fp);
    for ( i = 0; i < matvar->rank; i++ ) {
        mat_int32_t dim;
        dim = matvar->dims[i];
        nmemb *= dim;
        fwrite(&dim,4,1,mat->fp);
    }
    if ( matvar->rank % 2 != 0 )
        fwrite(&pad4,4,1,mat->fp);

    /* Name of variable */
    fwrite(&array_name_type,4,1,mat->fp);
    fwrite(&pad4,4,1,mat->fp);

    switch ( matvar->class_type ) {
        case MAT_C_DOUBLE:
        case MAT_C_SINGLE:
        case MAT_C_INT32:
        case MAT_C_UINT32:
        case MAT_C_INT16:
        case MAT_C_UINT16:
        case MAT_C_INT8:
        case MAT_C_UINT8:
        {
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data = matvar->data;


                nBytes=WriteData(mat,complex_data->Re,nmemb,matvar->data_type);
                if ( nBytes % 8 )
                    for ( i = nBytes % 8; i < 8; i++ )
                        fwrite(&pad1,1,1,mat->fp);
                nBytes=WriteData(mat,complex_data->Im,nmemb,matvar->data_type);
                if ( nBytes % 8 )
                    for ( i = nBytes % 8; i < 8; i++ )
                        fwrite(&pad1,1,1,mat->fp);
            } else {
                nBytes=WriteData(mat,matvar->data,nmemb,matvar->data_type);
                if ( nBytes % 8 )
                    for ( i = nBytes % 8; i < 8; i++ )
                        fwrite(&pad1,1,1,mat->fp);
            }
            break;
        }
        case MAT_C_CHAR:
            nBytes=WriteCharData(mat,matvar->data,nmemb,matvar->data_type);
            break;
        case MAT_C_CELL:
        {
            int nfields = matvar->nbytes / matvar->data_size;
            matvar_t **fields = (matvar_t **)matvar->data;

            for ( i = 0; i < nfields; i++ )
                WriteCellArrayField(mat,fields[i],COMPRESSION_NONE);
            break;
        }
        case MAT_C_STRUCT:
        {
            char **fieldnames, *padzero;
            int    fieldname_size, nfields;
            size_t maxlen = 0;
            matvar_t **fields = (matvar_t **)matvar->data;
            unsigned fieldname;

            nfields = matvar->nbytes / (nmemb*matvar->data_size);
            fieldnames = malloc(nfields*sizeof(char *));
            for ( i = 0; i < nfields; i++ ) {
                fieldnames[i] = fields[i]->name;
                if ( strlen(fieldnames[i]) > maxlen )
                    maxlen = strlen(fieldnames[i]);
            }
            maxlen++;
            fieldname_size = maxlen;
            while ( nfields*fieldname_size % 8 != 0 )
                fieldname_size++;
#if 0
            fwrite(&fieldname_type,2,1,mat->fp);
            fwrite(&fieldname_data_size,2,1,mat->fp);
#else
            fieldname = (fieldname_data_size<<16) | fieldname_type;
            fwrite(&fieldname,4,1,mat->fp);
#endif
            fwrite(&fieldname_size,4,1,mat->fp);
            fwrite(&array_name_type,4,1,mat->fp);
            nBytes = nfields*fieldname_size;
            fwrite(&nBytes,4,1,mat->fp);
            padzero = calloc(fieldname_size,1);
            for ( i = 0; i < nfields; i++ ) {
                fwrite(fieldnames[i],1,strlen(fieldnames[i]),mat->fp);
                fwrite(padzero,1,fieldname_size-strlen(fieldnames[i]),
                       mat->fp);
            }
            free(fieldnames);
            free(padzero);
            for ( i = 0; i < nmemb*nfields; i++ )
                WriteStructField(mat,fields[i]);
            break;
        }
        case MAT_C_SPARSE:
        {
            sparse_t *sparse = matvar->data;

            nBytes = WriteData(mat,sparse->ir,sparse->nir,MAT_T_INT32);
            if ( nBytes % 8 )
                for ( i = nBytes % 8; i < 8; i++ )
                    fwrite(&pad1,1,1,mat->fp);
            nBytes = WriteData(mat,sparse->jc,sparse->njc,MAT_T_INT32);
            if ( nBytes % 8 )
                for ( i = nBytes % 8; i < 8; i++ )
                    fwrite(&pad1,1,1,mat->fp);
            nBytes = WriteData(mat,sparse->data,sparse->ndata,
                       matvar->data_type);
            if ( nBytes % 8 )
                for ( i = nBytes % 8; i < 8; i++ )
                    fwrite(&pad1,1,1,mat->fp);
        }
    }
    end = ftell(mat->fp);
    nBytes = (int)(end-start);
    fseek(mat->fp,(long)-(nBytes+4),SEEK_CUR);
    fwrite(&nBytes,4,1,mat->fp);
    fseek(mat->fp,end,SEEK_SET);
    return 0;
}

#if defined(HAVE_ZLIB)
int
WriteCompressedStructField(mat_t *mat,matvar_t *matvar,z_stream *z)
{
    mat_uint32_t array_flags = 0x0; 
    mat_int16_t  array_name_type     = MAT_T_INT8;
    mat_int16_t  fieldname_type      = MAT_T_INT32;
    mat_int16_t  fieldname_data_size = 4;
    int      array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int      array_flags_size = 8, pad4 = 0;
    mat_int8_t   pad1 = 0;
    int      nBytes, i, nmemb = 1, nzmax = 0;
    long     start = 0;

    mat_uint32_t comp_buf[512];
    mat_uint32_t uncomp_buf[512] = {0,};
    int buf_size = 512, err;
    size_t byteswritten = 0;

    if ( NULL == matvar || NULL == mat || NULL == z)
        return 1;

    start = ftell(mat->fp);

    /* Array Flags */
    array_flags = matvar->class_type & MAT_F_CLASS_T;
    if ( matvar->isComplex )
        array_flags |= MAT_F_COMPLEX;
    if ( matvar->isGlobal )
        array_flags |= MAT_F_GLOBAL;
    if ( matvar->isLogical )
        array_flags |= MAT_F_LOGICAL;
    if ( matvar->class_type == MAT_C_SPARSE )
        nzmax = ((sparse_t *)matvar->data)->nzmax;

    uncomp_buf[0] = MAT_T_MATRIX;
    uncomp_buf[1] = (int)GetMatrixMaxBufSize(matvar);
    z->next_out  = comp_buf;
    z->next_in   = uncomp_buf;
    z->avail_out = buf_size*sizeof(*comp_buf);
    z->avail_in  = 8;
    err = deflate(z,Z_NO_FLUSH);
    byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-z->avail_out,
        mat->fp);
    uncomp_buf[0] = array_flags_type;
    uncomp_buf[1] = array_flags_size;
    uncomp_buf[2] = array_flags;
    uncomp_buf[3] = nzmax;
    /* Rank and Dimension */
    nBytes = matvar->rank * 4;
    uncomp_buf[4] = dims_array_type;
    uncomp_buf[5] = nBytes;
    for ( i = 0; i < matvar->rank; i++ ) {
        mat_int32_t dim;
        dim = matvar->dims[i];
        nmemb *= dim;
        uncomp_buf[6+i] = dim;
    }
    if ( matvar->rank % 2 != 0 ) {
        uncomp_buf[6+i] = pad4;
        i++;
    }

    z->next_out  = comp_buf;
    z->next_in   = uncomp_buf;
    z->avail_out = buf_size*sizeof(*comp_buf);
    z->avail_in  = (6+i)*sizeof(*uncomp_buf);
    err = deflate(z,Z_NO_FLUSH);
    byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-z->avail_out,
        mat->fp);
    /* Name of variable */
    uncomp_buf[0] = array_name_type;
    uncomp_buf[1] = 0;
    z->next_out  = comp_buf;
    z->next_in   = uncomp_buf;
    z->avail_out = buf_size*sizeof(*comp_buf);
    z->avail_in  = 8;
    err = deflate(z,Z_NO_FLUSH);
    byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-z->avail_out,
        mat->fp);

    matvar->datapos = ftell(mat->fp);
    switch ( matvar->class_type ) {
        case MAT_C_DOUBLE:
        case MAT_C_SINGLE:
        case MAT_C_INT32:
        case MAT_C_UINT32:
        case MAT_C_INT16:
        case MAT_C_UINT16:
        case MAT_C_INT8:
        case MAT_C_UINT8:
        {
            /* WriteCompressedData makes sure uncomressed data is aligned
             * on an 8-byte boundary */
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data = matvar->data;

                byteswritten += WriteCompressedData(mat,z,
                    complex_data->Re,nmemb,matvar->data_type);
                byteswritten += WriteCompressedData(mat,z,
                    complex_data->Im,nmemb,matvar->data_type);
            } else {
                byteswritten += WriteCompressedData(mat,z,
                    matvar->data,nmemb,matvar->data_type);
            }
            break;
        }
        case MAT_C_CELL:
        {
            int        ncells;
            matvar_t **cells = (matvar_t **)matvar->data;

            /* Check for an empty cell array */
            if ( matvar->nbytes == 0 || matvar->data_size == 0 ||
                 matvar->data   == NULL )
                break;
            ncells  = matvar->nbytes / matvar->data_size;
            for ( i = 0; i < ncells; i++ )
                WriteCellArrayField(mat,cells[i],COMPRESSION_ZLIB);
            break;
        }
        case MAT_C_STRUCT:
        {
            char     **fieldnames;
            unsigned char *padzero;
            int        fieldname_size, nfields;
            size_t     maxlen = 0;
            matvar_t **fields = matvar->data;
            unsigned   fieldname;

            /* Check for a structure with no fields */
            if ( matvar->nbytes == 0 || matvar->data_size == 0 ||
                 matvar->data   == NULL ) {
                fieldname_size = 1;
#if 0
                fwrite(&fieldname_type,2,1,mat->fp);
                fwrite(&fieldname_data_size,2,1,mat->fp);
#else
                fieldname = (fieldname_data_size<<16) | fieldname_type;
                fwrite(&fieldname,4,1,mat->fp);
#endif
                fwrite(&fieldname_size,4,1,mat->fp);
                fwrite(&array_name_type,2,1,mat->fp);
                fwrite(&pad1,1,1,mat->fp);
                fwrite(&pad1,1,1,mat->fp);
                nBytes = 0;
                fwrite(&nBytes,4,1,mat->fp);
                break;
            }
            nfields = matvar->nbytes / (nmemb*matvar->data_size);
            fieldnames = malloc(nfields*sizeof(char *));
            for ( i = 0; i < nfields; i++ ) {
                fieldnames[i] = fields[i]->name;
                if ( strlen(fieldnames[i]) > maxlen )
                    maxlen = strlen(fieldnames[i]);
            }
            maxlen++;
            fieldname_size = maxlen;
            while ( nfields*fieldname_size % 8 != 0 )
                fieldname_size++;
            uncomp_buf[0] = (fieldname_type << 16) | fieldname_data_size;
            uncomp_buf[1] = fieldname_size;
            uncomp_buf[2] = (array_name_type << 16) | 0x0000;
            uncomp_buf[3] = nfields*fieldname_size;

            padzero = calloc(fieldname_size,1);
            z->next_out  = comp_buf;
            z->next_in   = uncomp_buf;
            z->avail_out = buf_size*sizeof(*comp_buf);
            z->avail_in  = 16;
            err = deflate(z,Z_NO_FLUSH);
            byteswritten += fwrite(comp_buf,1,
                    buf_size*sizeof(*comp_buf)-z->avail_out,mat->fp);
            for ( i = 0; i < nfields; i++ ) {
                memset(padzero,'\0',fieldname_size);
                memcpy(padzero,fieldnames[i],strlen(fieldnames[i]));
                z->next_out  = comp_buf;
                z->next_in   = padzero;
                z->avail_out = buf_size*sizeof(*comp_buf);
                z->avail_in  = fieldname_size;
                err = deflate(z,Z_NO_FLUSH);
                byteswritten += fwrite(comp_buf,1,
                        buf_size*sizeof(*comp_buf)-z->avail_out,
                        mat->fp);
            }
            free(fieldnames);
            free(padzero);
            for ( i = 0; i < nmemb*nfields; i++ )
                WriteCompressedStructField(mat,fields[i],z);
            break;
        }
        case MAT_C_SPARSE:
        {
            sparse_t *sparse = matvar->data;

            byteswritten += WriteCompressedData(mat,z,sparse->ir,
                sparse->nir,MAT_T_INT32);
            byteswritten += WriteCompressedData(mat,z,sparse->jc,
                sparse->njc,MAT_T_INT32);
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data = sparse->data;
                byteswritten += WriteCompressedData(mat,z,
                    complex_data->Re,sparse->ndata,matvar->data_type);
                byteswritten += WriteCompressedData(mat,z,
                    complex_data->Im,sparse->ndata,matvar->data_type);
            } else {
                byteswritten += WriteCompressedData(mat,z,
                    sparse->data,sparse->ndata,matvar->data_type);
            }
            break;
        }
    }
#if 0
        z->avail_in  = 0;
        z->next_in   = NULL;
        z->next_out  = comp_buf;
        z->avail_out = buf_size*sizeof(*comp_buf);

        err = deflate(z,Z_FINISH);
        byteswritten += fwrite(comp_buf,1,
            buf_size*sizeof(*comp_buf)-z->avail_out,mat->fp);
        while ( err != Z_STREAM_END && !z->avail_out ) {
            z->next_out  = comp_buf;
            z->avail_out = buf_size*sizeof(*comp_buf);

            err = deflate(z,Z_FINISH);
            byteswritten += fwrite(comp_buf,1,
                buf_size*sizeof(*comp_buf)-z->avail_out,mat->fp);
        }
#endif
    return 0;
}
#endif

/*
 * FIXME: Check the tag of the complex variables
 */
void
Read5(mat_t *mat, matvar_t *matvar)
{
    int nBytes, len = 0, i, byteswap, packed_type, data_in_tag = 0;
    long fpos;
    mat_uint32_t tag[2];

    if ( matvar == NULL )
        return;
    else if ( matvar->rank == 0 )        /* An empty data set */
        return;

    fpos = ftell(mat->fp);
    len = 1;
    byteswap = mat->byteswap;
    switch ( matvar->class_type ) {
        case MAT_C_DOUBLE:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->datapos,SEEK_SET);

                matvar->z->avail_in = 0;
                InflateDataType(mat,matvar,tag);
                if ( byteswap )
                    (void)uint32Swap(tag);

                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar,tag+1);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
            }
            if ( nBytes == 0 ) {
                matvar->nbytes = 0;
                break;
            }
            for ( i = 0; i < matvar->rank; i++ )
                len *= matvar->dims[i];
            matvar->data_size = sizeof(double);
            matvar->data_type = MAT_T_DOUBLE;
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadDoubleData(mat,complex_data->Re,
                                 packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);

                    /* Complex Data Tag */
                    fread(tag,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag);
                    packed_type = tag[0] & 0x000000ff;
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadDoubleData(mat,complex_data->Im,packed_type,
                                            len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedDoubleData(mat,matvar->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar,tag);
                    if ( byteswap )
                        (void)uint32Swap(tag);

                    packed_type = tag[0] & 0x000000ff;
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar,tag+1);
                        if ( byteswap )
                            (void)uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedDoubleData(mat,matvar->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
                matvar->data = complex_data;
            } else { /* if ( isComplex ) */
                matvar->nbytes = len*matvar->data_size;
                matvar->data   = malloc(matvar->nbytes);
                if ( !matvar->data ) {
                    Mat_Critical("Failed to allocate %d bytes",matvar->nbytes);
                    break;
                }
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadDoubleData(mat,(double*)matvar->data,
                                 packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedDoubleData(mat,matvar->z,
                                 (double*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
            }
            break;
        case MAT_C_SINGLE:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->datapos,SEEK_SET);

                matvar->z->avail_in = 0;
                InflateDataType(mat,matvar,tag);
                if ( byteswap )
                    (void)uint32Swap(tag);

                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar,tag+1);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
            }
            if ( nBytes == 0 ) {
                matvar->nbytes = 0;
                break;
            }
            for ( i = 0; i < matvar->rank; i++ )
                len *= matvar->dims[i];
            matvar->data_size = sizeof(float);
            matvar->data_type = MAT_T_SINGLE;
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadSingleData(mat,complex_data->Re,
                                 packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);

                    /* Complex Data Tag */
                    fread(tag,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag);
                    packed_type = tag[0] & 0x000000ff;
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadSingleData(mat,complex_data->Im,
                               packed_type,len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedSingleData(mat,matvar->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar,tag);
                    if ( byteswap )
                        (void)uint32Swap(tag);

                    packed_type = tag[0] & 0x000000ff;
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar,tag+1);
                        if ( byteswap )
                            (void)uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedSingleData(mat,matvar->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
                matvar->data = complex_data;
            } else {
                matvar->nbytes = len*matvar->data_size;
                matvar->data = malloc(matvar->nbytes);
                if ( !matvar->data ) {
                    Mat_Critical("Failed to allocate %d bytes",matvar->nbytes);
                    break;
                }
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadSingleData(mat,(float*)matvar->data,
                                 packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedSingleData(mat,matvar->z,
                                 (float*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
            }
            break;
        case MAT_C_INT32:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->datapos,SEEK_SET);

                matvar->z->avail_in = 0;
                InflateDataType(mat,matvar,tag);
                if ( byteswap )
                    (void)uint32Swap(tag);

                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar,tag+1);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
            }
            if ( nBytes == 0 ) {
                matvar->nbytes = 0;
                break;
            }
            for ( i = 0; i < matvar->rank; i++ )
                len *= matvar->dims[i];
            matvar->data_size = sizeof(mat_int32_t);
            matvar->data_type = MAT_T_INT32;
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadInt32Data(mat,complex_data->Re,
                                 packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);

                    /* Complex Data Tag */
                    fread(tag,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag);
                    packed_type = tag[0] & 0x000000ff;
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadInt32Data(mat,complex_data->Im,
                               packed_type,len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedInt32Data(mat,matvar->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar,tag);
                    if ( byteswap )
                        (void)uint32Swap(tag);

                    packed_type = tag[0] & 0x000000ff;
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar,tag+1);
                        if ( byteswap )
                            (void)uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedInt32Data(mat,matvar->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
                matvar->data = complex_data;
            } else {
                matvar->nbytes = len*matvar->data_size;
                matvar->data   = malloc(matvar->nbytes);
                if ( !matvar->data ) {
                    Mat_Critical("Failed to allocate %d bytes",matvar->nbytes);
                    break;
                }
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadInt32Data(mat,(mat_int32_t*)matvar->data,
                                 packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt32Data(mat,matvar->z,
                                 (mat_int32_t*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
            }
            break;
        case MAT_C_UINT32:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->datapos,SEEK_SET);

                matvar->z->avail_in = 0;
                InflateDataType(mat,matvar,tag);
                if ( byteswap )
                    (void)uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar,tag+1);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
            }
            if ( nBytes == 0 ) {
                matvar->nbytes = 0;
                break;
            }
            for ( i = 0; i < matvar->rank; i++ )
                len *= matvar->dims[i];
            matvar->data_size = sizeof(mat_uint32_t);
            matvar->data_type = MAT_T_UINT32;
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadInt32Data(mat,complex_data->Re,
                                 packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);

                    /* Complex Data Tag */
                    fread(tag,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag);
                    packed_type = tag[0] & 0x000000ff;
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadInt32Data(mat,complex_data->Im,
                               packed_type,len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedInt32Data(mat,matvar->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar,tag);
                    if ( byteswap )
                        (void)uint32Swap(tag);

                    packed_type = tag[0] & 0x000000ff;
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar,tag+1);
                        if ( byteswap )
                            (void)uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedInt32Data(mat,matvar->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
                matvar->data = complex_data;
            } else {
                matvar->nbytes = len*matvar->data_size;
                matvar->data   = malloc(matvar->nbytes);
                if ( !matvar->data ) {
                    Mat_Critical("Failed to allocate %d bytes",matvar->nbytes);
                    break;
                }
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadInt32Data(mat,(mat_int32_t*)matvar->data,
                                 packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt32Data(mat,matvar->z,
                                 (mat_int32_t*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
            }
            break;
        case MAT_C_INT16:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->datapos,SEEK_SET);

                matvar->z->avail_in = 0;
                InflateDataType(mat,matvar,tag);
                if ( byteswap )
                    (void)uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar,tag+1);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
            }
            if ( nBytes == 0 ) {
                matvar->nbytes = 0;
                break;
            }
            for ( i = 0; i < matvar->rank; i++ )
                len *= matvar->dims[i];
            matvar->data_size = sizeof(mat_int16_t);
            matvar->data_type = MAT_T_INT16;
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadInt16Data(mat,complex_data->Re,
                                 packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);

                    /* Complex Data Tag */
                    fread(tag,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag);
                    packed_type = tag[0] & 0x000000ff;
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadInt16Data(mat,complex_data->Im,
                               packed_type,len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedInt16Data(mat,matvar->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar,tag);
                    if ( byteswap )
                        (void)uint32Swap(tag);

                    packed_type = tag[0] & 0x000000ff;
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar,tag+1);
                        if ( byteswap )
                            (void)uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedInt16Data(mat,matvar->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
                matvar->data = complex_data;
            } else {
                matvar->nbytes = len*matvar->data_size;
                matvar->data   = malloc(matvar->nbytes);
                if ( !matvar->data ) {
                    Mat_Critical("Failed to allocate %d bytes",matvar->nbytes);
                    break;
                }
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadInt16Data(mat,(mat_int16_t*)matvar->data,
                                 packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt16Data(mat,matvar->z,
                                 (mat_int16_t*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
            }
            break;
        case MAT_C_UINT16:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->datapos,SEEK_SET);

                matvar->z->avail_in = 0;
                InflateDataType(mat,matvar,tag);
                if ( byteswap )
                    (void)uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar,tag+1);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
            }
            if ( nBytes == 0 ) {
                matvar->nbytes = 0;
                break;
            }
            for ( i = 0; i < matvar->rank; i++ )
                len *= matvar->dims[i];
            matvar->data_size = sizeof(mat_uint16_t);
            matvar->data_type = MAT_T_UINT16;
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadInt16Data(mat,complex_data->Re,
                                 packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);

                    /* Complex Data Tag */
                    fread(tag,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag);
                    packed_type = tag[0] & 0x000000ff;
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadInt16Data(mat,complex_data->Im,
                               packed_type,len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedInt16Data(mat,matvar->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar,tag);
                    if ( byteswap )
                        (void)uint32Swap(tag);

                    packed_type = tag[0] & 0x000000ff;
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar,tag+1);
                        if ( byteswap )
                            (void)uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedInt16Data(mat,matvar->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
                matvar->data = complex_data;
            } else {
                matvar->nbytes = len*matvar->data_size;
                matvar->data   = malloc(matvar->nbytes);
                if ( !matvar->data ) {
                    Mat_Critical("Failed to allocate %d bytes",matvar->nbytes);
                    break;
                }
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadInt16Data(mat,(mat_int16_t*)matvar->data,
                                 packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt16Data(mat,matvar->z,
                                 (mat_int16_t*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
            }
            break;
        case MAT_C_INT8:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->datapos,SEEK_SET);

                matvar->z->avail_in = 0;
                InflateDataType(mat,matvar,tag);
                if ( byteswap )
                    (void)uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar,tag+1);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
            }
            if ( nBytes == 0 ) {
                matvar->nbytes = 0;
                break;
            }
            for ( i = 0; i < matvar->rank; i++ )
                len *= matvar->dims[i];
            matvar->data_size = sizeof(mat_int8_t);
            matvar->data_type = MAT_T_INT8;
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadInt8Data(mat,complex_data->Re,
                                 packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);

                    /* Complex Data Tag */
                    fread(tag,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag);
                    packed_type = tag[0] & 0x000000ff;
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadInt8Data(mat,complex_data->Im,
                               packed_type,len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedInt8Data(mat,matvar->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar,tag);
                    if ( byteswap )
                        (void)uint32Swap(tag);

                    packed_type = tag[0] & 0x000000ff;
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar,tag+1);
                        if ( byteswap )
                            (void)uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedInt8Data(mat,matvar->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
                matvar->data = complex_data;
            } else {
                matvar->nbytes = len*matvar->data_size;
                matvar->data   = malloc(matvar->nbytes);
                if ( !matvar->data ) {
                    Mat_Critical("Failed to allocate %d bytes",matvar->nbytes);
                    break;
                }
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadInt8Data(mat,(mat_int8_t*)matvar->data,
                                 packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt8Data(mat,matvar->z,
                                 (mat_int8_t*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
            }
            break;
        case MAT_C_UINT8:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->datapos,SEEK_SET);

                matvar->z->avail_in = 0;
                InflateDataType(mat,matvar,tag);
                if ( byteswap )
                    (void)uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar,tag+1);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
            }
            if ( nBytes == 0 ) {
                matvar->nbytes = 0;
                break;
            }
            for ( i = 0; i < matvar->rank; i++ )
                len *= matvar->dims[i];
            matvar->data_size = sizeof(mat_uint8_t);
            matvar->data_type = MAT_T_UINT8;
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadInt8Data(mat,complex_data->Re,
                                 packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);

                    /* Complex Data Tag */
                    fread(tag,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag);
                    packed_type = tag[0] & 0x000000ff;
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadInt8Data(mat,complex_data->Im,
                               packed_type,len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedInt8Data(mat,matvar->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar,tag);
                    if ( byteswap )
                        (void)uint32Swap(tag);

                    packed_type = tag[0] & 0x000000ff;
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar,tag+1);
                        if ( byteswap )
                            (void)uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedInt8Data(mat,matvar->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
                matvar->data = complex_data;
            } else {
                matvar->nbytes = len*matvar->data_size;
                matvar->data   = malloc(matvar->nbytes);
                if ( !matvar->data ) {
                    Mat_Critical("Failed to allocate %d bytes",matvar->nbytes);
                    break;
                }
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadInt8Data(mat,(mat_int8_t*)matvar->data,
                                 packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt8Data(mat,matvar->z,
                                 (mat_int8_t*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
            }
            break;
            break;
        case MAT_C_CHAR:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->datapos,SEEK_SET);

                matvar->z->avail_in = 0;
                InflateDataType(mat,matvar,tag);
                if ( byteswap )
                    (void)uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar,tag+1);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)uint32Swap(tag+1);
                    nBytes = tag[1];
                }
            }
            if ( nBytes == 0 ) {
                matvar->nbytes = 0;
                break;
            }
            for ( i = 0; i < matvar->rank; i++ )
                len *= matvar->dims[i];
            matvar->data_size = sizeof(char);
            /* FIXME: */
            matvar->data_type = MAT_T_UINT8;
            matvar->nbytes = len*matvar->data_size;
            matvar->data   = calloc(matvar->nbytes+1,1);
            if ( !matvar->data ) {
                Mat_Critical("Failed to allocate %d bytes",matvar->nbytes);
                break;
            }
            if ( matvar->compression == COMPRESSION_NONE) {
                nBytes = ReadCharData(mat,(char*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                if ( (nBytes % 8) != 0 )
                    fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);
#if defined(HAVE_ZLIB)
            } else if ( matvar->compression == COMPRESSION_ZLIB) {
                nBytes = ReadCompressedCharData(mat,matvar->z,
                             (char*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                if ( (nBytes % 8) != 0 )
                    InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
            }
            break;
        case MAT_C_STRUCT:
        {
            matvar_t **fields;
            int nfields = 0;

            if ( !matvar->nbytes || !matvar->data_size || NULL == matvar->data )
                break;
            nfields = matvar->nbytes / matvar->data_size;
            fields = (matvar_t **)matvar->data;
            for ( i = 0; i < nfields; i++ ) {
                fields[i]->fp = mat;
                Read5(mat,fields[i]);
            }
            /* FIXME: */
            matvar->data_type = MAT_T_STRUCT;
            break;
        }
        case MAT_C_CELL:
        {
            matvar_t **cells;

            if ( !matvar->data ) {
                Mat_Critical("Data is NULL for Cell Array %s",matvar->name);
                break;
            }
            for ( i = 0; i < matvar->rank; i++ )
                len *= matvar->dims[i];
            cells = (matvar_t **)matvar->data;
            for ( i = 0; i < len; i++ ) {
                cells[i]->fp = mat;
                Read5(mat,cells[i]);
            }
            /* FIXME: */
            matvar->data_type = MAT_T_CELL;
            break;
        }
        case MAT_C_SPARSE:
        {
            int N;
            sparse_t *data;

            matvar->data_size = sizeof(sparse_t);
            matvar->data      = malloc(matvar->data_size);
            if ( matvar->data == NULL ) {
                Mat_Critical("ReadData: Allocation of data pointer failed");
                break;
            }
            data = matvar->data;
            data->nzmax  = matvar->nbytes;
            fseek(mat->fp,matvar->datapos,SEEK_SET);
            /*  Read ir    */
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                matvar->z->avail_in = 0;
                InflateDataType(mat,matvar,tag);
                if ( mat->byteswap )
                    (void)uint32Swap(tag);

                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    N = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    (void)ReadCompressedInt32Data(mat,matvar->z,
                             (mat_int32_t*)&N,MAT_T_INT32,1);
                }
#endif
            } else {
                fread(tag,4,1,mat->fp);
                if ( mat->byteswap )
                    (void)uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    N = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(&N,4,1,mat->fp);
                    if ( mat->byteswap )
                        int32Swap(&N);
                }
            }
            data->nir = N / 4;
            data->ir = malloc(data->nir*sizeof(mat_int32_t));
            if ( data->ir != NULL ) {
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadInt32Data(mat,data->ir,packed_type,data->nir);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt32Data(mat,matvar->z,
                                 data->ir,packed_type,data->nir);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
            } else {
                Mat_Critical("ReadData: Allocation of ir pointer failed");
                break;
            }
            /*  Read jc    */
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                matvar->z->avail_in = 0;
                InflateDataType(mat,matvar,tag);
                if ( mat->byteswap )
                    uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    N = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    (void)ReadCompressedInt32Data(mat,matvar->z,
                             (mat_int32_t*)&N,MAT_T_INT32,1);
                }
#endif
            } else {
                fread(tag,4,1,mat->fp);
                if ( mat->byteswap )
                    uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    N = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(&N,4,1,mat->fp);
                    if ( mat->byteswap )
                        int32Swap(&N);
                }
            }
            data->njc = N / 4;
            data->jc = malloc(data->njc*sizeof(mat_int32_t));
            if ( data->jc != NULL ) {
                if ( matvar->compression == COMPRESSION_NONE) {
                    nBytes = ReadInt32Data(mat,data->jc,packed_type,data->njc);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt32Data(mat,matvar->z,
                                 data->jc,packed_type,data->njc);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif
                }
            } else {
                Mat_Critical("ReadData: Allocation of jc pointer failed");
                break;
            }
            /*  Read data    */
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                matvar->z->avail_in = 0;
                InflateDataType(mat,matvar,tag);
                if ( mat->byteswap )
                    uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    N = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    (void)ReadCompressedInt32Data(mat,matvar->z,
                             (mat_int32_t*)&N,MAT_T_INT32,1);
                }
#endif
            } else {
                fread(tag,4,1,mat->fp);
                if ( mat->byteswap )
                    uint32Swap(tag);
                packed_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    N = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(&N,4,1,mat->fp);
                    if ( mat->byteswap )
                        int32Swap(&N);
                }
            }
#if defined(EXTENDED_SPARSE)
            matvar->data_type = packed_type;
#else
            matvar->data_type = MAT_T_DOUBLE;
#endif
            data->ndata = N / Mat_SizeOf(packed_type);
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data;

                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(data->ndata*
                                          Mat_SizeOf(matvar->data_type));
                complex_data->Im = malloc(data->ndata*
                                          Mat_SizeOf(matvar->data_type));
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    if ( matvar->compression == COMPRESSION_NONE) {
#if defined(EXTENDED_SPARSE)
                        switch ( matvar->data_type ) {
                            case MAT_T_DOUBLE:
                                nBytes = ReadDoubleData(mat,complex_data->Re,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_SINGLE:
                                nBytes = ReadSingleData(mat,complex_data->Re,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_INT32:
                                nBytes = ReadInt32Data(mat,complex_data->Re,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_UINT32:
                                nBytes = ReadInt32Data(mat,complex_data->Re,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_INT16:
                                nBytes = ReadInt16Data(mat,complex_data->Re,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_UINT16:
                                nBytes = ReadInt16Data(mat,complex_data->Re,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_INT8:
                                nBytes = ReadInt8Data(mat,complex_data->Re,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_UINT8:
                                nBytes = ReadInt8Data(mat,complex_data->Re,
                                    packed_type,data->ndata);
                                break;
                        }
#else
                        nBytes = ReadDoubleData(mat,complex_data->Re,
                                     packed_type,data->ndata);
#endif
                        if ( data_in_tag )
                            nBytes+=4;
                        if ( (nBytes % 8) != 0 )
                            fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);

                        /* Complex Data Tag */
                        fread(tag,4,1,mat->fp);
                        if ( byteswap )
                            (void)uint32Swap(tag);
                        packed_type = tag[0] & 0x000000ff;
                        if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                            data_in_tag = 1;
                            nBytes = (tag[0] & 0xffff0000) >> 16;
                        } else {
                            data_in_tag = 0;
                            fread(tag+1,4,1,mat->fp);
                            if ( byteswap )
                                (void)uint32Swap(tag+1);
                            nBytes = tag[1];
                        }
#if defined(EXTENDED_SPARSE)
                        switch ( matvar->data_type ) {
                            case MAT_T_DOUBLE:
                                nBytes = ReadDoubleData(mat,complex_data->Im,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_SINGLE:
                                nBytes = ReadSingleData(mat,complex_data->Im,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_INT32:
                                nBytes = ReadInt32Data(mat,complex_data->Im,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_UINT32:
                                nBytes = ReadUInt32Data(mat,complex_data->Im,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_INT16:
                                nBytes = ReadInt16Data(mat,complex_data->Im,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_UINT16:
                                nBytes = ReadUInt16Data(mat,complex_data->Im,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_INT8:
                                nBytes = ReadInt8Data(mat,complex_data->Im,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_UINT8:
                                nBytes = ReadUInt8Data(mat,complex_data->Im,
                                    packed_type,data->ndata);
                                break;
                        }
#else /* EXTENDED_SPARSE */
                        nBytes = ReadDoubleData(mat,complex_data->Im,
                                     packed_type,data->ndata);
#endif /* EXTENDED_SPARSE */
                        if ( data_in_tag )
                            nBytes+=4;
                        if ( (nBytes % 8) != 0 )
                            fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);
#if defined(HAVE_ZLIB)
                    } else if ( matvar->compression == COMPRESSION_ZLIB ) {
#if defined(EXTENDED_SPARSE)
                        switch ( matvar->data_type ) {
                            case MAT_T_DOUBLE:
                                nBytes = ReadCompressedDoubleData(mat,matvar->z,
                                     complex_data->Re,packed_type,data->ndata);
                                break;
                            case MAT_T_SINGLE:
                                nBytes = ReadCompressedSingleData(mat,matvar->z,
                                     complex_data->Re,packed_type,data->ndata);
                                break;
                            case MAT_T_INT32:
                                nBytes = ReadCompressedInt32Data(mat,matvar->z,
                                     complex_data->Re,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT32:
                                nBytes = ReadCompressedInt32Data(mat,matvar->z,
                                     complex_data->Re,packed_type,data->ndata);
                                break;
                            case MAT_T_INT16:
                                nBytes = ReadCompressedInt16Data(mat,matvar->z,
                                     complex_data->Re,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT16:
                                nBytes = ReadCompressedInt16Data(mat,matvar->z,
                                     complex_data->Re,packed_type,data->ndata);
                                break;
                            case MAT_T_INT8:
                                nBytes = ReadCompressedInt8Data(mat,matvar->z,
                                     complex_data->Re,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT8:
                                nBytes = ReadCompressedInt8Data(mat,matvar->z,
                                     complex_data->Re,packed_type,data->ndata);
                                break;
                        }
#else    /* EXTENDED_SPARSE */
                        nBytes = ReadCompressedDoubleData(mat,matvar->z,
                                     complex_data->Re,packed_type,data->ndata);
#endif    /* EXTENDED_SPARSE */
                        if ( data_in_tag )
                            nBytes+=4;
                        if ( (nBytes % 8) != 0 )
                            InflateSkip(mat,matvar->z,8-(nBytes % 8));

                        /* Complex Data Tag */
                        InflateDataType(mat,matvar,tag);
                        if ( byteswap )
                            (void)uint32Swap(tag);

                        packed_type = tag[0] & 0x000000ff;
                        if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                            data_in_tag = 1;
                            nBytes = (tag[0] & 0xffff0000) >> 16;
                        } else {
                            data_in_tag = 0;
                            InflateDataType(mat,matvar,tag+1);
                            if ( byteswap )
                                (void)uint32Swap(tag+1);
                            nBytes = tag[1];
                        }
#if defined(EXTENDED_SPARSE)
                        switch ( matvar->data_type ) {
                            case MAT_T_DOUBLE:
                                nBytes = ReadCompressedDoubleData(mat,matvar->z,
                                     complex_data->Im,packed_type,data->ndata);
                                break;
                            case MAT_T_SINGLE:
                                nBytes = ReadCompressedSingleData(mat,matvar->z,
                                     complex_data->Im,packed_type,data->ndata);
                                break;
                            case MAT_T_INT32:
                                nBytes = ReadCompressedInt32Data(mat,matvar->z,
                                     complex_data->Im,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT32:
                                nBytes = ReadCompressedUInt32Data(mat,matvar->z,
                                     complex_data->Im,packed_type,data->ndata);
                                break;
                            case MAT_T_INT16:
                                nBytes = ReadCompressedInt16Data(mat,matvar->z,
                                     complex_data->Im,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT16:
                                nBytes = ReadCompressedUInt16Data(mat,matvar->z,
                                     complex_data->Im,packed_type,data->ndata);
                                break;
                            case MAT_T_INT8:
                                nBytes = ReadCompressedInt8Data(mat,matvar->z,
                                     complex_data->Im,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT8:
                                nBytes = ReadCompressedUInt8Data(mat,matvar->z,
                                     complex_data->Im,packed_type,data->ndata);
                                break;
                        }
#else    /* EXTENDED_SPARSE */
                        nBytes = ReadCompressedDoubleData(mat,matvar->z,
                                     complex_data->Im,packed_type,data->ndata);
#endif    /* EXTENDED_SPARSE */
                        if ( data_in_tag )
                            nBytes+=4;
                        if ( (nBytes % 8) != 0 )
                            InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif    /* HAVE_ZLIB */
                    }
                } else {
                    Mat_Critical("ReadData: Allocation of data pointer failed");
                    break;
                }
                data->data = complex_data;
            } else { /* isComplex */
                data->data = malloc(data->ndata*Mat_SizeOf(MAT_T_DOUBLE));
                if ( data->data != NULL ) {
                    if ( matvar->compression == COMPRESSION_NONE) {
#if defined(EXTENDED_SPARSE)
                        switch ( matvar->data_type ) {
                            case MAT_T_DOUBLE:
                                nBytes = ReadDoubleData(mat,data->data,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_SINGLE:
                                nBytes = ReadSingleData(mat,data->data,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_INT32:
                                nBytes = ReadInt32Data(mat,data->data,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_UINT32:
                                nBytes = ReadInt32Data(mat,data->data,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_INT16:
                                nBytes = ReadInt16Data(mat,data->data,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_UINT16:
                                nBytes = ReadInt16Data(mat,data->data,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_INT8:
                                nBytes = ReadInt8Data(mat,data->data,
                                    packed_type,data->ndata);
                                break;
                            case MAT_T_UINT8:
                                nBytes = ReadInt8Data(mat,data->data,
                                    packed_type,data->ndata);
                                break;
                        }
#else
                        nBytes = ReadDoubleData(mat,data->data,packed_type,
                                     data->ndata);
#endif
                        if ( data_in_tag )
                            nBytes+=4;
                        if ( (nBytes % 8) != 0 )
                            fseek(mat->fp,8-(nBytes % 8),SEEK_CUR);
#if defined(HAVE_ZLIB)
                    } else if ( matvar->compression == COMPRESSION_ZLIB) {
#if defined(EXTENDED_SPARSE)
                        switch ( matvar->data_type ) {
                            case MAT_T_DOUBLE:
                                nBytes = ReadCompressedDoubleData(mat,matvar->z,
                                     data->data,packed_type,data->ndata);
                                break;
                            case MAT_T_SINGLE:
                                nBytes = ReadCompressedSingleData(mat,matvar->z,
                                     data->data,packed_type,data->ndata);
                                break;
                            case MAT_T_INT32:
                                nBytes = ReadCompressedInt32Data(mat,matvar->z,
                                     data->data,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT32:
                                nBytes = ReadCompressedInt32Data(mat,matvar->z,
                                     data->data,packed_type,data->ndata);
                                break;
                            case MAT_T_INT16:
                                nBytes = ReadCompressedInt16Data(mat,matvar->z,
                                     data->data,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT16:
                                nBytes = ReadCompressedInt16Data(mat,matvar->z,
                                     data->data,packed_type,data->ndata);
                                break;
                            case MAT_T_INT8:
                                nBytes = ReadCompressedInt8Data(mat,matvar->z,
                                     data->data,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT8:
                                nBytes = ReadCompressedInt8Data(mat,matvar->z,
                                     data->data,packed_type,data->ndata);
                                break;
                        }
#else   /* EXTENDED_SPARSE */
                        nBytes = ReadCompressedDoubleData(mat,matvar->z,
                                     data->data,packed_type,data->ndata);
#endif   /* EXTENDED_SPARSE */
                        if ( data_in_tag )
                            nBytes+=4;
                        if ( (nBytes % 8) != 0 )
                            InflateSkip(mat,matvar->z,8-(nBytes % 8));
#endif   /* HAVE_ZLIB */
                    }
                } else {
                    Mat_Critical("ReadData: Allocation of data pointer failed");
                    break;
                }
            }
            break;
        }
        case MAT_C_FUNCTION:
        {
            matvar_t **functions;
            int nfunctions = 0;

            if ( !matvar->nbytes || !matvar->data_size )
                break;
            nfunctions = matvar->nbytes / matvar->data_size;
            functions = (matvar_t **)matvar->data;
            for ( i = 0; i < nfunctions; i++ ) {
                functions[i]->fp = mat;
                Read5(mat,functions[i]);
            }
            /* FIXME: */
            matvar->data_type = MAT_T_FUNCTION;
            break;
        }
        default:
            Mat_Critical("Read5: %d is not a supported Class", matvar->class_type);
    }
    fseek(mat->fp,fpos,SEEK_SET);

    return;
}

/* Reads a slab of data from the variable */
int 
ReadData5(mat_t *mat,matvar_t *matvar,void *data, 
    int *start,int *stride,int *edge)
{               
    int err = 0,real_bytes;
    mat_int32_t tag[2];
            
    fseek(mat->fp,matvar->datapos,SEEK_SET);
    if ( matvar->compression == COMPRESSION_NONE ) {
        fread(tag,4,2,mat->fp);
        if ( mat->byteswap ) {
            int32Swap(tag);
            int32Swap(tag+1);
        }
        matvar->data_type = tag[0] & 0x000000ff;
        if ( tag[0] & 0xffff0000 ) { /* Data is packed in the tag */
            fseek(mat->fp,-4,SEEK_CUR);
            real_bytes = 4+(tag[0] >> 16);
        } else {
            real_bytes = 8+tag[1];
        }
#if defined(HAVE_ZLIB)
    } else if ( matvar->compression == COMPRESSION_ZLIB ) {
        matvar->z->avail_in = 0;
        InflateDataType(mat,matvar,tag);
        if ( mat->byteswap ) {
            int32Swap(tag);
            int32Swap(tag+1);
        }
        matvar->data_type = tag[0] & 0x000000ff;
        if ( !(tag[0] & 0xffff0000) ) {/* Data is NOT packed in the tag */
            InflateSkip(mat,matvar->z,4);
            real_bytes = 8+tag[1];
        } else {
            real_bytes = 4+(tag[0] >> 16);
        }
#endif
    }
    if ( real_bytes % 8 )
        real_bytes += (8-(real_bytes % 8));

    if ( matvar->rank == 2 ) {
        if ( stride[0]*(edge[0]-1)+start[0]+1 > matvar->dims[0] )
            err = 1;
        else if ( stride[1]*(edge[1]-1)+start[1]+1 > matvar->dims[1] )
            err = 1;
        else if ( matvar->compression == COMPRESSION_NONE ) {
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data = data;

                ReadDataSlab2(mat,complex_data->Re,matvar->class_type,
                    matvar->data_type,matvar->dims,start,stride,edge);
                fseek(mat->fp,matvar->datapos+real_bytes,SEEK_SET);
                fread(tag,4,2,mat->fp);
                if ( mat->byteswap ) {
                    int32Swap(tag);
                    int32Swap(tag+1);
                }
                matvar->data_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is packed in the tag */
                    fseek(mat->fp,-4,SEEK_CUR);
                }
                ReadDataSlab2(mat,complex_data->Im,matvar->class_type,
                              matvar->data_type,matvar->dims,start,stride,edge);
            } else {
                ReadDataSlab2(mat,data,matvar->class_type,
                    matvar->data_type,matvar->dims,start,stride,edge);
            }
        }
#if defined(HAVE_ZLIB)
        else if ( matvar->compression == COMPRESSION_ZLIB ) {
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data = data;

                ReadCompressedDataSlab2(mat,matvar->z,complex_data->Re,
                    matvar->class_type,matvar->data_type,matvar->dims,
                    start,stride,edge);
                fseek(mat->fp,matvar->datapos+real_bytes,SEEK_SET);
                matvar->z->avail_in = 0;
                InflateDataType(mat,matvar,tag);
                if ( mat->byteswap ) {
                    int32Swap(tag);
                    int32Swap(tag+1);
                }
                matvar->data_type = tag[0] & 0x000000ff;
                if ( !(tag[0] & 0xffff0000) ) {/* Data is NOT packed in the tag */
                    InflateSkip(mat,matvar->z,4);
                }
                ReadCompressedDataSlab2(mat,matvar->z,complex_data->Im,
                    matvar->class_type,matvar->data_type,matvar->dims,
                    start,stride,edge);
            } else {
                ReadCompressedDataSlab2(mat,matvar->z,data,matvar->class_type,
                    matvar->data_type,matvar->dims,start,stride,edge);
            }
        }
#endif
    } else {
/* FIXME: ComplexSplit */
        if ( matvar->compression == COMPRESSION_NONE ) {
            ReadDataSlabN(mat,data,matvar->class_type,matvar->data_type,
                matvar->rank,matvar->dims,start,stride,edge);
            if ( matvar->isComplex ) {
                int i;
                size_t N = Mat_SizeOfClass(matvar->class_type);
                for ( i = 0; i < matvar->rank; i++ )
                    N *= edge[i];
                fseek(mat->fp,matvar->datapos+real_bytes,SEEK_SET);
                fread(tag,4,2,mat->fp);
                if ( mat->byteswap ) {
                    int32Swap(tag);
                    int32Swap(tag+1);
                }
                matvar->data_type = tag[0] & 0x000000ff;
                if ( tag[0] & 0xffff0000 ) { /* Data is packed in the tag */
                    fseek(mat->fp,-4,SEEK_CUR);
                }
                ReadDataSlab2(mat,(unsigned char *)data+N,matvar->class_type,
                              matvar->data_type,matvar->dims,start,stride,edge);
            }
        }
#if defined(HAVE_ZLIB)
        else if ( matvar->compression == COMPRESSION_ZLIB ) {
            ReadCompressedDataSlabN(mat,matvar->z,data,matvar->class_type,
                matvar->data_type,matvar->rank,matvar->dims,start,stride,edge);
            if ( matvar->isComplex ) {
                int i;
                size_t N = Mat_SizeOfClass(matvar->class_type);
                for ( i = 0; i < matvar->rank; i++ )
                    N *= edge[i];
                fseek(mat->fp,matvar->datapos+real_bytes,SEEK_SET);
                matvar->z->avail_in = 0;
                InflateDataType(mat,matvar,tag);
                if ( mat->byteswap ) {
                    int32Swap(tag);
                    int32Swap(tag+1);
                }
                matvar->data_type = tag[0] & 0x000000ff;
                if ( !(tag[0] & 0xffff0000) ) {/* Data is NOT packed in the tag */
                    InflateSkip(mat,matvar->z,4);
                }
            }
        }
#endif
    }
    if ( err )
        return err;

    switch(matvar->class_type) {
        case MAT_C_DOUBLE:
            matvar->data_type = MAT_T_DOUBLE;
            matvar->data_size = sizeof(double);
            break;
        case MAT_C_SINGLE:
            matvar->data_type = MAT_T_SINGLE;
            matvar->data_size = sizeof(float);
            break;
        case MAT_C_INT32:
            matvar->data_type = MAT_T_INT32;
            matvar->data_size = sizeof(mat_int32_t);
            break;
        case MAT_C_UINT32:
            matvar->data_type = MAT_T_UINT32;
            matvar->data_size = sizeof(mat_uint32_t);
            break;
        case MAT_C_INT16:
            matvar->data_type = MAT_T_INT16;
            matvar->data_size = sizeof(mat_int16_t);
            break;
        case MAT_C_UINT16:
            matvar->data_type = MAT_T_UINT16;
            matvar->data_size = sizeof(mat_uint16_t);
            break;
        case MAT_C_INT8:
            matvar->data_type = MAT_T_INT8;
            matvar->data_size = sizeof(mat_int8_t);
            break;
        case MAT_C_UINT8:
            matvar->data_type = MAT_T_UINT8;
            matvar->data_size = sizeof(mat_uint8_t);
            break;
    }

    return err;
}

/* Writes a matlab variable to a version 5 matlab file */
int
Write5(mat_t *mat,matvar_t *matvar,int compress)
{
    mat_uint32_t array_flags = 0x0;
    mat_int16_t  fieldname_type = MAT_T_INT32,fieldname_data_size=4;
    mat_int8_t  pad1 = 0;
    int      array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int      array_flags_size = 8, pad4 = 0, matrix_type = MAT_T_MATRIX;
    int      nBytes, i, nmemb = 1,nzmax = 0;
    long     start = 0, end = 0;

    /* FIXME: SEEK_END is not Guaranteed by the C standard */
    fseek(mat->fp,0,SEEK_END);         /* Always write at end of file */


    if ( compress == COMPRESSION_NONE ) {
        fwrite(&matrix_type,4,1,mat->fp);
        fwrite(&pad4,4,1,mat->fp);
        start = ftell(mat->fp);

        /* Array Flags */

        array_flags = matvar->class_type & MAT_F_CLASS_T;
        if ( matvar->isComplex )
            array_flags |= MAT_F_COMPLEX;
        if ( matvar->isGlobal )
            array_flags |= MAT_F_GLOBAL;
        if ( matvar->isLogical )
            array_flags |= MAT_F_LOGICAL;
        if ( matvar->class_type == MAT_C_SPARSE )
            nzmax = ((sparse_t *)matvar->data)->nzmax;

        fwrite(&array_flags_type,4,1,mat->fp);
        fwrite(&array_flags_size,4,1,mat->fp);
        fwrite(&array_flags,4,1,mat->fp);
        fwrite(&nzmax,4,1,mat->fp);
        /* Rank and Dimension */
        nBytes = matvar->rank * 4;
        fwrite(&dims_array_type,4,1,mat->fp);
        fwrite(&nBytes,4,1,mat->fp);
        for ( i = 0; i < matvar->rank; i++ ) {
            mat_int32_t dim;
            dim = matvar->dims[i];
            nmemb *= dim;
            fwrite(&dim,4,1,mat->fp);
        }
        if ( matvar->rank % 2 != 0 )
            fwrite(&pad4,4,1,mat->fp);
        /* Name of variable */
        if ( strlen(matvar->name) <= 4 ) {
            mat_int16_t  array_name_type = MAT_T_INT8;
            mat_int16_t array_name_len = (mat_int16_t)strlen(matvar->name);
            mat_int8_t  pad1 = 0;
            fwrite(&array_name_type,2,1,mat->fp);
            fwrite(&array_name_len,2,1,mat->fp);
            fwrite(matvar->name,1,array_name_len,mat->fp);
            for ( i = array_name_len; i < 4; i++ )
                fwrite(&pad1,1,1,mat->fp);
        } else {
            mat_int32_t array_name_type = MAT_T_INT8;
            mat_int32_t array_name_len  = (mat_int32_t)strlen(matvar->name);
            mat_int8_t  pad1 = 0;

            fwrite(&array_name_type,4,1,mat->fp);
            fwrite(&array_name_len,4,1,mat->fp);
            fwrite(matvar->name,1,array_name_len,mat->fp);
            if ( array_name_len % 8 )
                for ( i = array_name_len % 8; i < 8; i++ )
                    fwrite(&pad1,1,1,mat->fp);
        }

        matvar->datapos = ftell(mat->fp);
        switch ( matvar->class_type ) {
            case MAT_C_DOUBLE:
            case MAT_C_SINGLE:
            case MAT_C_INT32:
            case MAT_C_UINT32:
            case MAT_C_INT16:
            case MAT_C_UINT16:
            case MAT_C_INT8:
            case MAT_C_UINT8:
            {
                if ( matvar->isComplex ) {
                    struct ComplexSplit *complex_data = matvar->data;
                    nBytes = WriteData(mat,complex_data->Re,nmemb,
                        matvar->data_type);
                    if ( nBytes % 8 )
                        for ( i = nBytes % 8; i < 8; i++ )
                            fwrite(&pad1,1,1,mat->fp);
                    nBytes = WriteData(mat,complex_data->Im,nmemb,
                        matvar->data_type);
                    if ( nBytes % 8 )
                        for ( i = nBytes % 8; i < 8; i++ )
                            fwrite(&pad1,1,1,mat->fp);
                } else {
                    nBytes=WriteData(mat,matvar->data,nmemb,matvar->data_type);
                    if ( nBytes % 8 )
                        for ( i = nBytes % 8; i < 8; i++ )
                            fwrite(&pad1,1,1,mat->fp);
                }
                break;
            }
            case MAT_C_CHAR:
            {
                /* Check for a NULL character array */
                if ( matvar->data != NULL && nmemb > 0 )
                    WriteCharData(mat,matvar->data,nmemb,matvar->data_type);
                break;
            }
            case MAT_C_CELL:
            {
                int        ncells;
                matvar_t **cells = (matvar_t **)matvar->data;

                /* Check for an empty cell array */
                if ( matvar->nbytes == 0 || matvar->data_size == 0 ||
                     matvar->data   == NULL )
                    break;
                ncells  = matvar->nbytes / matvar->data_size;
                for ( i = 0; i < ncells; i++ )
                    WriteCellArrayField(mat,cells[i],compress);
                break;
            }
            case MAT_C_STRUCT:
            {
                char     **fieldnames, *padzero;
                int        fieldname_size, nfields;
                size_t     maxlen = 0;
                matvar_t **fields = (matvar_t **)matvar->data;
                mat_int32_t array_name_type = MAT_T_INT8;
                unsigned   fieldname;

                /* Check for a structure with no fields */
                if ( matvar->nbytes == 0 || matvar->data_size == 0 ||
                     matvar->data   == NULL ) {
#if 0
                    fwrite(&fieldname_type,2,1,mat->fp);
                    fwrite(&fieldname_data_size,2,1,mat->fp);
#else
                    fieldname = (fieldname_data_size<<16) | fieldname_type;
                    fwrite(&fieldname,4,1,mat->fp);
#endif
                    fieldname_size = 1;
                    fwrite(&fieldname_size,4,1,mat->fp);
                    fwrite(&array_name_type,4,1,mat->fp);
                    nBytes = 0;
                    fwrite(&nBytes,4,1,mat->fp);
                    break;
                }
                nfields = matvar->nbytes / (nmemb*matvar->data_size);
                fieldnames = malloc(nfields*sizeof(char *));
                for ( i = 0; i < nfields; i++ ) {
                    fieldnames[i] = fields[i]->name;
                    if ( strlen(fieldnames[i]) > maxlen )
                        maxlen = strlen(fieldnames[i]);
                }
                maxlen++;
                fieldname_size = maxlen;
                while ( nfields*fieldname_size % 8 != 0 )
                    fieldname_size++;
#if 0
                fwrite(&fieldname_type,2,1,mat->fp);
                fwrite(&fieldname_data_size,2,1,mat->fp);
#else
                fieldname = (fieldname_data_size<<16) | fieldname_type;
                fwrite(&fieldname,4,1,mat->fp);
#endif
                fwrite(&fieldname_size,4,1,mat->fp);
                fwrite(&array_name_type,4,1,mat->fp);
                nBytes = nfields*fieldname_size;
                fwrite(&nBytes,4,1,mat->fp);
                padzero = calloc(fieldname_size,1);
                for ( i = 0; i < nfields; i++ ) {
                    fwrite(fieldnames[i],1,strlen(fieldnames[i]),mat->fp);
                    fwrite(padzero,1,fieldname_size-strlen(fieldnames[i]),mat->fp);
                }
                free(fieldnames);
                free(padzero);
                for ( i = 0; i < nmemb*nfields; i++ )
                    WriteStructField(mat,fields[i]);
                break;
            }
            case MAT_C_SPARSE:
            {
                sparse_t *sparse = matvar->data;

                nBytes = WriteData(mat,sparse->ir,sparse->nir,MAT_T_INT32);
                if ( nBytes % 8 )
                    for ( i = nBytes % 8; i < 8; i++ )
                        fwrite(&pad1,1,1,mat->fp);
                nBytes = WriteData(mat,sparse->jc,sparse->njc,MAT_T_INT32);
                if ( nBytes % 8 )
                    for ( i = nBytes % 8; i < 8; i++ )
                        fwrite(&pad1,1,1,mat->fp);
                if ( matvar->isComplex ) {
                    struct ComplexSplit *complex_data = sparse->data;
                    nBytes = WriteData(mat,complex_data->Re,sparse->ndata,
                        matvar->data_type);
                    if ( nBytes % 8 )
                        for ( i = nBytes % 8; i < 8; i++ )
                            fwrite(&pad1,1,1,mat->fp);
                    nBytes = WriteData(mat,complex_data->Im,sparse->ndata,
                        matvar->data_type);
                    if ( nBytes % 8 )
                        for ( i = nBytes % 8; i < 8; i++ )
                            fwrite(&pad1,1,1,mat->fp);
                } else {
                    nBytes = WriteData(mat,sparse->data,sparse->ndata,matvar->data_type);
                    if ( nBytes % 8 )
                        for ( i = nBytes % 8; i < 8; i++ )
                            fwrite(&pad1,1,1,mat->fp);
                }
            }
        }
#if defined(HAVE_ZLIB)
    } else if ( compress == COMPRESSION_ZLIB ) {
        mat_uint32_t comp_buf[512];
        mat_uint32_t uncomp_buf[512] = {0,};
        int buf_size = 512, err;
        size_t byteswritten = 0;

        matvar->z         = malloc(sizeof(*matvar->z));
        matvar->z->zalloc = Z_NULL;
        matvar->z->zfree  = Z_NULL;
        err = deflateInit(matvar->z,Z_DEFAULT_COMPRESSION);

        matrix_type = MAT_T_COMPRESSED;
        fwrite(&matrix_type,4,1,mat->fp);
        fwrite(&pad4,4,1,mat->fp);
        start = ftell(mat->fp);

        /* Array Flags */
        array_flags = matvar->class_type & MAT_F_CLASS_T;
        if ( matvar->isComplex )
            array_flags |= MAT_F_COMPLEX;
        if ( matvar->isGlobal )
            array_flags |= MAT_F_GLOBAL;
        if ( matvar->isLogical )
            array_flags |= MAT_F_LOGICAL;
        if ( matvar->class_type == MAT_C_SPARSE )
            nzmax = ((sparse_t *)matvar->data)->nzmax;

        uncomp_buf[0] = MAT_T_MATRIX;
        uncomp_buf[1] = (int)GetMatrixMaxBufSize(matvar);
        matvar->z->next_out  = comp_buf;
        matvar->z->next_in   = uncomp_buf;
        matvar->z->avail_out = buf_size*sizeof(*comp_buf);
        matvar->z->avail_in  = 8;
        err = deflate(matvar->z,Z_NO_FLUSH);
        byteswritten += fwrite(comp_buf,1,
            buf_size*sizeof(*comp_buf)-matvar->z->avail_out,mat->fp);
        uncomp_buf[0] = array_flags_type;
        uncomp_buf[1] = array_flags_size;
        uncomp_buf[2] = array_flags;
        uncomp_buf[3] = nzmax;
        /* Rank and Dimension */
        nBytes = matvar->rank * 4;
        uncomp_buf[4] = dims_array_type;
        uncomp_buf[5] = nBytes;
        for ( i = 0; i < matvar->rank; i++ ) {
            mat_int32_t dim;
            dim = matvar->dims[i];
            nmemb *= dim;
            uncomp_buf[6+i] = dim;
        }
        if ( matvar->rank % 2 != 0 ) {
            uncomp_buf[6+i] = pad4;
            i++;
        }

        matvar->z->next_out  = comp_buf;
        matvar->z->next_in   = uncomp_buf;
        matvar->z->avail_out = buf_size*sizeof(*comp_buf);
        matvar->z->avail_in  = (6+i)*sizeof(*uncomp_buf);
        err = deflate(matvar->z,Z_NO_FLUSH);
        byteswritten += fwrite(comp_buf,1,
                buf_size*sizeof(*comp_buf)-matvar->z->avail_out,mat->fp);
        /* Name of variable */
        if ( strlen(matvar->name) <= 4 ) {
            mat_int16_t array_name_len = (mat_int16_t)strlen(matvar->name);
            mat_int16_t array_name_type = MAT_T_INT8;

            memset(uncomp_buf,0,8);
            uncomp_buf[0] = (array_name_len << 16) | array_name_type;
            memcpy(uncomp_buf+1,matvar->name,array_name_len);
            if ( array_name_len % 4 )
                array_name_len += 4-(array_name_len % 4);

            matvar->z->next_out  = comp_buf;
            matvar->z->next_in   = uncomp_buf;
            matvar->z->avail_out = buf_size*sizeof(*comp_buf);
            matvar->z->avail_in  = 8;
            err = deflate(matvar->z,Z_NO_FLUSH);
            byteswritten += fwrite(comp_buf,1,
                    buf_size*sizeof(*comp_buf)-matvar->z->avail_out,mat->fp);
        } else {
            mat_int32_t array_name_len = (mat_int32_t)strlen(matvar->name);
            mat_int32_t array_name_type = MAT_T_INT8;

            memset(uncomp_buf,0,buf_size*sizeof(*uncomp_buf));
            uncomp_buf[0] = array_name_type;
            uncomp_buf[1] = array_name_len;
            memcpy(uncomp_buf+2,matvar->name,array_name_len);
            if ( array_name_len % 8 )
                array_name_len += 8-(array_name_len % 8);
            matvar->z->next_out  = comp_buf;
            matvar->z->next_in   = uncomp_buf;
            matvar->z->avail_out = buf_size*sizeof(*comp_buf);
            matvar->z->avail_in  = 8+array_name_len;
            err = deflate(matvar->z,Z_NO_FLUSH);
            byteswritten += fwrite(comp_buf,1,
                    buf_size*sizeof(*comp_buf)-matvar->z->avail_out,mat->fp);
        }
        matvar->datapos = ftell(mat->fp);
        switch ( matvar->class_type ) {
            case MAT_C_DOUBLE:
            case MAT_C_SINGLE:
            case MAT_C_INT32:
            case MAT_C_UINT32:
            case MAT_C_INT16:
            case MAT_C_UINT16:
            case MAT_C_INT8:
            case MAT_C_UINT8:
            {
                /* WriteCompressedData makes sure uncomressed data is aligned
                 * on an 8-byte boundary */
                if ( matvar->isComplex ) {
                    struct ComplexSplit *complex_data = matvar->data;

                    byteswritten += WriteCompressedData(mat,matvar->z,
                        complex_data->Re,nmemb,matvar->data_type);
                    byteswritten += WriteCompressedData(mat,matvar->z,
                        complex_data->Im,nmemb,matvar->data_type);
                } else {
                    byteswritten += WriteCompressedData(mat,matvar->z,
                        matvar->data,nmemb,matvar->data_type);
                }
                break;
            }
#if 0
            case MAT_C_CELL:
            {
                int        ncells;
                matvar_t **cells = (matvar_t **)matvar->data;

                /* Check for an empty cell array */
                if ( matvar->nbytes == 0 || matvar->data_size == 0 ||
                     matvar->data   == NULL )
                    break;
                ncells  = matvar->nbytes / matvar->data_size;
                for ( i = 0; i < ncells; i++ )
                    WriteCellArrayField(mat,cells[i],compress);
                break;
            }
            case MAT_C_STRUCT:
            {
                char     **fieldnames;
                unsigned char *padzero;
                int        fieldname_size, nfields;
                size_t     maxlen = 0;
                matvar_t **fields = (matvar_t **)matvar->data;

                /* Check for a structure with no fields */
                if ( matvar->nbytes == 0 || matvar->data_size == 0 ||
                     matvar->data   == NULL ) {
                    fieldname_size = 1;
                    fwrite(&fieldname_type,2,1,mat->fp);
                    fwrite(&fieldname_data_size,2,1,mat->fp);
                    fwrite(&fieldname_size,4,1,mat->fp);
                    fwrite(&array_name_type,2,1,mat->fp);
                    fwrite(&pad1,1,1,mat->fp);
                    fwrite(&pad1,1,1,mat->fp);
                    nBytes = 0;
                    fwrite(&nBytes,4,1,mat->fp);
                    break;
                }
                nfields = matvar->nbytes / (nmemb*matvar->data_size);
                fieldnames = malloc(nfields*sizeof(char *));
                for ( i = 0; i < nfields; i++ ) {
                    fieldnames[i] = fields[i]->name;
                    if ( strlen(fieldnames[i]) > maxlen )
                        maxlen = strlen(fieldnames[i]);
                }
                maxlen++;
                fieldname_size = maxlen;
                while ( nfields*fieldname_size % 8 != 0 )
                    fieldname_size++;
                uncomp_buf[0] = (fieldname_type << 16) | fieldname_data_size;
                uncomp_buf[1] = fieldname_size;
                uncomp_buf[2] = (array_name_type << 16);
                uncomp_buf[3] = nfields*fieldname_size;

                padzero = calloc(fieldname_size,1);
                matvar->z->next_out  = comp_buf;
                matvar->z->next_in   = uncomp_buf;
                matvar->z->avail_out = buf_size*sizeof(*comp_buf);
                matvar->z->avail_in  = 16;
                err = deflate(matvar->z,Z_NO_FLUSH);
                byteswritten += fwrite(comp_buf,1,
                        buf_size*sizeof(*comp_buf)-matvar->z->avail_out,mat->fp);
                for ( i = 0; i < nfields; i++ ) {
                    memset(padzero,'\0',fieldname_size);
                    memcpy(padzero,fieldnames[i],strlen(fieldnames[i]));
                    matvar->z->next_out  = comp_buf;
                    matvar->z->next_in   = padzero;
                    matvar->z->avail_out = buf_size*sizeof(*comp_buf);
                    matvar->z->avail_in  = fieldname_size;
                    err = deflate(matvar->z,Z_NO_FLUSH);
                    byteswritten += fwrite(comp_buf,1,
                            buf_size*sizeof(*comp_buf)-matvar->z->avail_out,
                            mat->fp);
                }
                free(fieldnames);
                free(padzero);
                for ( i = 0; i < nmemb*nfields; i++ )
                    WriteCompressedStructField(mat,fields[i],matvar->z);
                break;
            }
            case MAT_C_SPARSE:
            {
                sparse_t *sparse = matvar->data;

                byteswritten += WriteCompressedData(mat,matvar->z,sparse->ir,
                    sparse->nir,MAT_T_INT32);
                byteswritten += WriteCompressedData(mat,matvar->z,sparse->jc,
                    sparse->njc,MAT_T_INT32);
                if ( matvar->isComplex ) {
                    struct ComplexSplit *complex_data = sparse->data;
                    byteswritten += WriteCompressedData(mat,matvar->z,
                        complex_data->Re,sparse->ndata,matvar->data_type);
                    byteswritten += WriteCompressedData(mat,matvar->z,
                        complex_data->Im,sparse->ndata,matvar->data_type);
                } else {
                    byteswritten += WriteCompressedData(mat,matvar->z,
                        sparse->data,sparse->ndata,matvar->data_type);
                }
                break;
            }
#endif
        }
        matvar->z->avail_in  = 0;
        matvar->z->next_in   = NULL;
        matvar->z->next_out  = comp_buf;
        matvar->z->avail_out = buf_size*sizeof(*comp_buf);

        err = deflate(matvar->z,Z_FINISH);
        byteswritten += fwrite(comp_buf,1,
            buf_size*sizeof(*comp_buf)-matvar->z->avail_out,mat->fp);
        while ( err != Z_STREAM_END && !matvar->z->avail_out ) {
            matvar->z->next_out  = comp_buf;
            matvar->z->avail_out = buf_size*sizeof(*comp_buf);

            err = deflate(matvar->z,Z_FINISH);
            byteswritten += fwrite(comp_buf,1,
                buf_size*sizeof(*comp_buf)-matvar->z->avail_out,mat->fp);
        }
        /* End the compression and set to NULL so Mat_VarFree doesn't try
         * to free matvar->z with inflateEnd
         */
#if 0
        if ( byteswritten % 8 )
            for ( i = 0; i < 8-(byteswritten % 8); i++ )
                fwrite(&pad1,1,1,mat->fp);
#endif
        err = deflateEnd(matvar->z);
        free(matvar->z);
        matvar->z = NULL;
#endif
    }
    end = ftell(mat->fp);
    nBytes = (int)(end-start);
    fseek(mat->fp,(long)-(nBytes+4),SEEK_CUR);
    fwrite(&nBytes,4,1,mat->fp);
    fseek(mat->fp,end,SEEK_SET);

    return 0;
}

/* Writes the variable information and empty data */
void
WriteInfo5(mat_t *mat, matvar_t *matvar)
{
    mat_uint32_t array_flags = 0x0;
    mat_int16_t  fieldname_type = MAT_T_INT32,fieldname_data_size=4;
    mat_int8_t  pad1 = 0;
    int      array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int      array_flags_size = 8, pad4 = 0, matrix_type = MAT_T_MATRIX;
    int      nBytes, i, nmemb = 1,nzmax;
    long     start = 0, end = 0;

    /* FIXME: SEEK_END is not Guaranteed by the C standard */
    fseek(mat->fp,0,SEEK_END);         /* Always write at end of file */


    if ( matvar->compression == COMPRESSION_NONE ) {
        fwrite(&matrix_type,4,1,mat->fp);
        fwrite(&pad4,4,1,mat->fp);
        start = ftell(mat->fp);

        /* Array Flags */

        array_flags = matvar->class_type & MAT_F_CLASS_T;
        if ( matvar->isComplex )
            array_flags |= MAT_F_COMPLEX;
        if ( matvar->isGlobal )
            array_flags |= MAT_F_GLOBAL;
        if ( matvar->isLogical )
            array_flags |= MAT_F_LOGICAL;
        if ( matvar->class_type == MAT_C_SPARSE )
            nzmax = ((sparse_t *)matvar->data)->nzmax;

        fwrite(&array_flags_type,4,1,mat->fp);
        fwrite(&array_flags_size,4,1,mat->fp);
        fwrite(&array_flags,4,1,mat->fp);
        fwrite(&nzmax,4,1,mat->fp);
        /* Rank and Dimension */
        nBytes = matvar->rank * 4;
        fwrite(&dims_array_type,4,1,mat->fp);
        fwrite(&nBytes,4,1,mat->fp);
        for ( i = 0; i < matvar->rank; i++ ) {
            mat_int32_t dim;
            dim = matvar->dims[i];
            nmemb *= dim;
            fwrite(&dim,4,1,mat->fp);
        }
        if ( matvar->rank % 2 != 0 )
            fwrite(&pad4,4,1,mat->fp);
        /* Name of variable */
        if ( strlen(matvar->name) <= 4 ) {
            mat_int16_t array_name_len = (mat_int16_t)strlen(matvar->name);
            mat_int8_t  pad1 = 0;
            mat_int16_t array_name_type = MAT_T_INT8;
            fwrite(&array_name_type,2,1,mat->fp);
            fwrite(&array_name_len,2,1,mat->fp);
            fwrite(matvar->name,1,array_name_len,mat->fp);
            for ( i = array_name_len; i < 4; i++ )
                fwrite(&pad1,1,1,mat->fp);
        } else {
            mat_int32_t array_name_len = (mat_int32_t)strlen(matvar->name);
            mat_int8_t  pad1 = 0;
            mat_int32_t  array_name_type = MAT_T_INT8;

            fwrite(&array_name_type,4,1,mat->fp);
            fwrite(&array_name_len,4,1,mat->fp);
            fwrite(matvar->name,1,array_name_len,mat->fp);
            if ( array_name_len % 8 )
                for ( i = array_name_len % 8; i < 8; i++ )
                    fwrite(&pad1,1,1,mat->fp);
        }

        matvar->datapos = ftell(mat->fp);
        switch ( matvar->class_type ) {
            case MAT_C_DOUBLE:
            case MAT_C_SINGLE:
            case MAT_C_INT32:
            case MAT_C_UINT32:
            case MAT_C_INT16:
            case MAT_C_UINT16:
            case MAT_C_INT8:
            case MAT_C_UINT8:
                nBytes = WriteEmptyData(mat,nmemb,matvar->data_type);
                if ( nBytes % 8 )
                    for ( i = nBytes % 8; i < 8; i++ )
                        fwrite(&pad1,1,1,mat->fp);
                if ( matvar->isComplex ) {
                    nBytes = WriteEmptyData(mat,nmemb,matvar->data_type);
                    if ( nBytes % 8 )
                        for ( i = nBytes % 8; i < 8; i++ )
                            fwrite(&pad1,1,1,mat->fp);
                }
                break;
            case MAT_C_CHAR:
            {
                WriteEmptyCharData(mat,nmemb,matvar->data_type);
                break;
            }
            case MAT_C_CELL:
            {
                int nfields = matvar->nbytes / matvar->data_size;
                matvar_t **fields = (matvar_t **)matvar->data;

                for ( i = 0; i < nfields; i++ )
                    WriteCellArrayFieldInfo(mat,fields[i],0);
                break;
            }
            case MAT_C_STRUCT:
            {
                char **fieldnames, *padzero;
                int maxlen = 0, fieldname_size;
                int nfields = matvar->nbytes / matvar->data_size;
                matvar_t **fields = (matvar_t **)matvar->data;
                mat_int32_t  array_name_type = MAT_T_INT8;
                unsigned fieldname;

                fieldnames = malloc(nfields*sizeof(char *));
                for ( i = 0; i < nfields; i++ ) {
                    fieldnames[i] = fields[i]->name;
                    if ( strlen(fieldnames[i]) > maxlen )
                        maxlen = strlen(fieldnames[i]);
                }
                maxlen++;
                fieldname_size = maxlen;
                while ( nfields*fieldname_size % 8 != 0 )
                    fieldname_size++;
#if 0
                fwrite(&fieldname_type,2,1,mat->fp);
                fwrite(&fieldname_data_size,2,1,mat->fp);
#else
                fieldname = (fieldname_data_size<<16) | fieldname_type;
                fwrite(&fieldname,4,1,mat->fp);
#endif
                fwrite(&fieldname_size,4,1,mat->fp);
                fwrite(&array_name_type,4,1,mat->fp);
                nBytes = nfields*fieldname_size;
                fwrite(&nBytes,4,1,mat->fp);
                padzero = calloc(fieldname_size,1);
                for ( i = 0; i < nfields; i++ ) {
                    fwrite(fieldnames[i],1,strlen(fieldnames[i]),mat->fp);
                    fwrite(padzero,1,fieldname_size-strlen(fieldnames[i]),mat->fp);
                }
                free(fieldnames);
                free(padzero);
                for ( i = 0; i < nfields; i++ )
                    WriteInfo5(mat,fields[i]);
                break;
            }
        }
    /* Does not work.
     * Can write empty data, but how to go back and add the real data?
     */
#if 0
    } else if ( matvar->compression == COMPRESSION_ZLIB ) {
#if defined(HAVE_ZLIB)
        mat_uint32_t comp_buf[512];
        mat_uint32_t uncomp_buf[512] = {0,};
        int buf_size = 512, err;
        size_t byteswritten = 0;

        matvar->z         = malloc(sizeof(*matvar->z));
        matvar->z->zalloc = Z_NULL;
        matvar->z->zfree  = Z_NULL;
        err = deflateInit(matvar->z,Z_DEFAULT_COMPRESSION);

        matrix_type = MAT_T_COMPRESSED;
        fwrite(&matrix_type,4,1,mat->fp);
        fwrite(&pad4,4,1,mat->fp);
        start = ftell(mat->fp);

        /* Array Flags */

        array_flags = matvar->class_type & MAT_F_CLASS_T;
        if ( matvar->isComplex )
            array_flags |= MAT_F_COMPLEX;
        if ( matvar->isGlobal )
            array_flags |= MAT_F_GLOBAL;
        if ( matvar->isLogical )
            array_flags |= MAT_F_LOGICAL;

        uncomp_buf[0] = MAT_T_MATRIX;
        uncomp_buf[1] = 448;
        matvar->z->next_out  = comp_buf;
        matvar->z->next_in   = uncomp_buf;
        matvar->z->avail_out = buf_size*sizeof(*comp_buf);
        matvar->z->avail_in  = 8;
        err = deflate(matvar->z,Z_SYNC_FLUSH);
        byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-matvar->z->avail_out,mat->fp);
        uncomp_buf[0] = array_flags_type;
        uncomp_buf[1] = array_flags_size;
        uncomp_buf[2] = array_flags;
        uncomp_buf[3] = 0;
        /* Rank and Dimension */
        nBytes = matvar->rank * 4;
        uncomp_buf[4] = dims_array_type;
        uncomp_buf[5] = nBytes;
        for ( i = 0; i < matvar->rank; i++ ) {
            mat_int32_t dim;
            dim = matvar->dims[i];
            nmemb *= dim;
            uncomp_buf[6+i] = dim;
        }
        if ( matvar->rank % 2 != 0 )
            uncomp_buf[6+i] = pad4;

        matvar->z->next_out  = comp_buf;
        matvar->z->next_in   = uncomp_buf;
        matvar->z->avail_out = buf_size*sizeof(*comp_buf);
        matvar->z->avail_in  = (6+i)*sizeof(*uncomp_buf);
        err = deflate(matvar->z,Z_NO_FLUSH);
        byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-matvar->z->avail_out,mat->fp);
        /* Name of variable */
        if ( strlen(matvar->name) <= 4 ) {
#if 0
            mat_int16_t array_name_len = (mat_int16_t)strlen(matvar->name);
            mat_int8_t  pad1 = 0;

            uncomp_buf[0] = (array_name_type << 16) | array_name_len;
            memcpy(uncomp_buf+1,matvar->name,array_name_len);

            matvar->z->next_out  = comp_buf;
            matvar->z->next_in   = uncomp_buf;
            matvar->z->avail_out = buf_size*sizeof(*comp_buf);
            matvar->z->avail_in  = 8;
            err = deflate(matvar->z,Z_NO_FLUSH);
            byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-matvar->z->avail_out,mat->fp);
        } else {
#endif
            mat_int32_t array_name_len = (mat_int32_t)strlen(matvar->name);

            memset(uncomp_buf,0,buf_size*sizeof(*uncomp_buf));
            uncomp_buf[0] = array_name_type;
            uncomp_buf[1] = array_name_len;
            memcpy(uncomp_buf+2,matvar->name,array_name_len);
            if ( array_name_len % 8 )
                array_name_len += array_name_len % 8;
            matvar->z->next_out  = comp_buf;
            matvar->z->next_in   = uncomp_buf;
            matvar->z->avail_out = buf_size*sizeof(*comp_buf);
            matvar->z->avail_in  = 8+array_name_len;
            err = deflate(matvar->z,Z_FULL_FLUSH);
            byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-matvar->z->avail_out,mat->fp);
        }
        matvar->datapos = ftell(mat->fp);
        deflateCopy(&z_save,matvar->z);
        switch ( matvar->class_type ) {
            case MAT_C_DOUBLE:
            case MAT_C_SINGLE:
            case MAT_C_INT32:
            case MAT_C_UINT32:
            case MAT_C_INT16:
            case MAT_C_UINT16:
            case MAT_C_INT8:
            case MAT_C_UINT8:
                byteswritten += WriteCompressedEmptyData(mat,matvar->z,nmemb,matvar->data_type);
#if 0
                if ( nBytes % 8 )
                    for ( i = nBytes % 8; i < 8; i++ )
                        fwrite(&pad1,1,1,mat->fp);
                if ( matvar->isComplex ) {
                    nBytes = WriteEmptyData(mat,nmemb,matvar->data_type);
                    if ( nBytes % 8 )
                        for ( i = nBytes % 8; i < 8; i++ )
                            fwrite(&pad1,1,1,mat->fp);
                }
#endif
                break;
        }
        matvar->z->next_out  = comp_buf;
        matvar->z->next_in   = NULL;
        matvar->z->avail_out = buf_size*sizeof(*comp_buf);
        matvar->z->avail_in  = 0;

        err = deflate(matvar->z,Z_FINISH);
        byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-matvar->z->avail_out,mat->fp);
                if ( byteswritten % 8 )
                    for ( i = byteswritten % 8; i < 8; i++ )
                        fwrite(&pad1,1,1,mat->fp);
        fprintf(stderr,"deflate Z_FINISH: err = %d,byteswritten = %u\n",err,byteswritten);

        err = deflateEnd(matvar->z);
        fprintf(stderr,"deflateEnd: err = %d\n",err);
#if 1
        err = deflateEnd(matvar->z);
        free(matvar->z);
        matvar->z = NULL;
#else
        memcpy(matvar->z,&z_save,sizeof(*matvar->z));
#endif
#endif
#endif
    }
    end = ftell(mat->fp);
    nBytes = (int)(end-start);
    fseek(mat->fp,(long)-(nBytes+4),SEEK_CUR);
    fwrite(&nBytes,4,1,mat->fp);
    fseek(mat->fp,end,SEEK_SET);
}

void
Mat_VarPrint5( matvar_t *matvar, int printdata )
{
    int i, j;

    if ( matvar == NULL )
        return;
    if ( matvar->name )
        Mat_Message("      Name: %s", matvar->name);
    Mat_Message("      Rank: %d", matvar->rank);
    if ( matvar->rank == 0 )
        return;
    if ( matvar->isComplex )
        Mat_Message("Class Type: %s (complex)",class_type_desc[matvar->class_type]);
    else
        Mat_Message("Class Type: %s",class_type_desc[matvar->class_type]);
    if ( matvar->data_type )
        Mat_Message(" Data Type: %s", data_type_desc[matvar->data_type]);
    if ( matvar->data != NULL && matvar->data_size > 0 ) {
        switch( matvar->class_type ) {
            case MAT_C_DOUBLE:
                if ( !printdata )
                    break;
                if ( matvar->rank > 2 ) {
                    printf("I can't print more than 2 dimensions\n");
                } else if ( matvar->rank == 1 && matvar->dims[0] > 15 ) {
                    printf("I won't print more than 15 elements in a vector\n");
                } else if ( matvar->rank == 2 &&
                         (matvar->dims[0] > 15 || matvar->dims[1] > 15) ) {
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%f ", ((double*)matvar->data)[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if ( matvar->rank == 2 ) {
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%f ", ((double*)matvar->data)[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                } else {
                    for ( i = 0; i < matvar->nbytes/matvar->data_size; i++ )
                        printf("%f\n", ((double*)matvar->data)[i]);
                }
                break;
            case MAT_C_SINGLE:
                if ( !printdata )
                    break;
                if ( matvar->rank > 2 ) {
                    printf("I can't print more than 2 dimensions\n");
                } else if ( matvar->rank == 1 && matvar->dims[0] > 15 ) {
                    printf("I won't print more than 15 elements in a vector\n");
                } else if ( matvar->rank == 2 &&
                         (matvar->dims[0] > 15 || matvar->dims[1] > 15) ) {
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%f ", ((float*)matvar->data)[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if ( matvar->rank == 2 ) {
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%f ", ((float*)matvar->data)[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                } else {
                    for ( i = 0; i < matvar->nbytes/matvar->data_size; i++ )
                        printf("%f\n", ((float*)matvar->data)[i]);
                }
                break;
            case MAT_C_INT32:
                if ( !printdata )
                    break;
                if ( matvar->rank > 2 ) {
                    printf("I can't print more than 2 dimensions\n");
                } else if ( matvar->rank == 1 && matvar->dims[0] > 15 ) {
                    printf("I won't print more than 15 elements in a vector\n");
                } else if ( matvar->rank == 2 &&
                         (matvar->dims[0] > 15 || matvar->dims[1] > 15) ) {
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%d ", ((mat_int32_t*)matvar->data)[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if ( matvar->rank == 2 ) {
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%d ", ((mat_uint32_t*)matvar->data)[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                } else {
                    for ( i = 0; i < matvar->nbytes/matvar->data_size; i++ )
                        printf("%d\n", ((mat_int32_t*)matvar->data)[i]);
                }
                break;
            case MAT_C_UINT32:
                if ( !printdata )
                    break;
                if ( matvar->rank > 2 ) {
                    printf("I can't print more than 2 dimensions\n");
                } else if ( matvar->rank == 1 && matvar->dims[0] > 15 ) {
                    printf("I won't print more than 15 elements in a vector\n");
                } else if ( matvar->rank == 2 &&
                         (matvar->dims[0] > 15 || matvar->dims[1] > 15) ) {
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%u ", ((mat_uint32_t*)matvar->data)[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if ( matvar->rank == 2 ) {
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%u ", ((mat_uint32_t*)matvar->data)[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                } else {
                    for ( i = 0; i < matvar->nbytes/matvar->data_size; i++ )
                        printf("%u\n", ((mat_int32_t*)matvar->data)[i]);
                }
                break;
            case MAT_C_INT16:
                if ( !printdata )
                    break;
                if ( matvar->rank > 2 ) {
                    printf("I can't print more than 2 dimensions\n");
                } else if ( matvar->rank == 1 && matvar->dims[0] > 15 ) {
                    printf("I won't print more than 15 elements in a vector\n");
                } else if ( matvar->rank == 2 &&
                         (matvar->dims[0] > 15 || matvar->dims[1] > 15) ) {
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%hd ", ((mat_int16_t*)matvar->data)[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if ( matvar->rank == 2 ) {
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%hd ", ((mat_uint16_t*)matvar->data)[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                } else {
                    for ( i = 0; i < matvar->nbytes/matvar->data_size; i++ )
                        printf("%hd\n", ((mat_int16_t*)matvar->data)[i]);
                }
                break;
            case MAT_C_UINT16:
                if ( !printdata )
                    break;
                if ( matvar->rank > 2 ) {
                    printf("I can't print more than 2 dimensions\n");
                } else if ( matvar->rank == 1 && matvar->dims[0] > 15 ) {
                    printf("I won't print more than 15 elements in a vector\n");
                } else if ( matvar->rank == 2 &&
                         (matvar->dims[0] > 15 || matvar->dims[1] > 15) ) {
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%hu ", ((mat_uint16_t*)matvar->data)[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if ( matvar->rank == 2 ) {
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%hu ", ((mat_uint16_t*)matvar->data)[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                } else {
                    for ( i = 0; i < matvar->nbytes/matvar->data_size; i++ )
                        printf("%hu\n", ((mat_int32_t*)matvar->data)[i]);
                }
                break;
            case MAT_C_INT8:
                if ( !printdata )
                    break;
                if ( matvar->rank > 2 ) {
                    printf("I can't print more than 2 dimensions\n");
                } else if ( matvar->rank == 1 && matvar->dims[0] > 15 ) {
                    printf("I won't print more than 15 elements in a vector\n");
                } else if ( matvar->rank == 2 &&
                         (matvar->dims[0] > 15 || matvar->dims[1] > 15) ) {
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%hd ", ((mat_int8_t*)matvar->data)[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if ( matvar->rank == 2 ) {
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%hd ", ((mat_int8_t*)matvar->data)[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                } else {
                    for ( i = 0; i < matvar->nbytes/matvar->data_size; i++ )
                        printf("%hd\n", ((mat_int8_t*)matvar->data)[i]);
                }
                break;
            case MAT_C_UINT8:
                if ( !printdata )
                    break;
                if ( matvar->rank > 2 ) {
                    printf("I can't print more than 2 dimensions\n");
                } else if ( matvar->rank == 1 && matvar->dims[0] > 15 ) {
                    printf("I won't print more than 15 elements in a vector\n");
                } else if ( matvar->rank == 2 &&
                         (matvar->dims[0] > 15 || matvar->dims[1] > 15) ) {
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%hu ", ((mat_uint8_t*)matvar->data)[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if ( matvar->rank == 2 ) {
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%hu ", ((mat_uint8_t*)matvar->data)[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                } else {
                    for ( i = 0; i < matvar->nbytes/matvar->data_size; i++ )
                        printf("%hu\n", ((mat_uint8_t*)matvar->data)[i]);
                }
                break;
            case MAT_C_CHAR:
                if ( !printdata )
                    break;
                if ( matvar->dims[0] == 1 ) {
                    printf("%s\n",(char *)matvar->data);
                } else {
                    int ndx = 0;
                    for ( i = 0; i < matvar->dims[1]; i++ ) {
                        ndx = i;
                        j = 0;
                        while ( j++ < matvar->dims[0] &&
                                *((char *)matvar->data+ndx) != '\0' ) {
                            printf("%c", *((char *)matvar->data+ndx));
                            ndx += matvar->dims[0];
                        }
                        printf("\n");
                    }
                }
                break;
            case MAT_C_STRUCT:
            {
                matvar_t **fields = (matvar_t **)matvar->data;
                int nfields = matvar->nbytes / matvar->data_size;
                Mat_Message("Fields[%d] {", nfields);
                for ( i = 0; i < nfields; i++ )
                    Mat_VarPrint(fields[i],printdata);
                Mat_Message("}");
                break;
            }
            case MAT_C_CELL:
            {
                matvar_t **fields = (matvar_t **)matvar->data;
                int nfields = matvar->nbytes / matvar->data_size;
                for ( i = 0; i < nfields; i++ )
                    Mat_VarPrint(fields[i],printdata);
                break;
            }
            case MAT_C_SPARSE:
            {
                sparse_t *sparse;
/* FIXME: ComplexSplit */
#if defined(EXTENDED_SPARSE)
                sparse = matvar->data;
                switch ( matvar->data_type ) {
                    case MAT_T_DOUBLE:
                    {
                        if ( matvar->isComplex ) {
                            struct ComplexSplit *complex_data = sparse->data;
                            double *re,*im;
                            re = complex_data->Re;
                            im = complex_data->Im;
                            for ( i = 0; i < sparse->njc-1; i++ ) {
                                for (j = sparse->jc[i];
                                     j<sparse->jc[i+1] && j<sparse->ndata;j++ )
                                    Mat_Message("    (%d,%d)  %f + %fi",
                                        sparse->ir[j]+1,i+1,re[j],im[j]);
                            }
                        } else {
                            double *data;
                            data = sparse->data;
                            for ( i = 0; i < sparse->njc-1; i++ ) {
                                for (j = sparse->jc[i];
                                     j<sparse->jc[i+1] && j<sparse->ndata;j++ )
                                    Mat_Message("    (%d,%d)  %f",
                                        sparse->ir[j]+1,i+1,data[j]);
                            }
                        }
                        break;
                    }
                    case MAT_T_SINGLE:
                    {
                        float *data;
                        data = sparse->data;
                        if ( matvar->isComplex ) {
                            for ( i = 0; i < sparse->njc-1; i++ ) {
                                for (j = sparse->jc[i];
                                     j<sparse->jc[i+1] && j<sparse->ndata;j++ )
                                    Mat_Message("    (%d,%d)  %f + %fi",
                                        sparse->ir[j]+1,i+1,data[j],
                                        data[sparse->ndata+j]);
                            }
                        } else {
                            for ( i = 0; i < sparse->njc-1; i++ ) {
                                for (j = sparse->jc[i];
                                     j<sparse->jc[i+1] && j<sparse->ndata;j++ )
                                    Mat_Message("    (%d,%d)  %f",
                                        sparse->ir[j]+1,i+1,data[j]);
                            }
                        }
                        break;
                    }
                    case MAT_T_INT32:
                    {
                        mat_int32_t *data;
                        data = sparse->data;
                        if ( matvar->isComplex ) {
                            for ( i = 0; i < sparse->njc-1; i++ ) {
                                for (j = sparse->jc[i];
                                     j<sparse->jc[i+1] && j<sparse->ndata;j++ )
                                    Mat_Message("    (%d,%d)  %d + %di",
                                        sparse->ir[j]+1,i+1,data[j],
                                        data[sparse->ndata+j]);
                            }
                        } else {
                            for ( i = 0; i < sparse->njc-1; i++ ) {
                                for (j = sparse->jc[i];
                                     j<sparse->jc[i+1] && j<sparse->ndata;j++ )
                                    Mat_Message("    (%d,%d)  %d",
                                        sparse->ir[j]+1,i+1,data[j]);
                            }
                        }
                        break;
                    }
                    case MAT_T_UINT32:
                    {
                        mat_uint32_t *data;
                        data = sparse->data;
                        if ( matvar->isComplex ) {
                            for ( i = 0; i < sparse->njc-1; i++ ) {
                                for (j = sparse->jc[i];
                                     j<sparse->jc[i+1] && j<sparse->ndata;j++ )
                                    Mat_Message("    (%d,%d)  %u + %ui",
                                        sparse->ir[j]+1,i+1,data[j],
                                        data[sparse->ndata+j]);
                            }
                        } else {
                            for ( i = 0; i < sparse->njc-1; i++ ) {
                                for (j = sparse->jc[i];
                                     j<sparse->jc[i+1] && j<sparse->ndata;j++ )
                                    Mat_Message("    (%d,%d)  %u",
                                        sparse->ir[j]+1,i+1,data[j]);
                            }
                        }
                        break;
                    }
                    case MAT_T_INT16:
                    {
                        mat_int16_t *data;
                        data = sparse->data;
                        if ( matvar->isComplex ) {
                            for ( i = 0; i < sparse->njc-1; i++ ) {
                                for (j = sparse->jc[i];
                                     j<sparse->jc[i+1] && j<sparse->ndata;j++ )
                                    Mat_Message("    (%d,%d)  %hd + %hdi",
                                        sparse->ir[j]+1,i+1,data[j],
                                        data[sparse->ndata+j]);
                            }
                        } else {
                            for ( i = 0; i < sparse->njc-1; i++ ) {
                                for (j = sparse->jc[i];
                                     j<sparse->jc[i+1] && j<sparse->ndata;j++ )
                                    Mat_Message("    (%d,%d)  %hd",
                                        sparse->ir[j]+1,i+1,data[j]);
                            }
                        }
                        break;
                    }
                    case MAT_T_UINT16:
                    {
                        mat_uint16_t *data;
                        data = sparse->data;
                        if ( matvar->isComplex ) {
                            for ( i = 0; i < sparse->njc-1; i++ ) {
                                for (j = sparse->jc[i];
                                     j<sparse->jc[i+1] && j<sparse->ndata;j++ )
                                    Mat_Message("    (%d,%d)  %hu + %hui",
                                        sparse->ir[j]+1,i+1,data[j],
                                        data[sparse->ndata+j]);
                            }
                        } else {
                            for ( i = 0; i < sparse->njc-1; i++ ) {
                                for (j = sparse->jc[i];
                                     j<sparse->jc[i+1] && j<sparse->ndata;j++ )
                                    Mat_Message("    (%d,%d)  %hu",
                                        sparse->ir[j]+1,i+1,data[j]);
                            }
                        }
                        break;
                    }
                    case MAT_T_INT8:
                    {
                        mat_int8_t *data;
                        data = sparse->data;
                        if ( matvar->isComplex ) {
                            for ( i = 0; i < sparse->njc-1; i++ ) {
                                for (j = sparse->jc[i];
                                     j<sparse->jc[i+1] && j<sparse->ndata;j++ )
                                    Mat_Message("    (%d,%d)  %hd + %hdi",
                                        sparse->ir[j]+1,i+1,data[j],
                                        data[sparse->ndata+j]);
                            }
                        } else {
                            for ( i = 0; i < sparse->njc-1; i++ ) {
                                for (j = sparse->jc[i];
                                     j<sparse->jc[i+1] && j<sparse->ndata;j++ )
                                    Mat_Message("    (%d,%d)  %hd",
                                        sparse->ir[j]+1,i+1,data[j]);
                            }
                        }
                        break;
                    }
                    case MAT_T_UINT8:
                    {
                        mat_uint8_t *data;
                        data = sparse->data;
                        if ( matvar->isComplex ) {
                            for ( i = 0; i < sparse->njc-1; i++ ) {
                                for (j = sparse->jc[i];
                                     j<sparse->jc[i+1] && j<sparse->ndata;j++ )
                                    Mat_Message("    (%d,%d)  %hu + %hui",
                                        sparse->ir[j]+1,i+1,data[j],
                                        (mat_uint16_t)data[sparse->ndata+j]);
                            }
                        } else {
                            for ( i = 0; i < sparse->njc-1; i++ ) {
                                for (j = sparse->jc[i];
                                     j<sparse->jc[i+1] && j<sparse->ndata;j++ )
                                    Mat_Message("    (%d,%d)  %hu",
                                        sparse->ir[j]+1,i+1,
                                        (mat_uint16_t)data[j]);
                            }
                        }
                        break;
                    }
                }
#else
                double *data;

                sparse = matvar->data;
                data = sparse->data;
                if ( matvar->isComplex ) {
                    for ( i = 0; i < sparse->njc-1; i++ ) {
                        for ( j = sparse->jc[i];
                              j < sparse->jc[i+1] && j < sparse->ndata; j++ )
                            Mat_Message("    (%d,%d)  %f + %fi",
                                sparse->ir[j]+1,i+1,data[j],
                                data[sparse->ndata+j]);
                    }
                } else {
                    for ( i = 0; i < sparse->njc-1; i++ ) {
                        for ( j = sparse->jc[i];
                              j < sparse->jc[i+1] && j < sparse->ndata; j++ )
                            Mat_Message("    (%d,%d)  %f", sparse->ir[j]+1,i+1,
                                data[j]);
                    }
                }
#endif
                break;
            }
            default:
                printf("I can't print this class\n");
        }
    } else {
        if ( printdata && !matvar->data  )
            Mat_Warning("Data is NULL");
        if ( printdata && matvar->data_size < 1 )
            Mat_Warning("data-size is %d",matvar->data_size);
    }
    Mat_Message("\n");
    return;
}

matvar_t *
Mat_VarReadNextInfo5( mat_t *mat )
{
    int err, data_type, nBytes, i;
    long  fpos;
    matvar_t *matvar = NULL; 
    mat_uint32_t array_flags;
    long     bytesread = 0;

    if( mat == NULL )
        return NULL; 

    fpos = ftell(mat->fp);
    err = fread(&data_type,4,1,mat->fp);
    if ( !err )
        return NULL;
    err = fread(&nBytes,4,1,mat->fp);
    if ( mat->byteswap ) {
        int32Swap(&data_type);
        int32Swap(&nBytes);
    }
    switch ( data_type ) {
#if defined(HAVE_ZLIB)
        case MAT_T_COMPRESSED:
        {
            mat_uint32_t uncomp_buf[16] = {0,};
            int      nbytes;

            matvar               = malloc(sizeof(*matvar));
            matvar->name         = NULL;
            matvar->data         = NULL;
            matvar->dims         = NULL;
            matvar->nbytes       = 0;
            matvar->data_type    = 0;
            matvar->class_type   = 0;
            matvar->data_size    = 0;
            matvar->mem_conserve = 0;
            matvar->compression  = 1;
            matvar->fpos         = fpos;
            matvar->fp           = mat;

            matvar->z = calloc(1,sizeof(z_stream));
            matvar->z->zalloc    = NULL;
            matvar->z->zfree     = NULL;
            matvar->z->opaque    = NULL;
            matvar->z->next_in   = NULL;
            matvar->z->next_out  = NULL;
            matvar->z->avail_in  = 0;
            matvar->z->avail_out = 0;
            err = inflateInit(matvar->z);
            if ( err != Z_OK ) {
                Mat_Critical("inflateInit2 returned %d",err);
                Mat_VarFree(matvar);
                break;
            }

            /* Read Variable tag */
            bytesread += InflateVarTag(mat,matvar,uncomp_buf);
            if ( mat->byteswap ) {
                (void)uint32Swap(uncomp_buf);
                (void)uint32Swap(uncomp_buf+1);
            }
            nbytes = uncomp_buf[1];
            if ( uncomp_buf[0] != MAT_T_MATRIX ) {
                Mat_Critical("Uncompressed type not MAT_T_MATRIX");
                fseek(mat->fp,nBytes-bytesread,SEEK_CUR);
                Mat_VarFree(matvar);
                matvar = NULL;
                break;
            }
            /* Inflate Array Flags */
            bytesread += InflateArrayFlags(mat,matvar,uncomp_buf);
            if ( mat->byteswap ) {
                (void)uint32Swap(uncomp_buf);
                (void)uint32Swap(uncomp_buf+2);
                (void)uint32Swap(uncomp_buf+3);
            }
            /* Array Flags */
            if ( uncomp_buf[0] == MAT_T_UINT32 ) {
               array_flags = uncomp_buf[2];
               matvar->class_type  = (array_flags & MAT_F_CLASS_T);
               matvar->isComplex   = (array_flags & MAT_F_COMPLEX);
               matvar->isGlobal    = (array_flags & MAT_F_GLOBAL);
               matvar->isLogical   = (array_flags & MAT_F_LOGICAL);
               if ( matvar->class_type == MAT_C_SPARSE ) {
                   /* Need to find a more appropriate place to store nzmax */
                   matvar->nbytes      = uncomp_buf[3];
               }
            }
            /* Inflate Dimensions */
            bytesread += InflateDimensions(mat,matvar,uncomp_buf);
            if ( mat->byteswap ) {
                (void)uint32Swap(uncomp_buf);
                (void)uint32Swap(uncomp_buf+1);
            }
            /* Rank and Dimension */
            if ( uncomp_buf[0] == MAT_T_INT32 ) {
                nbytes = uncomp_buf[1];
                matvar->rank = nbytes / 4;
                matvar->dims = malloc(matvar->rank*sizeof(int));
                if ( mat->byteswap ) {
                    for ( i = 0; i < matvar->rank; i++ )
                        matvar->dims[i] = uint32Swap(&(uncomp_buf[2+i]));
                } else {
                    for ( i = 0; i < matvar->rank; i++ )
                        matvar->dims[i] = uncomp_buf[2+i];
                }
            }
            /* Inflate variable name tag */
            bytesread += InflateVarNameTag(mat,matvar,uncomp_buf);
            if ( mat->byteswap )
                (void)uint32Swap(uncomp_buf);
            /* Name of variable */
            if ( uncomp_buf[0] == MAT_T_INT8 ) {    /* Name not in tag */
                int len;
                if ( mat->byteswap )
                    len = uint32Swap(uncomp_buf+1);
                else
                    len = uncomp_buf[1];

                if ( len % 8 == 0 )
                    i = len;
                else
                    i = len+(8-(len % 8));
                matvar->name = malloc(i+1);
                /* Inflate variable name */
                bytesread += InflateVarName(mat,matvar,matvar->name,i);
                matvar->name[len] = '\0';
            } else if ( ((uncomp_buf[0] & 0x0000ffff) == MAT_T_INT8) &&
                        ((uncomp_buf[0] & 0xffff0000) != 0x00) ) {
                /* Name packed in tag */
                int len;
                len = (uncomp_buf[0] & 0xffff0000) >> 16;
                matvar->name = malloc(len+1);
                memcpy(matvar->name,uncomp_buf+1,len);
                matvar->name[len] = '\0';
            }
            if ( matvar->class_type == MAT_C_STRUCT )
                ReadNextStructField(mat,matvar);
            else if ( matvar->class_type == MAT_C_CELL )
                ReadNextCell(mat,matvar);
            fseek(mat->fp,-(int)matvar->z->avail_in,SEEK_CUR);
            matvar->datapos = ftell(mat->fp);
            fseek(mat->fp,nBytes+8+fpos,SEEK_SET);
            break;
        }
#endif
        case MAT_T_MATRIX:
        {
            int      nbytes;
            mat_uint32_t buf[32];
            size_t   bytesread = 0;

            matvar = Mat_VarCalloc();
            matvar->fpos         = fpos;
            matvar->fp           = mat;

            /* Read Array Flags and The Dimensions Tag */
            bytesread  += fread(buf,4,6,mat->fp);
            if ( mat->byteswap ) {
                (void)uint32Swap(buf);
                (void)uint32Swap(buf+1);
                (void)uint32Swap(buf+2);
                (void)uint32Swap(buf+3);
                (void)uint32Swap(buf+4);
                (void)uint32Swap(buf+5);
            }
            /* Array Flags */
            if ( buf[0] == MAT_T_UINT32 ) {
               array_flags = buf[2];
               matvar->class_type  = (array_flags & MAT_F_CLASS_T);
               matvar->isComplex   = (array_flags & MAT_F_COMPLEX);
               matvar->isGlobal    = (array_flags & MAT_F_GLOBAL);
               matvar->isLogical   = (array_flags & MAT_F_LOGICAL);
               if ( matvar->class_type == MAT_C_SPARSE ) {
                   /* Need to find a more appropriate place to store nzmax */
                   matvar->nbytes      = buf[3];
               }
            }
            /* Rank and Dimension */
            if ( buf[4] == MAT_T_INT32 ) {
                nbytes = buf[5];

                matvar->rank = nbytes / 4;
                matvar->dims = malloc(matvar->rank*sizeof(int));

                /* Assumes rank <= 16 */
                if ( matvar->rank % 2 != 0 )
                    bytesread+=fread(buf,4,matvar->rank+1,mat->fp);
                else
                    bytesread+=fread(buf,4,matvar->rank,mat->fp);

                if ( mat->byteswap ) {
                    for ( i = 0; i < matvar->rank; i++ )
                        matvar->dims[i] = uint32Swap(buf+i);
                } else {
                    for ( i = 0; i < matvar->rank; i++ )
                        matvar->dims[i] = buf[i];
                }
            }
            /* Variable Name Tag */
            bytesread+=fread(buf,4,2,mat->fp);
            if ( mat->byteswap )
                (void)uint32Swap(buf);
            /* Name of variable */
            if ( buf[0] == MAT_T_INT8 ) {    /* Name not in tag */
                int len;

                if ( mat->byteswap )
                    len = uint32Swap(buf+1);
                else
                    len = buf[1];
                if ( len % 8 == 0 )
                    i = len;
                else
                    i = len+(8-(len % 8));
                bytesread+=fread(buf,1,i,mat->fp);

                matvar->name = malloc(len+1);
                memcpy(matvar->name,buf,len);
                matvar->name[len] = '\0';
            } else if ( ((buf[0] & 0x0000ffff) == MAT_T_INT8) &&
                        ((buf[0] & 0xffff0000) != 0x00) ) {
                /* Name packed in the tag */
                int len;

                len = (buf[0] & 0xffff0000) >> 16;
                matvar->name = malloc(len+1);
                memcpy(matvar->name,buf+1,len);
                matvar->name[len] = '\0';
            }
            if ( matvar->class_type == MAT_C_STRUCT )
                (void)ReadNextStructField(mat,matvar);
            else if ( matvar->class_type == MAT_C_CELL )
                (void)ReadNextCell(mat,matvar);
            else if ( matvar->class_type == MAT_C_FUNCTION )
                (void)ReadNextFunctionHandle(mat,matvar);
            matvar->datapos = ftell(mat->fp);
            fseek(mat->fp,nBytes+8+fpos,SEEK_SET);
            break;
        }
        default:
            Mat_Message("%d is not valid (MAT_T_MATRIX or MAT_T_COMPRESSED", data_type);
            return NULL;
    }

    return matvar;
}
