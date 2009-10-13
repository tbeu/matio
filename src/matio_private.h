/*
 * Copyright (C) 2008   Christopher C. Hulbert
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
#ifndef MATIO_PRIVATE_H
#define MATIO_PRIVATE_H

#include "matio.h"
#if MAT73
#   include <hdf5.h>
#else
#   define hobj_ref_t int
#   define hid_t int
#endif

#ifndef EXTERN
#   ifdef __cplusplus
#       define EXTERN extern "C"
#   else
#       define EXTERN extern
#   endif
#endif

/** @if mat_devman
 * @brief Matlab MAT File information
 *
 * Contains information about a Matlab MAT file
 * @ingroup mat_internal
 * @endif
 */
struct _mat_t {
    void *fp;               /**< File pointer for the MAT file */
    char *header;           /**< MAT File header string */
    char *subsys_offset;    /**< offset */
    char *filename;         /**< Filename of the MAT file */
    int   version;          /**< MAT File version */
    int   byteswap;         /**< 1 if byte swapping is required, 0 otherwise */
    int   mode;             /**< Access mode */
    long  bof;              /**< Beginning of file not including any header */
    long  next_index;       /**< Index/File position of next variable to read */
    long  num_datasets;     /**< Number of datasets in the file */
};

/** @if mat_devman
 * @brief internal structure for MAT variables
 * @ingroup mat_internal
 * @endif
 */
struct matvar_internal {
    char *hdf5_name;
    hobj_ref_t hdf5_ref;
    hid_t      id;
    long  fpos;         /**< Offset from the beginning of the MAT file to the variable */
    long  datapos;      /**< Offset from the beginning of the MAT file to the data */
     mat_t    *fp;      /**< Pointer to the MAT file structure (mat_t) */
#if defined(HAVE_ZLIB)
    z_stream *z;        /**< zlib compression state */
#endif
};

/*    snprintf.c    */
EXTERN int mat_snprintf(char *str,size_t count,const char *fmt,...);
EXTERN int mat_asprintf(char **ptr,const char *format, ...);
EXTERN int mat_vsnprintf(char *str,size_t count,const char *fmt,va_list args);
EXTERN int mat_vasprintf(char **ptr,const char *format,va_list ap);

/*   endian.c     */
EXTERN double        Mat_doubleSwap(double  *a);
EXTERN float         Mat_floatSwap(float   *a);
#ifdef HAVE_MAT_INT64_T
EXTERN mat_int64_t   Mat_int64Swap(mat_int64_t  *a);
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
EXTERN mat_uint64_t  Mat_uint64Swap(mat_uint64_t *a);
#endif /* HAVE_MAT_UINT64_T */
EXTERN mat_int32_t   Mat_int32Swap(mat_int32_t  *a);
EXTERN mat_uint32_t  Mat_uint32Swap(mat_uint32_t *a);
EXTERN mat_int16_t   Mat_int16Swap(mat_int16_t  *a);
EXTERN mat_uint16_t  Mat_uint16Swap(mat_uint16_t *a);

/* read_data.c */
EXTERN int ReadDoubleData(mat_t *mat,double  *data,int data_type,int len);
EXTERN int ReadSingleData(mat_t *mat,float   *data,int data_type,int len);
EXTERN int ReadInt32Data (mat_t *mat,mat_int32_t *data,int data_type,int len);
EXTERN int ReadUInt32Data(mat_t *mat,mat_uint32_t *data,int data_type,int len);
EXTERN int ReadInt16Data (mat_t *mat,mat_int16_t *data,int data_type,int len);
EXTERN int ReadUInt16Data(mat_t *mat,mat_uint16_t *data,int data_type,int len);
EXTERN int ReadInt8Data  (mat_t *mat,mat_int8_t  *data,int data_type,int len);
EXTERN int ReadUInt8Data (mat_t *mat,mat_uint8_t  *data,int data_type,int len);
EXTERN int ReadCharData  (mat_t *mat,char  *data,int data_type,int len);
EXTERN int ReadDataSlab1(mat_t *mat,void *data,int class_type,int data_type,
               int start,int stride,int edge);
EXTERN int ReadDataSlab2(mat_t *mat,void *data,int class_type,int data_type,
               int *dims,int *start,int *stride,int *edge);
EXTERN int ReadDataSlabN(mat_t *mat,void *data,int class_type,int data_type,
               int rank,int *dims,int *start,int *stride,int *edge);
#if defined(HAVE_ZLIB)
EXTERN int ReadCompressedDoubleData(mat_t *mat,z_stream *z,double  *data,
               int data_type,int len);
EXTERN int ReadCompressedSingleData(mat_t *mat,z_stream *z,float   *data,
               int data_type,int len);
EXTERN int ReadCompressedInt32Data(mat_t *mat,z_stream *z,mat_int32_t *data,
               int data_type,int len);
EXTERN int ReadCompressedUInt32Data(mat_t *mat,z_stream *z,mat_uint32_t *data,
               int data_type,int len);
EXTERN int ReadCompressedInt16Data(mat_t *mat,z_stream *z,mat_int16_t *data,
               int data_type,int len);
EXTERN int ReadCompressedUInt16Data(mat_t *mat,z_stream *z,mat_uint16_t *data,
               int data_type,int len);
EXTERN int ReadCompressedInt8Data(mat_t *mat,z_stream *z,mat_int8_t  *data,
               int data_type,int len);
EXTERN int ReadCompressedUInt8Data(mat_t *mat,z_stream *z,mat_uint8_t  *data,
               int data_type,int len);
EXTERN int ReadCompressedCharData(mat_t *mat,z_stream *z,char *data,
               int data_type,int len);
EXTERN int ReadCompressedDataSlab1(mat_t *mat,z_stream *z,void *data,
               int class_type,int data_type,int start,int stride,int edge);
EXTERN int ReadCompressedDataSlab2(mat_t *mat,z_stream *z,void *data,
               int class_type,int data_type,int *dims,int *start,int *stride,
               int *edge);
EXTERN int ReadCompressedDataSlabN(mat_t *mat,z_stream *z,void *data,
               int class_type,int data_type,int rank,int *dims,int *start,
               int *stride,int *edge);

/*   inflate.c    */
EXTERN int InflateSkip(mat_t *mat, z_stream *z, int nbytes);
EXTERN int InflateSkip2(mat_t *mat, matvar_t *matvar, int nbytes);
EXTERN int InflateSkipData(mat_t *mat,z_stream *z,int data_type,int len);
EXTERN int InflateVarTag(mat_t *mat, matvar_t *matvar, void *buf);
EXTERN int InflateArrayFlags(mat_t *mat, matvar_t *matvar, void *buf);
EXTERN int InflateDimensions(mat_t *mat, matvar_t *matvar, void *buf);
EXTERN int InflateVarNameTag(mat_t *mat, matvar_t *matvar, void *buf);
EXTERN int InflateVarName(mat_t *mat,matvar_t *matvar,void *buf,int N);
EXTERN int InflateDataTag(mat_t *mat, matvar_t *matvar, void *buf);
EXTERN int InflateDataType(mat_t *mat, z_stream *matvar, void *buf);
EXTERN int InflateData(mat_t *mat, z_stream *z, void *buf, int nBytes);
EXTERN int InflateFieldNameLength(mat_t *mat,matvar_t *matvar,void *buf);
EXTERN int InflateFieldNamesTag(mat_t *mat,matvar_t *matvar,void *buf);
EXTERN int InflateFieldNames(mat_t *mat,matvar_t *matvar,void *buf,int nfields,
               int fieldname_length,int padding);
#endif

#endif
