/*
 * Copyright (c) 2015-2022, The matio contributors
 * Copyright (c) 2008-2014, Christopher C. Hulbert
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

#ifndef MATIO_PRIVATE_H
#define MATIO_PRIVATE_H

#if defined(__MINGW32__) && !defined(__USE_MINGW_ANSI_STDIO)
#define __USE_MINGW_ANSI_STDIO 1
#endif

#include "matioConfig.h"
#include "matio.h"

#include <stdio.h>
#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

#if defined(__BORLANDC__) || defined(__MINGW32__) || defined(_MSC_VER)
#define mat_off_t __int64
#if defined(_MSC_VER) && defined(HAVE__FSEEKI64) && defined(HAVE__FTELLI64)
#define MATIO_LFS
#define fseeko _fseeki64
#define ftello _ftelli64
#elif defined(__BORLANDC__) && defined(HAVE__FSEEKI64) && defined(HAVE__FTELLI64)
#define MATIO_LFS
#define fseeko _fseeki64
#define ftello _ftelli64
#elif !defined(HAVE_FSEEKO) && !defined(HAVE_FTELLO) && defined(HAVE_FSEEKO64) && \
    defined(HAVE_FTELLO64)
#define MATIO_LFS
#define fseeko fseeko64
#define ftello ftello64
#endif
#elif defined(_FILE_OFFSET_BITS) && _FILE_OFFSET_BITS == 64 && defined(HAVE_FSEEKO) && \
    defined(HAVE_FTELLO)
#define MATIO_LFS
#define mat_off_t off_t
#endif

#if !defined(MATIO_LFS)
#define mat_off_t long
#define ftello ftell
#define fseeko fseek
#endif

#if HAVE_ZLIB
#include <zlib.h>
#endif
#if defined(MAT73) && MAT73
#include <hdf5.h>
#endif

#ifndef EXTERN
#ifdef __cplusplus
#define EXTERN extern "C"
#else
#define EXTERN extern
#endif
#endif

#if HAVE_ZLIB
#define ZLIB_BYTE_PTR(a) ((Bytef *)(a))
#endif

#if !defined(READ_BLOCK_SIZE)
#define READ_BLOCK_SIZE (8192)
#endif

#define CAT_(X, Y) X##Y
#define CAT(X, Y) CAT_(X, Y)

/** @if mat_devman
 * @brief Matlab MAT File information
 *
 * Contains information about a Matlab MAT file
 * @ingroup mat_internal
 * @endif
 */
struct _mat_t
{
    void *fp;            /**< File pointer for the MAT file */
    char *header;        /**< MAT file header string */
    char *subsys_offset; /**< Offset */
    char *filename;      /**< Filename of the MAT file */
    int version;         /**< MAT file version */
    int byteswap;        /**< 1 if byte swapping is required, 0 otherwise */
    int mode;            /**< Access mode */
    mat_off_t bof;       /**< Beginning of file not including any header */
    size_t next_index;   /**< Index/File position of next variable to read */
    size_t num_datasets; /**< Number of datasets in the file */
#if defined(MAT73) && MAT73
    hid_t refs_id; /**< Id of the /#refs# group in HDF5 */
#endif
    char **dir; /**< Names of the datasets in the file */
};

/** @if mat_devman
 * @brief internal structure for MAT variables
 * @ingroup mat_internal
 * @endif
 */
struct matvar_internal
{
#if defined(MAT73) && MAT73
    char *hdf5_name;     /**< Name */
    hobj_ref_t hdf5_ref; /**< Reference */
    hid_t id;            /**< Id */
#endif
    mat_off_t datapos;   /**< Offset from the beginning of the MAT file to the data */
    unsigned num_fields; /**< Number of fields */
    char **fieldnames;   /**< Pointer to fieldnames */
#if HAVE_ZLIB
    z_streamp z; /**< zlib compression state */
    void *data;  /**< Inflated data array */
#endif
};

/* snprintf.c */
#if !HAVE_VSNPRINTF
int rpl_vsnprintf(char *, size_t, const char *, va_list);
#define mat_vsnprintf rpl_vsnprintf
#else
#define mat_vsnprintf vsnprintf
#endif /* !HAVE_VSNPRINTF */
#if !HAVE_SNPRINTF
int rpl_snprintf(char *, size_t, const char *, ...);
#define mat_snprintf rpl_snprintf
#else
#define mat_snprintf snprintf
#endif /* !HAVE_SNPRINTF */
#if !HAVE_VASPRINTF
int rpl_vasprintf(char **, const char *, va_list);
#define mat_vasprintf rpl_vasprintf
#else
#define mat_vasprintf vasprintf
#endif /* !HAVE_VASPRINTF */
#if !HAVE_ASPRINTF
int rpl_asprintf(char **, const char *, ...);
#define mat_asprintf rpl_asprintf
#else
#define mat_asprintf asprintf
#endif /* !HAVE_ASPRINTF */

/* endian.c */
EXTERN double Mat_doubleSwap(double *a);
EXTERN float Mat_floatSwap(float *a);
#ifdef HAVE_MAT_INT64_T
EXTERN mat_int64_t Mat_int64Swap(mat_int64_t *a);
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
EXTERN mat_uint64_t Mat_uint64Swap(mat_uint64_t *a);
#endif /* HAVE_MAT_UINT64_T */
EXTERN mat_int32_t Mat_int32Swap(mat_int32_t *a);
EXTERN mat_uint32_t Mat_uint32Swap(mat_uint32_t *a);
EXTERN mat_int16_t Mat_int16Swap(mat_int16_t *a);
EXTERN mat_uint16_t Mat_uint16Swap(mat_uint16_t *a);

/* read_data.c */
EXTERN size_t ReadDoubleData(mat_t *mat, double *data, enum matio_types data_type, size_t len);
EXTERN size_t ReadSingleData(mat_t *mat, float *data, enum matio_types data_type, size_t len);
#ifdef HAVE_MAT_INT64_T
EXTERN size_t ReadInt64Data(mat_t *mat, mat_int64_t *data, enum matio_types data_type, size_t len);
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
EXTERN size_t ReadUInt64Data(mat_t *mat, mat_uint64_t *data, enum matio_types data_type,
                             size_t len);
#endif /* HAVE_MAT_UINT64_T */
EXTERN size_t ReadInt32Data(mat_t *mat, mat_int32_t *data, enum matio_types data_type, size_t len);
EXTERN size_t ReadUInt32Data(mat_t *mat, mat_uint32_t *data, enum matio_types data_type,
                             size_t len);
EXTERN size_t ReadInt16Data(mat_t *mat, mat_int16_t *data, enum matio_types data_type, size_t len);
EXTERN size_t ReadUInt16Data(mat_t *mat, mat_uint16_t *data, enum matio_types data_type,
                             size_t len);
EXTERN size_t ReadInt8Data(mat_t *mat, mat_int8_t *data, enum matio_types data_type, size_t len);
EXTERN size_t ReadUInt8Data(mat_t *mat, mat_uint8_t *data, enum matio_types data_type, size_t len);
EXTERN size_t ReadCharData(mat_t *mat, void *_data, enum matio_types data_type, size_t len);
EXTERN int ReadDataSlab1(mat_t *mat, void *data, enum matio_classes class_type,
                         enum matio_types data_type, int start, int stride, int edge);
EXTERN int ReadDataSlab2(mat_t *mat, void *data, enum matio_classes class_type,
                         enum matio_types data_type, size_t *dims, int *start, int *stride,
                         int *edge);
EXTERN int ReadDataSlabN(mat_t *mat, void *data, enum matio_classes class_type,
                         enum matio_types data_type, int rank, size_t *dims, int *start,
                         int *stride, int *edge);
#if HAVE_ZLIB
EXTERN int ReadCompressedDoubleData(mat_t *mat, z_streamp z, double *data,
                                    enum matio_types data_type, int len);
EXTERN int ReadCompressedSingleData(mat_t *mat, z_streamp z, float *data,
                                    enum matio_types data_type, int len);
#ifdef HAVE_MAT_INT64_T
EXTERN int ReadCompressedInt64Data(mat_t *mat, z_streamp z, mat_int64_t *data,
                                   enum matio_types data_type, int len);
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
EXTERN int ReadCompressedUInt64Data(mat_t *mat, z_streamp z, mat_uint64_t *data,
                                    enum matio_types data_type, int len);
#endif /* HAVE_MAT_UINT64_T */
EXTERN int ReadCompressedInt32Data(mat_t *mat, z_streamp z, mat_int32_t *data,
                                   enum matio_types data_type, int len);
EXTERN int ReadCompressedUInt32Data(mat_t *mat, z_streamp z, mat_uint32_t *data,
                                    enum matio_types data_type, int len);
EXTERN int ReadCompressedInt16Data(mat_t *mat, z_streamp z, mat_int16_t *data,
                                   enum matio_types data_type, int len);
EXTERN int ReadCompressedUInt16Data(mat_t *mat, z_streamp z, mat_uint16_t *data,
                                    enum matio_types data_type, int len);
EXTERN int ReadCompressedInt8Data(mat_t *mat, z_streamp z, mat_int8_t *data,
                                  enum matio_types data_type, int len);
EXTERN int ReadCompressedUInt8Data(mat_t *mat, z_streamp z, mat_uint8_t *data,
                                   enum matio_types data_type, int len);
EXTERN int ReadCompressedCharData(mat_t *mat, z_streamp z, void *data, enum matio_types data_type,
                                  size_t len);
EXTERN int ReadCompressedDataSlab1(mat_t *mat, z_streamp z, void *data,
                                   enum matio_classes class_type, enum matio_types data_type,
                                   int start, int stride, int edge);
EXTERN int ReadCompressedDataSlab2(mat_t *mat, z_streamp z, void *data,
                                   enum matio_classes class_type, enum matio_types data_type,
                                   size_t *dims, int *start, int *stride, int *edge);
EXTERN int ReadCompressedDataSlabN(mat_t *mat, z_streamp z, void *data,
                                   enum matio_classes class_type, enum matio_types data_type,
                                   int rank, size_t *dims, int *start, int *stride, int *edge);

/* inflate.c */
EXTERN int InflateSkip(mat_t *mat, z_streamp z, int nBytes, size_t *bytesread);
EXTERN int InflateSkipData(mat_t *mat, z_streamp z, enum matio_types data_type, int len);
EXTERN int InflateRankDims(mat_t *mat, z_streamp z, void *buf, size_t nBytes, mat_uint32_t **dims,
                           size_t *bytesread);
EXTERN int Inflate(mat_t *mat, z_streamp z, void *buf, unsigned int nBytes, size_t *bytesread);
EXTERN int InflateData(mat_t *mat, z_streamp z, void *buf, unsigned int nBytes);
#endif

/* mat.c */
EXTERN mat_complex_split_t *ComplexMalloc(size_t nbytes);
EXTERN void ComplexFree(mat_complex_split_t *complex_data);
EXTERN enum matio_types ClassType2DataType(enum matio_classes class_type);
EXTERN int Add(size_t *res, size_t a, size_t b);
EXTERN int Mul(size_t *res, size_t a, size_t b);
EXTERN int Mat_MulDims(const matvar_t *matvar, size_t *nelems);
EXTERN int Read(void *buf, size_t size, size_t count, FILE *fp, size_t *bytesread);
EXTERN int IsEndOfFile(FILE *fp, mat_off_t *fpos);

/* io.c */
#if defined(_WIN32) && defined(_MSC_VER)
EXTERN wchar_t *utf82u(const char *src);
#endif

#endif
