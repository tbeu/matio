/** @file read_data.c
 * Matlab MAT version 5 file functions
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

/* FIXME: Implement Unicode support */
#include "matio_private.h"
#if HAVE_ZLIB
#include <zlib.h>
#endif
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <time.h>

#define READ_DATA_NOSWAP(T)                                           \
    do {                                                              \
        const size_t block_size = READ_BLOCK_SIZE / data_size;        \
        if ( len <= block_size ) {                                    \
            readcount = fread(v, data_size, len, (FILE *)mat->fp);    \
            if ( readcount == len ) {                                 \
                for ( i = 0; i < len; i++ ) {                         \
                    data[i] = (T)v[i];                                \
                }                                                     \
            }                                                         \
        } else {                                                      \
            size_t j;                                                 \
            int err_ = 0;                                             \
            readcount = 0;                                            \
            for ( i = 0; i < len - block_size; i += block_size ) {    \
                j = fread(v, data_size, block_size, (FILE *)mat->fp); \
                readcount += j;                                       \
                if ( j == block_size ) {                              \
                    for ( j = 0; j < block_size; j++ ) {              \
                        data[i + j] = (T)v[j];                        \
                    }                                                 \
                } else {                                              \
                    err_ = 1;                                         \
                    break;                                            \
                }                                                     \
            }                                                         \
            if ( 0 == err_ && len > i ) {                             \
                j = fread(v, data_size, len - i, (FILE *)mat->fp);    \
                readcount += j;                                       \
                if ( j == len - i ) {                                 \
                    for ( j = 0; j < len - i; j++ ) {                 \
                        data[i + j] = (T)v[j];                        \
                    }                                                 \
                }                                                     \
            }                                                         \
        }                                                             \
    } while ( 0 )

#define READ_DATA(T, SwapFunc)                                            \
    do {                                                                  \
        if ( mat->byteswap ) {                                            \
            const size_t block_size = READ_BLOCK_SIZE / data_size;        \
            if ( len <= block_size ) {                                    \
                readcount = fread(v, data_size, len, (FILE *)mat->fp);    \
                if ( readcount == len ) {                                 \
                    for ( i = 0; i < len; i++ ) {                         \
                        data[i] = (T)SwapFunc(&v[i]);                     \
                    }                                                     \
                }                                                         \
            } else {                                                      \
                size_t j;                                                 \
                int err_ = 0;                                             \
                readcount = 0;                                            \
                for ( i = 0; i < len - block_size; i += block_size ) {    \
                    j = fread(v, data_size, block_size, (FILE *)mat->fp); \
                    readcount += j;                                       \
                    if ( j == block_size ) {                              \
                        for ( j = 0; j < block_size; j++ ) {              \
                            data[i + j] = (T)SwapFunc(&v[j]);             \
                        }                                                 \
                    } else {                                              \
                        err_ = 1;                                         \
                        break;                                            \
                    }                                                     \
                }                                                         \
                if ( 0 == err_ && len > i ) {                             \
                    j = fread(v, data_size, len - i, (FILE *)mat->fp);    \
                    readcount += j;                                       \
                    if ( j == len - i ) {                                 \
                        for ( j = 0; j < len - i; j++ ) {                 \
                            data[i + j] = (T)SwapFunc(&v[j]);             \
                        }                                                 \
                    }                                                     \
                }                                                         \
            }                                                             \
        } else {                                                          \
            READ_DATA_NOSWAP(T);                                          \
        }                                                                 \
    } while ( 0 )

#if HAVE_ZLIB
#define READ_COMPRESSED_DATA_NOSWAP(T)                         \
    do {                                                       \
        const size_t block_size = READ_BLOCK_SIZE / data_size; \
        if ( len <= block_size ) {                             \
            InflateData(mat, z, v, len *data_size);            \
            for ( i = 0; i < len; i++ ) {                      \
                data[i] = (T)v[i];                             \
            }                                                  \
        } else {                                               \
            mat_uint32_t j;                                    \
            len -= block_size;                                 \
            for ( i = 0; i < len; i += block_size ) {          \
                InflateData(mat, z, v, block_size *data_size); \
                for ( j = 0; j < block_size; j++ ) {           \
                    data[i + j] = (T)v[j];                     \
                }                                              \
            }                                                  \
            len -= (i - block_size);                           \
            InflateData(mat, z, v, len *data_size);            \
            for ( j = 0; j < len; j++ ) {                      \
                data[i + j] = (T)v[j];                         \
            }                                                  \
        }                                                      \
    } while ( 0 )

#define READ_COMPRESSED_DATA(T, SwapFunc)                          \
    do {                                                           \
        if ( mat->byteswap ) {                                     \
            const size_t block_size = READ_BLOCK_SIZE / data_size; \
            if ( len <= block_size ) {                             \
                InflateData(mat, z, v, len *data_size);            \
                for ( i = 0; i < len; i++ ) {                      \
                    data[i] = (T)SwapFunc(&v[i]);                  \
                }                                                  \
            } else {                                               \
                mat_uint32_t j;                                    \
                len -= block_size;                                 \
                for ( i = 0; i < len; i += block_size ) {          \
                    InflateData(mat, z, v, block_size *data_size); \
                    for ( j = 0; j < block_size; j++ ) {           \
                        data[i + j] = (T)SwapFunc(&v[j]);          \
                    }                                              \
                }                                                  \
                len -= (i - block_size);                           \
                InflateData(mat, z, v, len *data_size);            \
                for ( j = 0; j < len; j++ ) {                      \
                    data[i + j] = (T)SwapFunc(&v[j]);              \
                }                                                  \
            }                                                      \
        } else {                                                   \
            READ_COMPRESSED_DATA_NOSWAP(T);                        \
        }                                                          \
    } while ( 0 )

#endif

/*
 * --------------------------------------------------------------------------
 *    Routines to read data of any type into arrays of a specific type
 * --------------------------------------------------------------------------
 */

/** @cond mat_devman */

#define READ_TYPE_DOUBLE 1
#define READ_TYPE_SINGLE 2
#define READ_TYPE_INT64 3
#define READ_TYPE_UINT64 4
#define READ_TYPE_INT32 5
#define READ_TYPE_UINT32 6
#define READ_TYPE_INT16 7
#define READ_TYPE_UINT16 8
#define READ_TYPE_INT8 9
#define READ_TYPE_UINT8 10

#define READ_TYPE double
#define READ_TYPE_TYPE READ_TYPE_DOUBLE
#define READ_TYPED_FUNC1 ReadDoubleData
#define READ_TYPED_FUNC2 ReadCompressedDoubleData
#include "read_data_impl.h"
#undef READ_TYPE
#undef READ_TYPE_TYPE
#undef READ_TYPED_FUNC1
#undef READ_TYPED_FUNC2

#define READ_TYPE float
#define READ_TYPE_TYPE READ_TYPE_SINGLE
#define READ_TYPED_FUNC1 ReadSingleData
#define READ_TYPED_FUNC2 ReadCompressedSingleData
#include "read_data_impl.h"
#undef READ_TYPE
#undef READ_TYPE_TYPE
#undef READ_TYPED_FUNC1
#undef READ_TYPED_FUNC2

#ifdef HAVE_MAT_INT64_T
#define READ_TYPE mat_int64_t
#define READ_TYPE_TYPE READ_TYPE_INT64
#define READ_TYPED_FUNC1 ReadInt64Data
#define READ_TYPED_FUNC2 ReadCompressedInt64Data
#include "read_data_impl.h"
#undef READ_TYPE
#undef READ_TYPE_TYPE
#undef READ_TYPED_FUNC1
#undef READ_TYPED_FUNC2
#endif /* HAVE_MAT_INT64_T */

#ifdef HAVE_MAT_UINT64_T
#define READ_TYPE mat_uint64_t
#define READ_TYPE_TYPE READ_TYPE_UINT64
#define READ_TYPED_FUNC1 ReadUInt64Data
#define READ_TYPED_FUNC2 ReadCompressedUInt64Data
#include "read_data_impl.h"
#undef READ_TYPE
#undef READ_TYPE_TYPE
#undef READ_TYPED_FUNC1
#undef READ_TYPED_FUNC2
#endif /* HAVE_MAT_UINT64_T */

#define READ_TYPE mat_int32_t
#define READ_TYPE_TYPE READ_TYPE_INT32
#define READ_TYPED_FUNC1 ReadInt32Data
#define READ_TYPED_FUNC2 ReadCompressedInt32Data
#include "read_data_impl.h"
#undef READ_TYPE
#undef READ_TYPE_TYPE
#undef READ_TYPED_FUNC1
#undef READ_TYPED_FUNC2

#define READ_TYPE mat_uint32_t
#define READ_TYPE_TYPE READ_TYPE_UINT32
#define READ_TYPED_FUNC1 ReadUInt32Data
#define READ_TYPED_FUNC2 ReadCompressedUInt32Data
#include "read_data_impl.h"
#undef READ_TYPE
#undef READ_TYPE_TYPE
#undef READ_TYPED_FUNC1
#undef READ_TYPED_FUNC2

#define READ_TYPE mat_int16_t
#define READ_TYPE_TYPE READ_TYPE_INT16
#define READ_TYPED_FUNC1 ReadInt16Data
#define READ_TYPED_FUNC2 ReadCompressedInt16Data
#include "read_data_impl.h"
#undef READ_TYPE
#undef READ_TYPE_TYPE
#undef READ_TYPED_FUNC1
#undef READ_TYPED_FUNC2

#define READ_TYPE mat_uint16_t
#define READ_TYPE_TYPE READ_TYPE_UINT16
#define READ_TYPED_FUNC1 ReadUInt16Data
#define READ_TYPED_FUNC2 ReadCompressedUInt16Data
#include "read_data_impl.h"
#undef READ_TYPE
#undef READ_TYPE_TYPE
#undef READ_TYPED_FUNC1
#undef READ_TYPED_FUNC2

#define READ_TYPE mat_int8_t
#define READ_TYPE_TYPE READ_TYPE_INT8
#define READ_TYPED_FUNC1 ReadInt8Data
#define READ_TYPED_FUNC2 ReadCompressedInt8Data
#include "read_data_impl.h"
#undef READ_TYPE
#undef READ_TYPE_TYPE
#undef READ_TYPED_FUNC1
#undef READ_TYPED_FUNC2

#define READ_TYPE mat_uint8_t
#define READ_TYPE_TYPE READ_TYPE_UINT8
#define READ_TYPED_FUNC1 ReadUInt8Data
#define READ_TYPED_FUNC2 ReadCompressedUInt8Data
#include "read_data_impl.h"
#undef READ_TYPE
#undef READ_TYPE_TYPE
#undef READ_TYPED_FUNC1
#undef READ_TYPED_FUNC2

#if HAVE_ZLIB
/** @brief Reads data of type @c data_type into a char type
 *
 * Reads from the MAT file @c len compressed elements of data type @c data_type
 * storing them as char's in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param z Pointer to the zlib stream for inflation
 * @param data Pointer to store the output char values (len*sizeof(char))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadCompressedCharData(mat_t *mat, z_streamp z, void *data, enum matio_types data_type, size_t len)
{
    size_t nBytes = 0;
    int err;

    if ( mat == NULL || data == NULL || mat->fp == NULL )
        return 0;

    err = Mul(&nBytes, len, Mat_SizeOf(data_type));
    if ( err ) {
        return 0;
    }

    switch ( data_type ) {
        case MAT_T_UINT8:
        case MAT_T_UTF8:
            err = InflateData(mat, z, data, (mat_uint32_t)nBytes);
            break;
        case MAT_T_UINT16:
        case MAT_T_UTF16:
            err = InflateData(mat, z, data, (mat_uint32_t)nBytes);
            if ( mat->byteswap ) {
                mat_uint16_t *ptr = (mat_uint16_t *)data;
                size_t i;
                for ( i = 0; i < len; i++ ) {
                    Mat_uint16Swap(&ptr[i]);
                }
            }
            break;
        default:
            Mat_Warning(
                "ReadCompressedCharData: %d is not a supported data "
                "type for character data",
                data_type);
            break;
    }

    if ( err ) {
        nBytes = 0;
    }
    return (int)nBytes;
}
#endif

size_t
ReadCharData(mat_t *mat, void *_data, enum matio_types data_type, size_t len)
{
    size_t nBytes = 0;
    int err = 0;
    size_t data_size;

    if ( mat == NULL || _data == NULL || mat->fp == NULL )
        return 0;

    data_size = Mat_SizeOf(data_type);

    switch ( data_type ) {
        case MAT_T_UINT8:
        case MAT_T_UTF8: {
            err = Read(_data, data_size, len, (FILE *)mat->fp, &nBytes);
            break;
        }
        case MAT_T_UINT16:
        case MAT_T_UTF16: {
            size_t i, readcount;
            mat_uint16_t *data = (mat_uint16_t *)_data;
            mat_uint16_t v[READ_BLOCK_SIZE / sizeof(mat_uint16_t)];
            READ_DATA(mat_uint16_t, Mat_uint16Swap);
            err = Mul(&nBytes, readcount, data_size);
            break;
        }
        default:
            Mat_Warning(
                "ReadCharData: %d is not a supported data type for "
                "character data",
                data_type);
            break;
    }
    if ( err ) {
        nBytes = 0;
    }
    return nBytes;
}

#undef READ_DATA
#undef READ_DATA_NOSWAP

/*
 *-------------------------------------------------------------------
 *  Routines to read "slabs" of data
 *-------------------------------------------------------------------
 */

#define READ_DATA_SLABN_RANK_LOOP                                                                \
    do {                                                                                         \
        for ( j = 1; j < rank; j++ ) {                                                           \
            cnt[j]++;                                                                            \
            if ( (cnt[j] % edge[j]) == 0 ) {                                                     \
                cnt[j] = 0;                                                                      \
                if ( (I % dimp[j]) != 0 ) {                                                      \
                    (void)fseek((FILE *)mat->fp,                                                 \
                                data_size *(dimp[j] - (I % dimp[j]) + dimp[j - 1] * start[j]),   \
                                SEEK_CUR);                                                       \
                    I += dimp[j] - (I % dimp[j]) + (ptrdiff_t)dimp[j - 1] * start[j];            \
                } else if ( start[j] ) {                                                         \
                    (void)fseek((FILE *)mat->fp, data_size *(dimp[j - 1] * start[j]), SEEK_CUR); \
                    I += (ptrdiff_t)dimp[j - 1] * start[j];                                      \
                }                                                                                \
            } else {                                                                             \
                I += inc[j];                                                                     \
                (void)fseek((FILE *)mat->fp, data_size *inc[j], SEEK_CUR);                       \
                break;                                                                           \
            }                                                                                    \
        }                                                                                        \
    } while ( 0 )

#define READ_DATA_SLABN(ReadDataFunc)                                                              \
    do {                                                                                           \
        inc[0] = stride[0] - 1;                                                                    \
        dimp[0] = dims[0];                                                                         \
        N = edge[0];                                                                               \
        I = 0; /* start[0]; */                                                                     \
        for ( i = 1; i < rank; i++ ) {                                                             \
            inc[i] = stride[i] - 1;                                                                \
            dimp[i] = dims[i - 1];                                                                 \
            for ( j = i; j--; ) {                                                                  \
                inc[i] *= dims[j];                                                                 \
                dimp[i] *= dims[j + 1];                                                            \
            }                                                                                      \
            N *= edge[i];                                                                          \
            I += (ptrdiff_t)dimp[i - 1] * start[i];                                                \
        }                                                                                          \
        (void)fseek((FILE *)mat->fp, I *data_size, SEEK_CUR);                                      \
        if ( stride[0] == 1 ) {                                                                    \
            for ( i = 0; i < N; i += edge[0] ) {                                                   \
                if ( start[0] ) {                                                                  \
                    (void)fseek((FILE *)mat->fp, start[0] * data_size, SEEK_CUR);                  \
                    I += start[0];                                                                 \
                }                                                                                  \
                ReadDataFunc(mat, ptr + i, data_type, edge[0]);                                    \
                I += dims[0] - start[0];                                                           \
                (void)fseek((FILE *)mat->fp, data_size *(dims[0] - edge[0] - start[0]), SEEK_CUR); \
                READ_DATA_SLABN_RANK_LOOP;                                                         \
            }                                                                                      \
        } else {                                                                                   \
            for ( i = 0; i < N; i += edge[0] ) {                                                   \
                if ( start[0] ) {                                                                  \
                    (void)fseek((FILE *)mat->fp, start[0] * data_size, SEEK_CUR);                  \
                    I += start[0];                                                                 \
                }                                                                                  \
                for ( j = 0; j < edge[0]; j++ ) {                                                  \
                    ReadDataFunc(mat, ptr + i + j, data_type, 1);                                  \
                    (void)fseek((FILE *)mat->fp, data_size *(stride[0] - 1), SEEK_CUR);            \
                    I += stride[0];                                                                \
                }                                                                                  \
                I += dims[0] - (ptrdiff_t)edge[0] * stride[0] - start[0];                          \
                (void)fseek((FILE *)mat->fp,                                                       \
                            data_size *(dims[0] - (ptrdiff_t)edge[0] * stride[0] - start[0]),      \
                            SEEK_CUR);                                                             \
                READ_DATA_SLABN_RANK_LOOP;                                                         \
            }                                                                                      \
        }                                                                                          \
    } while ( 0 )

/** @brief Reads data of type @c data_type by user-defined dimensions
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param data Pointer to store the output data
 * @param class_type Type of data class (matio_classes enumerations)
 * @param data_type Datatype of the stored data (matio_types enumerations)
 * @param rank Number of dimensions in the data
 * @param dims Dimensions of the data
 * @param start Index to start reading data in each dimension
 * @param stride Read every @c stride elements in each dimension
 * @param edge Number of elements to read in each dimension
 * @retval Number of bytes read from the file, or -1 on error
 */
int
ReadDataSlabN(mat_t *mat, void *data, enum matio_classes class_type, enum matio_types data_type,
              int rank, size_t *dims, int *start, int *stride, int *edge)
{
    int nBytes = 0, i, j, N, I = 0;
    int inc[10] =
        {
            0,
        },
        cnt[10] =
            {
                0,
            },
        dimp[10] = {
            0,
        };
    size_t data_size;

    if ( (mat == NULL) || (data == NULL) || (mat->fp == NULL) || (start == NULL) ||
         (stride == NULL) || (edge == NULL) ) {
        return -1;
    } else if ( rank > 10 ) {
        return -1;
    }

    data_size = Mat_SizeOf(data_type);

    switch ( class_type ) {
        case MAT_C_DOUBLE: {
            double *ptr = (double *)data;
            READ_DATA_SLABN(ReadDoubleData);
            break;
        }
        case MAT_C_SINGLE: {
            float *ptr = (float *)data;
            READ_DATA_SLABN(ReadSingleData);
            break;
        }
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64: {
            mat_int64_t *ptr = (mat_int64_t *)data;
            READ_DATA_SLABN(ReadInt64Data);
            break;
        }
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64: {
            mat_uint64_t *ptr = (mat_uint64_t *)data;
            READ_DATA_SLABN(ReadUInt64Data);
            break;
        }
#endif /* HAVE_MAT_UINT64_T */
        case MAT_C_INT32: {
            mat_int32_t *ptr = (mat_int32_t *)data;
            READ_DATA_SLABN(ReadInt32Data);
            break;
        }
        case MAT_C_UINT32: {
            mat_uint32_t *ptr = (mat_uint32_t *)data;
            READ_DATA_SLABN(ReadUInt32Data);
            break;
        }
        case MAT_C_INT16: {
            mat_int16_t *ptr = (mat_int16_t *)data;
            READ_DATA_SLABN(ReadInt16Data);
            break;
        }
        case MAT_C_UINT16: {
            mat_uint16_t *ptr = (mat_uint16_t *)data;
            READ_DATA_SLABN(ReadUInt16Data);
            break;
        }
        case MAT_C_INT8: {
            mat_int8_t *ptr = (mat_int8_t *)data;
            READ_DATA_SLABN(ReadInt8Data);
            break;
        }
        case MAT_C_UINT8: {
            mat_uint8_t *ptr = (mat_uint8_t *)data;
            READ_DATA_SLABN(ReadUInt8Data);
            break;
        }
        default:
            nBytes = 0;
    }
    return nBytes;
}

#undef READ_DATA_SLABN
#undef READ_DATA_SLABN_RANK_LOOP

#if HAVE_ZLIB
#define READ_COMPRESSED_DATA_SLABN_RANK_LOOP                                           \
    do {                                                                               \
        for ( j = 1; j < rank; j++ ) {                                                 \
            cnt[j]++;                                                                  \
            if ( (cnt[j] % edge[j]) == 0 ) {                                           \
                cnt[j] = 0;                                                            \
                if ( (I % dimp[j]) != 0 ) {                                            \
                    InflateSkipData(mat, &z_copy, data_type,                           \
                                    dimp[j] - (I % dimp[j]) + dimp[j - 1] * start[j]); \
                    I += dimp[j] - (I % dimp[j]) + (ptrdiff_t)dimp[j - 1] * start[j];  \
                } else if ( start[j] ) {                                               \
                    InflateSkipData(mat, &z_copy, data_type, dimp[j - 1] * start[j]);  \
                    I += (ptrdiff_t)dimp[j - 1] * start[j];                            \
                }                                                                      \
            } else {                                                                   \
                if ( inc[j] ) {                                                        \
                    I += inc[j];                                                       \
                    InflateSkipData(mat, &z_copy, data_type, inc[j]);                  \
                }                                                                      \
                break;                                                                 \
            }                                                                          \
        }                                                                              \
    } while ( 0 )

#define READ_COMPRESSED_DATA_SLABN(ReadDataFunc)                                                \
    do {                                                                                        \
        inc[0] = stride[0] - 1;                                                                 \
        dimp[0] = dims[0];                                                                      \
        N = edge[0];                                                                            \
        I = 0;                                                                                  \
        for ( i = 1; i < rank; i++ ) {                                                          \
            inc[i] = stride[i] - 1;                                                             \
            dimp[i] = dims[i - 1];                                                              \
            for ( j = i; j--; ) {                                                               \
                inc[i] *= dims[j];                                                              \
                dimp[i] *= dims[j + 1];                                                         \
            }                                                                                   \
            N *= edge[i];                                                                       \
            I += (ptrdiff_t)dimp[i - 1] * start[i];                                             \
        }                                                                                       \
        /* Skip all data to the starting indices */                                             \
        InflateSkipData(mat, &z_copy, data_type, I);                                            \
        if ( stride[0] == 1 ) {                                                                 \
            for ( i = 0; i < N; i += edge[0] ) {                                                \
                if ( start[0] ) {                                                               \
                    InflateSkipData(mat, &z_copy, data_type, start[0]);                         \
                    I += start[0];                                                              \
                }                                                                               \
                ReadDataFunc(mat, &z_copy, ptr + i, data_type, edge[0]);                        \
                InflateSkipData(mat, &z_copy, data_type, dims[0] - start[0] - edge[0]);         \
                I += dims[0] - start[0];                                                        \
                READ_COMPRESSED_DATA_SLABN_RANK_LOOP;                                           \
            }                                                                                   \
        } else {                                                                                \
            for ( i = 0; i < N; i += edge[0] ) {                                                \
                if ( start[0] ) {                                                               \
                    InflateSkipData(mat, &z_copy, data_type, start[0]);                         \
                    I += start[0];                                                              \
                }                                                                               \
                for ( j = 0; j < edge[0] - 1; j++ ) {                                           \
                    ReadDataFunc(mat, &z_copy, ptr + i + j, data_type, 1);                      \
                    InflateSkipData(mat, &z_copy, data_type, (stride[0] - 1));                  \
                    I += stride[0];                                                             \
                }                                                                               \
                ReadDataFunc(mat, &z_copy, ptr + i + j, data_type, 1);                          \
                I += dims[0] - (ptrdiff_t)(edge[0] - 1) * stride[0] - start[0];                 \
                InflateSkipData(mat, &z_copy, data_type,                                        \
                                dims[0] - (ptrdiff_t)(edge[0] - 1) * stride[0] - start[0] - 1); \
                READ_COMPRESSED_DATA_SLABN_RANK_LOOP;                                           \
            }                                                                                   \
        }                                                                                       \
    } while ( 0 )

/** @brief Reads data of type @c data_type by user-defined dimensions
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param z zlib compression stream
 * @param data Pointer to store the output data
 * @param class_type Type of data class (matio_classes enumerations)
 * @param data_type Datatype of the stored data (matio_types enumerations)
 * @param rank Number of dimensions in the data
 * @param dims Dimensions of the data
 * @param start Index to start reading data in each dimension
 * @param stride Read every @c stride elements in each dimension
 * @param edge Number of elements to read in each dimension
 * @retval Number of bytes read from the file, or -1 on error
 */
int
ReadCompressedDataSlabN(mat_t *mat, z_streamp z, void *data, enum matio_classes class_type,
                        enum matio_types data_type, int rank, size_t *dims, int *start, int *stride,
                        int *edge)
{
    int nBytes = 0, i, j, N, I = 0, err;
    int inc[10] =
        {
            0,
        },
        cnt[10] =
            {
                0,
            },
        dimp[10] = {
            0,
        };
    z_stream z_copy = {
        0,
    };

    if ( (mat == NULL) || (data == NULL) || (mat->fp == NULL) || (start == NULL) ||
         (stride == NULL) || (edge == NULL) ) {
        return 0;
    } else if ( rank > 10 ) {
        return 0;
    }

    err = inflateCopy(&z_copy, z);
    if ( err != Z_OK ) {
        Mat_Critical("inflateCopy returned error %s", zError(err));
        return -1;
    }
    switch ( class_type ) {
        case MAT_C_DOUBLE: {
            double *ptr = (double *)data;
            READ_COMPRESSED_DATA_SLABN(ReadCompressedDoubleData);
            break;
        }
        case MAT_C_SINGLE: {
            float *ptr = (float *)data;
            READ_COMPRESSED_DATA_SLABN(ReadCompressedSingleData);
            break;
        }
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64: {
            mat_int64_t *ptr = (mat_int64_t *)data;
            READ_COMPRESSED_DATA_SLABN(ReadCompressedInt64Data);
            break;
        }
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64: {
            mat_uint64_t *ptr = (mat_uint64_t *)data;
            READ_COMPRESSED_DATA_SLABN(ReadCompressedUInt64Data);
            break;
        }
#endif /* HAVE_MAT_UINT64_T */
        case MAT_C_INT32: {
            mat_int32_t *ptr = (mat_int32_t *)data;
            READ_COMPRESSED_DATA_SLABN(ReadCompressedInt32Data);
            break;
        }
        case MAT_C_UINT32: {
            mat_uint32_t *ptr = (mat_uint32_t *)data;
            READ_COMPRESSED_DATA_SLABN(ReadCompressedUInt32Data);
            break;
        }
        case MAT_C_INT16: {
            mat_int16_t *ptr = (mat_int16_t *)data;
            READ_COMPRESSED_DATA_SLABN(ReadCompressedInt16Data);
            break;
        }
        case MAT_C_UINT16: {
            mat_uint16_t *ptr = (mat_uint16_t *)data;
            READ_COMPRESSED_DATA_SLABN(ReadCompressedUInt16Data);
            break;
        }
        case MAT_C_INT8: {
            mat_int8_t *ptr = (mat_int8_t *)data;
            READ_COMPRESSED_DATA_SLABN(ReadCompressedInt8Data);
            break;
        }
        case MAT_C_UINT8: {
            mat_uint8_t *ptr = (mat_uint8_t *)data;
            READ_COMPRESSED_DATA_SLABN(ReadCompressedUInt8Data);
            break;
        }
        default:
            nBytes = 0;
    }
    inflateEnd(&z_copy);
    return nBytes;
}

#undef READ_COMPRESSED_DATA_SLABN
#undef READ_COMPRESSED_DATA_SLABN_RANK_LOOP
#endif

#define READ_DATA_SLAB1(ReadDataFunc)                                  \
    do {                                                               \
        if ( !stride ) {                                               \
            bytesread += ReadDataFunc(mat, ptr, data_type, edge);      \
        } else {                                                       \
            for ( i = 0; i < edge; i++ ) {                             \
                bytesread += ReadDataFunc(mat, ptr + i, data_type, 1); \
                (void)fseek((FILE *)mat->fp, stride, SEEK_CUR);        \
            }                                                          \
        }                                                              \
    } while ( 0 )

/** @brief Reads data of type @c data_type by user-defined dimensions for 1-D
 *         data
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param data Pointer to store the output data
 * @param class_type Type of data class (matio_classes enumerations)
 * @param data_type Datatype of the stored data (matio_types enumerations)
 * @param start Index to start reading data
 * @param stride Read every @c stride elements
 * @param edge Number of elements to read
 * @return Number of bytes read from the file, or -1 on error
 */
int
ReadDataSlab1(mat_t *mat, void *data, enum matio_classes class_type, enum matio_types data_type,
              int start, int stride, int edge)
{
    int i;
    size_t data_size;
    int bytesread = 0;

    data_size = Mat_SizeOf(data_type);
    (void)fseek((FILE *)mat->fp, start * data_size, SEEK_CUR);
    stride = data_size * (stride - 1);

    switch ( class_type ) {
        case MAT_C_DOUBLE: {
            double *ptr = (double *)data;
            READ_DATA_SLAB1(ReadDoubleData);
            break;
        }
        case MAT_C_SINGLE: {
            float *ptr = (float *)data;
            READ_DATA_SLAB1(ReadSingleData);
            break;
        }
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64: {
            mat_int64_t *ptr = (mat_int64_t *)data;
            READ_DATA_SLAB1(ReadInt64Data);
            break;
        }
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64: {
            mat_uint64_t *ptr = (mat_uint64_t *)data;
            READ_DATA_SLAB1(ReadUInt64Data);
            break;
        }
#endif /* HAVE_MAT_UINT64_T */
        case MAT_C_INT32: {
            mat_int32_t *ptr = (mat_int32_t *)data;
            READ_DATA_SLAB1(ReadInt32Data);
            break;
        }
        case MAT_C_UINT32: {
            mat_uint32_t *ptr = (mat_uint32_t *)data;
            READ_DATA_SLAB1(ReadUInt32Data);
            break;
        }
        case MAT_C_INT16: {
            mat_int16_t *ptr = (mat_int16_t *)data;
            READ_DATA_SLAB1(ReadInt16Data);
            break;
        }
        case MAT_C_UINT16: {
            mat_uint16_t *ptr = (mat_uint16_t *)data;
            READ_DATA_SLAB1(ReadUInt16Data);
            break;
        }
        case MAT_C_INT8: {
            mat_int8_t *ptr = (mat_int8_t *)data;
            READ_DATA_SLAB1(ReadInt8Data);
            break;
        }
        case MAT_C_UINT8: {
            mat_uint8_t *ptr = (mat_uint8_t *)data;
            READ_DATA_SLAB1(ReadUInt8Data);
            break;
        }
        default:
            return 0;
    }

    return bytesread;
}

#undef READ_DATA_SLAB1

#define READ_DATA_SLAB2(ReadDataFunc)                                                     \
    do {                                                                                  \
        /* If stride[0] is 1 and stride[1] is 1, we are reading all of the */             \
        /* data so get rid of the loops. */                                               \
        if ( (stride[0] == 1 && (size_t)edge[0] == dims[0]) && (stride[1] == 1) ) {       \
            ReadDataFunc(mat, ptr, data_type, (ptrdiff_t)edge[0] * edge[1]);              \
        } else {                                                                          \
            row_stride = (long)(stride[0] - 1) * data_size;                               \
            col_stride = (long)stride[1] * dims[0] * data_size;                           \
            pos = ftell((FILE *)mat->fp);                                                 \
            if ( pos == -1L ) {                                                           \
                Mat_Critical("Couldn't determine file position");                         \
                return -1;                                                                \
            }                                                                             \
            (void)fseek((FILE *)mat->fp, (long)start[1] * dims[0] * data_size, SEEK_CUR); \
            for ( i = 0; i < edge[1]; i++ ) {                                             \
                pos = ftell((FILE *)mat->fp);                                             \
                if ( pos == -1L ) {                                                       \
                    Mat_Critical("Couldn't determine file position");                     \
                    return -1;                                                            \
                }                                                                         \
                (void)fseek((FILE *)mat->fp, (long)start[0] * data_size, SEEK_CUR);       \
                for ( j = 0; j < edge[0]; j++ ) {                                         \
                    ReadDataFunc(mat, ptr++, data_type, 1);                               \
                    (void)fseek((FILE *)mat->fp, row_stride, SEEK_CUR);                   \
                }                                                                         \
                pos2 = ftell((FILE *)mat->fp);                                            \
                if ( pos2 == -1L ) {                                                      \
                    Mat_Critical("Couldn't determine file position");                     \
                    return -1;                                                            \
                }                                                                         \
                pos += col_stride - pos2;                                                 \
                (void)fseek((FILE *)mat->fp, pos, SEEK_CUR);                              \
            }                                                                             \
        }                                                                                 \
    } while ( 0 )

/** @brief Reads data of type @c data_type by user-defined dimensions for 2-D
 *         data
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param data Pointer to store the output data
 * @param class_type Type of data class (matio_classes enumerations)
 * @param data_type Datatype of the stored data (matio_types enumerations)
 * @param dims Dimensions of the data
 * @param start Index to start reading data in each dimension
 * @param stride Read every @c stride elements in each dimension
 * @param edge Number of elements to read in each dimension
 * @retval Number of bytes read from the file, or -1 on error
 */
int
ReadDataSlab2(mat_t *mat, void *data, enum matio_classes class_type, enum matio_types data_type,
              size_t *dims, int *start, int *stride, int *edge)
{
    int nBytes = 0, data_size, i, j;
    long pos, row_stride, col_stride, pos2;

    if ( (mat == NULL) || (data == NULL) || (mat->fp == NULL) || (start == NULL) ||
         (stride == NULL) || (edge == NULL) ) {
        return 0;
    }

    data_size = Mat_SizeOf(data_type);

    switch ( class_type ) {
        case MAT_C_DOUBLE: {
            double *ptr = (double *)data;
            READ_DATA_SLAB2(ReadDoubleData);
            break;
        }
        case MAT_C_SINGLE: {
            float *ptr = (float *)data;
            READ_DATA_SLAB2(ReadSingleData);
            break;
        }
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64: {
            mat_int64_t *ptr = (mat_int64_t *)data;
            READ_DATA_SLAB2(ReadInt64Data);
            break;
        }
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64: {
            mat_uint64_t *ptr = (mat_uint64_t *)data;
            READ_DATA_SLAB2(ReadUInt64Data);
            break;
        }
#endif /* HAVE_MAT_UINT64_T */
        case MAT_C_INT32: {
            mat_int32_t *ptr = (mat_int32_t *)data;
            READ_DATA_SLAB2(ReadInt32Data);
            break;
        }
        case MAT_C_UINT32: {
            mat_uint32_t *ptr = (mat_uint32_t *)data;
            READ_DATA_SLAB2(ReadUInt32Data);
            break;
        }
        case MAT_C_INT16: {
            mat_int16_t *ptr = (mat_int16_t *)data;
            READ_DATA_SLAB2(ReadInt16Data);
            break;
        }
        case MAT_C_UINT16: {
            mat_uint16_t *ptr = (mat_uint16_t *)data;
            READ_DATA_SLAB2(ReadUInt16Data);
            break;
        }
        case MAT_C_INT8: {
            mat_int8_t *ptr = (mat_int8_t *)data;
            READ_DATA_SLAB2(ReadInt8Data);
            break;
        }
        case MAT_C_UINT8: {
            mat_uint8_t *ptr = (mat_uint8_t *)data;
            READ_DATA_SLAB2(ReadUInt8Data);
            break;
        }
        default:
            nBytes = 0;
    }
    return nBytes;
}

#undef READ_DATA_SLAB2

#if HAVE_ZLIB
#define READ_COMPRESSED_DATA_SLAB1(ReadDataFunc)                             \
    do {                                                                     \
        if ( !stride ) {                                                     \
            nBytes += ReadDataFunc(mat, &z_copy, ptr, data_type, edge);      \
        } else {                                                             \
            for ( i = 0; i < edge; i++ ) {                                   \
                nBytes += ReadDataFunc(mat, &z_copy, ptr + i, data_type, 1); \
                InflateSkipData(mat, &z_copy, data_type, stride);            \
            }                                                                \
        }                                                                    \
    } while ( 0 )

/** @brief Reads data of type @c data_type by user-defined dimensions for 1-D
 *         data
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param z zlib compression stream
 * @param data Pointer to store the output data
 * @param class_type Type of data class (matio_classes enumerations)
 * @param data_type Datatype of the stored data (matio_types enumerations)
 * @param start Index to start reading data in each dimension
 * @param stride Read every @c stride elements in each dimension
 * @param edge Number of elements to read in each dimension
 * @retval Number of bytes read from the file, or -1 on error
 */
int
ReadCompressedDataSlab1(mat_t *mat, z_streamp z, void *data, enum matio_classes class_type,
                        enum matio_types data_type, int start, int stride, int edge)
{
    int nBytes = 0, i, err;
    z_stream z_copy = {
        0,
    };

    if ( (mat == NULL) || (data == NULL) || (mat->fp == NULL) )
        return 0;

    stride--;
    err = inflateCopy(&z_copy, z);
    if ( err != Z_OK ) {
        Mat_Critical("inflateCopy returned error %s", zError(err));
        return -1;
    }
    InflateSkipData(mat, &z_copy, data_type, start);
    switch ( class_type ) {
        case MAT_C_DOUBLE: {
            double *ptr = (double *)data;
            READ_COMPRESSED_DATA_SLAB1(ReadCompressedDoubleData);
            break;
        }
        case MAT_C_SINGLE: {
            float *ptr = (float *)data;
            READ_COMPRESSED_DATA_SLAB1(ReadCompressedSingleData);
            break;
        }
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64: {
            mat_int64_t *ptr = (mat_int64_t *)data;
            READ_COMPRESSED_DATA_SLAB1(ReadCompressedInt64Data);
            break;
        }
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64: {
            mat_uint64_t *ptr = (mat_uint64_t *)data;
            READ_COMPRESSED_DATA_SLAB1(ReadCompressedUInt64Data);
            break;
        }
#endif /* HAVE_MAT_UINT64_T */
        case MAT_C_INT32: {
            mat_int32_t *ptr = (mat_int32_t *)data;
            READ_COMPRESSED_DATA_SLAB1(ReadCompressedInt32Data);
            break;
        }
        case MAT_C_UINT32: {
            mat_uint32_t *ptr = (mat_uint32_t *)data;
            READ_COMPRESSED_DATA_SLAB1(ReadCompressedUInt32Data);
            break;
        }
        case MAT_C_INT16: {
            mat_int16_t *ptr = (mat_int16_t *)data;
            READ_COMPRESSED_DATA_SLAB1(ReadCompressedInt16Data);
            break;
        }
        case MAT_C_UINT16: {
            mat_uint16_t *ptr = (mat_uint16_t *)data;
            READ_COMPRESSED_DATA_SLAB1(ReadCompressedUInt16Data);
            break;
        }
        case MAT_C_INT8: {
            mat_int8_t *ptr = (mat_int8_t *)data;
            READ_COMPRESSED_DATA_SLAB1(ReadCompressedInt8Data);
            break;
        }
        case MAT_C_UINT8: {
            mat_uint8_t *ptr = (mat_uint8_t *)data;
            READ_COMPRESSED_DATA_SLAB1(ReadCompressedUInt8Data);
            break;
        }
        default:
            break;
    }
    inflateEnd(&z_copy);
    return nBytes;
}

#undef READ_COMPRESSED_DATA_SLAB1

#define READ_COMPRESSED_DATA_SLAB2(ReadDataFunc)                                                  \
    do {                                                                                          \
        row_stride = (stride[0] - 1);                                                             \
        col_stride = (stride[1] - 1) * dims[0];                                                   \
        InflateSkipData(mat, &z_copy, data_type, start[1] * dims[0]);                             \
        /* If stride[0] is 1 and stride[1] is 1, we are reading all of the */                     \
        /* data so get rid of the loops.  If stride[0] is 1 and stride[1] */                      \
        /* is not 0, we are reading whole columns, so get rid of inner loop */                    \
        /* to speed up the code */                                                                \
        if ( (stride[0] == 1 && (size_t)edge[0] == dims[0]) && (stride[1] == 1) ) {               \
            ReadDataFunc(mat, &z_copy, ptr, data_type, (ptrdiff_t)edge[0] * edge[1]);             \
        } else if ( stride[0] == 1 ) {                                                            \
            for ( i = 0; i < edge[1]; i++ ) {                                                     \
                InflateSkipData(mat, &z_copy, data_type, start[0]);                               \
                ReadDataFunc(mat, &z_copy, ptr, data_type, edge[0]);                              \
                ptr += edge[0];                                                                   \
                pos = dims[0] - (ptrdiff_t)(edge[0] - 1) * stride[0] - 1 - start[0] + col_stride; \
                InflateSkipData(mat, &z_copy, data_type, pos);                                    \
            }                                                                                     \
        } else {                                                                                  \
            for ( i = 0; i < edge[1]; i++ ) {                                                     \
                InflateSkipData(mat, &z_copy, data_type, start[0]);                               \
                for ( j = 0; j < edge[0] - 1; j++ ) {                                             \
                    ReadDataFunc(mat, &z_copy, ptr++, data_type, 1);                              \
                    InflateSkipData(mat, &z_copy, data_type, row_stride);                         \
                }                                                                                 \
                ReadDataFunc(mat, &z_copy, ptr++, data_type, 1);                                  \
                pos = dims[0] - (ptrdiff_t)(edge[0] - 1) * stride[0] - 1 - start[0] + col_stride; \
                InflateSkipData(mat, &z_copy, data_type, pos);                                    \
            }                                                                                     \
        }                                                                                         \
    } while ( 0 )

/** @brief Reads data of type @c data_type by user-defined dimensions for 2-D
 *         data
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param z zlib compression stream
 * @param data Pointer to store the output data
 * @param class_type Type of data class (matio_classes enumerations)
 * @param data_type Datatype of the stored data (matio_types enumerations)
 * @param dims Dimensions of the data
 * @param start Index to start reading data in each dimension
 * @param stride Read every @c stride elements in each dimension
 * @param edge Number of elements to read in each dimension
 * @retval Number of bytes read from the file, or -1 on error
 */
int
ReadCompressedDataSlab2(mat_t *mat, z_streamp z, void *data, enum matio_classes class_type,
                        enum matio_types data_type, size_t *dims, int *start, int *stride,
                        int *edge)
{
    int nBytes = 0, i, j, err;
    int pos, row_stride, col_stride;
    z_stream z_copy = {
        0,
    };

    if ( (mat == NULL) || (data == NULL) || (mat->fp == NULL) || (start == NULL) ||
         (stride == NULL) || (edge == NULL) ) {
        return 0;
    }

    err = inflateCopy(&z_copy, z);
    if ( err != Z_OK ) {
        Mat_Critical("inflateCopy returned error %s", zError(err));
        return -1;
    }
    switch ( class_type ) {
        case MAT_C_DOUBLE: {
            double *ptr = (double *)data;
            READ_COMPRESSED_DATA_SLAB2(ReadCompressedDoubleData);
            break;
        }
        case MAT_C_SINGLE: {
            float *ptr = (float *)data;
            READ_COMPRESSED_DATA_SLAB2(ReadCompressedSingleData);
            break;
        }
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64: {
            mat_int64_t *ptr = (mat_int64_t *)data;
            READ_COMPRESSED_DATA_SLAB2(ReadCompressedInt64Data);
            break;
        }
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64: {
            mat_uint64_t *ptr = (mat_uint64_t *)data;
            READ_COMPRESSED_DATA_SLAB2(ReadCompressedUInt64Data);
            break;
        }
#endif /* HAVE_MAT_UINT64_T */
        case MAT_C_INT32: {
            mat_int32_t *ptr = (mat_int32_t *)data;
            READ_COMPRESSED_DATA_SLAB2(ReadCompressedInt32Data);
            break;
        }
        case MAT_C_UINT32: {
            mat_uint32_t *ptr = (mat_uint32_t *)data;
            READ_COMPRESSED_DATA_SLAB2(ReadCompressedUInt32Data);
            break;
        }
        case MAT_C_INT16: {
            mat_int16_t *ptr = (mat_int16_t *)data;
            READ_COMPRESSED_DATA_SLAB2(ReadCompressedInt16Data);
            break;
        }
        case MAT_C_UINT16: {
            mat_uint16_t *ptr = (mat_uint16_t *)data;
            READ_COMPRESSED_DATA_SLAB2(ReadCompressedUInt16Data);
            break;
        }
        case MAT_C_INT8: {
            mat_int8_t *ptr = (mat_int8_t *)data;
            READ_COMPRESSED_DATA_SLAB2(ReadCompressedInt8Data);
            break;
        }
        case MAT_C_UINT8: {
            mat_uint8_t *ptr = (mat_uint8_t *)data;
            READ_COMPRESSED_DATA_SLAB2(ReadCompressedUInt8Data);
            break;
        }
        default:
            nBytes = 0;
    }
    inflateEnd(&z_copy);
    return nBytes;
}

#undef READ_COMPRESSED_DATA_SLAB2
#endif

/** @endcond */
