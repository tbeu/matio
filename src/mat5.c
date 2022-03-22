/** @file mat5.c
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
#include "mat5.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
#if defined(_MSC_VER) || defined(__MINGW32__)
#define strdup _strdup
#endif

/** Get type from tag */
#define TYPE_FROM_TAG(a) \
    (((a)&0x000000ff) <= MAT_T_FUNCTION) ? (enum matio_types)((a)&0x000000ff) : MAT_T_UNKNOWN
/** Get class from array flag */
#define CLASS_FROM_ARRAY_FLAGS(a) \
    (((a)&0x000000ff) <= MAT_C_OPAQUE) ? ((enum matio_classes)((a)&0x000000ff)) : MAT_C_EMPTY
/** Class type mask */
#define CLASS_TYPE_MASK 0x000000ff

static mat_complex_split_t null_complex_data = {NULL, NULL};

/*===========================================================================
 *  Private functions
 *===========================================================================
 */
static int GetTypeBufSize(matvar_t *matvar, size_t *size);
static int GetStructFieldBufSize(matvar_t *matvar, size_t *size);
static int GetCellArrayFieldBufSize(matvar_t *matvar, size_t *size);
static void SetFieldNames(matvar_t *matvar, char *buf, size_t nfields,
                          mat_uint32_t fieldname_length);
static size_t ReadSparse(mat_t *mat, matvar_t *matvar, mat_uint32_t *n, mat_uint32_t **v);
#if HAVE_ZLIB
static int GetMatrixMaxBufSize(matvar_t *matvar, size_t *size);
#endif
static int GetEmptyMatrixMaxBufSize(const char *name, int rank, size_t *size);
static size_t WriteCharData(mat_t *mat, void *data, size_t N, enum matio_types data_type);
static size_t ReadNextCell(mat_t *mat, matvar_t *matvar);
static size_t ReadNextStructField(mat_t *mat, matvar_t *matvar);
static size_t ReadNextFunctionHandle(mat_t *mat, matvar_t *matvar);
static int ReadRankDims(mat_t *mat, matvar_t *matvar, enum matio_types data_type,
                        mat_uint32_t nbytes, size_t *read_bytes);
static int WriteType(mat_t *mat, matvar_t *matvar);
static int WriteCellArrayField(mat_t *mat, matvar_t *matvar);
static int WriteStructField(mat_t *mat, matvar_t *matvar);
static int WriteData(mat_t *mat, void *data, size_t N, enum matio_types data_type);
static size_t Mat_WriteEmptyVariable5(mat_t *mat, const char *name, int rank, size_t *dims);
static int Mat_VarReadNumeric5(mat_t *mat, matvar_t *matvar, void *data, size_t N);
#if HAVE_ZLIB
static size_t WriteCompressedCharData(mat_t *mat, z_streamp z, void *data, size_t N,
                                      enum matio_types data_type);
static size_t WriteCompressedData(mat_t *mat, z_streamp z, void *data, int N,
                                  enum matio_types data_type);
static size_t WriteCompressedTypeArrayFlags(mat_t *mat, matvar_t *matvar, z_streamp z);
static size_t WriteCompressedType(mat_t *mat, matvar_t *matvar, z_streamp z);
static size_t WriteCompressedCellArrayField(mat_t *mat, matvar_t *matvar, z_streamp z);
static size_t WriteCompressedStructField(mat_t *mat, matvar_t *matvar, z_streamp z);
static size_t Mat_WriteCompressedEmptyVariable5(mat_t *mat, const char *name, int rank,
                                                size_t *dims, z_streamp z);
#endif

/** @brief determines the number of bytes for a given class type
 *
 * @ingroup mat_internal
 * @param matvar MAT variable
 * @param size the number of bytes needed to store the MAT variable
 * @return 0 on success
 */
static int
GetTypeBufSize(matvar_t *matvar, size_t *size)
{
    int err;
    size_t nBytes, data_bytes;
    size_t tag_size = 8;
    size_t nelems = 1;
    size_t rank_size;

    *size = 0;

    err = Mat_MulDims(matvar, &nelems);
    if ( err )
        return err;

    /* Add rank and dimensions, padded to an 8 byte block */
    err = Mul(&rank_size, matvar->rank, 4);
    if ( err )
        return err;

    if ( matvar->rank % 2 )
        nBytes = tag_size + 4;
    else
        nBytes = tag_size;

    err = Add(&nBytes, nBytes, rank_size);
    if ( err )
        return err;

    switch ( matvar->class_type ) {
        case MAT_C_STRUCT: {
            matvar_t **fields = (matvar_t **)matvar->data;
            size_t nfields = matvar->internal->num_fields;
            size_t maxlen = 0, i, field_buf_size;

            for ( i = 0; i < nfields; i++ ) {
                char *fieldname = matvar->internal->fieldnames[i];
                if ( NULL != fieldname && strlen(fieldname) > maxlen )
                    maxlen = strlen(fieldname);
            }
            maxlen++;
            while ( nfields * maxlen % 8 != 0 )
                maxlen++;

            err = Mul(&field_buf_size, maxlen, nfields);
            if ( err )
                return err;
            err = Add(&nBytes, nBytes, tag_size + tag_size);
            if ( err )
                return err;
            err = Add(&nBytes, nBytes, field_buf_size);
            if ( err )
                return err;

            /* FIXME: Add bytes for the fieldnames */
            if ( NULL != fields && nfields > 0 ) {
                size_t nelems_x_nfields = 1;
                err = Mul(&nelems_x_nfields, nelems, nfields);
                if ( err )
                    return err;

                for ( i = 0; i < nelems_x_nfields; i++ ) {
                    err = GetStructFieldBufSize(fields[i], &field_buf_size);
                    if ( err )
                        return err;
                    err = Add(&nBytes, nBytes, tag_size);
                    if ( err )
                        return err;
                    err = Add(&nBytes, nBytes, field_buf_size);
                    if ( err )
                        return err;
                }
            }
            break;
        }
        case MAT_C_CELL: {
            matvar_t **cells = (matvar_t **)matvar->data;

            if ( matvar->nbytes == 0 || matvar->data_size == 0 )
                break;

            nelems = matvar->nbytes / matvar->data_size;
            if ( NULL != cells && nelems > 0 ) {
                size_t i, field_buf_size;
                for ( i = 0; i < nelems; i++ ) {
                    err = GetCellArrayFieldBufSize(cells[i], &field_buf_size);
                    if ( err )
                        return err;
                    err = Add(&nBytes, nBytes, tag_size);
                    if ( err )
                        return err;
                    err = Add(&nBytes, nBytes, field_buf_size);
                    if ( err )
                        return err;
                }
            }
            break;
        }
        case MAT_C_SPARSE: {
            mat_sparse_t *sparse = (mat_sparse_t *)matvar->data;

            err = Mul(&data_bytes, sparse->nir, sizeof(mat_uint32_t));
            if ( err )
                return err;
            if ( data_bytes % 8 ) {
                err = Add(&data_bytes, data_bytes, 8 - data_bytes % 8);
                if ( err )
                    return err;
            }
            err = Add(&nBytes, nBytes, tag_size);
            if ( err )
                return err;
            err = Add(&nBytes, nBytes, data_bytes);
            if ( err )
                return err;

            err = Mul(&data_bytes, sparse->njc, sizeof(mat_uint32_t));
            if ( err )
                return err;
            if ( data_bytes % 8 ) {
                err = Add(&data_bytes, data_bytes, 8 - data_bytes % 8);
                if ( err )
                    return err;
            }
            err = Add(&nBytes, nBytes, tag_size);
            if ( err )
                return err;
            err = Add(&nBytes, nBytes, data_bytes);
            if ( err )
                return err;

            err = Mul(&data_bytes, sparse->ndata, Mat_SizeOf(matvar->data_type));
            if ( err )
                return err;
            if ( data_bytes % 8 ) {
                err = Add(&data_bytes, data_bytes, 8 - data_bytes % 8);
                if ( err )
                    return err;
            }
            err = Add(&nBytes, nBytes, tag_size);
            if ( err )
                return err;
            err = Add(&nBytes, nBytes, data_bytes);
            if ( err )
                return err;

            if ( matvar->isComplex ) {
                err = Add(&nBytes, nBytes, tag_size);
                if ( err )
                    return err;
                err = Add(&nBytes, nBytes, data_bytes);
                if ( err )
                    return err;
            }

            break;
        }
        case MAT_C_CHAR:
            if ( MAT_T_UINT8 == matvar->data_type || MAT_T_INT8 == matvar->data_type )
                err = Mul(&data_bytes, nelems, Mat_SizeOf(MAT_T_UINT16));
            else
                err = Mul(&data_bytes, nelems, Mat_SizeOf(matvar->data_type));
            if ( err )
                return err;
            if ( data_bytes % 8 ) {
                err = Add(&data_bytes, data_bytes, 8 - data_bytes % 8);
                if ( err )
                    return err;
            }

            err = Add(&nBytes, nBytes, tag_size);
            if ( err )
                return err;
            err = Add(&nBytes, nBytes, data_bytes);
            if ( err )
                return err;

            if ( matvar->isComplex ) {
                err = Add(&nBytes, nBytes, tag_size);
                if ( err )
                    return err;
                err = Add(&nBytes, nBytes, data_bytes);
                if ( err )
                    return err;
            }

            break;
        default:
            err = Mul(&data_bytes, nelems, Mat_SizeOf(matvar->data_type));
            if ( err )
                return err;
            if ( data_bytes % 8 ) {
                err = Add(&data_bytes, data_bytes, 8 - data_bytes % 8);
                if ( err )
                    return err;
            }

            err = Add(&nBytes, nBytes, tag_size);
            if ( err )
                return err;
            err = Add(&nBytes, nBytes, data_bytes);
            if ( err )
                return err;

            if ( matvar->isComplex ) {
                err = Add(&nBytes, nBytes, tag_size);
                if ( err )
                    return err;
                err = Add(&nBytes, nBytes, data_bytes);
                if ( err )
                    return err;
            }
    } /* switch ( matvar->class_type ) */

    *size = nBytes;
    return MATIO_E_NO_ERROR;
}

/** @brief determines the number of bytes needed to store the given struct field
 *
 * @ingroup mat_internal
 * @param matvar field of a structure
 * @param size the number of bytes needed to store the struct field
 * @return 0 on success
 */
static int
GetStructFieldBufSize(matvar_t *matvar, size_t *size)
{
    int err;
    size_t nBytes = 0, type_buf_size;
    size_t tag_size = 8, array_flags_size = 8;

    *size = 0;

    if ( matvar == NULL )
        return GetEmptyMatrixMaxBufSize(NULL, 2, size);

    /* Add the Array Flags tag and space to the number of bytes */
    nBytes += tag_size + array_flags_size;

    /* In a struct field, the name is just a tag with 0 bytes */
    nBytes += tag_size;

    err = GetTypeBufSize(matvar, &type_buf_size);
    if ( err )
        return err;
    err = Add(&nBytes, nBytes, type_buf_size);
    if ( err )
        return err;

    *size = nBytes;
    return MATIO_E_NO_ERROR;
}

/** @brief determines the number of bytes needed to store the cell array element
 *
 * @ingroup mat_internal
 * @param matvar MAT variable
 * @param size the number of bytes needed to store the variable
 * @return 0 on success
 */
static int
GetCellArrayFieldBufSize(matvar_t *matvar, size_t *size)
{
    int err;
    size_t nBytes = 0, type_buf_size;
    size_t tag_size = 8, array_flags_size = 8;

    *size = 0;

    if ( matvar == NULL )
        return MATIO_E_BAD_ARGUMENT;

    /* Add the Array Flags tag and space to the number of bytes */
    nBytes += tag_size + array_flags_size;

    /* In an element of a cell array, the name is just a tag with 0 bytes */
    nBytes += tag_size;

    err = GetTypeBufSize(matvar, &type_buf_size);
    if ( err )
        return err;
    err = Add(&nBytes, nBytes, type_buf_size);
    if ( err )
        return err;

    *size = nBytes;
    return MATIO_E_NO_ERROR;
}

/** @brief determines the number of bytes needed to store the given variable
 *
 * @ingroup mat_internal
 * @param name MAT variable
 * @param rank rank of the variable
 * @param size the number of bytes needed to store the variable
 * @return 0 on success
 */
static int
GetEmptyMatrixMaxBufSize(const char *name, int rank, size_t *size)
{
    int err = 0;
    size_t nBytes = 0, len, rank_size;
    size_t tag_size = 8, array_flags_size = 8;

    /* Add the Array Flags tag and space to the number of bytes */
    nBytes += tag_size + array_flags_size;

    /* Get size of variable name, pad it to an 8 byte block, and add it to nBytes */
    if ( NULL != name )
        len = strlen(name);
    else
        len = 4;

    if ( len <= 4 ) {
        nBytes += tag_size;
    } else {
        nBytes += tag_size;
        if ( len % 8 ) {
            err = Add(&len, len, 8 - len % 8);
            if ( err )
                return err;
        }

        err = Add(&nBytes, nBytes, len);
        if ( err )
            return err;
    }

    /* Add rank and dimensions, padded to an 8 byte block */
    err = Mul(&rank_size, rank, 4);
    if ( err )
        return err;
    if ( rank % 2 )
        err = Add(&nBytes, nBytes, tag_size + 4);
    else
        err = Add(&nBytes, nBytes, tag_size);
    if ( err )
        return err;

    err = Add(&nBytes, nBytes, rank_size);
    if ( err )
        return err;
    /* Data tag */
    err = Add(&nBytes, nBytes, tag_size);
    if ( err )
        return err;

    *size = nBytes;
    return MATIO_E_NO_ERROR;
}

static void
SetFieldNames(matvar_t *matvar, char *buf, size_t nfields, mat_uint32_t fieldname_length)
{
    matvar->internal->num_fields = nfields;
    matvar->internal->fieldnames = (char **)calloc(nfields, sizeof(*matvar->internal->fieldnames));
    if ( NULL != matvar->internal->fieldnames ) {
        size_t i;
        for ( i = 0; i < nfields; i++ ) {
            matvar->internal->fieldnames[i] = (char *)malloc(fieldname_length);
            if ( NULL != matvar->internal->fieldnames[i] ) {
                memcpy(matvar->internal->fieldnames[i], buf + i * fieldname_length,
                       fieldname_length);
                matvar->internal->fieldnames[i][fieldname_length - 1] = '\0';
            }
        }
    }
}

static size_t
ReadSparse(mat_t *mat, matvar_t *matvar, mat_uint32_t *n, mat_uint32_t **v)
{
    int data_in_tag = 0;
    enum matio_types packed_type;
    mat_uint32_t tag[2] = {0, 0};
    size_t bytesread = 0;
    mat_uint32_t N = 0;

    if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
#if HAVE_ZLIB
        matvar->internal->z->avail_in = 0;
        if ( 0 != Inflate(mat, matvar->internal->z, tag, 4, &bytesread) ) {
            return bytesread;
        }
        if ( mat->byteswap )
            (void)Mat_uint32Swap(tag);
        packed_type = TYPE_FROM_TAG(tag[0]);
        if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
            data_in_tag = 1;
            N = (tag[0] & 0xffff0000) >> 16;
        } else {
            data_in_tag = 0;
            (void)ReadCompressedUInt32Data(mat, matvar->internal->z, &N, MAT_T_UINT32, 1);
        }
#endif
    } else {
        if ( 0 != Read(tag, 4, 1, (FILE *)mat->fp, &bytesread) ) {
            return bytesread;
        }
        if ( mat->byteswap )
            (void)Mat_uint32Swap(tag);
        packed_type = TYPE_FROM_TAG(tag[0]);
        if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
            data_in_tag = 1;
            N = (tag[0] & 0xffff0000) >> 16;
        } else {
            data_in_tag = 0;
            if ( 0 != Read(&N, 4, 1, (FILE *)mat->fp, &bytesread) ) {
                return bytesread;
            }
            if ( mat->byteswap )
                (void)Mat_uint32Swap(&N);
        }
    }
    if ( 0 == N )
        return bytesread;
    *n = N / 4;
    *v = (mat_uint32_t *)calloc(N, 1);
    if ( NULL != *v ) {
        int nBytes;
        if ( matvar->compression == MAT_COMPRESSION_NONE ) {
            nBytes = ReadUInt32Data(mat, *v, packed_type, *n);
            /*
                * If the data was in the tag we started on a 4-byte
                * boundary so add 4 to make it an 8-byte
                */
            nBytes *= Mat_SizeOf(packed_type);
            if ( data_in_tag )
                nBytes += 4;
            if ( (nBytes % 8) != 0 )
                (void)fseeko((FILE *)mat->fp, 8 - (nBytes % 8), SEEK_CUR);
#if HAVE_ZLIB
        } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
            nBytes = ReadCompressedUInt32Data(mat, matvar->internal->z, *v, packed_type, *n);
            /*
                * If the data was in the tag we started on a 4-byte
                * boundary so add 4 to make it an 8-byte
                */
            if ( data_in_tag )
                nBytes += 4;
            if ( (nBytes % 8) != 0 )
                InflateSkip(mat, matvar->internal->z, 8 - (nBytes % 8), NULL);
#endif
        }
    } else {
        Mat_Critical("Couldn't allocate memory");
    }

    return bytesread;
}

#if HAVE_ZLIB
/** @brief determines the number of bytes needed to store the given variable
 *
 * @ingroup mat_internal
 * @param matvar MAT variable
 * @param size the number of bytes needed to store the variable
 * @return 0 on success
 */
static int
GetMatrixMaxBufSize(matvar_t *matvar, size_t *size)
{
    int err = MATIO_E_NO_ERROR;
    size_t nBytes = 0, len, type_buf_size;
    size_t tag_size = 8, array_flags_size = 8;

    if ( matvar == NULL )
        return MATIO_E_BAD_ARGUMENT;

    /* Add the Array Flags tag and space to the number of bytes */
    nBytes += tag_size + array_flags_size;

    /* Get size of variable name, pad it to an 8 byte block, and add it to nBytes */
    if ( NULL != matvar->name )
        len = strlen(matvar->name);
    else
        len = 4;

    if ( len <= 4 ) {
        nBytes += tag_size;
    } else {
        nBytes += tag_size;
        if ( len % 8 ) {
            err = Add(&len, len, 8 - len % 8);
            if ( err )
                return err;
        }

        err = Add(&nBytes, nBytes, len);
        if ( err )
            return err;
    }

    err = GetTypeBufSize(matvar, &type_buf_size);
    if ( err )
        return err;
    err = Add(&nBytes, nBytes, type_buf_size);
    if ( err )
        return err;

    *size = nBytes;
    return MATIO_E_NO_ERROR;
}
#endif

/** @if mat_devman
 * @brief Creates a new Matlab MAT version 5 file
 *
 * Tries to create a new Matlab MAT file with the given name and optional
 * header string.  If no header string is given, the default string
 * is used containing the software, version, and date in it.  If a header
 * string is given, at most the first 116 characters is written to the file.
 * The given header string need not be the full 116 characters, but MUST be
 * NULL terminated.
 * @ingroup MAT
 * @param matname Name of MAT file to create
 * @param hdr_str Optional header string, NULL to use default
 * @return A pointer to the MAT file or NULL if it failed.  This is not a
 * simple FILE * and should not be used as one.
 * @endif
 */
mat_t *
Mat_Create5(const char *matname, const char *hdr_str)
{
    FILE *fp = NULL;
    mat_int16_t endian = 0, version;
    mat_t *mat = NULL;
    size_t err;
    time_t t;

#if defined(_WIN32) && defined(_MSC_VER)
    wchar_t *wname = utf82u(matname);
    if ( NULL != wname ) {
        fp = _wfopen(wname, L"w+b");
        free(wname);
    }
#else
    fp = fopen(matname, "w+b");
#endif
    if ( !fp )
        return NULL;

    mat = (mat_t *)malloc(sizeof(*mat));
    if ( mat == NULL ) {
        fclose(fp);
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
#if defined(MAT73) && MAT73
    mat->refs_id = -1;
#endif
    mat->dir = NULL;

    t = time(NULL);
    mat->fp = fp;
    mat->filename = strdup(matname);
    mat->mode = MAT_ACC_RDWR;
    mat->byteswap = 0;
    mat->header = (char *)malloc(128 * sizeof(char));
    mat->subsys_offset = (char *)malloc(8 * sizeof(char));
    memset(mat->header, ' ', 128);
    if ( hdr_str == NULL ) {
        err = mat_snprintf(mat->header, 116,
                           "MATLAB 5.0 MAT-file, Platform: %s, "
                           "Created by: libmatio v%d.%d.%d on %s",
                           MATIO_PLATFORM, MATIO_MAJOR_VERSION, MATIO_MINOR_VERSION,
                           MATIO_RELEASE_LEVEL, ctime(&t));
    } else {
        err = mat_snprintf(mat->header, 116, "%s", hdr_str);
    }
    if ( err >= 116 )
        mat->header[115] = '\0'; /* Just to make sure it's NULL terminated */
    memset(mat->subsys_offset, ' ', 8);
    mat->version = 0x0100;
    endian = 0x4d49;

    version = 0x0100;

    fwrite(mat->header, 1, 116, (FILE *)mat->fp);
    fwrite(mat->subsys_offset, 1, 8, (FILE *)mat->fp);
    fwrite(&version, 2, 1, (FILE *)mat->fp);
    fwrite(&endian, 2, 1, (FILE *)mat->fp);

    return mat;
}

/** @if mat_devman
 * @brief Writes @c data as character data
 *
 * This function uses the knowledge that the data is part of a character class
 * to avoid some pitfalls with Matlab listed below.
 *   @li Matlab character data cannot be unsigned 8-bit integers, it needs at
 *       least unsigned 16-bit integers
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param data character data to write
 * @param N Number of elements to write
 * @param data_type character data type (enum matio_types)
 * @return number of bytes written
 * @endif
 */
static size_t
WriteCharData(mat_t *mat, void *data, size_t N, enum matio_types data_type)
{
    mat_uint32_t nBytes = 0;
    size_t nbytes = 0, i;
    size_t byteswritten = 0;
    const mat_uint8_t pad1 = 0;
    int err;

    switch ( data_type ) {
        case MAT_T_UINT8:
        case MAT_T_UINT16:
        case MAT_T_UTF8:
        case MAT_T_UTF16: {
            data_type = MAT_T_UINT8 == data_type ? MAT_T_UTF8 : data_type;
            err = Mul(&nbytes, N, Mat_SizeOf(data_type));
            if ( err ) {
                return 0;
            }
            nBytes = (mat_uint32_t)nbytes;
            fwrite(&data_type, 4, 1, (FILE *)mat->fp);
            fwrite(&nBytes, 4, 1, (FILE *)mat->fp);
            if ( NULL != data && N > 0 )
                fwrite(data, 1, nbytes, (FILE *)mat->fp);
            if ( nBytes % 8 ) {
                for ( i = nbytes % 8; i < 8; i++ )
                    fwrite(&pad1, 1, 1, (FILE *)mat->fp);
            }
            break;
        }
        case MAT_T_INT8: {
            mat_int8_t *ptr;
            mat_uint16_t c;

            /* Matlab can't read MAT_C_CHAR as int8, needs uint16 */
            data_type = MAT_T_UINT16;
            err = Mul(&nbytes, N, Mat_SizeOf(data_type));
            if ( err ) {
                return 0;
            }
            nBytes = (mat_uint32_t)nbytes;
            fwrite(&data_type, 4, 1, (FILE *)mat->fp);
            fwrite(&nBytes, 4, 1, (FILE *)mat->fp);
            ptr = (mat_int8_t *)data;
            if ( NULL == data )
                break;
            for ( i = 0; i < N; i++ ) {
                c = (mat_uint16_t) * (char *)ptr;
                fwrite(&c, 2, 1, (FILE *)mat->fp);
                ptr++;
            }
            if ( nbytes % 8 )
                for ( i = nbytes % 8; i < 8; i++ )
                    fwrite(&pad1, 1, 1, (FILE *)mat->fp);
            break;
        }
        case MAT_T_UNKNOWN: {
            /* Sometimes empty char data will have MAT_T_UNKNOWN, so just write
             * a data tag
             */
            data_type = MAT_T_UINT16;
            err = Mul(&nbytes, N, Mat_SizeOf(data_type));
            if ( err ) {
                return 0;
            }
            nBytes = (mat_uint32_t)nbytes;
            fwrite(&data_type, 4, 1, (FILE *)mat->fp);
            fwrite(&nBytes, 4, 1, (FILE *)mat->fp);
            break;
        }
        default:
            nbytes = 0;
            break;
    }
    byteswritten += nbytes;
    return byteswritten;
}

#if HAVE_ZLIB
/** @brief Writes @c data as compressed character data
 *
 * This function uses the knowledge that the data is part of a character class
 * to avoid some pitfalls with Matlab listed below.
 *   @li Matlab character data cannot be unsigned 8-bit integers, it needs at
 *       least unsigned 16-bit integers
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param z pointer to the zlib compression stream
 * @param data character data to write
 * @param N Number of elements to write
 * @param data_type character data type (enum matio_types)
 * @return number of bytes written
 */
static size_t
WriteCompressedCharData(mat_t *mat, z_streamp z, void *data, size_t N, enum matio_types data_type)
{
    size_t data_size, byteswritten = 0, nbytes = 0;
    mat_uint32_t data_tag[2] = {0, 0};
    int buf_size = 1024;
    int err;
    mat_uint8_t buf[1024], pad[8] = {0, 0, 0, 0, 0, 0, 0, 0};

    if ( mat == NULL || mat->fp == NULL )
        return 0;

    if ( data_type == MAT_T_UNKNOWN ) {
        data_size = Mat_SizeOf(MAT_T_UINT16);
    } else {
        data_size = Mat_SizeOf(data_type);
    }

    err = Mul(&nbytes, N, data_size);
    if ( err ) {
        return byteswritten;
    }

    switch ( data_type ) {
        case MAT_T_UINT8:
        case MAT_T_UINT16:
        case MAT_T_UTF8:
        case MAT_T_UTF16:
            data_tag[0] = MAT_T_UINT8 == data_type ? MAT_T_UTF8 : data_type;
            data_tag[1] = (mat_uint32_t)nbytes;
            z->next_in = ZLIB_BYTE_PTR(data_tag);
            z->avail_in = 8;
            do {
                z->next_out = buf;
                z->avail_out = buf_size;
                deflate(z, Z_NO_FLUSH);
                byteswritten += fwrite(buf, 1, buf_size - z->avail_out, (FILE *)mat->fp);
            } while ( z->avail_out == 0 );

            /* exit early if this is an empty data */
            if ( NULL == data || N < 1 )
                break;

            z->next_in = (Bytef *)data;
            z->avail_in = (mat_uint32_t)nbytes;
            do {
                z->next_out = buf;
                z->avail_out = buf_size;
                deflate(z, Z_NO_FLUSH);
                byteswritten += fwrite(buf, 1, buf_size - z->avail_out, (FILE *)mat->fp);
            } while ( z->avail_out == 0 );
            /* Add/Compress padding to pad to 8-byte boundary */
            if ( nbytes % 8 ) {
                z->next_in = pad;
                z->avail_in = 8 - (nbytes % 8);
                do {
                    z->next_out = buf;
                    z->avail_out = buf_size;
                    deflate(z, Z_NO_FLUSH);
                    byteswritten += fwrite(buf, 1, buf_size - z->avail_out, (FILE *)mat->fp);
                } while ( z->avail_out == 0 );
            }
            break;
        case MAT_T_UNKNOWN:
            /* Sometimes empty char data will have MAT_T_UNKNOWN, so just write a data tag */
            data_tag[0] = MAT_T_UINT16;
            data_tag[1] = (mat_uint32_t)nbytes;
            z->next_in = ZLIB_BYTE_PTR(data_tag);
            z->avail_in = 8;
            do {
                z->next_out = buf;
                z->avail_out = buf_size;
                deflate(z, Z_NO_FLUSH);
                byteswritten += fwrite(buf, 1, buf_size - z->avail_out, (FILE *)mat->fp);
            } while ( z->avail_out == 0 );
            break;
        default:
            break;
    }

    return byteswritten;
}
#endif

/** @brief Writes the data buffer to the file
 *
 * @param mat MAT file pointer
 * @param data pointer to the data to write
 * @param N number of elements to write
 * @param data_type data type of the data
 * @return number of bytes written
 */
static int
WriteData(mat_t *mat, void *data, size_t N, enum matio_types data_type)
{
    int nBytes = 0, data_size;

    if ( mat == NULL || mat->fp == NULL )
        return 0;

    data_size = Mat_SizeOf(data_type);
    nBytes = N * data_size;
    fwrite(&data_type, 4, 1, (FILE *)mat->fp);
    fwrite(&nBytes, 4, 1, (FILE *)mat->fp);

    if ( data != NULL && N > 0 )
        fwrite(data, data_size, N, (FILE *)mat->fp);

    return nBytes;
}

#if HAVE_ZLIB
/* Compresses the data buffer and writes it to the file */
static size_t
WriteCompressedData(mat_t *mat, z_streamp z, void *data, int N, enum matio_types data_type)
{
    int nBytes = 0, data_size, data_tag[2], byteswritten = 0;
    int buf_size = 1024;
    mat_uint8_t buf[1024], pad[8] = {0, 0, 0, 0, 0, 0, 0, 0};

    if ( mat == NULL || mat->fp == NULL )
        return 0;

    data_size = Mat_SizeOf(data_type);
    data_tag[0] = data_type;
    data_tag[1] = data_size * N;
    z->next_in = ZLIB_BYTE_PTR(data_tag);
    z->avail_in = 8;
    do {
        z->next_out = buf;
        z->avail_out = buf_size;
        deflate(z, Z_NO_FLUSH);
        byteswritten += fwrite(buf, 1, buf_size - z->avail_out, (FILE *)mat->fp);
    } while ( z->avail_out == 0 );

    /* exit early if this is an empty data */
    if ( NULL == data || N < 1 )
        return byteswritten;

    z->next_in = (Bytef *)data;
    z->avail_in = N * data_size;
    do {
        z->next_out = buf;
        z->avail_out = buf_size;
        deflate(z, Z_NO_FLUSH);
        byteswritten += fwrite(buf, 1, buf_size - z->avail_out, (FILE *)mat->fp);
    } while ( z->avail_out == 0 );
    /* Add/Compress padding to pad to 8-byte boundary */
    if ( N * data_size % 8 ) {
        z->next_in = pad;
        z->avail_in = 8 - (N * data_size % 8);
        do {
            z->next_out = buf;
            z->avail_out = buf_size;
            deflate(z, Z_NO_FLUSH);
            byteswritten += fwrite(buf, 1, buf_size - z->avail_out, (FILE *)mat->fp);
        } while ( z->avail_out == 0 );
    }
    nBytes = byteswritten;
    return nBytes;
}
#endif

/** @brief Reads the next cell of the cell array in @c matvar
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar MAT variable pointer
 * @return Number of bytes read
 */
static size_t
ReadNextCell(mat_t *mat, matvar_t *matvar)
{
    size_t bytesread = 0, i;
    int err;
    matvar_t **cells = NULL;
    size_t nelems = 1;

    err = Mat_MulDims(matvar, &nelems);
    if ( err ) {
        Mat_Critical("Integer multiplication overflow");
        return bytesread;
    }
    matvar->data_size = sizeof(matvar_t *);
    err = Mul(&matvar->nbytes, nelems, matvar->data_size);
    if ( err ) {
        Mat_Critical("Integer multiplication overflow");
        return bytesread;
    }

    matvar->data = calloc(nelems, matvar->data_size);
    if ( NULL == matvar->data ) {
        if ( NULL != matvar->name )
            Mat_Critical("Couldn't allocate memory for %s->data", matvar->name);
        return bytesread;
    }
    cells = (matvar_t **)matvar->data;

    if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
#if HAVE_ZLIB
        mat_uint32_t uncomp_buf[16];
        mat_uint32_t nBytes;
        mat_uint32_t array_flags;

        memset(&uncomp_buf, 0, sizeof(uncomp_buf));
        for ( i = 0; i < nelems; i++ ) {
            cells[i] = Mat_VarCalloc();
            if ( NULL == cells[i] ) {
                Mat_Critical("Couldn't allocate memory for cell %zu", i);
                continue;
            }

            /* Read variable tag for cell */
            uncomp_buf[0] = 0;
            uncomp_buf[1] = 0;
            err = Inflate(mat, matvar->internal->z, uncomp_buf, 8, &bytesread);
            if ( err ) {
                Mat_VarFree(cells[i]);
                cells[i] = NULL;
                break;
            }
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(uncomp_buf);
                (void)Mat_uint32Swap(uncomp_buf + 1);
            }
            nBytes = uncomp_buf[1];
            if ( 0 == nBytes ) {
                /* Empty cell: Memory optimization */
                free(cells[i]->internal);
                cells[i]->internal = NULL;
                continue;
            } else if ( uncomp_buf[0] != MAT_T_MATRIX ) {
                Mat_VarFree(cells[i]);
                cells[i] = NULL;
                Mat_Critical("cells[%zu], Uncompressed type not MAT_T_MATRIX", i);
                break;
            }
            cells[i]->compression = MAT_COMPRESSION_ZLIB;
            err = Inflate(mat, matvar->internal->z, uncomp_buf, 16, &bytesread);
            if ( err ) {
                Mat_VarFree(cells[i]);
                cells[i] = NULL;
                break;
            }
            nBytes -= 16;
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(uncomp_buf);
                (void)Mat_uint32Swap(uncomp_buf + 1);
                (void)Mat_uint32Swap(uncomp_buf + 2);
                (void)Mat_uint32Swap(uncomp_buf + 3);
            }
            /* Array Flags */
            if ( uncomp_buf[0] == MAT_T_UINT32 ) {
                array_flags = uncomp_buf[2];
                cells[i]->class_type = CLASS_FROM_ARRAY_FLAGS(array_flags);
                cells[i]->isComplex = (array_flags & MAT_F_COMPLEX);
                cells[i]->isGlobal = (array_flags & MAT_F_GLOBAL);
                cells[i]->isLogical = (array_flags & MAT_F_LOGICAL);
                if ( cells[i]->class_type == MAT_C_SPARSE ) {
                    /* Need to find a more appropriate place to store nzmax */
                    cells[i]->nbytes = uncomp_buf[3];
                }
            } else {
                Mat_Critical("Expected MAT_T_UINT32 for array tags, got %d", uncomp_buf[0]);
                InflateSkip(mat, matvar->internal->z, nBytes, &bytesread);
            }
            if ( cells[i]->class_type != MAT_C_OPAQUE ) {
                mat_uint32_t *dims = NULL;
                int do_clean = 0;
                err = InflateRankDims(mat, matvar->internal->z, uncomp_buf, sizeof(uncomp_buf),
                                      &dims, &bytesread);
                if ( NULL == dims ) {
                    dims = uncomp_buf + 2;
                } else {
                    do_clean = 1;
                }
                if ( err ) {
                    if ( do_clean ) {
                        free(dims);
                    }
                    Mat_VarFree(cells[i]);
                    cells[i] = NULL;
                    break;
                }
                nBytes -= 8;
                if ( mat->byteswap ) {
                    (void)Mat_uint32Swap(uncomp_buf);
                    (void)Mat_uint32Swap(uncomp_buf + 1);
                }
                /* Rank and Dimension */
                if ( uncomp_buf[0] == MAT_T_INT32 ) {
                    int j;
                    size_t size;
                    cells[i]->rank = uncomp_buf[1];
                    nBytes -= cells[i]->rank;
                    cells[i]->rank /= 4;
                    if ( 0 == do_clean && cells[i]->rank > 13 ) {
                        int rank = cells[i]->rank;
                        cells[i]->rank = 0;
                        Mat_Critical("%d is not a valid rank", rank);
                        continue;
                    }
                    err = Mul(&size, cells[i]->rank, sizeof(*cells[i]->dims));
                    if ( err ) {
                        if ( do_clean ) {
                            free(dims);
                        }
                        Mat_VarFree(cells[i]);
                        cells[i] = NULL;
                        Mat_Critical("Integer multiplication overflow");
                        continue;
                    }
                    cells[i]->dims = (size_t *)malloc(size);
                    if ( mat->byteswap ) {
                        for ( j = 0; j < cells[i]->rank; j++ )
                            cells[i]->dims[j] = Mat_uint32Swap(dims + j);
                    } else {
                        for ( j = 0; j < cells[i]->rank; j++ )
                            cells[i]->dims[j] = dims[j];
                    }
                    if ( cells[i]->rank % 2 != 0 )
                        nBytes -= 4;
                }
                if ( do_clean ) {
                    free(dims);
                }
                /* Variable name tag */
                err = Inflate(mat, matvar->internal->z, uncomp_buf, 8, &bytesread);
                if ( err ) {
                    Mat_VarFree(cells[i]);
                    cells[i] = NULL;
                    break;
                }
                nBytes -= 8;
                if ( mat->byteswap ) {
                    (void)Mat_uint32Swap(uncomp_buf);
                    (void)Mat_uint32Swap(uncomp_buf + 1);
                }
                /* Handle cell elements written with a variable name */
                if ( uncomp_buf[1] > 0 ) {
                    /* Name of variable */
                    if ( uncomp_buf[0] == MAT_T_INT8 ) { /* Name not in tag */
                        mat_uint32_t len = uncomp_buf[1];

                        if ( len % 8 > 0 ) {
                            if ( len < UINT32_MAX - 8 + (len % 8) )
                                len = len + 8 - (len % 8);
                            else {
                                Mat_VarFree(cells[i]);
                                cells[i] = NULL;
                                break;
                            }
                        }
                        cells[i]->name = (char *)malloc(len + 1);
                        nBytes -= len;
                        if ( NULL != cells[i]->name ) {
                            /* Variable name */
                            err =
                                Inflate(mat, matvar->internal->z, cells[i]->name, len, &bytesread);
                            if ( err ) {
                                Mat_VarFree(cells[i]);
                                cells[i] = NULL;
                                break;
                            }
                            cells[i]->name[len] = '\0';
                        }
                    } else {
                        mat_uint32_t len = (uncomp_buf[0] & 0xffff0000) >> 16;
                        if ( ((uncomp_buf[0] & 0x0000ffff) == MAT_T_INT8) && len > 0 && len <= 4 ) {
                            /* Name packed in tag */
                            cells[i]->name = (char *)malloc(len + 1);
                            if ( NULL != cells[i]->name ) {
                                memcpy(cells[i]->name, uncomp_buf + 1, len);
                                cells[i]->name[len] = '\0';
                            }
                        }
                    }
                }
                cells[i]->internal->z = (z_streamp)calloc(1, sizeof(z_stream));
                if ( cells[i]->internal->z != NULL ) {
                    err = inflateCopy(cells[i]->internal->z, matvar->internal->z);
                    if ( err == Z_OK ) {
                        cells[i]->internal->datapos = ftello((FILE *)mat->fp);
                        if ( cells[i]->internal->datapos != -1L ) {
                            cells[i]->internal->datapos -= matvar->internal->z->avail_in;
                            if ( cells[i]->class_type == MAT_C_STRUCT )
                                bytesread += ReadNextStructField(mat, cells[i]);
                            else if ( cells[i]->class_type == MAT_C_CELL )
                                bytesread += ReadNextCell(mat, cells[i]);
                            else if ( nBytes <= (1 << MAX_WBITS) ) {
                                /* Memory optimization: Read data if less in size
                                   than the zlib inflate state (approximately) */
                                err = Mat_VarRead5(mat, cells[i]);
                                cells[i]->internal->data = cells[i]->data;
                                cells[i]->data = NULL;
                            }
                            (void)fseeko((FILE *)mat->fp, cells[i]->internal->datapos, SEEK_SET);
                        } else {
                            Mat_Critical("Couldn't determine file position");
                        }
                        if ( cells[i]->internal->data != NULL ||
                             cells[i]->class_type == MAT_C_STRUCT ||
                             cells[i]->class_type == MAT_C_CELL ) {
                            /* Memory optimization: Free inflate state */
                            inflateEnd(cells[i]->internal->z);
                            free(cells[i]->internal->z);
                            cells[i]->internal->z = NULL;
                        }
                    } else {
                        Mat_Critical("inflateCopy returned error %s", zError(err));
                    }
                } else {
                    Mat_Critical("Couldn't allocate memory");
                }
            }
            InflateSkip(mat, matvar->internal->z, nBytes, &bytesread);
        }
#else
        Mat_Critical("Not compiled with zlib support");
#endif

    } else {
        mat_uint32_t buf[6] = {0, 0, 0, 0, 0, 0};
        mat_uint32_t nBytes;
        mat_uint32_t array_flags;

        for ( i = 0; i < nelems; i++ ) {
            size_t nbytes = 0;
            mat_uint32_t name_len;
            cells[i] = Mat_VarCalloc();
            if ( NULL == cells[i] ) {
                Mat_Critical("Couldn't allocate memory for cell %zu", i);
                continue;
            }

            /* Read variable tag for cell */
            err = Read(buf, 4, 2, (FILE *)mat->fp, &nbytes);

            /* Empty cells at the end of a file may cause an EOF */
            if ( 0 == err && 0 == nbytes )
                continue;
            bytesread += nbytes;
            if ( err ) {
                Mat_VarFree(cells[i]);
                cells[i] = NULL;
                break;
            }
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(buf);
                (void)Mat_uint32Swap(buf + 1);
            }
            nBytes = buf[1];
            if ( 0 == nBytes ) {
                /* Empty cell: Memory optimization */
                free(cells[i]->internal);
                cells[i]->internal = NULL;
                continue;
            } else if ( buf[0] != MAT_T_MATRIX ) {
                Mat_VarFree(cells[i]);
                cells[i] = NULL;
#if HAVE_LONG_LONG_INT
                Mat_Critical("cells[%zu] not MAT_T_MATRIX, fpos = %lld", i,
                             (long long)ftello((FILE *)mat->fp));
#else
                Mat_Critical("cells[%zu] not MAT_T_MATRIX", i);
#endif
                break;
            }

            /* Read array flags and the dimensions tag */
            err = Read(buf, 4, 6, (FILE *)mat->fp, &bytesread);
            if ( err ) {
                Mat_VarFree(cells[i]);
                cells[i] = NULL;
                break;
            }
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(buf);
                (void)Mat_uint32Swap(buf + 1);
                (void)Mat_uint32Swap(buf + 2);
                (void)Mat_uint32Swap(buf + 3);
                (void)Mat_uint32Swap(buf + 4);
                (void)Mat_uint32Swap(buf + 5);
            }
            nBytes -= 24;
            /* Array flags */
            if ( buf[0] == MAT_T_UINT32 ) {
                array_flags = buf[2];
                cells[i]->class_type = CLASS_FROM_ARRAY_FLAGS(array_flags);
                cells[i]->isComplex = (array_flags & MAT_F_COMPLEX);
                cells[i]->isGlobal = (array_flags & MAT_F_GLOBAL);
                cells[i]->isLogical = (array_flags & MAT_F_LOGICAL);
                if ( cells[i]->class_type == MAT_C_SPARSE ) {
                    /* Need to find a more appropriate place to store nzmax */
                    cells[i]->nbytes = buf[3];
                }
            }
            /* Rank and dimension */
            nbytes = 0;
            err = ReadRankDims(mat, cells[i], (enum matio_types)buf[4], buf[5], &nbytes);
            bytesread += nbytes;
            nBytes -= nbytes;
            if ( err ) {
                Mat_VarFree(cells[i]);
                cells[i] = NULL;
                break;
            }
            /* Variable name tag */
            if ( 0 != Read(buf, 1, 8, (FILE *)mat->fp, &bytesread) ) {
                Mat_VarFree(cells[i]);
                cells[i] = NULL;
                break;
            }
            nBytes -= 8;
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(buf);
                (void)Mat_uint32Swap(buf + 1);
            }
            name_len = 0;
            if ( buf[1] > 0 ) {
                /* Name of variable */
                if ( buf[0] == MAT_T_INT8 ) { /* Name not in tag */
                    name_len = buf[1];
                    if ( name_len % 8 > 0 ) {
                        if ( name_len < UINT32_MAX - 8 + (name_len % 8) ) {
                            name_len = name_len + 8 - (name_len % 8);
                        } else {
                            Mat_VarFree(cells[i]);
                            cells[i] = NULL;
                            break;
                        }
                    }
                    nBytes -= name_len;
                    (void)fseeko((FILE *)mat->fp, name_len, SEEK_CUR);
                }
            }
            cells[i]->internal->datapos = ftello((FILE *)mat->fp);
            if ( cells[i]->internal->datapos != -1L ) {
                if ( cells[i]->class_type == MAT_C_STRUCT )
                    bytesread += ReadNextStructField(mat, cells[i]);
                if ( cells[i]->class_type == MAT_C_CELL )
                    bytesread += ReadNextCell(mat, cells[i]);
                (void)fseeko((FILE *)mat->fp, cells[i]->internal->datapos + nBytes, SEEK_SET);
            } else {
                Mat_Critical("Couldn't determine file position");
            }
        }
    }

    return bytesread;
}

/** @brief Reads the next struct field of the structure in @c matvar
 *
 * Reads the next struct fields (fieldname length,names,data headers for all
 * the fields
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar MAT variable pointer
 * @return Number of bytes read
 */
static size_t
ReadNextStructField(mat_t *mat, matvar_t *matvar)
{
    mat_uint32_t fieldname_size;
    int err;
    size_t bytesread = 0, nfields, i;
    matvar_t **fields = NULL;
    size_t nelems = 1, nelems_x_nfields;

    err = Mat_MulDims(matvar, &nelems);
    if ( err ) {
        Mat_Critical("Integer multiplication overflow");
        return bytesread;
    }
    if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
#if HAVE_ZLIB
        mat_uint32_t uncomp_buf[16];
        mat_uint32_t array_flags, len;

        memset(&uncomp_buf, 0, sizeof(uncomp_buf));
        /* Field name length */
        err = Inflate(mat, matvar->internal->z, uncomp_buf, 8, &bytesread);
        if ( err ) {
            return bytesread;
        }
        if ( mat->byteswap ) {
            (void)Mat_uint32Swap(uncomp_buf);
            (void)Mat_uint32Swap(uncomp_buf + 1);
        }
        if ( (uncomp_buf[0] & 0x0000ffff) == MAT_T_INT32 && uncomp_buf[1] > 0 ) {
            fieldname_size = uncomp_buf[1];
        } else {
            Mat_Critical("Error getting fieldname size");
            return bytesread;
        }

        /* Field name tag */
        err = Inflate(mat, matvar->internal->z, uncomp_buf, 8, &bytesread);
        if ( err ) {
            return bytesread;
        }
        if ( mat->byteswap )
            (void)Mat_uint32Swap(uncomp_buf);
        /* Name of field */
        if ( uncomp_buf[0] == MAT_T_INT8 ) { /* Name not in tag */
            if ( mat->byteswap )
                len = Mat_uint32Swap(uncomp_buf + 1);
            else
                len = uncomp_buf[1];
            nfields = len / fieldname_size;
            if ( nfields * fieldname_size % 8 != 0 )
                i = 8 - (nfields * fieldname_size % 8);
            else
                i = 0;
            if ( nfields ) {
                char *ptr = (char *)malloc(nfields * fieldname_size + i);
                if ( NULL != ptr ) {
                    err = Inflate(mat, matvar->internal->z, ptr,
                                  (unsigned int)(nfields * fieldname_size + i), &bytesread);
                    if ( 0 == err ) {
                        SetFieldNames(matvar, ptr, nfields, fieldname_size);
                    } else {
                        matvar->internal->num_fields = nfields;
                        matvar->internal->fieldnames = NULL;
                    }
                    free(ptr);
                }
            } else {
                matvar->internal->num_fields = 0;
                matvar->internal->fieldnames = NULL;
            }
        } else {
            len = (uncomp_buf[0] & 0xffff0000) >> 16;
            if ( ((uncomp_buf[0] & 0x0000ffff) == MAT_T_INT8) && len > 0 && len <= 4 ) {
                /* Name packed in tag */
                nfields = len / fieldname_size;
                if ( nfields ) {
                    SetFieldNames(matvar, (char *)(uncomp_buf + 1), nfields, fieldname_size);
                } else {
                    matvar->internal->num_fields = 0;
                    matvar->internal->fieldnames = NULL;
                }
            } else {
                nfields = 0;
            }
        }

        matvar->data_size = sizeof(matvar_t *);
        err = Mul(&nelems_x_nfields, nelems, nfields);
        if ( err ) {
            Mat_Critical("Integer multiplication overflow");
            return bytesread;
        }
        err = Mul(&matvar->nbytes, nelems_x_nfields, matvar->data_size);
        if ( err ) {
            Mat_Critical("Integer multiplication overflow");
            return bytesread;
        }
        if ( !matvar->nbytes )
            return bytesread;

        matvar->data = calloc(nelems_x_nfields, matvar->data_size);
        if ( NULL == matvar->data ) {
            Mat_Critical("Couldn't allocate memory for the data");
            return bytesread;
        }

        fields = (matvar_t **)matvar->data;
        for ( i = 0; i < nelems; i++ ) {
            size_t k;
            for ( k = 0; k < nfields; k++ ) {
                fields[i * nfields + k] = Mat_VarCalloc();
            }
        }
        if ( NULL != matvar->internal->fieldnames ) {
            for ( i = 0; i < nelems; i++ ) {
                size_t k;
                for ( k = 0; k < nfields; k++ ) {
                    if ( NULL != matvar->internal->fieldnames[k] ) {
                        fields[i * nfields + k]->name = strdup(matvar->internal->fieldnames[k]);
                    }
                }
            }
        }

        for ( i = 0; i < nelems_x_nfields; i++ ) {
            mat_uint32_t nBytes;
            /* Read variable tag for struct field */
            err = Inflate(mat, matvar->internal->z, uncomp_buf, 8, &bytesread);
            if ( err ) {
                Mat_VarFree(fields[i]);
                fields[i] = NULL;
                break;
            }
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(uncomp_buf);
                (void)Mat_uint32Swap(uncomp_buf + 1);
            }
            nBytes = uncomp_buf[1];
            if ( uncomp_buf[0] != MAT_T_MATRIX ) {
                Mat_VarFree(fields[i]);
                fields[i] = NULL;
                Mat_Critical("fields[%zu], Uncompressed type not MAT_T_MATRIX", i);
                break;
            } else if ( 0 == nBytes ) {
                /* Empty field: Memory optimization */
                free(fields[i]->internal);
                fields[i]->internal = NULL;
                continue;
            }
            fields[i]->compression = MAT_COMPRESSION_ZLIB;
            err = Inflate(mat, matvar->internal->z, uncomp_buf, 16, &bytesread);
            if ( err ) {
                Mat_VarFree(fields[i]);
                fields[i] = NULL;
                break;
            }
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(uncomp_buf);
                (void)Mat_uint32Swap(uncomp_buf + 1);
                (void)Mat_uint32Swap(uncomp_buf + 2);
                (void)Mat_uint32Swap(uncomp_buf + 3);
            }
            nBytes -= 16;
            /* Array flags */
            if ( uncomp_buf[0] == MAT_T_UINT32 ) {
                array_flags = uncomp_buf[2];
                fields[i]->class_type = CLASS_FROM_ARRAY_FLAGS(array_flags);
                fields[i]->isComplex = (array_flags & MAT_F_COMPLEX);
                fields[i]->isGlobal = (array_flags & MAT_F_GLOBAL);
                fields[i]->isLogical = (array_flags & MAT_F_LOGICAL);
                if ( fields[i]->class_type == MAT_C_SPARSE ) {
                    /* Need to find a more appropriate place to store nzmax */
                    fields[i]->nbytes = uncomp_buf[3];
                }
            } else {
                Mat_Critical("Expected MAT_T_UINT32 for array tags, got %d", uncomp_buf[0]);
                InflateSkip(mat, matvar->internal->z, nBytes, &bytesread);
            }
            if ( fields[i]->class_type != MAT_C_OPAQUE ) {
                mat_uint32_t *dims = NULL;
                int do_clean = 0;
                err = InflateRankDims(mat, matvar->internal->z, uncomp_buf, sizeof(uncomp_buf),
                                      &dims, &bytesread);
                if ( NULL == dims ) {
                    dims = uncomp_buf + 2;
                } else {
                    do_clean = 1;
                }
                if ( err ) {
                    if ( do_clean ) {
                        free(dims);
                    }
                    Mat_VarFree(fields[i]);
                    fields[i] = NULL;
                    break;
                }
                nBytes -= 8;
                if ( mat->byteswap ) {
                    (void)Mat_uint32Swap(uncomp_buf);
                    (void)Mat_uint32Swap(uncomp_buf + 1);
                }
                /* Rank and dimension */
                if ( uncomp_buf[0] == MAT_T_INT32 ) {
                    int j;
                    size_t size;
                    fields[i]->rank = uncomp_buf[1];
                    nBytes -= fields[i]->rank;
                    fields[i]->rank /= 4;
                    if ( 0 == do_clean && fields[i]->rank > 13 ) {
                        int rank = fields[i]->rank;
                        fields[i]->rank = 0;
                        Mat_Critical("%d is not a valid rank", rank);
                        continue;
                    }
                    err = Mul(&size, fields[i]->rank, sizeof(*fields[i]->dims));
                    if ( err ) {
                        if ( do_clean ) {
                            free(dims);
                        }
                        Mat_VarFree(fields[i]);
                        fields[i] = NULL;
                        Mat_Critical("Integer multiplication overflow");
                        continue;
                    }
                    fields[i]->dims = (size_t *)malloc(size);
                    if ( mat->byteswap ) {
                        for ( j = 0; j < fields[i]->rank; j++ )
                            fields[i]->dims[j] = Mat_uint32Swap(dims + j);
                    } else {
                        for ( j = 0; j < fields[i]->rank; j++ )
                            fields[i]->dims[j] = dims[j];
                    }
                    if ( fields[i]->rank % 2 != 0 )
                        nBytes -= 4;
                }
                if ( do_clean ) {
                    free(dims);
                }
                /* Variable name tag */
                err = Inflate(mat, matvar->internal->z, uncomp_buf, 8, &bytesread);
                if ( err ) {
                    Mat_VarFree(fields[i]);
                    fields[i] = NULL;
                    break;
                }
                nBytes -= 8;
                fields[i]->internal->z = (z_streamp)calloc(1, sizeof(z_stream));
                if ( fields[i]->internal->z != NULL ) {
                    err = inflateCopy(fields[i]->internal->z, matvar->internal->z);
                    if ( err == Z_OK ) {
                        fields[i]->internal->datapos = ftello((FILE *)mat->fp);
                        if ( fields[i]->internal->datapos != -1L ) {
                            fields[i]->internal->datapos -= matvar->internal->z->avail_in;
                            if ( fields[i]->class_type == MAT_C_STRUCT )
                                bytesread += ReadNextStructField(mat, fields[i]);
                            else if ( fields[i]->class_type == MAT_C_CELL )
                                bytesread += ReadNextCell(mat, fields[i]);
                            else if ( nBytes <= (1 << MAX_WBITS) ) {
                                /* Memory optimization: Read data if less in size
                                   than the zlib inflate state (approximately) */
                                err = Mat_VarRead5(mat, fields[i]);
                                fields[i]->internal->data = fields[i]->data;
                                fields[i]->data = NULL;
                            }
                            (void)fseeko((FILE *)mat->fp, fields[i]->internal->datapos, SEEK_SET);
                        } else {
                            Mat_Critical("Couldn't determine file position");
                        }
                        if ( fields[i]->internal->data != NULL ||
                             fields[i]->class_type == MAT_C_STRUCT ||
                             fields[i]->class_type == MAT_C_CELL ) {
                            /* Memory optimization: Free inflate state */
                            inflateEnd(fields[i]->internal->z);
                            free(fields[i]->internal->z);
                            fields[i]->internal->z = NULL;
                        }
                    } else {
                        Mat_Critical("inflateCopy returned error %s", zError(err));
                    }
                } else {
                    Mat_Critical("Couldn't allocate memory");
                }
            }
            InflateSkip(mat, matvar->internal->z, nBytes, &bytesread);
        }
#else
        Mat_Critical("Not compiled with zlib support");
#endif
    } else {
        mat_uint32_t buf[6] = {0, 0, 0, 0, 0, 0};
        mat_uint32_t array_flags, len;

        err = Read(buf, 4, 2, (FILE *)mat->fp, &bytesread);
        if ( err ) {
            return bytesread;
        }
        if ( mat->byteswap ) {
            (void)Mat_uint32Swap(buf);
            (void)Mat_uint32Swap(buf + 1);
        }
        if ( (buf[0] & 0x0000ffff) == MAT_T_INT32 && buf[1] > 0 ) {
            fieldname_size = buf[1];
        } else {
            Mat_Critical("Error getting fieldname size");
            return bytesread;
        }

        /* Field name tag */
        err = Read(buf, 4, 2, (FILE *)mat->fp, &bytesread);
        if ( err ) {
            return bytesread;
        }
        if ( mat->byteswap )
            (void)Mat_uint32Swap(buf);
        /* Name of field */
        if ( buf[0] == MAT_T_INT8 ) { /* Name not in tag */
            if ( mat->byteswap )
                len = Mat_uint32Swap(buf + 1);
            else
                len = buf[1];
            nfields = len / fieldname_size;
            if ( nfields ) {
                char *ptr = (char *)malloc(nfields * fieldname_size);
                if ( NULL != ptr ) {
                    err = Read(ptr, 1, nfields * fieldname_size, (FILE *)mat->fp, &bytesread);
                    if ( 0 == err ) {
                        SetFieldNames(matvar, ptr, nfields, fieldname_size);
                    } else {
                        matvar->internal->num_fields = nfields;
                        matvar->internal->fieldnames = NULL;
                    }
                    free(ptr);
                }
                if ( (nfields * fieldname_size) % 8 ) {
                    (void)fseeko((FILE *)mat->fp, 8 - ((nfields * fieldname_size) % 8), SEEK_CUR);
                    bytesread += 8 - ((nfields * fieldname_size) % 8);
                }
            } else {
                matvar->internal->num_fields = 0;
                matvar->internal->fieldnames = NULL;
            }
        } else {
            len = (buf[0] & 0xffff0000) >> 16;
            if ( ((buf[0] & 0x0000ffff) == MAT_T_INT8) && len > 0 && len <= 4 ) {
                /* Name packed in tag */
                nfields = len / fieldname_size;
                if ( nfields ) {
                    SetFieldNames(matvar, (char *)(buf + 1), nfields, fieldname_size);
                } else {
                    matvar->internal->num_fields = 0;
                    matvar->internal->fieldnames = NULL;
                }
            } else {
                nfields = 0;
            }
        }

        matvar->data_size = sizeof(matvar_t *);
        err = Mul(&nelems_x_nfields, nelems, nfields);
        if ( err ) {
            Mat_Critical("Integer multiplication overflow");
            return bytesread;
        }
        err = Mul(&matvar->nbytes, nelems_x_nfields, matvar->data_size);
        if ( err ) {
            Mat_Critical("Integer multiplication overflow");
            return bytesread;
        }
        if ( !matvar->nbytes )
            return bytesread;

        matvar->data = calloc(nelems_x_nfields, matvar->data_size);
        if ( NULL == matvar->data ) {
            Mat_Critical("Couldn't allocate memory for the data");
            return bytesread;
        }

        fields = (matvar_t **)matvar->data;
        for ( i = 0; i < nelems_x_nfields; i++ ) {
            mat_uint32_t nBytes;

            fields[i] = Mat_VarCalloc();
            if ( NULL == fields[i] ) {
                Mat_Critical("Couldn't allocate memory for field %zu", i);
                continue;
            }

            /* Read variable tag for struct field */
            err = Read(buf, 4, 2, (FILE *)mat->fp, &bytesread);
            if ( err ) {
                Mat_VarFree(fields[i]);
                fields[i] = NULL;
                break;
            }
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(buf);
                (void)Mat_uint32Swap(buf + 1);
            }
            nBytes = buf[1];
            if ( buf[0] != MAT_T_MATRIX ) {
                Mat_VarFree(fields[i]);
                fields[i] = NULL;
#if HAVE_LONG_LONG_INT
                Mat_Critical("fields[%zu] not MAT_T_MATRIX, fpos = %lld", i,
                             (long long)ftello((FILE *)mat->fp));
#else
                Mat_Critical("fields[%zu] not MAT_T_MATRIX", i);
#endif
                break;
            } else if ( 0 == nBytes ) {
                /* Empty field: Memory optimization */
                free(fields[i]->internal);
                fields[i]->internal = NULL;
                continue;
            }

            /* Read array flags and the dimensions tag */
            err = Read(buf, 4, 6, (FILE *)mat->fp, &bytesread);
            if ( err ) {
                Mat_VarFree(fields[i]);
                fields[i] = NULL;
                break;
            }
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(buf);
                (void)Mat_uint32Swap(buf + 1);
                (void)Mat_uint32Swap(buf + 2);
                (void)Mat_uint32Swap(buf + 3);
                (void)Mat_uint32Swap(buf + 4);
                (void)Mat_uint32Swap(buf + 5);
            }
            nBytes -= 24;
            /* Array flags */
            if ( buf[0] == MAT_T_UINT32 ) {
                array_flags = buf[2];
                fields[i]->class_type = CLASS_FROM_ARRAY_FLAGS(array_flags);
                fields[i]->isComplex = (array_flags & MAT_F_COMPLEX);
                fields[i]->isGlobal = (array_flags & MAT_F_GLOBAL);
                fields[i]->isLogical = (array_flags & MAT_F_LOGICAL);
                if ( fields[i]->class_type == MAT_C_SPARSE ) {
                    /* Need to find a more appropriate place to store nzmax */
                    fields[i]->nbytes = buf[3];
                }
            }
            /* Rank and dimension */
            {
                size_t nbytes = 0;
                err = ReadRankDims(mat, fields[i], (enum matio_types)buf[4], buf[5], &nbytes);
                bytesread += nbytes;
                nBytes -= nbytes;
                if ( err ) {
                    Mat_VarFree(fields[i]);
                    fields[i] = NULL;
                    break;
                }
            }
            /* Variable name tag */
            err = Read(buf, 1, 8, (FILE *)mat->fp, &bytesread);
            if ( err ) {
                Mat_VarFree(fields[i]);
                fields[i] = NULL;
                break;
            }
            nBytes -= 8;
            fields[i]->internal->datapos = ftello((FILE *)mat->fp);
            if ( fields[i]->internal->datapos != -1L ) {
                if ( fields[i]->class_type == MAT_C_STRUCT )
                    bytesread += ReadNextStructField(mat, fields[i]);
                else if ( fields[i]->class_type == MAT_C_CELL )
                    bytesread += ReadNextCell(mat, fields[i]);
                (void)fseeko((FILE *)mat->fp, fields[i]->internal->datapos + nBytes, SEEK_SET);
            } else {
                Mat_Critical("Couldn't determine file position");
            }
        }

        if ( NULL != matvar->internal->fieldnames ) {
            for ( i = 0; i < nelems; i++ ) {
                size_t k;
                for ( k = 0; k < nfields; k++ ) {
                    if ( NULL != matvar->internal->fieldnames[k] &&
                         NULL != fields[i * nfields + k] ) {
                        fields[i * nfields + k]->name = strdup(matvar->internal->fieldnames[k]);
                    }
                }
            }
        }
    }

    return bytesread;
}

/** @brief Reads the function handle data of the function handle in @c matvar
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar MAT variable pointer
 * @return Number of bytes read
 */
static size_t
ReadNextFunctionHandle(mat_t *mat, matvar_t *matvar)
{
    int err;
    size_t nelems = 1;

    err = Mat_MulDims(matvar, &nelems);
    matvar->data_size = sizeof(matvar_t *);
    err |= Mul(&matvar->nbytes, nelems, matvar->data_size);
    if ( err )
        return 0;

    matvar->data = malloc(matvar->nbytes);
    if ( matvar->data != NULL ) {
        size_t i;
        matvar_t **functions = (matvar_t **)matvar->data;
        for ( i = 0; i < nelems; i++ ) {
            functions[i] = Mat_VarReadNextInfo(mat);
            err = NULL == functions[i];
            if ( err )
                break;
        }
        if ( err ) {
            size_t j;
            for ( j = 0; j < i; j++ ) {
                free(functions[j]);
            }
            free(matvar->data);
            matvar->data = NULL;
            matvar->data_size = 0;
            matvar->nbytes = 0;
        }
    } else {
        matvar->data_size = 0;
        matvar->nbytes = 0;
    }

    return 0;
}

/** @brief Reads the rank and dimensions in @c matvar
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar MAT variable pointer
 * @param data_type data type of dimension array
 * @param nbytes len of dimension array in bytes
 * @param[out] read_bytes Read bytes
 * @retval 0 on success
 */
static int
ReadRankDims(mat_t *mat, matvar_t *matvar, enum matio_types data_type, mat_uint32_t nbytes,
             size_t *read_bytes)
{
    int err = MATIO_E_NO_ERROR;
    /* Rank and dimension */
    if ( data_type == MAT_T_INT32 ) {
        matvar->rank = nbytes / sizeof(mat_uint32_t);
        matvar->dims = (size_t *)malloc(matvar->rank * sizeof(*matvar->dims));
        if ( NULL != matvar->dims ) {
            int i;
            mat_uint32_t buf;

            for ( i = 0; i < matvar->rank; i++ ) {
                err = Read(&buf, sizeof(mat_uint32_t), 1, (FILE *)mat->fp, read_bytes);
                if ( MATIO_E_NO_ERROR == err ) {
                    if ( mat->byteswap ) {
                        matvar->dims[i] = Mat_uint32Swap(&buf);
                    } else {
                        matvar->dims[i] = buf;
                    }
                } else {
                    free(matvar->dims);
                    matvar->dims = NULL;
                    matvar->rank = 0;
                    return err;
                }
            }

            if ( matvar->rank % 2 != 0 ) {
                err = Read(&buf, sizeof(mat_uint32_t), 1, (FILE *)mat->fp, read_bytes);
                if ( err ) {
                    free(matvar->dims);
                    matvar->dims = NULL;
                    matvar->rank = 0;
                    return err;
                }
            }
        } else {
            matvar->rank = 0;
            err = MATIO_E_OUT_OF_MEMORY;
            Mat_Critical("Error allocating memory for dims");
        }
    }
    return err;
}

/** @brief Writes the header and data for a given type
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @retval 0 on success
 */
static int
WriteType(mat_t *mat, matvar_t *matvar)
{
    int err;
    const mat_uint8_t pad1 = 0;
    int nBytes, j;
    size_t nelems = 1;

    err = Mat_MulDims(matvar, &nelems);
    if ( err )
        return err;

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
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = (mat_complex_split_t *)matvar->data;

                if ( NULL == matvar->data )
                    complex_data = &null_complex_data;

                nBytes = WriteData(mat, complex_data->Re, nelems, matvar->data_type);
                if ( nBytes % 8 )
                    for ( j = nBytes % 8; j < 8; j++ )
                        fwrite(&pad1, 1, 1, (FILE *)mat->fp);
                nBytes = WriteData(mat, complex_data->Im, nelems, matvar->data_type);
                if ( nBytes % 8 )
                    for ( j = nBytes % 8; j < 8; j++ )
                        fwrite(&pad1, 1, 1, (FILE *)mat->fp);
            } else {
                nBytes = WriteData(mat, matvar->data, nelems, matvar->data_type);
                if ( nBytes % 8 )
                    for ( j = nBytes % 8; j < 8; j++ )
                        fwrite(&pad1, 1, 1, (FILE *)mat->fp);
            }
            break;
        }
        case MAT_C_CHAR:
            if ( matvar->data_type == MAT_T_UTF8 ) {
                nelems = matvar->nbytes;
            }
            nBytes = WriteCharData(mat, matvar->data, nelems, matvar->data_type);
            break;
        case MAT_C_CELL: {
            size_t i;
            matvar_t **cells = (matvar_t **)matvar->data;

            /* Check for an empty cell array */
            if ( matvar->nbytes == 0 || matvar->data_size == 0 || matvar->data == NULL )
                break;
            nelems = matvar->nbytes / matvar->data_size;
            for ( i = 0; i < nelems; i++ )
                WriteCellArrayField(mat, cells[i]);
            break;
        }
        case MAT_C_STRUCT: {
            const mat_uint32_t array_name_type = MAT_T_INT8;
            const mat_uint32_t fieldname_type = MAT_T_INT32;
            const mat_uint32_t fieldname_data_size = 4;
            char *padzero;
            mat_uint32_t fieldname_size;
            size_t maxlen = 0, nfields, i, nelems_x_nfields;
            matvar_t **fields = (matvar_t **)matvar->data;
            mat_uint32_t fieldname;

            /* nelems*matvar->data_size can be zero when saving a struct that
             * contains an empty struct in one of its fields
             * (e.g. x.y = struct('z', {})). If it's zero, we would divide
             * by zero.
             */
            nfields = matvar->internal->num_fields;
            /* Check for a structure with no fields */
            if ( nfields < 1 ) {
                fieldname = (fieldname_data_size << 16) | fieldname_type;
                fwrite(&fieldname, 4, 1, (FILE *)mat->fp);
                fieldname_size = 1;
                fwrite(&fieldname_size, 4, 1, (FILE *)mat->fp);
                fwrite(&array_name_type, 4, 1, (FILE *)mat->fp);
                nBytes = 0;
                fwrite(&nBytes, 4, 1, (FILE *)mat->fp);
                break;
            }

            for ( i = 0; i < nfields; i++ ) {
                size_t len = strlen(matvar->internal->fieldnames[i]);
                if ( len > maxlen )
                    maxlen = len;
            }
            maxlen++;
            fieldname_size = maxlen;
            while ( nfields * fieldname_size % 8 != 0 )
                fieldname_size++;
            fieldname = (fieldname_data_size << 16) | fieldname_type;
            fwrite(&fieldname, 4, 1, (FILE *)mat->fp);
            fwrite(&fieldname_size, 4, 1, (FILE *)mat->fp);
            fwrite(&array_name_type, 4, 1, (FILE *)mat->fp);
            nBytes = nfields * fieldname_size;
            fwrite(&nBytes, 4, 1, (FILE *)mat->fp);
            padzero = (char *)calloc(fieldname_size, 1);
            for ( i = 0; i < nfields; i++ ) {
                size_t len = strlen(matvar->internal->fieldnames[i]);
                fwrite(matvar->internal->fieldnames[i], 1, len, (FILE *)mat->fp);
                fwrite(padzero, 1, fieldname_size - len, (FILE *)mat->fp);
            }
            free(padzero);
            err = Mul(&nelems_x_nfields, nelems, nfields);
            if ( err )
                break;
            for ( i = 0; i < nelems_x_nfields; i++ )
                WriteStructField(mat, fields[i]);
            break;
        }
        case MAT_C_SPARSE: {
            mat_sparse_t *sparse = (mat_sparse_t *)matvar->data;

            nBytes = WriteData(mat, sparse->ir, sparse->nir, MAT_T_UINT32);
            if ( nBytes % 8 )
                for ( j = nBytes % 8; j < 8; j++ )
                    fwrite(&pad1, 1, 1, (FILE *)mat->fp);
            nBytes = WriteData(mat, sparse->jc, sparse->njc, MAT_T_UINT32);
            if ( nBytes % 8 )
                for ( j = nBytes % 8; j < 8; j++ )
                    fwrite(&pad1, 1, 1, (FILE *)mat->fp);
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = (mat_complex_split_t *)sparse->data;
                nBytes = WriteData(mat, complex_data->Re, sparse->ndata, matvar->data_type);
                if ( nBytes % 8 )
                    for ( j = nBytes % 8; j < 8; j++ )
                        fwrite(&pad1, 1, 1, (FILE *)mat->fp);
                nBytes = WriteData(mat, complex_data->Im, sparse->ndata, matvar->data_type);
                if ( nBytes % 8 )
                    for ( j = nBytes % 8; j < 8; j++ )
                        fwrite(&pad1, 1, 1, (FILE *)mat->fp);
            } else {
                nBytes = WriteData(mat, sparse->data, sparse->ndata, matvar->data_type);
                if ( nBytes % 8 )
                    for ( j = nBytes % 8; j < 8; j++ )
                        fwrite(&pad1, 1, 1, (FILE *)mat->fp);
            }
            break;
        }
        case MAT_C_FUNCTION:
        case MAT_C_OBJECT:
        case MAT_C_EMPTY:
        case MAT_C_OPAQUE:
            break;
        default:
            err = MATIO_E_OUTPUT_BAD_DATA;
            break;
    }

    return err;
}

/** @brief Writes the header and data for an element of a cell array
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @retval 0 on success
 */
static int
WriteCellArrayField(mat_t *mat, matvar_t *matvar)
{
    mat_uint32_t array_flags, nzmax = 0;
    int array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int array_flags_size = 8, matrix_type = MAT_T_MATRIX;
    const mat_uint32_t pad4 = 0;
    const mat_uint8_t pad1 = 0;
    int nBytes, i;
    mat_off_t start = 0, end = 0;

    if ( matvar == NULL || mat == NULL )
        return MATIO_E_BAD_ARGUMENT;

    fwrite(&matrix_type, 4, 1, (FILE *)mat->fp);
    fwrite(&pad4, 4, 1, (FILE *)mat->fp);
    if ( MAT_C_EMPTY == matvar->class_type ) {
        /* exit early if this is an empty data */
        return MATIO_E_NO_ERROR;
    }
    start = ftello((FILE *)mat->fp);

    /* Array Flags */
    array_flags = matvar->class_type & CLASS_TYPE_MASK;
    if ( matvar->isComplex )
        array_flags |= MAT_F_COMPLEX;
    if ( matvar->isGlobal )
        array_flags |= MAT_F_GLOBAL;
    if ( matvar->isLogical )
        array_flags |= MAT_F_LOGICAL;
    if ( matvar->class_type == MAT_C_SPARSE )
        nzmax = ((mat_sparse_t *)matvar->data)->nzmax;

    if ( mat->byteswap )
        array_flags = Mat_int32Swap((mat_int32_t *)&array_flags);
    fwrite(&array_flags_type, 4, 1, (FILE *)mat->fp);
    fwrite(&array_flags_size, 4, 1, (FILE *)mat->fp);
    fwrite(&array_flags, 4, 1, (FILE *)mat->fp);
    fwrite(&nzmax, 4, 1, (FILE *)mat->fp);
    /* Rank and Dimension */
    nBytes = matvar->rank * 4;
    fwrite(&dims_array_type, 4, 1, (FILE *)mat->fp);
    fwrite(&nBytes, 4, 1, (FILE *)mat->fp);
    for ( i = 0; i < matvar->rank; i++ ) {
        mat_int32_t dim;
        dim = matvar->dims[i];
        fwrite(&dim, 4, 1, (FILE *)mat->fp);
    }
    if ( matvar->rank % 2 != 0 )
        fwrite(&pad4, 4, 1, (FILE *)mat->fp);
    /* Name of variable */
    if ( !matvar->name ) {
        const mat_uint32_t array_name_type = MAT_T_INT8;
        fwrite(&array_name_type, 4, 1, (FILE *)mat->fp);
        fwrite(&pad4, 4, 1, (FILE *)mat->fp);
    } else if ( strlen(matvar->name) <= 4 ) {
        mat_uint32_t array_name_type = MAT_T_INT8;
        const mat_uint32_t array_name_len = (mat_uint32_t)strlen(matvar->name);
        array_name_type |= array_name_len << 16;
        fwrite(&array_name_type, 4, 1, (FILE *)mat->fp);
        fwrite(matvar->name, 1, array_name_len, (FILE *)mat->fp);
        for ( i = array_name_len; i < 4; i++ )
            fwrite(&pad1, 1, 1, (FILE *)mat->fp);
    } else {
        const mat_uint32_t array_name_type = MAT_T_INT8;
        const mat_uint32_t array_name_len = (mat_uint32_t)strlen(matvar->name);
        fwrite(&array_name_type, 4, 1, (FILE *)mat->fp);
        fwrite(&array_name_len, 4, 1, (FILE *)mat->fp);
        fwrite(matvar->name, 1, array_name_len, (FILE *)mat->fp);
        if ( array_name_len % 8 )
            for ( i = array_name_len % 8; i < 8; i++ )
                fwrite(&pad1, 1, 1, (FILE *)mat->fp);
    }

    WriteType(mat, matvar);
    end = ftello((FILE *)mat->fp);
    if ( start != -1L && end != -1L ) {
        nBytes = (int)(end - start);
        (void)fseeko((FILE *)mat->fp, (mat_off_t) - (nBytes + 4), SEEK_CUR);
        fwrite(&nBytes, 4, 1, (FILE *)mat->fp);
        (void)fseeko((FILE *)mat->fp, end, SEEK_SET);
    } else {
        Mat_Critical("Couldn't determine file position");
    }

    return MATIO_E_NO_ERROR;
}

#if HAVE_ZLIB
/** @brief Writes the header and data for a given class type
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @return number of bytes written to the MAT file
 */
static size_t
WriteCompressedTypeArrayFlags(mat_t *mat, matvar_t *matvar, z_streamp z)
{
    mat_uint32_t array_flags;
    const mat_uint32_t array_name_type = MAT_T_INT8;
    int array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int array_flags_size = 8;
    int nBytes, i, nzmax = 0;

    mat_uint32_t comp_buf[512];
    mat_uint32_t uncomp_buf[512];
    int buf_size = 512;
    size_t byteswritten = 0;

    if ( MAT_C_EMPTY == matvar->class_type ) {
        /* exit early if this is an empty data */
        return byteswritten;
    }

    memset(&uncomp_buf, 0, sizeof(uncomp_buf));
    /* Array Flags */
    array_flags = matvar->class_type & CLASS_TYPE_MASK;
    if ( matvar->isComplex )
        array_flags |= MAT_F_COMPLEX;
    if ( matvar->isGlobal )
        array_flags |= MAT_F_GLOBAL;
    if ( matvar->isLogical )
        array_flags |= MAT_F_LOGICAL;
    if ( matvar->class_type == MAT_C_SPARSE )
        nzmax = ((mat_sparse_t *)matvar->data)->nzmax;
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
        uncomp_buf[6 + i] = dim;
    }
    if ( matvar->rank % 2 != 0 ) {
        const mat_uint32_t pad4 = 0;
        uncomp_buf[6 + i] = pad4;
        i++;
    }

    z->next_in = ZLIB_BYTE_PTR(uncomp_buf);
    z->avail_in = (6 + i) * sizeof(*uncomp_buf);
    do {
        z->next_out = ZLIB_BYTE_PTR(comp_buf);
        z->avail_out = buf_size * sizeof(*comp_buf);
        deflate(z, Z_NO_FLUSH);
        byteswritten +=
            fwrite(comp_buf, 1, buf_size * sizeof(*comp_buf) - z->avail_out, (FILE *)mat->fp);
    } while ( z->avail_out == 0 );
    /* Name of variable */
    uncomp_buf[0] = array_name_type;
    uncomp_buf[1] = 0;
    z->next_in = ZLIB_BYTE_PTR(uncomp_buf);
    z->avail_in = 8;
    do {
        z->next_out = ZLIB_BYTE_PTR(comp_buf);
        z->avail_out = buf_size * sizeof(*comp_buf);
        deflate(z, Z_NO_FLUSH);
        byteswritten +=
            fwrite(comp_buf, 1, buf_size * sizeof(*comp_buf) - z->avail_out, (FILE *)mat->fp);
    } while ( z->avail_out == 0 );

    matvar->internal->datapos = ftello((FILE *)mat->fp);
    if ( matvar->internal->datapos == -1L ) {
        Mat_Critical("Couldn't determine file position");
    }

    byteswritten += WriteCompressedType(mat, matvar, z);
    return byteswritten;
}

/** @brief Writes the header and data for a given class type
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @return number of bytes written to the MAT file
 */
static size_t
WriteCompressedType(mat_t *mat, matvar_t *matvar, z_streamp z)
{
    int err;
    mat_uint32_t comp_buf[512];
    mat_uint32_t uncomp_buf[512];
    size_t byteswritten = 0, nelems = 1;

    if ( MAT_C_EMPTY == matvar->class_type ) {
        /* exit early if this is an empty data */
        return byteswritten;
    }

    memset(&uncomp_buf, 0, sizeof(uncomp_buf));
    err = Mat_MulDims(matvar, &nelems);
    if ( err ) {
        Mat_Critical("Integer multiplication overflow");
        return byteswritten;
    }

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
            /* WriteCompressedData makes sure uncompressed data is aligned
             * on an 8-byte boundary */
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = (mat_complex_split_t *)matvar->data;

                if ( NULL == matvar->data )
                    complex_data = &null_complex_data;

                byteswritten +=
                    WriteCompressedData(mat, z, complex_data->Re, nelems, matvar->data_type);
                byteswritten +=
                    WriteCompressedData(mat, z, complex_data->Im, nelems, matvar->data_type);
            } else {
                byteswritten +=
                    WriteCompressedData(mat, z, matvar->data, nelems, matvar->data_type);
            }
            break;
        }
        case MAT_C_CHAR: {
            if ( matvar->data_type == MAT_T_UTF8 ) {
                nelems = matvar->nbytes;
            }
            byteswritten +=
                WriteCompressedCharData(mat, z, matvar->data, nelems, matvar->data_type);
            break;
        }
        case MAT_C_CELL: {
            size_t i;
            matvar_t **cells = (matvar_t **)matvar->data;

            /* Check for an empty cell array */
            if ( matvar->nbytes == 0 || matvar->data_size == 0 || matvar->data == NULL )
                break;
            nelems = matvar->nbytes / matvar->data_size;
            for ( i = 0; i < nelems; i++ )
                WriteCompressedCellArrayField(mat, cells[i], z);
            break;
        }
        case MAT_C_STRUCT: {
            int buf_size = 512;
            const mat_uint32_t fieldname_type = MAT_T_INT32;
            const mat_uint32_t fieldname_data_size = 4;
            unsigned char *padzero;
            int fieldname_size;
            size_t maxlen = 0, nfields, i, nelems_x_nfields;
            const mat_uint32_t array_name_type = MAT_T_INT8;
            matvar_t **fields = (matvar_t **)matvar->data;

            nfields = matvar->internal->num_fields;
            /* Check for a structure with no fields */
            if ( nfields < 1 ) {
                fieldname_size = 1;
                uncomp_buf[0] = (fieldname_data_size << 16) | fieldname_type;
                uncomp_buf[1] = fieldname_size;
                uncomp_buf[2] = array_name_type;
                uncomp_buf[3] = 0;
                z->next_in = ZLIB_BYTE_PTR(uncomp_buf);
                z->avail_in = 16;
                do {
                    z->next_out = ZLIB_BYTE_PTR(comp_buf);
                    z->avail_out = buf_size * sizeof(*comp_buf);
                    deflate(z, Z_NO_FLUSH);
                    byteswritten += fwrite(comp_buf, 1, buf_size * sizeof(*comp_buf) - z->avail_out,
                                           (FILE *)mat->fp);
                } while ( z->avail_out == 0 );
                break;
            }

            for ( i = 0; i < nfields; i++ ) {
                size_t len = strlen(matvar->internal->fieldnames[i]);
                if ( len > maxlen )
                    maxlen = len;
            }
            maxlen++;
            fieldname_size = maxlen;
            while ( nfields * fieldname_size % 8 != 0 )
                fieldname_size++;
            uncomp_buf[0] = (fieldname_data_size << 16) | fieldname_type;
            uncomp_buf[1] = fieldname_size;
            uncomp_buf[2] = array_name_type;
            uncomp_buf[3] = nfields * fieldname_size;

            padzero = (unsigned char *)calloc(fieldname_size, 1);
            z->next_in = ZLIB_BYTE_PTR(uncomp_buf);
            z->avail_in = 16;
            do {
                z->next_out = ZLIB_BYTE_PTR(comp_buf);
                z->avail_out = buf_size * sizeof(*comp_buf);
                deflate(z, Z_NO_FLUSH);
                byteswritten += fwrite(comp_buf, 1, buf_size * sizeof(*comp_buf) - z->avail_out,
                                       (FILE *)mat->fp);
            } while ( z->avail_out == 0 );
            for ( i = 0; i < nfields; i++ ) {
                size_t len = strlen(matvar->internal->fieldnames[i]);
                memset(padzero, '\0', fieldname_size);
                memcpy(padzero, matvar->internal->fieldnames[i], len);
                z->next_in = ZLIB_BYTE_PTR(padzero);
                z->avail_in = fieldname_size;
                do {
                    z->next_out = ZLIB_BYTE_PTR(comp_buf);
                    z->avail_out = buf_size * sizeof(*comp_buf);
                    deflate(z, Z_NO_FLUSH);
                    byteswritten += fwrite(comp_buf, 1, buf_size * sizeof(*comp_buf) - z->avail_out,
                                           (FILE *)mat->fp);
                } while ( z->avail_out == 0 );
            }
            free(padzero);
            err = Mul(&nelems_x_nfields, nelems, nfields);
            if ( err ) {
                Mat_Critical("Integer multiplication overflow");
                return byteswritten;
            }
            for ( i = 0; i < nelems_x_nfields; i++ )
                byteswritten += WriteCompressedStructField(mat, fields[i], z);
            break;
        }
        case MAT_C_SPARSE: {
            mat_sparse_t *sparse = (mat_sparse_t *)matvar->data;

            byteswritten += WriteCompressedData(mat, z, sparse->ir, sparse->nir, MAT_T_UINT32);
            byteswritten += WriteCompressedData(mat, z, sparse->jc, sparse->njc, MAT_T_UINT32);
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = (mat_complex_split_t *)sparse->data;
                byteswritten +=
                    WriteCompressedData(mat, z, complex_data->Re, sparse->ndata, matvar->data_type);
                byteswritten +=
                    WriteCompressedData(mat, z, complex_data->Im, sparse->ndata, matvar->data_type);
            } else {
                byteswritten +=
                    WriteCompressedData(mat, z, sparse->data, sparse->ndata, matvar->data_type);
            }
            break;
        }
        case MAT_C_FUNCTION:
        case MAT_C_OBJECT:
        case MAT_C_EMPTY:
        case MAT_C_OPAQUE:
            break;
    }

    return byteswritten;
}

/** @brief Writes the header and data for a field of a compressed cell array
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @return number of bytes written to the MAT file
 */
static size_t
WriteCompressedCellArrayField(mat_t *mat, matvar_t *matvar, z_streamp z)
{
    mat_uint32_t comp_buf[512];
    mat_uint32_t uncomp_buf[512];
    int buf_size = 512;
    size_t byteswritten = 0, field_buf_size;

    if ( NULL == matvar || NULL == mat || NULL == z )
        return 0;

    memset(&uncomp_buf, 0, sizeof(uncomp_buf));
    uncomp_buf[0] = MAT_T_MATRIX;
    if ( MAT_C_EMPTY != matvar->class_type ) {
        int err = GetCellArrayFieldBufSize(matvar, &field_buf_size);
        if ( err || field_buf_size > UINT32_MAX )
            return 0;

        uncomp_buf[1] = field_buf_size;
    } else {
        uncomp_buf[1] = 0;
    }
    z->next_in = ZLIB_BYTE_PTR(uncomp_buf);
    z->avail_in = 8;
    do {
        z->next_out = ZLIB_BYTE_PTR(comp_buf);
        z->avail_out = buf_size * sizeof(*comp_buf);
        deflate(z, Z_NO_FLUSH);
        byteswritten +=
            fwrite(comp_buf, 1, buf_size * sizeof(*comp_buf) - z->avail_out, (FILE *)mat->fp);
    } while ( z->avail_out == 0 );

    byteswritten += WriteCompressedTypeArrayFlags(mat, matvar, z);
    return byteswritten;
}
#endif

/** @brief Writes the header and data for a field of a struct array
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @retval 0 on success
 */
static int
WriteStructField(mat_t *mat, matvar_t *matvar)
{
    mat_uint32_t array_flags;
    const mat_uint32_t array_name_type = MAT_T_INT8;
    int array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int array_flags_size = 8, matrix_type = MAT_T_MATRIX;
    const mat_uint32_t pad4 = 0;
    int nBytes, i, nzmax = 0;
    mat_off_t start = 0, end = 0;

    if ( mat == NULL )
        return MATIO_E_BAD_ARGUMENT;

    if ( NULL == matvar ) {
        size_t dims[2] = {0, 0};
        Mat_WriteEmptyVariable5(mat, NULL, 2, dims);
        return MATIO_E_NO_ERROR;
    }

    fwrite(&matrix_type, 4, 1, (FILE *)mat->fp);
    fwrite(&pad4, 4, 1, (FILE *)mat->fp);
    if ( MAT_C_EMPTY == matvar->class_type ) {
        /* exit early if this is an empty data */
        return MATIO_E_NO_ERROR;
    }
    start = ftello((FILE *)mat->fp);

    /* Array Flags */
    array_flags = matvar->class_type & CLASS_TYPE_MASK;
    if ( matvar->isComplex )
        array_flags |= MAT_F_COMPLEX;
    if ( matvar->isGlobal )
        array_flags |= MAT_F_GLOBAL;
    if ( matvar->isLogical )
        array_flags |= MAT_F_LOGICAL;
    if ( matvar->class_type == MAT_C_SPARSE )
        nzmax = ((mat_sparse_t *)matvar->data)->nzmax;

    if ( mat->byteswap )
        array_flags = Mat_int32Swap((mat_int32_t *)&array_flags);
    fwrite(&array_flags_type, 4, 1, (FILE *)mat->fp);
    fwrite(&array_flags_size, 4, 1, (FILE *)mat->fp);
    fwrite(&array_flags, 4, 1, (FILE *)mat->fp);
    fwrite(&nzmax, 4, 1, (FILE *)mat->fp);
    /* Rank and Dimension */
    nBytes = matvar->rank * 4;
    fwrite(&dims_array_type, 4, 1, (FILE *)mat->fp);
    fwrite(&nBytes, 4, 1, (FILE *)mat->fp);
    for ( i = 0; i < matvar->rank; i++ ) {
        mat_int32_t dim;
        dim = matvar->dims[i];
        fwrite(&dim, 4, 1, (FILE *)mat->fp);
    }
    if ( matvar->rank % 2 != 0 )
        fwrite(&pad4, 4, 1, (FILE *)mat->fp);

    /* Name of variable */
    fwrite(&array_name_type, 4, 1, (FILE *)mat->fp);
    fwrite(&pad4, 4, 1, (FILE *)mat->fp);

    WriteType(mat, matvar);
    end = ftello((FILE *)mat->fp);
    if ( start != -1L && end != -1L ) {
        nBytes = (int)(end - start);
        (void)fseeko((FILE *)mat->fp, (mat_off_t) - (nBytes + 4), SEEK_CUR);
        fwrite(&nBytes, 4, 1, (FILE *)mat->fp);
        (void)fseeko((FILE *)mat->fp, end, SEEK_SET);
    } else {
        Mat_Critical("Couldn't determine file position");
    }

    return MATIO_E_NO_ERROR;
}

#if HAVE_ZLIB
/** @brief Writes the header and data for a field of a compressed struct array
 *
 * @ingroup mat_internal
 * @fixme Currently does not work for cell arrays or sparse data
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @return number of bytes written to the MAT file
 */
static size_t
WriteCompressedStructField(mat_t *mat, matvar_t *matvar, z_streamp z)
{
    mat_uint32_t comp_buf[512];
    mat_uint32_t uncomp_buf[512];
    int buf_size = 512;
    size_t byteswritten = 0, field_buf_size;

    if ( NULL == mat || NULL == z )
        return 0;

    if ( NULL == matvar ) {
        size_t dims[2] = {0, 0};
        byteswritten = Mat_WriteCompressedEmptyVariable5(mat, NULL, 2, dims, z);
        return byteswritten;
    }

    memset(&uncomp_buf, 0, sizeof(uncomp_buf));
    uncomp_buf[0] = MAT_T_MATRIX;
    if ( MAT_C_EMPTY != matvar->class_type ) {
        int err = GetStructFieldBufSize(matvar, &field_buf_size);
        if ( err || field_buf_size > UINT32_MAX )
            return 0;
        uncomp_buf[1] = field_buf_size;
    } else {
        uncomp_buf[1] = 0;
    }
    z->next_in = ZLIB_BYTE_PTR(uncomp_buf);
    z->avail_in = 8;
    do {
        z->next_out = ZLIB_BYTE_PTR(comp_buf);
        z->avail_out = buf_size * sizeof(*comp_buf);
        deflate(z, Z_NO_FLUSH);
        byteswritten +=
            fwrite(comp_buf, 1, buf_size * sizeof(*comp_buf) - z->avail_out, (FILE *)mat->fp);
    } while ( z->avail_out == 0 );

    byteswritten += WriteCompressedTypeArrayFlags(mat, matvar, z);
    return byteswritten;
}
#endif

static size_t
Mat_WriteEmptyVariable5(mat_t *mat, const char *name, int rank, size_t *dims)
{
    mat_uint32_t array_flags;
    mat_uint32_t array_name_type = MAT_T_INT8;
    const mat_uint32_t matrix_type = MAT_T_MATRIX;
    int array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int array_flags_size = 8, nBytes, i;
    const mat_uint32_t pad4 = 0;
    const mat_uint8_t pad1 = 0;
    size_t byteswritten = 0;
    mat_off_t start = 0, end = 0;

    fwrite(&matrix_type, 4, 1, (FILE *)mat->fp);
    fwrite(&pad4, 4, 1, (FILE *)mat->fp);
    start = ftello((FILE *)mat->fp);

    /* Array Flags */
    array_flags = MAT_C_DOUBLE;

    if ( mat->byteswap )
        array_flags = Mat_int32Swap((mat_int32_t *)&array_flags);
    byteswritten += fwrite(&array_flags_type, 4, 1, (FILE *)mat->fp);
    byteswritten += fwrite(&array_flags_size, 4, 1, (FILE *)mat->fp);
    byteswritten += fwrite(&array_flags, 4, 1, (FILE *)mat->fp);
    byteswritten += fwrite(&pad4, 4, 1, (FILE *)mat->fp);
    /* Rank and Dimension */
    nBytes = rank * 4;
    byteswritten += fwrite(&dims_array_type, 4, 1, (FILE *)mat->fp);
    byteswritten += fwrite(&nBytes, 4, 1, (FILE *)mat->fp);
    for ( i = 0; i < rank; i++ ) {
        mat_int32_t dim;
        dim = dims[i];
        byteswritten += fwrite(&dim, 4, 1, (FILE *)mat->fp);
    }
    if ( rank % 2 != 0 )
        byteswritten += fwrite(&pad4, 4, 1, (FILE *)mat->fp);

    if ( NULL == name ) {
        /* Name of variable */
        byteswritten += fwrite(&array_name_type, 4, 1, (FILE *)mat->fp);
        byteswritten += fwrite(&pad4, 4, 1, (FILE *)mat->fp);
    } else {
        mat_int32_t array_name_len = (mat_int32_t)strlen(name);
        /* Name of variable */
        if ( array_name_len <= 4 ) {
            array_name_type |= array_name_len << 16;
            byteswritten += fwrite(&array_name_type, 4, 1, (FILE *)mat->fp);
            byteswritten += fwrite(name, 1, array_name_len, (FILE *)mat->fp);
            for ( i = array_name_len; i < 4; i++ )
                byteswritten += fwrite(&pad1, 1, 1, (FILE *)mat->fp);
        } else {
            byteswritten += fwrite(&array_name_type, 4, 1, (FILE *)mat->fp);
            byteswritten += fwrite(&array_name_len, 4, 1, (FILE *)mat->fp);
            byteswritten += fwrite(name, 1, array_name_len, (FILE *)mat->fp);
            if ( array_name_len % 8 )
                for ( i = array_name_len % 8; i < 8; i++ )
                    byteswritten += fwrite(&pad1, 1, 1, (FILE *)mat->fp);
        }
    }

    nBytes = WriteData(mat, NULL, 0, MAT_T_DOUBLE);
    byteswritten += nBytes;
    if ( nBytes % 8 )
        for ( i = nBytes % 8; i < 8; i++ )
            byteswritten += fwrite(&pad1, 1, 1, (FILE *)mat->fp);

    end = ftello((FILE *)mat->fp);
    if ( start != -1L && end != -1L ) {
        nBytes = (int)(end - start);
        (void)fseeko((FILE *)mat->fp, (mat_off_t) - (nBytes + 4), SEEK_CUR);
        fwrite(&nBytes, 4, 1, (FILE *)mat->fp);
        (void)fseeko((FILE *)mat->fp, end, SEEK_SET);
    } else {
        Mat_Critical("Couldn't determine file position");
    }

    return byteswritten;
}

#if HAVE_ZLIB
static size_t
Mat_WriteCompressedEmptyVariable5(mat_t *mat, const char *name, int rank, size_t *dims, z_streamp z)
{
    mat_uint32_t array_flags;
    int array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int array_flags_size = 8;
    int i, err;
    size_t nBytes, empty_matrix_max_buf_size;

    mat_uint32_t comp_buf[512];
    mat_uint32_t uncomp_buf[512];
    int buf_size = 512;
    size_t byteswritten = 0, buf_size_bytes;

    if ( NULL == mat || NULL == z )
        return byteswritten;

    buf_size_bytes = buf_size * sizeof(*comp_buf);

    /* Array Flags */
    array_flags = MAT_C_DOUBLE;

    memset(&uncomp_buf, 0, sizeof(uncomp_buf));
    uncomp_buf[0] = MAT_T_MATRIX;
    err = GetEmptyMatrixMaxBufSize(name, rank, &empty_matrix_max_buf_size);
    if ( err || empty_matrix_max_buf_size > UINT32_MAX )
        return byteswritten;
    uncomp_buf[1] = empty_matrix_max_buf_size;
    z->next_in = ZLIB_BYTE_PTR(uncomp_buf);
    z->avail_in = 8;
    do {
        z->next_out = ZLIB_BYTE_PTR(comp_buf);
        z->avail_out = buf_size_bytes;
        deflate(z, Z_NO_FLUSH);
        byteswritten += fwrite(comp_buf, 1, buf_size_bytes - z->avail_out, (FILE *)mat->fp);
    } while ( z->avail_out == 0 );
    uncomp_buf[0] = array_flags_type;
    uncomp_buf[1] = array_flags_size;
    uncomp_buf[2] = array_flags;
    uncomp_buf[3] = 0;
    /* Rank and Dimension */
    nBytes = rank * 4;
    uncomp_buf[4] = dims_array_type;
    uncomp_buf[5] = nBytes;
    for ( i = 0; i < rank; i++ ) {
        mat_int32_t dim;
        dim = dims[i];
        uncomp_buf[6 + i] = dim;
    }
    if ( rank % 2 != 0 ) {
        const mat_uint32_t pad4 = 0;
        uncomp_buf[6 + i] = pad4;
        i++;
    }

    z->next_in = ZLIB_BYTE_PTR(uncomp_buf);
    z->avail_in = (6 + i) * sizeof(*uncomp_buf);
    do {
        z->next_out = ZLIB_BYTE_PTR(comp_buf);
        z->avail_out = buf_size_bytes;
        deflate(z, Z_NO_FLUSH);
        byteswritten += fwrite(comp_buf, 1, buf_size_bytes - z->avail_out, (FILE *)mat->fp);
    } while ( z->avail_out == 0 );
    /* Name of variable */
    if ( NULL == name ) {
        const mat_uint32_t array_name_type = MAT_T_INT8;
        uncomp_buf[0] = array_name_type;
        uncomp_buf[1] = 0;
        z->next_in = ZLIB_BYTE_PTR(uncomp_buf);
        z->avail_in = 8;
        do {
            z->next_out = ZLIB_BYTE_PTR(comp_buf);
            z->avail_out = buf_size_bytes;
            deflate(z, Z_NO_FLUSH);
            byteswritten += fwrite(comp_buf, 1, buf_size_bytes - z->avail_out, (FILE *)mat->fp);
        } while ( z->avail_out == 0 );
    } else if ( strlen(name) <= 4 ) {
        mat_uint32_t array_name_len = (mat_uint32_t)strlen(name);
        const mat_uint32_t array_name_type = MAT_T_INT8;

        memset(uncomp_buf, 0, 8);
        uncomp_buf[0] = (array_name_len << 16) | array_name_type;
        memcpy(uncomp_buf + 1, name, array_name_len);
        if ( array_name_len % 4 )
            array_name_len += 4 - (array_name_len % 4);

        z->next_in = ZLIB_BYTE_PTR(uncomp_buf);
        z->avail_in = 8;
        do {
            z->next_out = ZLIB_BYTE_PTR(comp_buf);
            z->avail_out = buf_size_bytes;
            deflate(z, Z_NO_FLUSH);
            byteswritten += fwrite(comp_buf, 1, buf_size_bytes - z->avail_out, (FILE *)mat->fp);
        } while ( z->avail_out == 0 );
    } else {
        mat_uint32_t array_name_len = (mat_uint32_t)strlen(name);
        const mat_uint32_t array_name_type = MAT_T_INT8;

        memset(uncomp_buf, 0, buf_size * sizeof(*uncomp_buf));
        uncomp_buf[0] = array_name_type;
        uncomp_buf[1] = array_name_len;
        memcpy(uncomp_buf + 2, name, array_name_len);
        if ( array_name_len % 8 )
            array_name_len += 8 - (array_name_len % 8);
        z->next_in = ZLIB_BYTE_PTR(uncomp_buf);
        z->avail_in = 8 + array_name_len;
        do {
            z->next_out = ZLIB_BYTE_PTR(comp_buf);
            z->avail_out = buf_size_bytes;
            deflate(z, Z_NO_FLUSH);
            byteswritten += fwrite(comp_buf, 1, buf_size_bytes - z->avail_out, (FILE *)mat->fp);
        } while ( z->avail_out == 0 );
    }

    byteswritten += WriteCompressedData(mat, z, NULL, 0, MAT_T_DOUBLE);
    return byteswritten;
}
#endif

/** @if mat_devman
 * @brief Reads a data element including tag and data
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar MAT variable pointer
 * @param data Pointer to store the data
 * @param N number of data elements allocated for the pointer
 * @retval 0 on success
 * @endif
 */
static int
Mat_VarReadNumeric5(mat_t *mat, matvar_t *matvar, void *data, size_t N)
{
    int nBytes = 0, data_in_tag = 0, err = MATIO_E_NO_ERROR;
    enum matio_types packed_type = MAT_T_UNKNOWN;
    mat_uint32_t tag[2] = {0, 0};

    if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
#if HAVE_ZLIB
        matvar->internal->z->avail_in = 0;
        err = Inflate(mat, matvar->internal->z, tag, 4, NULL);
        if ( err ) {
            return err;
        }
        if ( mat->byteswap )
            (void)Mat_uint32Swap(tag);

        packed_type = TYPE_FROM_TAG(tag[0]);
        if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
            data_in_tag = 1;
            nBytes = (tag[0] & 0xffff0000) >> 16;
        } else {
            data_in_tag = 0;
            err = Inflate(mat, matvar->internal->z, tag + 1, 4, NULL);
            if ( err ) {
                return err;
            }
            if ( mat->byteswap )
                (void)Mat_uint32Swap(tag + 1);
            nBytes = tag[1];
        }
#endif
    } else {
        err = Read(tag, 4, 1, (FILE *)mat->fp, NULL);
        if ( err ) {
            return err;
        }
        if ( mat->byteswap )
            (void)Mat_uint32Swap(tag);
        packed_type = TYPE_FROM_TAG(tag[0]);
        if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
            data_in_tag = 1;
            nBytes = (tag[0] & 0xffff0000) >> 16;
        } else {
            data_in_tag = 0;
            err = Read(tag + 1, 4, 1, (FILE *)mat->fp, NULL);
            if ( err ) {
                return err;
            }
            if ( mat->byteswap )
                (void)Mat_uint32Swap(tag + 1);
            nBytes = tag[1];
        }
    }
    if ( nBytes == 0 ) {
        matvar->nbytes = 0;
        return err;
    }

    if ( matvar->compression == MAT_COMPRESSION_NONE ) {
        switch ( matvar->class_type ) {
            case MAT_C_DOUBLE:
                nBytes = ReadDoubleData(mat, (double *)data, packed_type, N);
                break;
            case MAT_C_SINGLE:
                nBytes = ReadSingleData(mat, (float *)data, packed_type, N);
                break;
            case MAT_C_INT64:
#ifdef HAVE_MAT_INT64_T
                nBytes = ReadInt64Data(mat, (mat_int64_t *)data, packed_type, N);
#endif
                break;
            case MAT_C_UINT64:
#ifdef HAVE_MAT_UINT64_T
                nBytes = ReadUInt64Data(mat, (mat_uint64_t *)data, packed_type, N);
#endif
                break;
            case MAT_C_INT32:
                nBytes = ReadInt32Data(mat, (mat_int32_t *)data, packed_type, N);
                break;
            case MAT_C_UINT32:
                nBytes = ReadUInt32Data(mat, (mat_uint32_t *)data, packed_type, N);
                break;
            case MAT_C_INT16:
                nBytes = ReadInt16Data(mat, (mat_int16_t *)data, packed_type, N);
                break;
            case MAT_C_UINT16:
                nBytes = ReadUInt16Data(mat, (mat_uint16_t *)data, packed_type, N);
                break;
            case MAT_C_INT8:
                nBytes = ReadInt8Data(mat, (mat_int8_t *)data, packed_type, N);
                break;
            case MAT_C_UINT8:
                nBytes = ReadUInt8Data(mat, (mat_uint8_t *)data, packed_type, N);
                break;
            default:
                break;
        }
        nBytes *= Mat_SizeOf(packed_type);
        /*
         * If the data was in the tag we started on a 4-byte
         * boundary so add 4 to make it an 8-byte
         */
        if ( data_in_tag )
            nBytes += 4;
        if ( (nBytes % 8) != 0 )
            (void)fseeko((FILE *)mat->fp, 8 - (nBytes % 8), SEEK_CUR);
#if HAVE_ZLIB
    } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
        switch ( matvar->class_type ) {
            case MAT_C_DOUBLE:
                nBytes = ReadCompressedDoubleData(mat, matvar->internal->z, (double *)data,
                                                  packed_type, N);
                break;
            case MAT_C_SINGLE:
                nBytes = ReadCompressedSingleData(mat, matvar->internal->z, (float *)data,
                                                  packed_type, N);
                break;
            case MAT_C_INT64:
#ifdef HAVE_MAT_INT64_T
                nBytes = ReadCompressedInt64Data(mat, matvar->internal->z, (mat_int64_t *)data,
                                                 packed_type, N);
#endif
                break;
            case MAT_C_UINT64:
#ifdef HAVE_MAT_UINT64_T
                nBytes = ReadCompressedUInt64Data(mat, matvar->internal->z, (mat_uint64_t *)data,
                                                  packed_type, N);
#endif
                break;
            case MAT_C_INT32:
                nBytes = ReadCompressedInt32Data(mat, matvar->internal->z, (mat_int32_t *)data,
                                                 packed_type, N);
                break;
            case MAT_C_UINT32:
                nBytes = ReadCompressedUInt32Data(mat, matvar->internal->z, (mat_uint32_t *)data,
                                                  packed_type, N);
                break;
            case MAT_C_INT16:
                nBytes = ReadCompressedInt16Data(mat, matvar->internal->z, (mat_int16_t *)data,
                                                 packed_type, N);
                break;
            case MAT_C_UINT16:
                nBytes = ReadCompressedUInt16Data(mat, matvar->internal->z, (mat_uint16_t *)data,
                                                  packed_type, N);
                break;
            case MAT_C_INT8:
                nBytes = ReadCompressedInt8Data(mat, matvar->internal->z, (mat_int8_t *)data,
                                                packed_type, N);
                break;
            case MAT_C_UINT8:
                nBytes = ReadCompressedUInt8Data(mat, matvar->internal->z, (mat_uint8_t *)data,
                                                 packed_type, N);
                break;
            default:
                break;
        }
        /*
         * If the data was in the tag we started on a 4-byte
         * boundary so add 4 to make it an 8-byte
         */
        if ( data_in_tag )
            nBytes += 4;
        if ( (nBytes % 8) != 0 )
            err = InflateSkip(mat, matvar->internal->z, 8 - (nBytes % 8), NULL);
#endif
    }
    return err;
}

/** @if mat_devman
 * @brief Reads the data of a version 5 MAT variable
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar MAT variable pointer to read the data
 * @retval 0 on success
 * @endif
 */
int
Mat_VarRead5(mat_t *mat, matvar_t *matvar)
{
    int nBytes = 0, byteswap, data_in_tag = 0, err;
    size_t nelems = 1;
    enum matio_types packed_type = MAT_T_UNKNOWN;
    mat_off_t fpos;
    mat_uint32_t tag[2] = {0, 0};
    size_t bytesread = 0;

    if ( matvar == NULL )
        return MATIO_E_BAD_ARGUMENT;
    else if ( matvar->rank == 0 ) /* An empty data set */
        return MATIO_E_NO_ERROR;
#if HAVE_ZLIB
    else if ( NULL != matvar->internal->data ) {
        /* Data already read in ReadNextStructField or ReadNextCell */
        matvar->data = matvar->internal->data;
        matvar->internal->data = NULL;
        return MATIO_E_NO_ERROR;
    }
#endif
    fpos = ftello((FILE *)mat->fp);
    if ( fpos == -1L ) {
        Mat_Critical("Couldn't determine file position");
        return MATIO_E_GENERIC_READ_ERROR;
    }
    err = Mat_MulDims(matvar, &nelems);
    if ( err ) {
        Mat_Critical("Integer multiplication overflow");
        return err;
    }
    byteswap = mat->byteswap;
    switch ( matvar->class_type ) {
        case MAT_C_EMPTY:
            matvar->nbytes = 0;
            matvar->data_size = sizeof(double);
            matvar->data_type = MAT_T_DOUBLE;
            matvar->rank = 2;
            if ( NULL != matvar->dims ) {
                free(matvar->dims);
            }
            matvar->dims = (size_t *)calloc(matvar->rank, sizeof(*(matvar->dims)));
            break;
        case MAT_C_DOUBLE:
            (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);
            matvar->data_size = sizeof(double);
            matvar->data_type = MAT_T_DOUBLE;
            break;
        case MAT_C_SINGLE:
            (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);
            matvar->data_size = sizeof(float);
            matvar->data_type = MAT_T_SINGLE;
            break;
        case MAT_C_INT64:
#ifdef HAVE_MAT_INT64_T
            (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);
            matvar->data_size = sizeof(mat_int64_t);
            matvar->data_type = MAT_T_INT64;
#endif
            break;
        case MAT_C_UINT64:
#ifdef HAVE_MAT_UINT64_T
            (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);
            matvar->data_size = sizeof(mat_uint64_t);
            matvar->data_type = MAT_T_UINT64;
#endif
            break;
        case MAT_C_INT32:
            (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);
            matvar->data_size = sizeof(mat_int32_t);
            matvar->data_type = MAT_T_INT32;
            break;
        case MAT_C_UINT32:
            (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);
            matvar->data_size = sizeof(mat_uint32_t);
            matvar->data_type = MAT_T_UINT32;
            break;
        case MAT_C_INT16:
            (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);
            matvar->data_size = sizeof(mat_int16_t);
            matvar->data_type = MAT_T_INT16;
            break;
        case MAT_C_UINT16:
            (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);
            matvar->data_size = sizeof(mat_uint16_t);
            matvar->data_type = MAT_T_UINT16;
            break;
        case MAT_C_INT8:
            (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);
            matvar->data_size = sizeof(mat_int8_t);
            matvar->data_type = MAT_T_INT8;
            break;
        case MAT_C_UINT8:
            (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);
            matvar->data_size = sizeof(mat_uint8_t);
            matvar->data_type = MAT_T_UINT8;
            break;
        case MAT_C_CHAR:
            (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);
            if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
#if HAVE_ZLIB
                matvar->internal->z->avail_in = 0;
                err = Inflate(mat, matvar->internal->z, tag, 4, &bytesread);
                if ( err ) {
                    break;
                }
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    err = Inflate(mat, matvar->internal->z, tag + 1, 4, &bytesread);
                    if ( err ) {
                        break;
                    }
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag + 1);
                    nBytes = tag[1];
                }
#endif
                matvar->data_type = packed_type;
                matvar->data_size = Mat_SizeOf(matvar->data_type);
                matvar->nbytes = nBytes;
            } else {
                err = Read(tag, 4, 1, (FILE *)mat->fp, &bytesread);
                if ( err ) {
                    break;
                }
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    err = Read(tag + 1, 4, 1, (FILE *)mat->fp, &bytesread);
                    if ( err ) {
                        break;
                    }
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag + 1);
                    nBytes = tag[1];
                }
                matvar->data_type = packed_type;
                matvar->data_size = Mat_SizeOf(matvar->data_type);
                matvar->nbytes = nBytes;
            }
            if ( matvar->isComplex ) {
                break;
            }
            if ( 0 == matvar->nbytes ) {
                matvar->data = calloc(1, 1);
            } else {
                matvar->data = calloc(matvar->nbytes, 1);
            }
            if ( NULL == matvar->data ) {
                err = MATIO_E_OUT_OF_MEMORY;
                Mat_Critical("Couldn't allocate memory for the data");
                break;
            }
            if ( 0 == matvar->nbytes ) {
                break;
            }
            {
                size_t nbytes = 0;
                err = Mul(&nbytes, nelems, matvar->data_size);
                if ( err || nbytes > matvar->nbytes ) {
                    break;
                }
            }
            if ( matvar->data_type == MAT_T_UTF8 ) {
                nelems = matvar->nbytes;
            }
            if ( matvar->compression == MAT_COMPRESSION_NONE ) {
                nBytes = ReadCharData(mat, matvar->data, matvar->data_type, nelems);
                /*
                 * If the data was in the tag we started on a 4-byte
                 * boundary so add 4 to make it an 8-byte
                 */
                if ( data_in_tag )
                    nBytes += 4;
                if ( (nBytes % 8) != 0 )
                    (void)fseeko((FILE *)mat->fp, 8 - (nBytes % 8), SEEK_CUR);
#if HAVE_ZLIB
            } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
                nBytes = ReadCompressedCharData(mat, matvar->internal->z, matvar->data,
                                                matvar->data_type, nelems);
                /*
                 * If the data was in the tag we started on a 4-byte
                 * boundary so add 4 to make it an 8-byte
                 */
                if ( data_in_tag )
                    nBytes += 4;
                if ( (nBytes % 8) != 0 )
                    InflateSkip(mat, matvar->internal->z, 8 - (nBytes % 8), NULL);
#endif
            }
            break;
        case MAT_C_STRUCT: {
            matvar_t **fields;
            size_t i, nelems_x_nfields;

            matvar->data_type = MAT_T_STRUCT;
            err = Mul(&nelems_x_nfields, nelems, matvar->internal->num_fields);
            if ( err || !matvar->nbytes || !matvar->data_size || NULL == matvar->data )
                break;
            fields = (matvar_t **)matvar->data;
            for ( i = 0; i < nelems_x_nfields; i++ ) {
                if ( NULL != fields[i] ) {
                    err = Mat_VarRead5(mat, fields[i]);
                    if ( err )
                        break;
                }
            }
            break;
        }
        case MAT_C_CELL: {
            matvar_t **cells;
            size_t i;

            if ( NULL == matvar->data ) {
                Mat_Critical("Data is NULL for cell array %s", matvar->name);
                err = MATIO_E_FILE_FORMAT_VIOLATION;
                break;
            }
            cells = (matvar_t **)matvar->data;
            for ( i = 0; i < nelems; i++ ) {
                if ( NULL != cells[i] ) {
                    err = Mat_VarRead5(mat, cells[i]);
                    if ( err )
                        break;
                }
            }
            /* FIXME: */
            matvar->data_type = MAT_T_CELL;
            break;
        }
        case MAT_C_SPARSE: {
            mat_uint32_t N = 0;
            mat_sparse_t *sparse;

            matvar->data_size = sizeof(mat_sparse_t);
            matvar->data = calloc(1, matvar->data_size);
            if ( matvar->data == NULL ) {
                err = MATIO_E_OUT_OF_MEMORY;
                Mat_Critical("Mat_VarRead5: Allocation of data pointer failed");
                break;
            }
            sparse = (mat_sparse_t *)matvar->data;
            sparse->nzmax = matvar->nbytes;
            (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);
            /*  Read ir    */
            bytesread += ReadSparse(mat, matvar, &sparse->nir, &sparse->ir);
            /*  Read jc    */
            bytesread += ReadSparse(mat, matvar, &sparse->njc, &sparse->jc);
            /*  Read data  */
            if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
#if HAVE_ZLIB
                matvar->internal->z->avail_in = 0;
                err = Inflate(mat, matvar->internal->z, tag, 4, &bytesread);
                if ( err ) {
                    break;
                }
                if ( mat->byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    N = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    (void)ReadCompressedUInt32Data(mat, matvar->internal->z, &N, MAT_T_UINT32, 1);
                }
#endif
            } else {
                err = Read(tag, 4, 1, (FILE *)mat->fp, &bytesread);
                if ( err ) {
                    break;
                }
                if ( mat->byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    N = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    err = Read(&N, 4, 1, (FILE *)mat->fp, &bytesread);
                    if ( err ) {
                        break;
                    }
                    if ( mat->byteswap )
                        (void)Mat_uint32Swap(&N);
                }
            }
            if ( matvar->isLogical && packed_type == MAT_T_DOUBLE ) {
                /* For some reason, MAT says the data type is a double,
                 * but it appears to be written as 8-bit unsigned integer.
                 */
                packed_type = MAT_T_UINT8;
            }
#if defined(EXTENDED_SPARSE)
            matvar->data_type = packed_type;
#else
            matvar->data_type = MAT_T_DOUBLE;
#endif
            {
                size_t s_type = Mat_SizeOf(packed_type);
                if ( s_type == 0 )
                    break;
                sparse->ndata = N / s_type;
            }
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data;
                size_t nbytes = 0;
                err = Mul(&nbytes, sparse->ndata, Mat_SizeOf(matvar->data_type));
                if ( err ) {
                    Mat_Critical("Integer multiplication overflow");
                    break;
                }
                complex_data = ComplexMalloc(nbytes);
                if ( NULL == complex_data ) {
                    err = MATIO_E_OUT_OF_MEMORY;
                    Mat_Critical("Couldn't allocate memory for the complex sparse data");
                    break;
                }
                if ( matvar->compression == MAT_COMPRESSION_NONE ) {
#if defined(EXTENDED_SPARSE)
                    switch ( matvar->data_type ) {
                        case MAT_T_DOUBLE:
                            nBytes = ReadDoubleData(mat, (double *)complex_data->Re, packed_type,
                                                    sparse->ndata);
                            break;
                        case MAT_T_SINGLE:
                            nBytes = ReadSingleData(mat, (float *)complex_data->Re, packed_type,
                                                    sparse->ndata);
                            break;
                        case MAT_T_INT64:
#ifdef HAVE_MAT_INT64_T
                            nBytes = ReadInt64Data(mat, (mat_int64_t *)complex_data->Re,
                                                   packed_type, sparse->ndata);
#endif
                            break;
                        case MAT_T_UINT64:
#ifdef HAVE_MAT_UINT64_T
                            nBytes = ReadUInt64Data(mat, (mat_uint64_t *)complex_data->Re,
                                                    packed_type, sparse->ndata);
#endif
                            break;
                        case MAT_T_INT32:
                            nBytes = ReadInt32Data(mat, (mat_int32_t *)complex_data->Re,
                                                   packed_type, sparse->ndata);
                            break;
                        case MAT_T_UINT32:
                            nBytes = ReadUInt32Data(mat, (mat_uint32_t *)complex_data->Re,
                                                    packed_type, sparse->ndata);
                            break;
                        case MAT_T_INT16:
                            nBytes = ReadInt16Data(mat, (mat_int16_t *)complex_data->Re,
                                                   packed_type, sparse->ndata);
                            break;
                        case MAT_T_UINT16:
                            nBytes = ReadUInt16Data(mat, (mat_uint16_t *)complex_data->Re,
                                                    packed_type, sparse->ndata);
                            break;
                        case MAT_T_INT8:
                            nBytes = ReadInt8Data(mat, (mat_int8_t *)complex_data->Re, packed_type,
                                                  sparse->ndata);
                            break;
                        case MAT_T_UINT8:
                            nBytes = ReadUInt8Data(mat, (mat_uint8_t *)complex_data->Re,
                                                   packed_type, sparse->ndata);
                            break;
                        default:
                            break;
                    }
#else
                    nBytes =
                        ReadDoubleData(mat, (double *)complex_data->Re, packed_type, sparse->ndata);
#endif
                    nBytes *= Mat_SizeOf(packed_type);
                    if ( data_in_tag )
                        nBytes += 4;
                    if ( (nBytes % 8) != 0 )
                        (void)fseeko((FILE *)mat->fp, 8 - (nBytes % 8), SEEK_CUR);

                    /* Complex Data Tag */
                    err = Read(tag, 4, 1, (FILE *)mat->fp, &bytesread);
                    if ( err ) {
                        ComplexFree(complex_data);
                        break;
                    }
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag);
                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        err = Read(tag + 1, 4, 1, (FILE *)mat->fp, &bytesread);
                        if ( err ) {
                            ComplexFree(complex_data);
                            break;
                        }
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag + 1);
                        nBytes = tag[1];
                    }
#if defined(EXTENDED_SPARSE)
                    switch ( matvar->data_type ) {
                        case MAT_T_DOUBLE:
                            nBytes = ReadDoubleData(mat, (double *)complex_data->Im, packed_type,
                                                    sparse->ndata);
                            break;
                        case MAT_T_SINGLE:
                            nBytes = ReadSingleData(mat, (float *)complex_data->Im, packed_type,
                                                    sparse->ndata);
                            break;
                        case MAT_T_INT64:
#ifdef HAVE_MAT_INT64_T
                            nBytes = ReadInt64Data(mat, (mat_int64_t *)complex_data->Im,
                                                   packed_type, sparse->ndata);
#endif
                            break;
                        case MAT_T_UINT64:
#ifdef HAVE_MAT_UINT64_T
                            nBytes = ReadUInt64Data(mat, (mat_uint64_t *)complex_data->Im,
                                                    packed_type, sparse->ndata);
#endif
                            break;
                        case MAT_T_INT32:
                            nBytes = ReadInt32Data(mat, (mat_int32_t *)complex_data->Im,
                                                   packed_type, sparse->ndata);
                            break;
                        case MAT_T_UINT32:
                            nBytes = ReadUInt32Data(mat, (mat_uint32_t *)complex_data->Im,
                                                    packed_type, sparse->ndata);
                            break;
                        case MAT_T_INT16:
                            nBytes = ReadInt16Data(mat, (mat_int16_t *)complex_data->Im,
                                                   packed_type, sparse->ndata);
                            break;
                        case MAT_T_UINT16:
                            nBytes = ReadUInt16Data(mat, (mat_uint16_t *)complex_data->Im,
                                                    packed_type, sparse->ndata);
                            break;
                        case MAT_T_INT8:
                            nBytes = ReadInt8Data(mat, (mat_int8_t *)complex_data->Im, packed_type,
                                                  sparse->ndata);
                            break;
                        case MAT_T_UINT8:
                            nBytes = ReadUInt8Data(mat, (mat_uint8_t *)complex_data->Im,
                                                   packed_type, sparse->ndata);
                            break;
                        default:
                            break;
                    }
#else  /* EXTENDED_SPARSE */
                    nBytes =
                        ReadDoubleData(mat, (double *)complex_data->Im, packed_type, sparse->ndata);
#endif /* EXTENDED_SPARSE */
                    nBytes *= Mat_SizeOf(packed_type);
                    if ( data_in_tag )
                        nBytes += 4;
                    if ( (nBytes % 8) != 0 )
                        (void)fseeko((FILE *)mat->fp, 8 - (nBytes % 8), SEEK_CUR);
#if HAVE_ZLIB
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
#if defined(EXTENDED_SPARSE)
                    switch ( matvar->data_type ) {
                        case MAT_T_DOUBLE:
                            nBytes = ReadCompressedDoubleData(mat, matvar->internal->z,
                                                              (double *)complex_data->Re,
                                                              packed_type, sparse->ndata);
                            break;
                        case MAT_T_SINGLE:
                            nBytes = ReadCompressedSingleData(mat, matvar->internal->z,
                                                              (float *)complex_data->Re,
                                                              packed_type, sparse->ndata);
                            break;
                        case MAT_T_INT64:
#ifdef HAVE_MAT_INT64_T
                            nBytes = ReadCompressedInt64Data(mat, matvar->internal->z,
                                                             (mat_int64_t *)complex_data->Re,
                                                             packed_type, sparse->ndata);
#endif
                            break;
                        case MAT_T_UINT64:
#ifdef HAVE_MAT_UINT64_T
                            nBytes = ReadCompressedUInt64Data(mat, matvar->internal->z,
                                                              (mat_uint64_t *)complex_data->Re,
                                                              packed_type, sparse->ndata);
#endif
                            break;
                        case MAT_T_INT32:
                            nBytes = ReadCompressedInt32Data(mat, matvar->internal->z,
                                                             (mat_int32_t *)complex_data->Re,
                                                             packed_type, sparse->ndata);
                            break;
                        case MAT_T_UINT32:
                            nBytes = ReadCompressedUInt32Data(mat, matvar->internal->z,
                                                              (mat_uint32_t *)complex_data->Re,
                                                              packed_type, sparse->ndata);
                            break;
                        case MAT_T_INT16:
                            nBytes = ReadCompressedInt16Data(mat, matvar->internal->z,
                                                             (mat_int16_t *)complex_data->Re,
                                                             packed_type, sparse->ndata);
                            break;
                        case MAT_T_UINT16:
                            nBytes = ReadCompressedUInt16Data(mat, matvar->internal->z,
                                                              (mat_uint16_t *)complex_data->Re,
                                                              packed_type, sparse->ndata);
                            break;
                        case MAT_T_INT8:
                            nBytes = ReadCompressedInt8Data(mat, matvar->internal->z,
                                                            (mat_int8_t *)complex_data->Re,
                                                            packed_type, sparse->ndata);
                            break;
                        case MAT_T_UINT8:
                            nBytes = ReadCompressedUInt8Data(mat, matvar->internal->z,
                                                             (mat_uint8_t *)complex_data->Re,
                                                             packed_type, sparse->ndata);
                            break;
                        default:
                            break;
                    }
#else  /* EXTENDED_SPARSE */
                    nBytes = ReadCompressedDoubleData(mat, matvar->internal->z,
                                                      (double *)complex_data->Re, packed_type,
                                                      sparse->ndata);
#endif /* EXTENDED_SPARSE */
                    if ( data_in_tag )
                        nBytes += 4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat, matvar->internal->z, 8 - (nBytes % 8), NULL);

                    /* Complex Data Tag */
                    err = Inflate(mat, matvar->internal->z, tag, 4, NULL);
                    if ( err ) {
                        ComplexFree(complex_data);
                        break;
                    }
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag);

                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        err = Inflate(mat, matvar->internal->z, tag + 1, 4, NULL);
                        if ( err ) {
                            ComplexFree(complex_data);
                            break;
                        }
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag + 1);
                        nBytes = tag[1];
                    }
#if defined(EXTENDED_SPARSE)
                    switch ( matvar->data_type ) {
                        case MAT_T_DOUBLE:
                            nBytes = ReadCompressedDoubleData(mat, matvar->internal->z,
                                                              (double *)complex_data->Im,
                                                              packed_type, sparse->ndata);
                            break;
                        case MAT_T_SINGLE:
                            nBytes = ReadCompressedSingleData(mat, matvar->internal->z,
                                                              (float *)complex_data->Im,
                                                              packed_type, sparse->ndata);
                            break;
                        case MAT_T_INT64:
#ifdef HAVE_MAT_INT64_T
                            nBytes = ReadCompressedInt64Data(mat, matvar->internal->z,
                                                             (mat_int64_t *)complex_data->Im,
                                                             packed_type, sparse->ndata);
#endif
                            break;
                        case MAT_T_UINT64:
#ifdef HAVE_MAT_UINT64_T
                            nBytes = ReadCompressedUInt64Data(mat, matvar->internal->z,
                                                              (mat_uint64_t *)complex_data->Im,
                                                              packed_type, sparse->ndata);
#endif
                            break;
                        case MAT_T_INT32:
                            nBytes = ReadCompressedInt32Data(mat, matvar->internal->z,
                                                             (mat_int32_t *)complex_data->Im,
                                                             packed_type, sparse->ndata);
                            break;
                        case MAT_T_UINT32:
                            nBytes = ReadCompressedUInt32Data(mat, matvar->internal->z,
                                                              (mat_uint32_t *)complex_data->Im,
                                                              packed_type, sparse->ndata);
                            break;
                        case MAT_T_INT16:
                            nBytes = ReadCompressedInt16Data(mat, matvar->internal->z,
                                                             (mat_int16_t *)complex_data->Im,
                                                             packed_type, sparse->ndata);
                            break;
                        case MAT_T_UINT16:
                            nBytes = ReadCompressedUInt16Data(mat, matvar->internal->z,
                                                              (mat_uint16_t *)complex_data->Im,
                                                              packed_type, sparse->ndata);
                            break;
                        case MAT_T_INT8:
                            nBytes = ReadCompressedInt8Data(mat, matvar->internal->z,
                                                            (mat_int8_t *)complex_data->Im,
                                                            packed_type, sparse->ndata);
                            break;
                        case MAT_T_UINT8:
                            nBytes = ReadCompressedUInt8Data(mat, matvar->internal->z,
                                                             (mat_uint8_t *)complex_data->Im,
                                                             packed_type, sparse->ndata);
                            break;
                        default:
                            break;
                    }
#else  /* EXTENDED_SPARSE */
                    nBytes = ReadCompressedDoubleData(mat, matvar->internal->z,
                                                      (double *)complex_data->Im, packed_type,
                                                      sparse->ndata);
#endif /* EXTENDED_SPARSE */
                    if ( data_in_tag )
                        nBytes += 4;
                    if ( (nBytes % 8) != 0 )
                        err = InflateSkip(mat, matvar->internal->z, 8 - (nBytes % 8), NULL);
#endif /* HAVE_ZLIB */
                }
                sparse->data = complex_data;
            } else { /* isComplex */
                size_t nbytes = 0;
                err = Mul(&nbytes, sparse->ndata, Mat_SizeOf(matvar->data_type));
                if ( err ) {
                    Mat_Critical("Integer multiplication overflow");
                    break;
                }
                sparse->data = malloc(nbytes);
                if ( sparse->data == NULL ) {
                    err = MATIO_E_OUT_OF_MEMORY;
                    Mat_Critical("Couldn't allocate memory for the sparse data");
                    break;
                }
                if ( matvar->compression == MAT_COMPRESSION_NONE ) {
#if defined(EXTENDED_SPARSE)
                    switch ( matvar->data_type ) {
                        case MAT_T_DOUBLE:
                            nBytes = ReadDoubleData(mat, (double *)sparse->data, packed_type,
                                                    sparse->ndata);
                            break;
                        case MAT_T_SINGLE:
                            nBytes = ReadSingleData(mat, (float *)sparse->data, packed_type,
                                                    sparse->ndata);
                            break;
                        case MAT_T_INT64:
#ifdef HAVE_MAT_INT64_T
                            nBytes = ReadInt64Data(mat, (mat_int64_t *)sparse->data, packed_type,
                                                   sparse->ndata);
#endif
                            break;
                        case MAT_T_UINT64:
#ifdef HAVE_MAT_UINT64_T
                            nBytes = ReadUInt64Data(mat, (mat_uint64_t *)sparse->data, packed_type,
                                                    sparse->ndata);
#endif
                            break;
                        case MAT_T_INT32:
                            nBytes = ReadInt32Data(mat, (mat_int32_t *)sparse->data, packed_type,
                                                   sparse->ndata);
                            break;
                        case MAT_T_UINT32:
                            nBytes = ReadUInt32Data(mat, (mat_uint32_t *)sparse->data, packed_type,
                                                    sparse->ndata);
                            break;
                        case MAT_T_INT16:
                            nBytes = ReadInt16Data(mat, (mat_int16_t *)sparse->data, packed_type,
                                                   sparse->ndata);
                            break;
                        case MAT_T_UINT16:
                            nBytes = ReadUInt16Data(mat, (mat_uint16_t *)sparse->data, packed_type,
                                                    sparse->ndata);
                            break;
                        case MAT_T_INT8:
                            nBytes = ReadInt8Data(mat, (mat_int8_t *)sparse->data, packed_type,
                                                  sparse->ndata);
                            break;
                        case MAT_T_UINT8:
                            nBytes = ReadUInt8Data(mat, (mat_uint8_t *)sparse->data, packed_type,
                                                   sparse->ndata);
                            break;
                        default:
                            break;
                    }
#else
                    nBytes =
                        ReadDoubleData(mat, (double *)sparse->data, packed_type, sparse->ndata);
#endif
                    nBytes *= Mat_SizeOf(packed_type);
                    if ( data_in_tag )
                        nBytes += 4;
                    if ( (nBytes % 8) != 0 )
                        (void)fseeko((FILE *)mat->fp, 8 - (nBytes % 8), SEEK_CUR);
#if HAVE_ZLIB
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
#if defined(EXTENDED_SPARSE)
                    switch ( matvar->data_type ) {
                        case MAT_T_DOUBLE:
                            nBytes = ReadCompressedDoubleData(mat, matvar->internal->z,
                                                              (double *)sparse->data, packed_type,
                                                              sparse->ndata);
                            break;
                        case MAT_T_SINGLE:
                            nBytes = ReadCompressedSingleData(mat, matvar->internal->z,
                                                              (float *)sparse->data, packed_type,
                                                              sparse->ndata);
                            break;
                        case MAT_T_INT64:
#ifdef HAVE_MAT_INT64_T
                            nBytes = ReadCompressedInt64Data(mat, matvar->internal->z,
                                                             (mat_int64_t *)sparse->data,
                                                             packed_type, sparse->ndata);
#endif
                            break;
                        case MAT_T_UINT64:
#ifdef HAVE_MAT_UINT64_T
                            nBytes = ReadCompressedUInt64Data(mat, matvar->internal->z,
                                                              (mat_uint64_t *)sparse->data,
                                                              packed_type, sparse->ndata);
#endif
                            break;
                        case MAT_T_INT32:
                            nBytes = ReadCompressedInt32Data(mat, matvar->internal->z,
                                                             (mat_int32_t *)sparse->data,
                                                             packed_type, sparse->ndata);
                            break;
                        case MAT_T_UINT32:
                            nBytes = ReadCompressedUInt32Data(mat, matvar->internal->z,
                                                              (mat_uint32_t *)sparse->data,
                                                              packed_type, sparse->ndata);
                            break;
                        case MAT_T_INT16:
                            nBytes = ReadCompressedInt16Data(mat, matvar->internal->z,
                                                             (mat_int16_t *)sparse->data,
                                                             packed_type, sparse->ndata);
                            break;
                        case MAT_T_UINT16:
                            nBytes = ReadCompressedUInt16Data(mat, matvar->internal->z,
                                                              (mat_uint16_t *)sparse->data,
                                                              packed_type, sparse->ndata);
                            break;
                        case MAT_T_INT8:
                            nBytes = ReadCompressedInt8Data(mat, matvar->internal->z,
                                                            (mat_int8_t *)sparse->data, packed_type,
                                                            sparse->ndata);
                            break;
                        case MAT_T_UINT8:
                            nBytes = ReadCompressedUInt8Data(mat, matvar->internal->z,
                                                             (mat_uint8_t *)sparse->data,
                                                             packed_type, sparse->ndata);
                            break;
                        default:
                            break;
                    }
#else  /* EXTENDED_SPARSE */
                    nBytes =
                        ReadCompressedDoubleData(mat, matvar->internal->z, (double *)sparse->data,
                                                 packed_type, sparse->ndata);
#endif /* EXTENDED_SPARSE */
                    if ( data_in_tag )
                        nBytes += 4;
                    if ( (nBytes % 8) != 0 )
                        err = InflateSkip(mat, matvar->internal->z, 8 - (nBytes % 8), NULL);
#endif /* HAVE_ZLIB */
                }
            }
            break;
        }
        case MAT_C_FUNCTION: {
            matvar_t **functions;
            size_t nfunctions = 0;

            if ( !matvar->nbytes || !matvar->data_size )
                break;
            nfunctions = matvar->nbytes / matvar->data_size;
            functions = (matvar_t **)matvar->data;
            if ( NULL != functions ) {
                size_t i;
                for ( i = 0; i < nfunctions; i++ ) {
                    err = Mat_VarRead5(mat, functions[i]);
                    if ( err )
                        break;
                }
            }
            /* FIXME: */
            matvar->data_type = MAT_T_FUNCTION;
            break;
        }
        case MAT_C_OBJECT:
            Mat_Warning("Mat_VarRead5: %d is not a supported class", matvar->class_type);
            break;
        default:
            err = MATIO_E_OPERATION_NOT_SUPPORTED;
            Mat_Critical("Mat_VarRead5: %d is not a supported class", matvar->class_type);
            break;
    }
    switch ( matvar->class_type ) {
        case MAT_C_DOUBLE:
        case MAT_C_SINGLE:
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
#endif
        case MAT_C_INT32:
        case MAT_C_UINT32:
        case MAT_C_INT16:
        case MAT_C_UINT16:
        case MAT_C_INT8:
        case MAT_C_UINT8:
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data;

                err = Mul(&matvar->nbytes, nelems, matvar->data_size);
                if ( err ) {
                    Mat_Critical("Integer multiplication overflow");
                    break;
                }

                complex_data = ComplexMalloc(matvar->nbytes);
                if ( NULL == complex_data ) {
                    err = MATIO_E_OUT_OF_MEMORY;
                    Mat_Critical("Couldn't allocate memory for the complex data");
                    break;
                }

                err = Mat_VarReadNumeric5(mat, matvar, complex_data->Re, nelems);
                if ( err ) {
                    ComplexFree(complex_data);
                    break;
                }
                err = Mat_VarReadNumeric5(mat, matvar, complex_data->Im, nelems);
                if ( err ) {
                    ComplexFree(complex_data);
                    break;
                }
                matvar->data = complex_data;
            } else {
                void *data;
                err = Mul(&matvar->nbytes, nelems, matvar->data_size);
                if ( err ) {
                    Mat_Critical("Integer multiplication overflow");
                    break;
                }

                data = malloc(matvar->nbytes);
                if ( NULL == data ) {
                    err = MATIO_E_OUT_OF_MEMORY;
                    Mat_Critical("Couldn't allocate memory for the data");
                    break;
                }
                err = Mat_VarReadNumeric5(mat, matvar, data, nelems);
                if ( err ) {
                    free(data);
                    break;
                }
                matvar->data = data;
            }
        default:
            break;
    }
    (void)fseeko((FILE *)mat->fp, fpos, SEEK_SET);

    return err;
}

#if HAVE_ZLIB
#define GET_DATA_SLABN_RANK_LOOP                                                \
    do {                                                                        \
        for ( j = 1; j < rank; j++ ) {                                          \
            cnt[j]++;                                                           \
            if ( (cnt[j] % edge[j]) == 0 ) {                                    \
                cnt[j] = 0;                                                     \
                if ( (I % dimp[j]) != 0 ) {                                     \
                    ptr_in += dimp[j] - (I % dimp[j]) + dimp[j - 1] * start[j]; \
                    I += dimp[j] - (I % dimp[j]) + dimp[j - 1] * start[j];      \
                } else if ( start[j] ) {                                        \
                    ptr_in += dimp[j - 1] * start[j];                           \
                    I += dimp[j - 1] * start[j];                                \
                }                                                               \
            } else {                                                            \
                I += inc[j];                                                    \
                ptr_in += inc[j];                                               \
                break;                                                          \
            }                                                                   \
        }                                                                       \
    } while ( 0 )

#define GET_DATA_SLAB2(T)                              \
    do {                                               \
        ptr_in += start[1] * dims[0] + start[0];       \
        for ( i = 0; i < edge[1]; i++ ) {              \
            for ( j = 0; j < edge[0]; j++ ) {          \
                *ptr = (T)(*(ptr_in + j * stride[0])); \
                ptr++;                                 \
            }                                          \
            ptr_in += stride[1] * dims[0];             \
        }                                              \
    } while ( 0 )

#define GET_DATA_SLABN(T)                                                      \
    do {                                                                       \
        inc[0] = stride[0] - 1;                                                \
        dimp[0] = dims[0];                                                     \
        N = edge[0];                                                           \
        I = 0; /* start[0]; */                                                 \
        for ( i = 1; i < rank; i++ ) {                                         \
            inc[i] = stride[i] - 1;                                            \
            dimp[i] = dims[i - 1];                                             \
            for ( j = i; j--; ) {                                              \
                inc[i] *= dims[j];                                             \
                dimp[i] *= dims[j + 1];                                        \
            }                                                                  \
            N *= edge[i];                                                      \
            I += dimp[i - 1] * start[i];                                       \
        }                                                                      \
        ptr_in += I;                                                           \
        if ( stride[0] == 1 ) {                                                \
            for ( i = 0; i < N; i += edge[0] ) {                               \
                int k;                                                         \
                if ( start[0] ) {                                              \
                    ptr_in += start[0];                                        \
                    I += start[0];                                             \
                }                                                              \
                for ( k = 0; k < edge[0]; k++ ) {                              \
                    *(ptr + i + k) = (T)(*(ptr_in + k));                       \
                }                                                              \
                I += dims[0] - start[0];                                       \
                ptr_in += dims[0] - start[0];                                  \
                GET_DATA_SLABN_RANK_LOOP;                                      \
            }                                                                  \
        } else {                                                               \
            for ( i = 0; i < N; i += edge[0] ) {                               \
                if ( start[0] ) {                                              \
                    ptr_in += start[0];                                        \
                    I += start[0];                                             \
                }                                                              \
                for ( j = 0; j < edge[0]; j++ ) {                              \
                    *(ptr + i + j) = (T)(*ptr_in);                             \
                    ptr_in += stride[0];                                       \
                    I += stride[0];                                            \
                }                                                              \
                I += dims[0] - (ptrdiff_t)edge[0] * stride[0] - start[0];      \
                ptr_in += dims[0] - (ptrdiff_t)edge[0] * stride[0] - start[0]; \
                GET_DATA_SLABN_RANK_LOOP;                                      \
            }                                                                  \
        }                                                                      \
    } while ( 0 )

#ifdef HAVE_MAT_INT64_T
#define GET_DATA_SLAB2_INT64(T)                           \
    do {                                                  \
        if ( MAT_T_INT64 == data_type ) {                 \
            mat_int64_t *ptr_in = (mat_int64_t *)data_in; \
            GET_DATA_SLAB2(T);                            \
            err = MATIO_E_NO_ERROR;                       \
        }                                                 \
    } while ( 0 )
#else
#define GET_DATA_SLAB2_INT64(T)
#endif /* HAVE_MAT_INT64_T */

#ifdef HAVE_MAT_UINT64_T
#define GET_DATA_SLAB2_UINT64(T)                            \
    do {                                                    \
        if ( MAT_T_UINT64 == data_type ) {                  \
            mat_uint64_t *ptr_in = (mat_uint64_t *)data_in; \
            GET_DATA_SLAB2(T);                              \
            err = MATIO_E_NO_ERROR;                         \
        }                                                   \
    } while ( 0 )
#else
#define GET_DATA_SLAB2_UINT64(T)
#endif /* HAVE_MAT_UINT64_T */

#define GET_DATA_SLAB2_TYPE(T)                                  \
    do {                                                        \
        switch ( data_type ) {                                  \
            case MAT_T_DOUBLE: {                                \
                double *ptr_in = (double *)data_in;             \
                GET_DATA_SLAB2(T);                              \
                break;                                          \
            }                                                   \
            case MAT_T_SINGLE: {                                \
                float *ptr_in = (float *)data_in;               \
                GET_DATA_SLAB2(T);                              \
                break;                                          \
            }                                                   \
            case MAT_T_INT32: {                                 \
                mat_int32_t *ptr_in = (mat_int32_t *)data_in;   \
                GET_DATA_SLAB2(T);                              \
                break;                                          \
            }                                                   \
            case MAT_T_UINT32: {                                \
                mat_uint32_t *ptr_in = (mat_uint32_t *)data_in; \
                GET_DATA_SLAB2(T);                              \
                break;                                          \
            }                                                   \
            case MAT_T_INT16: {                                 \
                mat_int16_t *ptr_in = (mat_int16_t *)data_in;   \
                GET_DATA_SLAB2(T);                              \
                break;                                          \
            }                                                   \
            case MAT_T_UINT16: {                                \
                mat_uint16_t *ptr_in = (mat_uint16_t *)data_in; \
                GET_DATA_SLAB2(T);                              \
                break;                                          \
            }                                                   \
            case MAT_T_INT8: {                                  \
                mat_int8_t *ptr_in = (mat_int8_t *)data_in;     \
                GET_DATA_SLAB2(T);                              \
                break;                                          \
            }                                                   \
            case MAT_T_UINT8: {                                 \
                mat_uint8_t *ptr_in = (mat_uint8_t *)data_in;   \
                GET_DATA_SLAB2(T);                              \
                break;                                          \
            }                                                   \
            default:                                            \
                err = MATIO_E_OPERATION_NOT_SUPPORTED;          \
                GET_DATA_SLAB2_INT64(T);                        \
                GET_DATA_SLAB2_UINT64(T);                       \
                break;                                          \
        }                                                       \
    } while ( 0 )

#ifdef HAVE_MAT_INT64_T
#define GET_DATA_SLABN_INT64(T)                           \
    do {                                                  \
        if ( MAT_T_INT64 == data_type ) {                 \
            mat_int64_t *ptr_in = (mat_int64_t *)data_in; \
            GET_DATA_SLABN(T);                            \
            err = MATIO_E_NO_ERROR;                       \
        }                                                 \
    } while ( 0 )
#else
#define GET_DATA_SLABN_INT64(T)
#endif /* HAVE_MAT_INT64_T */

#ifdef HAVE_MAT_UINT64_T
#define GET_DATA_SLABN_UINT64(T)                            \
    do {                                                    \
        if ( MAT_T_UINT64 == data_type ) {                  \
            mat_uint64_t *ptr_in = (mat_uint64_t *)data_in; \
            GET_DATA_SLABN(T);                              \
            err = 0;                                        \
        }                                                   \
    } while ( 0 )
#else
#define GET_DATA_SLABN_UINT64(T)
#endif /* HAVE_MAT_UINT64_T */

#define GET_DATA_SLABN_TYPE(T)                                  \
    do {                                                        \
        switch ( data_type ) {                                  \
            case MAT_T_DOUBLE: {                                \
                double *ptr_in = (double *)data_in;             \
                GET_DATA_SLABN(T);                              \
                break;                                          \
            }                                                   \
            case MAT_T_SINGLE: {                                \
                float *ptr_in = (float *)data_in;               \
                GET_DATA_SLABN(T);                              \
                break;                                          \
            }                                                   \
            case MAT_T_INT32: {                                 \
                mat_int32_t *ptr_in = (mat_int32_t *)data_in;   \
                GET_DATA_SLABN(T);                              \
                break;                                          \
            }                                                   \
            case MAT_T_UINT32: {                                \
                mat_uint32_t *ptr_in = (mat_uint32_t *)data_in; \
                GET_DATA_SLABN(T);                              \
                break;                                          \
            }                                                   \
            case MAT_T_INT16: {                                 \
                mat_int16_t *ptr_in = (mat_int16_t *)data_in;   \
                GET_DATA_SLABN(T);                              \
                break;                                          \
            }                                                   \
            case MAT_T_UINT16: {                                \
                mat_uint16_t *ptr_in = (mat_uint16_t *)data_in; \
                GET_DATA_SLABN(T);                              \
                break;                                          \
            }                                                   \
            case MAT_T_INT8: {                                  \
                mat_int8_t *ptr_in = (mat_int8_t *)data_in;     \
                GET_DATA_SLABN(T);                              \
                break;                                          \
            }                                                   \
            case MAT_T_UINT8: {                                 \
                mat_uint8_t *ptr_in = (mat_uint8_t *)data_in;   \
                GET_DATA_SLABN(T);                              \
                break;                                          \
            }                                                   \
            default:                                            \
                err = MATIO_E_OPERATION_NOT_SUPPORTED;          \
                GET_DATA_SLABN_INT64(T);                        \
                GET_DATA_SLABN_UINT64(T);                       \
                break;                                          \
        }                                                       \
    } while ( 0 )

static int
GetDataSlab(void *data_in, void *data_out, enum matio_classes class_type,
            enum matio_types data_type, size_t *dims, int *start, int *stride, int *edge, int rank,
            size_t nbytes)
{
    int err = MATIO_E_NO_ERROR;
    int same_type = 0;
    if ( (class_type == MAT_C_DOUBLE && data_type == MAT_T_DOUBLE) ||
         (class_type == MAT_C_SINGLE && data_type == MAT_T_SINGLE) ||
         (class_type == MAT_C_INT16 && data_type == MAT_T_INT16) ||
         (class_type == MAT_C_INT32 && data_type == MAT_T_INT32) ||
         (class_type == MAT_C_INT64 && data_type == MAT_T_INT64) ||
         (class_type == MAT_C_INT8 && data_type == MAT_T_INT8) ||
         (class_type == MAT_C_UINT16 && data_type == MAT_T_UINT16) ||
         (class_type == MAT_C_UINT32 && data_type == MAT_T_UINT32) ||
         (class_type == MAT_C_UINT64 && data_type == MAT_T_UINT64) ||
         (class_type == MAT_C_UINT8 && data_type == MAT_T_UINT8) )
        same_type = 1;

    if ( rank == 2 ) {
        if ( (size_t)stride[0] * (edge[0] - 1) + start[0] + 1 > dims[0] )
            err = MATIO_E_BAD_ARGUMENT;
        else if ( (size_t)stride[1] * (edge[1] - 1) + start[1] + 1 > dims[1] )
            err = MATIO_E_BAD_ARGUMENT;
        else if ( (stride[0] == 1 && (size_t)edge[0] == dims[0]) && (stride[1] == 1) &&
                  (same_type == 1) )
            memcpy(data_out, data_in, nbytes);
        else {
            int i, j;

            switch ( class_type ) {
                case MAT_C_DOUBLE: {
                    double *ptr = (double *)data_out;
                    GET_DATA_SLAB2_TYPE(double);
                    break;
                }
                case MAT_C_SINGLE: {
                    float *ptr = (float *)data_out;
                    GET_DATA_SLAB2_TYPE(float);
                    break;
                }
#ifdef HAVE_MAT_INT64_T
                case MAT_C_INT64: {
                    mat_int64_t *ptr = (mat_int64_t *)data_out;
                    GET_DATA_SLAB2_TYPE(mat_int64_t);
                    break;
                }
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
                case MAT_C_UINT64: {
                    mat_uint64_t *ptr = (mat_uint64_t *)data_out;
                    GET_DATA_SLAB2_TYPE(mat_uint64_t);
                    break;
                }
#endif /* HAVE_MAT_UINT64_T */
                case MAT_C_INT32: {
                    mat_int32_t *ptr = (mat_int32_t *)data_out;
                    GET_DATA_SLAB2_TYPE(mat_int32_t);
                    break;
                }
                case MAT_C_UINT32: {
                    mat_uint32_t *ptr = (mat_uint32_t *)data_out;
                    GET_DATA_SLAB2_TYPE(mat_uint32_t);
                    break;
                }
                case MAT_C_INT16: {
                    mat_int16_t *ptr = (mat_int16_t *)data_out;
                    GET_DATA_SLAB2_TYPE(mat_int16_t);
                    break;
                }
                case MAT_C_UINT16: {
                    mat_uint16_t *ptr = (mat_uint16_t *)data_out;
                    GET_DATA_SLAB2_TYPE(mat_uint16_t);
                    break;
                }
                case MAT_C_INT8: {
                    mat_int8_t *ptr = (mat_int8_t *)data_out;
                    GET_DATA_SLAB2_TYPE(mat_int8_t);
                    break;
                }
                case MAT_C_UINT8: {
                    mat_uint8_t *ptr = (mat_uint8_t *)data_out;
                    GET_DATA_SLAB2_TYPE(mat_uint8_t);
                    break;
                }
                default:
                    err = MATIO_E_OPERATION_NOT_SUPPORTED;
                    break;
            }
        }
    } else {
        int i, j, N, I = 0;
        int inc[10] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        int cnt[10] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
        int dimp[10] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

        switch ( class_type ) {
            case MAT_C_DOUBLE: {
                double *ptr = (double *)data_out;
                GET_DATA_SLABN_TYPE(double);
                break;
            }
            case MAT_C_SINGLE: {
                float *ptr = (float *)data_out;
                GET_DATA_SLABN_TYPE(float);
                break;
            }
#ifdef HAVE_MAT_INT64_T
            case MAT_C_INT64: {
                mat_int64_t *ptr = (mat_int64_t *)data_out;
                GET_DATA_SLABN_TYPE(mat_int64_t);
                break;
            }
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
            case MAT_C_UINT64: {
                mat_uint64_t *ptr = (mat_uint64_t *)data_out;
                GET_DATA_SLABN_TYPE(mat_uint64_t);
                break;
            }
#endif /* HAVE_MAT_UINT64_T */
            case MAT_C_INT32: {
                mat_int32_t *ptr = (mat_int32_t *)data_out;
                GET_DATA_SLABN_TYPE(mat_int32_t);
                break;
            }
            case MAT_C_UINT32: {
                mat_uint32_t *ptr = (mat_uint32_t *)data_out;
                GET_DATA_SLABN_TYPE(mat_uint32_t);
                break;
            }
            case MAT_C_INT16: {
                mat_int16_t *ptr = (mat_int16_t *)data_out;
                GET_DATA_SLABN_TYPE(mat_int16_t);
                break;
            }
            case MAT_C_UINT16: {
                mat_uint16_t *ptr = (mat_uint16_t *)data_out;
                GET_DATA_SLABN_TYPE(mat_uint16_t);
                break;
            }
            case MAT_C_INT8: {
                mat_int8_t *ptr = (mat_int8_t *)data_out;
                GET_DATA_SLABN_TYPE(mat_int8_t);
                break;
            }
            case MAT_C_UINT8: {
                mat_uint8_t *ptr = (mat_uint8_t *)data_out;
                GET_DATA_SLABN_TYPE(mat_uint8_t);
                break;
            }
            default:
                err = MATIO_E_OPERATION_NOT_SUPPORTED;
                break;
        }
    }

    return err;
}

#undef GET_DATA_SLAB2
#undef GET_DATA_SLAB2_TYPE
#undef GET_DATA_SLAB2_INT64
#undef GET_DATA_SLAB2_UINT64
#undef GET_DATA_SLABN
#undef GET_DATA_SLABN_TYPE
#undef GET_DATA_SLABN_INT64
#undef GET_DATA_SLABN_UINT64
#undef GET_DATA_SLABN_RANK_LOOP

#define GET_DATA_LINEAR                                        \
    do {                                                       \
        ptr_in += start;                                       \
        if ( !stride ) {                                       \
            memcpy(ptr, ptr_in, (size_t)edge *data_size);      \
        } else {                                               \
            int i;                                             \
            for ( i = 0; i < edge; i++ )                       \
                memcpy(ptr++, ptr_in + i * stride, data_size); \
        }                                                      \
    } while ( 0 )

static int
GetDataLinear(void *data_in, void *data_out, enum matio_classes class_type,
              enum matio_types data_type, int start, int stride, int edge)
{
    int err = MATIO_E_NO_ERROR;
    size_t data_size = Mat_SizeOf(data_type);

    switch ( class_type ) {
        case MAT_C_DOUBLE: {
            double *ptr = (double *)data_out;
            double *ptr_in = (double *)data_in;
            GET_DATA_LINEAR;
            break;
        }
        case MAT_C_SINGLE: {
            float *ptr = (float *)data_out;
            float *ptr_in = (float *)data_in;
            GET_DATA_LINEAR;
            break;
        }
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64: {
            mat_int64_t *ptr = (mat_int64_t *)data_out;
            mat_int64_t *ptr_in = (mat_int64_t *)data_in;
            GET_DATA_LINEAR;
            break;
        }
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64: {
            mat_uint64_t *ptr = (mat_uint64_t *)data_out;
            mat_uint64_t *ptr_in = (mat_uint64_t *)data_in;
            GET_DATA_LINEAR;
            break;
        }
#endif /* HAVE_MAT_UINT64_T */
        case MAT_C_INT32: {
            mat_int32_t *ptr = (mat_int32_t *)data_out;
            mat_int32_t *ptr_in = (mat_int32_t *)data_in;
            GET_DATA_LINEAR;
            break;
        }
        case MAT_C_UINT32: {
            mat_uint32_t *ptr = (mat_uint32_t *)data_out;
            mat_uint32_t *ptr_in = (mat_uint32_t *)data_in;
            GET_DATA_LINEAR;
            break;
        }
        case MAT_C_INT16: {
            mat_int16_t *ptr = (mat_int16_t *)data_out;
            mat_int16_t *ptr_in = (mat_int16_t *)data_in;
            GET_DATA_LINEAR;
            break;
        }
        case MAT_C_UINT16: {
            mat_uint16_t *ptr = (mat_uint16_t *)data_out;
            mat_uint16_t *ptr_in = (mat_uint16_t *)data_in;
            GET_DATA_LINEAR;
            break;
        }
        case MAT_C_INT8: {
            mat_int8_t *ptr = (mat_int8_t *)data_out;
            mat_int8_t *ptr_in = (mat_int8_t *)data_in;
            GET_DATA_LINEAR;
            break;
        }
        case MAT_C_UINT8: {
            mat_uint8_t *ptr = (mat_uint8_t *)data_out;
            mat_uint8_t *ptr_in = (mat_uint8_t *)data_in;
            GET_DATA_LINEAR;
            break;
        }
        default:
            err = MATIO_E_OPERATION_NOT_SUPPORTED;
            break;
    }

    return err;
}

#undef GET_DATA_LINEAR
#endif

/** @if mat_devman
 * @brief Reads a slab of data from the mat variable @c matvar
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
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
Mat_VarReadData5(mat_t *mat, matvar_t *matvar, void *data, int *start, int *stride, int *edge)
{
    int err = MATIO_E_NO_ERROR, real_bytes = 0;
    mat_uint32_t tag[2] = {0, 0};
#if HAVE_ZLIB
    z_stream z;
#endif

    (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);
    if ( matvar->compression == MAT_COMPRESSION_NONE ) {
        err = Read(tag, 4, 2, (FILE *)mat->fp, NULL);
        if ( err ) {
            return err;
        }
        if ( mat->byteswap ) {
            (void)Mat_uint32Swap(tag);
            (void)Mat_uint32Swap(tag + 1);
        }
        matvar->data_type = TYPE_FROM_TAG(tag[0]);
        if ( tag[0] & 0xffff0000 ) { /* Data is packed in the tag */
            (void)fseeko((FILE *)mat->fp, -4, SEEK_CUR);
            real_bytes = 4 + (tag[0] >> 16);
        } else {
            real_bytes = 8 + tag[1];
        }
#if HAVE_ZLIB
    } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
        if ( NULL != matvar->internal->data ) {
            /* Data already read in ReadNextStructField or ReadNextCell */
            if ( matvar->isComplex ) {
                mat_complex_split_t *ci, *co;

                co = (mat_complex_split_t *)data;
                ci = (mat_complex_split_t *)matvar->internal->data;
                err = GetDataSlab(ci->Re, co->Re, matvar->class_type, matvar->data_type,
                                  matvar->dims, start, stride, edge, matvar->rank, matvar->nbytes);
                if ( MATIO_E_NO_ERROR == err )
                    err = GetDataSlab(ci->Im, co->Im, matvar->class_type, matvar->data_type,
                                      matvar->dims, start, stride, edge, matvar->rank,
                                      matvar->nbytes);
                return err;
            } else {
                return GetDataSlab(matvar->internal->data, data, matvar->class_type,
                                   matvar->data_type, matvar->dims, start, stride, edge,
                                   matvar->rank, matvar->nbytes);
            }
        }

        err = inflateCopy(&z, matvar->internal->z);
        if ( err != Z_OK ) {
            Mat_Critical("inflateCopy returned error %s", zError(err));
            return MATIO_E_FILE_FORMAT_VIOLATION;
        }
        z.avail_in = 0;
        err = Inflate(mat, &z, tag, 4, NULL);
        if ( err ) {
            return err;
        }
        if ( mat->byteswap ) {
            (void)Mat_uint32Swap(tag);
        }
        matvar->data_type = TYPE_FROM_TAG(tag[0]);
        if ( !(tag[0] & 0xffff0000) ) { /* Data is NOT packed in the tag */
            err = Inflate(mat, &z, tag + 1, 4, NULL);
            if ( err ) {
                return err;
            }
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(tag + 1);
            }
            real_bytes = 8 + tag[1];
        } else {
            real_bytes = 4 + (tag[0] >> 16);
        }
#endif
    }
    if ( real_bytes % 8 )
        real_bytes += (8 - (real_bytes % 8));

    if ( matvar->rank == 2 ) {
        if ( (size_t)stride[0] * (edge[0] - 1) + start[0] + 1 > matvar->dims[0] )
            err = MATIO_E_BAD_ARGUMENT;
        else if ( (size_t)stride[1] * (edge[1] - 1) + start[1] + 1 > matvar->dims[1] )
            err = MATIO_E_BAD_ARGUMENT;
        else if ( matvar->compression == MAT_COMPRESSION_NONE ) {
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = (mat_complex_split_t *)data;

                ReadDataSlab2(mat, complex_data->Re, matvar->class_type, matvar->data_type,
                              matvar->dims, start, stride, edge);
                (void)fseeko((FILE *)mat->fp, matvar->internal->datapos + real_bytes, SEEK_SET);
                err = Read(tag, 4, 2, (FILE *)mat->fp, NULL);
                if ( err ) {
                    return err;
                }
                if ( mat->byteswap ) {
                    (void)Mat_uint32Swap(tag);
                    (void)Mat_uint32Swap(tag + 1);
                }
                matvar->data_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is packed in the tag */
                    (void)fseeko((FILE *)mat->fp, -4, SEEK_CUR);
                }
                ReadDataSlab2(mat, complex_data->Im, matvar->class_type, matvar->data_type,
                              matvar->dims, start, stride, edge);
            } else {
                ReadDataSlab2(mat, data, matvar->class_type, matvar->data_type, matvar->dims, start,
                              stride, edge);
            }
        }
#if HAVE_ZLIB
        else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = (mat_complex_split_t *)data;

                ReadCompressedDataSlab2(mat, &z, complex_data->Re, matvar->class_type,
                                        matvar->data_type, matvar->dims, start, stride, edge);

                (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);

                /* Reset zlib knowledge to before reading real tag */
                inflateEnd(&z);
                err = inflateCopy(&z, matvar->internal->z);
                if ( err != Z_OK ) {
                    Mat_Critical("inflateCopy returned error %s", zError(err));
                    return MATIO_E_FILE_FORMAT_VIOLATION;
                }
                InflateSkip(mat, &z, real_bytes, NULL);
                z.avail_in = 0;
                err = Inflate(mat, &z, tag, 4, NULL);
                if ( err ) {
                    return err;
                }
                if ( mat->byteswap ) {
                    (void)Mat_uint32Swap(tag);
                }
                matvar->data_type = TYPE_FROM_TAG(tag[0]);
                if ( !(tag[0] & 0xffff0000) ) { /*Data is NOT packed in the tag*/
                    InflateSkip(mat, &z, 4, NULL);
                }
                ReadCompressedDataSlab2(mat, &z, complex_data->Im, matvar->class_type,
                                        matvar->data_type, matvar->dims, start, stride, edge);
            } else {
                ReadCompressedDataSlab2(mat, &z, data, matvar->class_type, matvar->data_type,
                                        matvar->dims, start, stride, edge);
            }
            inflateEnd(&z);
        }
#endif
    } else {
        if ( matvar->compression == MAT_COMPRESSION_NONE ) {
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = (mat_complex_split_t *)data;

                ReadDataSlabN(mat, complex_data->Re, matvar->class_type, matvar->data_type,
                              matvar->rank, matvar->dims, start, stride, edge);

                (void)fseeko((FILE *)mat->fp, matvar->internal->datapos + real_bytes, SEEK_SET);
                err = Read(tag, 4, 2, (FILE *)mat->fp, NULL);
                if ( err ) {
                    return err;
                }
                if ( mat->byteswap ) {
                    (void)Mat_uint32Swap(tag);
                    (void)Mat_uint32Swap(tag + 1);
                }
                matvar->data_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is packed in the tag */
                    (void)fseeko((FILE *)mat->fp, -4, SEEK_CUR);
                }
                ReadDataSlabN(mat, complex_data->Im, matvar->class_type, matvar->data_type,
                              matvar->rank, matvar->dims, start, stride, edge);
            } else {
                ReadDataSlabN(mat, data, matvar->class_type, matvar->data_type, matvar->rank,
                              matvar->dims, start, stride, edge);
            }
        }
#if HAVE_ZLIB
        else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = (mat_complex_split_t *)data;

                ReadCompressedDataSlabN(mat, &z, complex_data->Re, matvar->class_type,
                                        matvar->data_type, matvar->rank, matvar->dims, start,
                                        stride, edge);

                (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);
                /* Reset zlib knowledge to before reading real tag */
                inflateEnd(&z);
                err = inflateCopy(&z, matvar->internal->z);
                if ( err != Z_OK ) {
                    Mat_Critical("inflateCopy returned error %s", zError(err));
                    return MATIO_E_FILE_FORMAT_VIOLATION;
                }
                InflateSkip(mat, &z, real_bytes, NULL);
                z.avail_in = 0;
                err = Inflate(mat, &z, tag, 4, NULL);
                if ( err ) {
                    return err;
                }
                if ( mat->byteswap ) {
                    (void)Mat_uint32Swap(tag);
                }
                matvar->data_type = TYPE_FROM_TAG(tag[0]);
                if ( !(tag[0] & 0xffff0000) ) { /*Data is NOT packed in the tag*/
                    InflateSkip(mat, &z, 4, NULL);
                }
                ReadCompressedDataSlabN(mat, &z, complex_data->Im, matvar->class_type,
                                        matvar->data_type, matvar->rank, matvar->dims, start,
                                        stride, edge);
            } else {
                ReadCompressedDataSlabN(mat, &z, data, matvar->class_type, matvar->data_type,
                                        matvar->rank, matvar->dims, start, stride, edge);
            }
            inflateEnd(&z);
        }
#endif
    }
    if ( err == MATIO_E_NO_ERROR ) {
        matvar->data_type = ClassType2DataType(matvar->class_type);
        matvar->data_size = Mat_SizeOfClass(matvar->class_type);
    }
    return err;
}

/** @brief Reads a subset of a MAT variable using a 1-D indexing
 *
 * Reads data from a MAT variable using a linear (1-D) indexing mode. The
 * variable must have been read by Mat_VarReadInfo.
 * @ingroup MAT
 * @param mat MAT file to read data from
 * @param matvar MAT variable information
 * @param data pointer to store data in (must be pre-allocated)
 * @param start starting index
 * @param stride stride of data
 * @param edge number of elements to read
 * @retval 0 on success
 */
int
Mat_VarReadDataLinear5(mat_t *mat, matvar_t *matvar, void *data, int start, int stride, int edge)
{
    int err = MATIO_E_NO_ERROR, real_bytes = 0;
    mat_uint32_t tag[2] = {0, 0};
#if HAVE_ZLIB
    z_stream z;
#endif
    size_t nelems = 1;

    if ( mat->version == MAT_FT_MAT4 )
        return -1;
    (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);
    if ( matvar->compression == MAT_COMPRESSION_NONE ) {
        err = Read(tag, 4, 2, (FILE *)mat->fp, NULL);
        if ( err ) {
            return err;
        }
        if ( mat->byteswap ) {
            (void)Mat_uint32Swap(tag);
            (void)Mat_uint32Swap(tag + 1);
        }
        matvar->data_type = (enum matio_types)(tag[0] & 0x000000ff);
        if ( tag[0] & 0xffff0000 ) { /* Data is packed in the tag */
            (void)fseeko((FILE *)mat->fp, -4, SEEK_CUR);
            real_bytes = 4 + (tag[0] >> 16);
        } else {
            real_bytes = 8 + tag[1];
        }
#if HAVE_ZLIB
    } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
        if ( NULL != matvar->internal->data ) {
            /* Data already read in ReadNextStructField or ReadNextCell */
            if ( matvar->isComplex ) {
                mat_complex_split_t *ci, *co;

                co = (mat_complex_split_t *)data;
                ci = (mat_complex_split_t *)matvar->internal->data;
                err = GetDataLinear(ci->Re, co->Re, matvar->class_type, matvar->data_type, start,
                                    stride, edge);
                if ( err == MATIO_E_NO_ERROR )
                    err = GetDataLinear(ci->Im, co->Im, matvar->class_type, matvar->data_type,
                                        start, stride, edge);
                return err;
            } else {
                return GetDataLinear(matvar->internal->data, data, matvar->class_type,
                                     matvar->data_type, start, stride, edge);
            }
        }

        matvar->internal->z->avail_in = 0;
        err = inflateCopy(&z, matvar->internal->z);
        if ( err != Z_OK ) {
            Mat_Critical("inflateCopy returned error %s", zError(err));
            return MATIO_E_FILE_FORMAT_VIOLATION;
        }
        err = Inflate(mat, &z, tag, 4, NULL);
        if ( err ) {
            return err;
        }
        if ( mat->byteswap ) {
            (void)Mat_uint32Swap(tag);
            (void)Mat_uint32Swap(tag + 1);
        }
        matvar->data_type = (enum matio_types)(tag[0] & 0x000000ff);
        if ( !(tag[0] & 0xffff0000) ) { /* Data is NOT packed in the tag */
            err = Inflate(mat, &z, tag + 1, 4, NULL);
            if ( err ) {
                return err;
            }
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(tag + 1);
            }
            real_bytes = 8 + tag[1];
        } else {
            real_bytes = 4 + (tag[0] >> 16);
        }
#endif
    }
    if ( real_bytes % 8 )
        real_bytes += (8 - (real_bytes % 8));

    err = Mat_MulDims(matvar, &nelems);
    if ( err ) {
        Mat_Critical("Integer multiplication overflow");
        return err;
    }

    if ( (size_t)stride * (edge - 1) + start + 1 > nelems ) {
        err = MATIO_E_BAD_ARGUMENT;
    } else if ( matvar->compression == MAT_COMPRESSION_NONE ) {
        if ( matvar->isComplex ) {
            mat_complex_split_t *complex_data = (mat_complex_split_t *)data;

            ReadDataSlab1(mat, complex_data->Re, matvar->class_type, matvar->data_type, start,
                          stride, edge);
            (void)fseeko((FILE *)mat->fp, matvar->internal->datapos + real_bytes, SEEK_SET);
            err = Read(tag, 4, 2, (FILE *)mat->fp, NULL);
            if ( err ) {
                return err;
            }
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(tag);
                (void)Mat_uint32Swap(tag + 1);
            }
            matvar->data_type = (enum matio_types)(tag[0] & 0x000000ff);
            if ( tag[0] & 0xffff0000 ) { /* Data is packed in the tag */
                (void)fseeko((FILE *)mat->fp, -4, SEEK_CUR);
            }
            ReadDataSlab1(mat, complex_data->Im, matvar->class_type, matvar->data_type, start,
                          stride, edge);
        } else {
            ReadDataSlab1(mat, data, matvar->class_type, matvar->data_type, start, stride, edge);
        }
#if HAVE_ZLIB
    } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
        if ( matvar->isComplex ) {
            mat_complex_split_t *complex_data = (mat_complex_split_t *)data;

            ReadCompressedDataSlab1(mat, &z, complex_data->Re, matvar->class_type,
                                    matvar->data_type, start, stride, edge);

            (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);

            /* Reset zlib knowledge to before reading real tag */
            inflateEnd(&z);
            err = inflateCopy(&z, matvar->internal->z);
            if ( err != Z_OK ) {
                Mat_Critical("inflateCopy returned error %s", zError(err));
                return MATIO_E_FILE_FORMAT_VIOLATION;
            }
            InflateSkip(mat, &z, real_bytes, NULL);
            z.avail_in = 0;
            err = Inflate(mat, &z, tag, 4, NULL);
            if ( err ) {
                return err;
            }
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(tag);
            }
            matvar->data_type = (enum matio_types)(tag[0] & 0x000000ff);
            if ( !(tag[0] & 0xffff0000) ) { /*Data is NOT packed in the tag*/
                InflateSkip(mat, &z, 4, NULL);
            }
            ReadCompressedDataSlab1(mat, &z, complex_data->Im, matvar->class_type,
                                    matvar->data_type, start, stride, edge);
        } else {
            ReadCompressedDataSlab1(mat, &z, data, matvar->class_type, matvar->data_type, start,
                                    stride, edge);
        }
        inflateEnd(&z);
#endif
    }

    matvar->data_type = ClassType2DataType(matvar->class_type);
    matvar->data_size = Mat_SizeOfClass(matvar->class_type);

    return err;
}

/** @if mat_devman
 * @brief Writes a matlab variable to a version 5 matlab file
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
Mat_VarWrite5(mat_t *mat, matvar_t *matvar, int compress)
{
    mat_uint32_t array_flags;
    int array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int array_flags_size = 8, matrix_type = MAT_T_MATRIX;
    const mat_uint32_t pad4 = 0;
    int nBytes, i, nzmax = 0;
    mat_off_t start = 0, end = 0;

    if ( NULL == mat )
        return MATIO_E_BAD_ARGUMENT;

    /* FIXME: SEEK_END is not Guaranteed by the C standard */
    (void)fseeko((FILE *)mat->fp, 0, SEEK_END); /* Always write at end of file */

    if ( NULL == matvar || NULL == matvar->name )
        return MATIO_E_BAD_ARGUMENT;

#if HAVE_ZLIB
    if ( compress == MAT_COMPRESSION_NONE ) {
#else
    {
#endif
        fwrite(&matrix_type, 4, 1, (FILE *)mat->fp);
        fwrite(&pad4, 4, 1, (FILE *)mat->fp);
        start = ftello((FILE *)mat->fp);

        /* Array Flags */
        array_flags = matvar->class_type & CLASS_TYPE_MASK;
        if ( matvar->isComplex )
            array_flags |= MAT_F_COMPLEX;
        if ( matvar->isGlobal )
            array_flags |= MAT_F_GLOBAL;
        if ( matvar->isLogical )
            array_flags |= MAT_F_LOGICAL;
        if ( matvar->class_type == MAT_C_SPARSE )
            nzmax = ((mat_sparse_t *)matvar->data)->nzmax;

        fwrite(&array_flags_type, 4, 1, (FILE *)mat->fp);
        fwrite(&array_flags_size, 4, 1, (FILE *)mat->fp);
        fwrite(&array_flags, 4, 1, (FILE *)mat->fp);
        fwrite(&nzmax, 4, 1, (FILE *)mat->fp);
        /* Rank and Dimension */
        nBytes = matvar->rank * 4;
        fwrite(&dims_array_type, 4, 1, (FILE *)mat->fp);
        fwrite(&nBytes, 4, 1, (FILE *)mat->fp);
        for ( i = 0; i < matvar->rank; i++ ) {
            mat_int32_t dim;
            dim = matvar->dims[i];
            fwrite(&dim, 4, 1, (FILE *)mat->fp);
        }
        if ( matvar->rank % 2 != 0 )
            fwrite(&pad4, 4, 1, (FILE *)mat->fp);
        /* Name of variable */
        if ( strlen(matvar->name) <= 4 ) {
            mat_uint32_t array_name_type = MAT_T_INT8;
            const mat_uint32_t array_name_len = (mat_uint32_t)strlen(matvar->name);
            const mat_uint8_t pad1 = 0;
            array_name_type |= array_name_len << 16;
            fwrite(&array_name_type, 4, 1, (FILE *)mat->fp);
            fwrite(matvar->name, 1, array_name_len, (FILE *)mat->fp);
            for ( i = array_name_len; i < 4; i++ )
                fwrite(&pad1, 1, 1, (FILE *)mat->fp);
        } else {
            const mat_uint32_t array_name_type = MAT_T_INT8;
            const mat_uint32_t array_name_len = (mat_uint32_t)strlen(matvar->name);
            const mat_uint8_t pad1 = 0;

            fwrite(&array_name_type, 4, 1, (FILE *)mat->fp);
            fwrite(&array_name_len, 4, 1, (FILE *)mat->fp);
            fwrite(matvar->name, 1, array_name_len, (FILE *)mat->fp);
            if ( array_name_len % 8 )
                for ( i = array_name_len % 8; i < 8; i++ )
                    fwrite(&pad1, 1, 1, (FILE *)mat->fp);
        }

        if ( NULL != matvar->internal ) {
            matvar->internal->datapos = ftello((FILE *)mat->fp);
            if ( matvar->internal->datapos == -1L ) {
                Mat_Critical("Couldn't determine file position");
                return MATIO_E_GENERIC_READ_ERROR;
            }
        } else {
            /* Must be empty */
            matvar->class_type = MAT_C_EMPTY;
        }
        WriteType(mat, matvar);
#if HAVE_ZLIB
    } else if ( compress == MAT_COMPRESSION_ZLIB ) {
        mat_uint32_t comp_buf[512];
        mat_uint32_t uncomp_buf[512];
        int buf_size = 512, err;
        size_t byteswritten = 0, matrix_max_buf_size;
        z_streamp z;

        z = (z_streamp)calloc(1, sizeof(*z));
        if ( z == NULL )
            return MATIO_E_OUT_OF_MEMORY;
        err = deflateInit(z, Z_DEFAULT_COMPRESSION);
        if ( err != Z_OK ) {
            free(z);
            Mat_Critical("deflateInit returned %s", zError(err));
            return MATIO_E_FILE_FORMAT_VIOLATION;
        }

        matrix_type = MAT_T_COMPRESSED;
        fwrite(&matrix_type, 4, 1, (FILE *)mat->fp);
        fwrite(&pad4, 4, 1, (FILE *)mat->fp);
        start = ftello((FILE *)mat->fp);

        /* Array Flags */
        array_flags = matvar->class_type & CLASS_TYPE_MASK;
        if ( matvar->isComplex )
            array_flags |= MAT_F_COMPLEX;
        if ( matvar->isGlobal )
            array_flags |= MAT_F_GLOBAL;
        if ( matvar->isLogical )
            array_flags |= MAT_F_LOGICAL;
        if ( matvar->class_type == MAT_C_SPARSE )
            nzmax = ((mat_sparse_t *)matvar->data)->nzmax;

        memset(&uncomp_buf, 0, sizeof(uncomp_buf));
        uncomp_buf[0] = MAT_T_MATRIX;
        err = GetMatrixMaxBufSize(matvar, &matrix_max_buf_size);
        if ( err ) {
            free(z);
            return err;
        }
        if ( matrix_max_buf_size > UINT32_MAX ) {
            free(z);
            return MATIO_E_INDEX_TOO_BIG;
        }
        uncomp_buf[1] = matrix_max_buf_size;
        z->next_in = ZLIB_BYTE_PTR(uncomp_buf);
        z->avail_in = 8;
        do {
            z->next_out = ZLIB_BYTE_PTR(comp_buf);
            z->avail_out = buf_size * sizeof(*comp_buf);
            deflate(z, Z_NO_FLUSH);
            byteswritten +=
                fwrite(comp_buf, 1, buf_size * sizeof(*comp_buf) - z->avail_out, (FILE *)mat->fp);
        } while ( z->avail_out == 0 );
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
            uncomp_buf[6 + i] = dim;
        }
        if ( matvar->rank % 2 != 0 ) {
            uncomp_buf[6 + i] = pad4;
            i++;
        }

        z->next_in = ZLIB_BYTE_PTR(uncomp_buf);
        z->avail_in = (6 + i) * sizeof(*uncomp_buf);
        do {
            z->next_out = ZLIB_BYTE_PTR(comp_buf);
            z->avail_out = buf_size * sizeof(*comp_buf);
            deflate(z, Z_NO_FLUSH);
            byteswritten +=
                fwrite(comp_buf, 1, buf_size * sizeof(*comp_buf) - z->avail_out, (FILE *)mat->fp);
        } while ( z->avail_out == 0 );
        /* Name of variable */
        if ( strlen(matvar->name) <= 4 ) {
            mat_uint32_t array_name_len = (mat_uint32_t)strlen(matvar->name);
            const mat_uint32_t array_name_type = MAT_T_INT8;

            memset(uncomp_buf, 0, 8);
            uncomp_buf[0] = (array_name_len << 16) | array_name_type;
            memcpy(uncomp_buf + 1, matvar->name, array_name_len);
            if ( array_name_len % 4 )
                array_name_len += 4 - (array_name_len % 4);

            z->next_in = ZLIB_BYTE_PTR(uncomp_buf);
            z->avail_in = 8;
            do {
                z->next_out = ZLIB_BYTE_PTR(comp_buf);
                z->avail_out = buf_size * sizeof(*comp_buf);
                deflate(z, Z_NO_FLUSH);
                byteswritten += fwrite(comp_buf, 1, buf_size * sizeof(*comp_buf) - z->avail_out,
                                       (FILE *)mat->fp);
            } while ( z->avail_out == 0 );
        } else {
            mat_uint32_t array_name_len = (mat_uint32_t)strlen(matvar->name);
            const mat_uint32_t array_name_type = MAT_T_INT8;

            memset(uncomp_buf, 0, buf_size * sizeof(*uncomp_buf));
            uncomp_buf[0] = array_name_type;
            uncomp_buf[1] = array_name_len;
            memcpy(uncomp_buf + 2, matvar->name, array_name_len);
            if ( array_name_len % 8 )
                array_name_len += 8 - (array_name_len % 8);
            z->next_in = ZLIB_BYTE_PTR(uncomp_buf);
            z->avail_in = 8 + array_name_len;
            do {
                z->next_out = ZLIB_BYTE_PTR(comp_buf);
                z->avail_out = buf_size * sizeof(*comp_buf);
                deflate(z, Z_NO_FLUSH);
                byteswritten += fwrite(comp_buf, 1, buf_size * sizeof(*comp_buf) - z->avail_out,
                                       (FILE *)mat->fp);
            } while ( z->avail_out == 0 );
        }
        if ( NULL != matvar->internal ) {
            matvar->internal->datapos = ftello((FILE *)mat->fp);
            if ( matvar->internal->datapos == -1L ) {
                free(z);
                Mat_Critical("Couldn't determine file position");
                return MATIO_E_GENERIC_READ_ERROR;
            }
        } else {
            /* Must be empty */
            matvar->class_type = MAT_C_EMPTY;
        }
        WriteCompressedType(mat, matvar, z);
        z->next_in = NULL;
        z->avail_in = 0;
        do {
            z->next_out = ZLIB_BYTE_PTR(comp_buf);
            z->avail_out = buf_size * sizeof(*comp_buf);
            err = deflate(z, Z_FINISH);
            byteswritten +=
                fwrite(comp_buf, 1, buf_size * sizeof(*comp_buf) - z->avail_out, (FILE *)mat->fp);
        } while ( err != Z_STREAM_END && z->avail_out == 0 );
#if 0
        if ( byteswritten % 8 )
            for ( i = 0; i < 8-(byteswritten % 8); i++ )
                fwrite(&pad1,1,1,(FILE*)mat->fp);
#endif
        (void)deflateEnd(z);
        free(z);
#endif
    }
    end = ftello((FILE *)mat->fp);
    if ( start != -1L && end != -1L ) {
        nBytes = (int)(end - start);
        (void)fseeko((FILE *)mat->fp, (mat_off_t) - (nBytes + 4), SEEK_CUR);
        fwrite(&nBytes, 4, 1, (FILE *)mat->fp);
        (void)fseeko((FILE *)mat->fp, end, SEEK_SET);
    } else {
        Mat_Critical("Couldn't determine file position");
    }

    return MATIO_E_NO_ERROR;
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
Mat_VarReadNextInfo5(mat_t *mat)
{
    int err;
    mat_uint32_t data_type, array_flags, nBytes;
    mat_off_t fpos;
    matvar_t *matvar = NULL;

    if ( mat == NULL || mat->fp == NULL )
        return NULL;

    if ( IsEndOfFile((FILE *)mat->fp, &fpos) )
        return NULL;

    if ( fpos == -1L )
        return NULL;

    {
        size_t nbytes = 0;
        err = Read(&data_type, sizeof(mat_uint32_t), 1, (FILE *)mat->fp, &nbytes);
        if ( err || 0 == nbytes )
            return NULL;
    }
    err = Read(&nBytes, sizeof(mat_uint32_t), 1, (FILE *)mat->fp, NULL);
    if ( err )
        return NULL;
    if ( mat->byteswap ) {
        (void)Mat_uint32Swap(&data_type);
        (void)Mat_uint32Swap(&nBytes);
    }
    if ( nBytes > UINT32_MAX - 8 )
        return NULL;
    switch ( data_type ) {
        case MAT_T_COMPRESSED: {
#if HAVE_ZLIB
            mat_uint32_t uncomp_buf[16];
            int nbytes;
            size_t bytesread = 0;

            memset(&uncomp_buf, 0, sizeof(uncomp_buf));
            matvar = Mat_VarCalloc();
            if ( NULL == matvar ) {
                Mat_Critical("Couldn't allocate memory");
                break;
            }

            matvar->compression = MAT_COMPRESSION_ZLIB;
            matvar->internal->z = (z_streamp)calloc(1, sizeof(z_stream));
            err = inflateInit(matvar->internal->z);
            if ( err != Z_OK ) {
                Mat_VarFree(matvar);
                matvar = NULL;
                Mat_Critical("inflateInit returned %s", zError(err));
                break;
            }

            /* Read variable tag */
            err = Inflate(mat, matvar->internal->z, uncomp_buf, 8, &bytesread);
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(uncomp_buf);
                (void)Mat_uint32Swap(uncomp_buf + 1);
            }
            nbytes = uncomp_buf[1];
            if ( uncomp_buf[0] != MAT_T_MATRIX ) {
                (void)fseeko((FILE *)mat->fp, (mat_off_t)(nBytes - bytesread), SEEK_CUR);
                Mat_VarFree(matvar);
                matvar = NULL;
                Mat_Critical("Uncompressed type not MAT_T_MATRIX");
                break;
            }
            /* Array flags */
            err = Inflate(mat, matvar->internal->z, uncomp_buf, 16, &bytesread);
            if ( err ) {
                Mat_VarFree(matvar);
                matvar = NULL;
                break;
            }
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(uncomp_buf);
                (void)Mat_uint32Swap(uncomp_buf + 2);
                (void)Mat_uint32Swap(uncomp_buf + 3);
            }
            /* Array flags */
            if ( uncomp_buf[0] == MAT_T_UINT32 ) {
                array_flags = uncomp_buf[2];
                matvar->class_type = CLASS_FROM_ARRAY_FLAGS(array_flags);
                matvar->isComplex = (array_flags & MAT_F_COMPLEX);
                matvar->isGlobal = (array_flags & MAT_F_GLOBAL);
                matvar->isLogical = (array_flags & MAT_F_LOGICAL);
                if ( matvar->class_type == MAT_C_SPARSE ) {
                    /* Need to find a more appropriate place to store nzmax */
                    matvar->nbytes = uncomp_buf[3];
                }
            }
            if ( matvar->class_type != MAT_C_OPAQUE ) {
                mat_uint32_t *dims = NULL;
                int do_clean = 0;
                err = InflateRankDims(mat, matvar->internal->z, uncomp_buf, sizeof(uncomp_buf),
                                      &dims, &bytesread);
                if ( NULL == dims ) {
                    dims = uncomp_buf + 2;
                } else {
                    do_clean = 1;
                }
                if ( err ) {
                    if ( do_clean ) {
                        free(dims);
                    }
                    Mat_VarFree(matvar);
                    matvar = NULL;
                    break;
                }
                if ( mat->byteswap ) {
                    (void)Mat_uint32Swap(uncomp_buf);
                    (void)Mat_uint32Swap(uncomp_buf + 1);
                }
                /* Rank and dimension */
                if ( uncomp_buf[0] == MAT_T_INT32 ) {
                    int j;
                    size_t size;
                    nbytes = uncomp_buf[1];
                    matvar->rank = nbytes / 4;
                    if ( 0 == do_clean && matvar->rank > 13 ) {
                        int rank = matvar->rank;
                        matvar->rank = 0;
                        Mat_Critical("%d is not a valid rank", rank);
                        break;
                    }
                    err = Mul(&size, matvar->rank, sizeof(*matvar->dims));
                    if ( err ) {
                        if ( do_clean ) {
                            free(dims);
                        }
                        (void)fseeko((FILE *)mat->fp, (mat_off_t)(nBytes - bytesread), SEEK_CUR);
                        Mat_VarFree(matvar);
                        matvar = NULL;
                        Mat_Critical("Integer multiplication overflow");
                        break;
                    }
                    matvar->dims = (size_t *)malloc(size);
                    if ( NULL == matvar->dims ) {
                        if ( do_clean )
                            free(dims);
                        (void)fseeko((FILE *)mat->fp, (mat_off_t)(nBytes - bytesread), SEEK_CUR);
                        Mat_VarFree(matvar);
                        matvar = NULL;
                        Mat_Critical("Couldn't allocate memory");
                        break;
                    }
                    if ( mat->byteswap ) {
                        for ( j = 0; j < matvar->rank; j++ )
                            matvar->dims[j] = Mat_uint32Swap(dims + j);
                    } else {
                        for ( j = 0; j < matvar->rank; j++ )
                            matvar->dims[j] = dims[j];
                    }
                }
                if ( do_clean ) {
                    free(dims);
                }
                /* Variable name tag */
                err = Inflate(mat, matvar->internal->z, uncomp_buf, 8, &bytesread);
                if ( err ) {
                    Mat_VarFree(matvar);
                    matvar = NULL;
                    break;
                }
                if ( mat->byteswap )
                    (void)Mat_uint32Swap(uncomp_buf);
                /* Name of variable */
                if ( uncomp_buf[0] == MAT_T_INT8 ) { /* Name not in tag */
                    mat_uint32_t len, len_pad;
                    if ( mat->byteswap )
                        len = Mat_uint32Swap(uncomp_buf + 1);
                    else
                        len = uncomp_buf[1];

                    if ( len % 8 == 0 )
                        len_pad = len;
                    else if ( len < UINT32_MAX - 8 + (len % 8) )
                        len_pad = len + 8 - (len % 8);
                    else {
                        Mat_VarFree(matvar);
                        matvar = NULL;
                        break;
                    }
                    matvar->name = (char *)malloc(len_pad + 1);
                    if ( NULL != matvar->name ) {
                        /* Variable name */
                        err = Inflate(mat, matvar->internal->z, matvar->name, len_pad, &bytesread);
                        if ( err ) {
                            Mat_VarFree(matvar);
                            matvar = NULL;
                            break;
                        }
                        matvar->name[len] = '\0';
                    }
                } else {
                    mat_uint32_t len = (uncomp_buf[0] & 0xffff0000) >> 16;
                    if ( ((uncomp_buf[0] & 0x0000ffff) == MAT_T_INT8) && len > 0 && len <= 4 ) {
                        /* Name packed in tag */
                        matvar->name = (char *)malloc(len + 1);
                        if ( NULL != matvar->name ) {
                            memcpy(matvar->name, uncomp_buf + 1, len);
                            matvar->name[len] = '\0';
                        }
                    }
                }
                if ( matvar->class_type == MAT_C_STRUCT )
                    (void)ReadNextStructField(mat, matvar);
                else if ( matvar->class_type == MAT_C_CELL )
                    (void)ReadNextCell(mat, matvar);
                (void)fseeko((FILE *)mat->fp, -(mat_off_t)matvar->internal->z->avail_in, SEEK_CUR);
                matvar->internal->datapos = ftello((FILE *)mat->fp);
                if ( matvar->internal->datapos == -1L ) {
                    Mat_Critical("Couldn't determine file position");
                }
            }
            (void)fseeko((FILE *)mat->fp, nBytes + 8 + fpos, SEEK_SET);
            break;
#else
            Mat_Critical(
                "Compressed variable found in \"%s\", but matio was "
                "built without zlib support",
                mat->filename);
            (void)fseeko((FILE *)mat->fp, nBytes + 8 + fpos, SEEK_SET);
            return NULL;
#endif
        }
        case MAT_T_MATRIX: {
            mat_uint32_t buf[6];

            /* Read array flags and the dimensions tag */
            err = Read(buf, 4, 6, (FILE *)mat->fp, NULL);
            if ( err ) {
                (void)fseeko((FILE *)mat->fp, fpos, SEEK_SET);
                break;
            }
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(buf);
                (void)Mat_uint32Swap(buf + 1);
                (void)Mat_uint32Swap(buf + 2);
                (void)Mat_uint32Swap(buf + 3);
                (void)Mat_uint32Swap(buf + 4);
                (void)Mat_uint32Swap(buf + 5);
            }

            matvar = Mat_VarCalloc();
            if ( NULL == matvar ) {
                Mat_Critical("Couldn't allocate memory");
                break;
            }

            /* Array flags */
            if ( buf[0] == MAT_T_UINT32 || buf[0] == MAT_T_INT32 ) { /* Also allow INT32 for SWAN */
                array_flags = buf[2];
                matvar->class_type = CLASS_FROM_ARRAY_FLAGS(array_flags);
                matvar->isComplex = (array_flags & MAT_F_COMPLEX);
                matvar->isGlobal = (array_flags & MAT_F_GLOBAL);
                matvar->isLogical = (array_flags & MAT_F_LOGICAL);
                if ( matvar->class_type == MAT_C_SPARSE ) {
                    /* Need to find a more appropriate place to store nzmax */
                    matvar->nbytes = buf[3];
                }
            }
            /* Rank and dimension */
            {
                size_t nbytes = 0;
                err = ReadRankDims(mat, matvar, (enum matio_types)buf[4], buf[5], &nbytes);
                if ( err ) {
                    Mat_VarFree(matvar);
                    matvar = NULL;
                    (void)fseeko((FILE *)mat->fp, fpos, SEEK_SET);
                    break;
                }
            }
            /* Variable name tag */
            err = Read(buf, 4, 2, (FILE *)mat->fp, NULL);
            if ( err ) {
                Mat_VarFree(matvar);
                matvar = NULL;
                (void)fseeko((FILE *)mat->fp, fpos, SEEK_SET);
                break;
            }
            if ( mat->byteswap )
                (void)Mat_uint32Swap(buf);
            /* Name of variable */
            if ( buf[0] == MAT_T_INT8 ) { /* Name not in tag */
                mat_uint32_t len, len_pad;
                if ( mat->byteswap )
                    len = Mat_uint32Swap(buf + 1);
                else
                    len = buf[1];
                if ( len % 8 == 0 )
                    len_pad = len;
                else if ( len < UINT32_MAX - 8 + (len % 8) )
                    len_pad = len + 8 - (len % 8);
                else {
                    Mat_VarFree(matvar);
                    matvar = NULL;
                    (void)fseeko((FILE *)mat->fp, fpos, SEEK_SET);
                    break;
                }
                matvar->name = (char *)malloc(len_pad + 1);
                if ( NULL != matvar->name ) {
                    err = Read(matvar->name, 1, len_pad, (FILE *)mat->fp, NULL);
                    if ( MATIO_E_NO_ERROR == err ) {
                        matvar->name[len] = '\0';
                    } else {
                        Mat_VarFree(matvar);
                        matvar = NULL;
                        (void)fseeko((FILE *)mat->fp, fpos, SEEK_SET);
                        break;
                    }
                }
            } else {
                mat_uint32_t len = (buf[0] & 0xffff0000) >> 16;
                if ( ((buf[0] & 0x0000ffff) == MAT_T_INT8) && len > 0 && len <= 4 ) {
                    /* Name packed in tag */
                    matvar->name = (char *)malloc(len + 1);
                    if ( NULL != matvar->name ) {
                        memcpy(matvar->name, buf + 1, len);
                        matvar->name[len] = '\0';
                    }
                }
            }
            if ( matvar->class_type == MAT_C_STRUCT )
                (void)ReadNextStructField(mat, matvar);
            else if ( matvar->class_type == MAT_C_CELL )
                (void)ReadNextCell(mat, matvar);
            else if ( matvar->class_type == MAT_C_FUNCTION )
                (void)ReadNextFunctionHandle(mat, matvar);
            matvar->internal->datapos = ftello((FILE *)mat->fp);
            if ( matvar->internal->datapos == -1L ) {
                Mat_Critical("Couldn't determine file position");
            }
            (void)fseeko((FILE *)mat->fp, nBytes + 8 + fpos, SEEK_SET);
            break;
        }
        default:
            Mat_Critical("%u is not valid (MAT_T_MATRIX or MAT_T_COMPRESSED)", data_type);
            return NULL;
    }

    return matvar;
}
