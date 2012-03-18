/** @file mat5.c
 * Matlab MAT version 5 file functions
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

/* FIXME: Implement Unicode support */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
#include "matio_private.h"
#include "mat5.h"

#define TYPE_FROM_TAG(a)          (enum matio_types)((a) & 0x000000ff)
#define CLASS_FROM_ARRAY_FLAGS(a) (enum matio_classes)((a) & 0x000000ff)
#define CLASS_TYPE_MASK           0x000000ff

static mat_complex_split_t null_complex_data = {NULL,NULL};

/*
 * -------------------------------------------------------------
 *   Private Functions
 * -------------------------------------------------------------
 */

/** @brief determines the number of bytes needed to store the given struct field
 *
 * @ingroup mat_internal
 * @param matvar field of a structure
 * @return the number of bytes needed to store the struct field
 */
static size_t
GetStructFieldBufSize(matvar_t *matvar)
{
    size_t nBytes = 0, data_bytes = 0;
    size_t tag_size = 8, array_flags_size = 8;
    int    nmemb = 1, i;

    if ( matvar == NULL )
        return nBytes;

    /* Add the Array Flags tag and space to the number of bytes */
    nBytes += tag_size + array_flags_size;

    /* In a struct field, the name is just a tag with 0 bytes */
    nBytes += tag_size;

    /* Add rank and dimensions, padded to an 8 byte block */
    for ( i = 0; i < matvar->rank; i++ )
        nmemb *= matvar->dims[i];
    if ( matvar->rank % 2 )
        nBytes += tag_size + matvar->rank*4 + 4;
    else
        nBytes += tag_size + matvar->rank*4;

    switch ( matvar->class_type ) {
    case MAT_C_STRUCT:
    {
        matvar_t **fields = matvar->data;
        int i, nfields = 0;
        size_t maxlen = 0;

        nfields = matvar->internal->num_fields;
        for ( i = 0; i < nfields; i++ ) {
            char *fieldname = matvar->internal->fieldnames[i];
            if ( NULL != fieldname && strlen(fieldname) > maxlen )
                maxlen = strlen(fieldname);
        }
        maxlen++;
        while ( nfields*maxlen % 8 != 0 )
            maxlen++;

        nBytes += tag_size + tag_size + maxlen*nfields;

        /* FIXME: Add bytes for the fieldnames */
        if ( NULL != fields && nfields > 0 ) {
            for ( i = 0; i < nfields*nmemb; i++ )
                nBytes += tag_size + GetStructFieldBufSize(fields[i]);
        }
        break;
    }
    case MAT_C_CELL:
    {
        matvar_t **cells = matvar->data;
        int i, ncells;

        if ( matvar->nbytes == 0 || matvar->data_size == 0 )
            break;

        ncells = matvar->nbytes / matvar->data_size;

        if ( NULL != cells && ncells > 0 ) {
            for ( i = 0; i < ncells; i++ )
                nBytes += tag_size + GetCellArrayFieldBufSize(cells[i]);
        }
        break;
    }
    case MAT_C_SPARSE:
    {
        mat_sparse_t *sparse = matvar->data;

        data_bytes = sparse->nir*sizeof(mat_int32_t);
        if ( data_bytes % 8 )
            data_bytes += (8 - (data_bytes % 8));
        nBytes += tag_size + data_bytes;

        data_bytes = sparse->njc*sizeof(mat_int32_t);
        if ( data_bytes % 8 )
            data_bytes += (8 - (data_bytes % 8));
        nBytes += tag_size + data_bytes;

        data_bytes = sparse->ndata*Mat_SizeOf(matvar->data_type);
        if ( data_bytes % 8 )
            data_bytes += (8 - (data_bytes % 8));
        nBytes += tag_size + data_bytes;

        if ( matvar->isComplex )
            nBytes += tag_size + data_bytes;

        break;
    }
    default:
        data_bytes = nmemb*Mat_SizeOf(matvar->data_type);
        if ( data_bytes % 8 )
            data_bytes += (8 - (data_bytes % 8));
        nBytes += tag_size + data_bytes;
        if ( matvar->isComplex )
            nBytes += tag_size + data_bytes;
    } /* switch ( matvar->class_type ) */

    return nBytes;
}

/** @brief determines the number of bytes needed to store the cell array element
 *
 * @ingroup mat_internal
 * @param matvar MAT variable
 * @return the number of bytes needed to store the variable
 */
static size_t
GetCellArrayFieldBufSize(matvar_t *matvar)
{
    size_t nBytes = 0, len, data_bytes;
    size_t tag_size = 8, array_flags_size = 8;
    int    nmemb = 1, i;

    if ( matvar == NULL )
        return nBytes;

    /* Add the Array Flags tag and space to the number of bytes */
    nBytes += tag_size + array_flags_size;

    /* In an element of a cell array, the name is just a tag with 0 bytes */
    nBytes += tag_size;

    /* Add rank and dimensions, padded to an 8 byte block */
    for ( i = 0, len = 0; i < matvar->rank; i++ )
        nmemb *= matvar->dims[i];
    if ( matvar->rank % 2 )
        nBytes += tag_size + matvar->rank*4 + 4;
    else
        nBytes += tag_size + matvar->rank*4;

    switch ( matvar->class_type ) {
    case MAT_C_STRUCT:
    {
        matvar_t **fields = matvar->data;
        int i, nfields = 0;
        size_t maxlen = 0;

        nfields = matvar->internal->num_fields;
        for ( i = 0; i < nfields; i++ ) {
            char *fieldname = matvar->internal->fieldnames[i];
            if ( NULL != fieldname && strlen(fieldname) > maxlen )
                maxlen = strlen(fieldname);
        }
        maxlen++;
        while ( nfields*maxlen % 8 != 0 )
            maxlen++;

        nBytes += tag_size + tag_size + maxlen*nfields;

        if ( NULL != fields && nfields > 0 ) {
            for ( i = 0; i < nfields*nmemb; i++ )
                nBytes += tag_size + GetStructFieldBufSize(fields[i]);
        }
        break;
    }
    case MAT_C_CELL:
    {
        matvar_t **cells = matvar->data;
        int i, ncells;

        if ( matvar->nbytes == 0 || matvar->data_size == 0 )
            break;

        ncells = matvar->nbytes / matvar->data_size;

        if ( NULL != cells && ncells > 0 ) {
            for ( i = 0; i < ncells; i++ )
                nBytes += tag_size + GetCellArrayFieldBufSize(cells[i]);
        }
        break;
    }
    case MAT_C_SPARSE:
    {
        mat_sparse_t *sparse = matvar->data;

        data_bytes = sparse->nir*sizeof(mat_int32_t);
        if ( data_bytes % 8 )
            data_bytes += (8 - (data_bytes % 8));
        nBytes += tag_size + data_bytes;

        data_bytes = sparse->njc*sizeof(mat_int32_t);
        if ( data_bytes % 8 )
            data_bytes += (8 - (data_bytes % 8));
        nBytes += tag_size + data_bytes;

        data_bytes = sparse->ndata*Mat_SizeOf(matvar->data_type);
        if ( data_bytes % 8 )
            data_bytes += (8 - (data_bytes % 8));
        nBytes += tag_size + data_bytes;

        if ( matvar->isComplex )
            nBytes += tag_size + data_bytes;
        break;
    }
    default:
        data_bytes = nmemb*Mat_SizeOf(matvar->data_type);
        if ( data_bytes % 8 )
            data_bytes += (8 - (data_bytes % 8));
        nBytes += tag_size + data_bytes;
        if ( matvar->isComplex )
            nBytes += tag_size + data_bytes;
    } /* switch ( matvar->class_type ) */

    return nBytes;
}

/** @brief determines the number of bytes needed to store the given variable
 *
 * @ingroup mat_internal
 * @param matvar MAT variable
 * @return the number of bytes needed to store the variable
 */
static size_t
GetEmptyMatrixMaxBufSize(const char *name,int rank)
{
    size_t nBytes = 0, len;
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
        if ( len % 8 )
            len = len + (8 - len % 8);
        nBytes += tag_size + len;
    }

    /* Add rank and dimensions, padded to an 8 byte block */
    if ( rank % 2 )
        nBytes += tag_size + rank*4 + 4;
    else
        nBytes += tag_size + rank*4;

    /* Data tag */
    nBytes += tag_size;

    return nBytes;
}

/** @brief determines the number of bytes needed to store the given variable
 *
 * @ingroup mat_internal
 * @param matvar MAT variable
 * @return the number of bytes needed to store the variable
 */
static size_t
GetMatrixMaxBufSize(matvar_t *matvar)
{
    size_t nBytes = 0, len, data_bytes;
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
        len=4;

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

    switch ( matvar->class_type ) {
    case MAT_C_STRUCT:
    {
        matvar_t **fields = matvar->data;
        int i, nfields = 0;
        size_t maxlen = 0;

        nfields = matvar->internal->num_fields;
        for ( i = 0; i < nfields; i++ ) {
            char *fieldname = matvar->internal->fieldnames[i];
            if ( NULL != fieldname && strlen(fieldname) > maxlen )
                maxlen = strlen(fieldname);
        }
        maxlen++;
        while ( nfields*maxlen % 8 != 0 )
            maxlen++;

        nBytes += tag_size + tag_size + maxlen*nfields;

        /* FIXME: Add bytes for the fieldnames */
        if ( NULL != fields && nfields > 0 ) {
            for ( i = 0; i < nfields*nmemb; i++ )
                nBytes += tag_size + GetStructFieldBufSize(fields[i]);
        }
        break;
    }
    case MAT_C_CELL:
    {
        matvar_t **cells = matvar->data;
        int i, ncells;

        if ( matvar->nbytes == 0 || matvar->data_size == 0 )
            break;

        ncells = matvar->nbytes / matvar->data_size;

        if ( NULL != cells && ncells > 0 ) {
            for ( i = 0; i < ncells; i++ )
                nBytes += tag_size + GetCellArrayFieldBufSize(cells[i]);
        }
        break;
    }
    case MAT_C_SPARSE:
    {
        mat_sparse_t *sparse = matvar->data;

        data_bytes = sparse->nir*sizeof(mat_int32_t);
        if ( data_bytes % 8 )
            data_bytes += (8 - (data_bytes % 8));
        nBytes += tag_size + data_bytes;

        data_bytes = sparse->njc*sizeof(mat_int32_t);
        if ( data_bytes % 8 )
            data_bytes += (8 - (data_bytes % 8));
        nBytes += tag_size + data_bytes;

        data_bytes = sparse->ndata*Mat_SizeOf(matvar->data_type);
        if ( data_bytes % 8 )
            data_bytes += (8 - (data_bytes % 8));
        nBytes += tag_size + data_bytes;

        if ( matvar->isComplex )
            nBytes += tag_size + data_bytes;

        break;
    }
    default:
        data_bytes = nmemb*Mat_SizeOf(matvar->data_type);
        if ( data_bytes % 8 )
            data_bytes += (8 - (data_bytes % 8));
        nBytes += tag_size + data_bytes;
        if ( matvar->isComplex )
            nBytes += tag_size + data_bytes;
    } /* switch ( matvar->class_type ) */

    return nBytes;
}

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
Mat_Create5(const char *matname,const char *hdr_str)
{
    FILE *fp = NULL;
    mat_int16_t endian = 0, version;
    mat_t *mat = NULL;
    size_t err;
    time_t t;

    fp = fopen(matname,"wb");
    if ( !fp )
        return NULL;

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
    mat->fp = fp;
    mat->filename = strdup_printf("%s",matname);
    mat->mode     = MAT_ACC_RDWR;
    mat->byteswap = 0;
    mat->header   = calloc(1,128);
    mat->subsys_offset = calloc(1,16);
    memset(mat->header,' ',128);
    if ( hdr_str == NULL ) {
        err = mat_snprintf(mat->header,116,"MATLAB 5.0 MAT-file, Platform: %s, "
                "Created By: libmatio v%d.%d.%d on %s", MATIO_PLATFORM,
                MATIO_MAJOR_VERSION, MATIO_MINOR_VERSION, MATIO_RELEASE_LEVEL,
                ctime(&t));
        mat->header[115] = '\0';    /* Just to make sure it's NULL terminated */    } else {
        err = mat_snprintf(mat->header,116,"%s",hdr_str);
    }
    mat->header[err] = ' ';
    mat_snprintf(mat->subsys_offset,15,"            ");
    mat->version = (int)0x0100;
    endian = 0x4d49;

    version = 0x0100;

    err = fwrite(mat->header,1,116,mat->fp);
    err = fwrite(mat->subsys_offset,1,8,mat->fp);
    err = fwrite(&version,2,1,mat->fp);
    err = fwrite(&endian,2,1,mat->fp);

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
int
WriteCharData(mat_t *mat, void *data, int N,enum matio_types data_type)
{
    int nBytes = 0, bytesread = 0, i;
    mat_int8_t pad1 = 0;

    switch ( data_type ) {
        case MAT_T_UINT16:
        {
            nBytes = N*2;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            if ( NULL != data && N > 0 )
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
            if ( NULL == ptr )
                break;
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
            if ( NULL != ptr && nBytes > 0 )
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

#if defined(HAVE_ZLIB)
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
WriteCompressedCharData(mat_t *mat,z_stream *z,void *data,int N,
    enum matio_types data_type)
{
    int data_size, data_tag[2], err, byteswritten = 0;
    int buf_size = 1024, i;
    mat_uint8_t   buf[1024], pad[8] = {0,};

    if ((mat == NULL) || (mat->fp == NULL))
        return 0;

    switch ( data_type ) {
        case MAT_T_UINT16:
        {
            data_size = 2;
            data_tag[0]  = MAT_T_UINT16;
            data_tag[1]  = N*data_size;
            z->next_in   = ZLIB_BYTE_PTR(data_tag);
            z->avail_in  = 8;
            z->next_out  = buf;
            z->avail_out = buf_size;
            err = deflate(z,Z_NO_FLUSH);
            byteswritten += fwrite(buf,1,buf_size-z->avail_out,mat->fp);

            /* exit early if this is a empty data */
            if ( NULL == data || N < 1 )
                break;

            z->next_in   = data;
            z->avail_in  = data_size*N;
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
            break;
        }
        case MAT_T_INT8:
        case MAT_T_UINT8:
        {
            mat_uint8_t *ptr;
            mat_uint16_t c;

            /* Matlab can't read MAT_C_CHAR as uint8, needs uint16 */
            data_size    = 2;
            data_tag[0]  = MAT_T_UINT16;
            data_tag[1]  = N*data_size;
            z->next_in   = ZLIB_BYTE_PTR(data_tag);
            z->avail_in  = 8;
            z->next_out  = buf;
            z->avail_out = buf_size;
            err = deflate(z,Z_NO_FLUSH);
            byteswritten += fwrite(buf,1,buf_size-z->avail_out,mat->fp);

            /* exit early if this is a empty data */
            if ( NULL == data || N < 1 )
                break;

            z->next_in   = data;
            z->avail_in  = data_size*N;
            ptr = data;
            for ( i = 0; i < N; i++ ) {
                c = (mat_uint16_t)*(char *)ptr;
                z->next_in   = ZLIB_BYTE_PTR(&c);
                z->avail_in  = 2;
                z->next_out  = buf;
                z->avail_out = buf_size;
                err = deflate(z,Z_NO_FLUSH);
                byteswritten += fwrite(buf,1,buf_size-z->avail_out,mat->fp);
                ptr++;
            }
            /* Add/Compress padding to pad to 8-byte boundary */
            if ( N*data_size % 8 ) {
                z->next_in   = pad;
                z->avail_in  = 8 - (N*data_size % 8);
                z->next_out  = buf;
                z->avail_out = buf_size;
                err = deflate(z,Z_NO_FLUSH);
                byteswritten += fwrite(buf,1,buf_size-z->avail_out,mat->fp);
            }
            break;
        }
        case MAT_T_UTF8:
        {
            data_size = 1;
            data_tag[0]  = MAT_T_UTF8;
            data_tag[1]  = N*data_size;
            z->next_in   = ZLIB_BYTE_PTR(data_tag);
            z->avail_in  = 8;
            z->next_out  = buf;
            z->avail_out = buf_size;
            err = deflate(z,Z_NO_FLUSH);
            byteswritten += fwrite(buf,1,buf_size-z->avail_out,mat->fp);

            /* exit early if this is a empty data */
            if ( NULL == data || N < 1 )
                break;

            z->next_in   = data;
            z->avail_in  = data_size*N;
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
            break;
        }
    }
    return byteswritten;
}
#endif

/** @if mat_devman
 * @brief Writes empty characters to the MAT file
 *
 * This function uses the knowledge that the data is part of a character class
 * to avoid some pitfalls with Matlab listed below.
 *   @li Matlab character data cannot be unsigned 8-bit integers, it needs at
 *       least unsigned 16-bit integers
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param data character data to write
 * @param N Number of elements to write
 * @param data_type character data type (enum matio_types)
 * @return number of bytes written
 * @endif
 */
static int
WriteEmptyCharData(mat_t *mat, int N, enum matio_types data_type)
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

/** @if mat_devman
 * @brief Writes the data tags and empty data to the file
 *
 * Writes the data tags and empty data to the file to save space for the
 * variable when the actual data is written
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param N number of elements to write
 * @param data_type data type to write
 * @return Number of bytes written
 * @endif
 */
static int
WriteEmptyData(mat_t *mat,int N,enum matio_types data_type)
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
#ifdef HAVE_MAT_INT64_T
        case MAT_T_INT64:
        {
            mat_int64_t i64 = 0;

            data_size = sizeof(mat_int64_t);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&i64,data_size,1,mat->fp);
            break;
        }
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_T_UINT64:
        {
            mat_uint64_t ui64 = 0;

            data_size = sizeof(mat_uint64_t);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&ui64,data_size,1,mat->fp);
            break;
        }
#endif
        default:
            nBytes = 0;
    }
    return nBytes;
}

#if defined(HAVE_ZLIB)
static int
WriteCompressedEmptyData(mat_t *mat,z_stream *z,int N,
    enum matio_types data_type)
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
            z->next_out  = ZLIB_BYTE_PTR(comp_buf);
            z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
            z->avail_out = 32*sizeof(*comp_buf);
            z->avail_in  = 8;
            err = deflate(z,Z_NO_FLUSH);
            byteswritten += fwrite(comp_buf,1,32*sizeof(*comp_buf)-z->avail_out,mat->fp);
            for ( i = 0; i < N; i++ ) {
                z->next_out  = ZLIB_BYTE_PTR(comp_buf);
                z->next_in   = ZLIB_BYTE_PTR(data_uncomp_buf);
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
#ifdef HAVE_MAT_INT64_T
        case MAT_T_INT64:
        {
            mat_int64_t i64 = 0;

            data_size = sizeof(mat_int64_t);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&i64,data_size,1,mat->fp);
            break;
        }
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_T_UINT64:
        {
            mat_uint64_t ui64 = 0;

            data_size = sizeof(mat_uint64_t);
            nBytes = N*data_size;
            fwrite(&data_type,4,1,mat->fp);
            fwrite(&nBytes,4,1,mat->fp);
            for ( i = 0; i < N; i++ )
                fwrite(&ui64,data_size,1,mat->fp);
            break;
        }
#endif
        default:
            nBytes = 0;
    }
    return byteswritten;
}
#endif

/** @if mat_devman
 * @param Writes a 2-D slab of data to the MAT file
 *
 * @ingroup mat_internal
 * @fixme should return the number of bytes written, but currently returns 0
 * @param mat MAT file pointer
 * @param data pointer to the slab of data
 * @param data_type data type of the data (enum matio_types)
 * @param dims dimensions of the dataset
 * @param start index to start writing the data in each dimension
 * @param stride write data every @c stride elements
 * @param edge number of elements to write in each dimension
 * @return number of byteswritten
 * @endif
 */
int
WriteDataSlab2(mat_t *mat,void *data,enum matio_types data_type,size_t *dims,
    int *start,int *stride,int *edge)
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
#ifdef HAVE_MAT_INT64_T
        case MAT_T_INT64:
        {
            mat_int64_t *ptr;

            data_size = sizeof(mat_int64_t);
            ptr = (mat_int64_t *)data;
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
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_T_UINT64:
        {
            mat_uint64_t *ptr;

            data_size = sizeof(mat_uint64_t);
            ptr = (mat_uint64_t *)data;
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
#endif
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

/** @if mat_devman
 * @param Writes a 2-D slab of character data to the MAT file
 *
 * This function uses the knowledge that the data is part of a character class
 * to avoid some pitfalls with Matlab listed below.
 *   @li Matlab character data cannot be unsigned 8-bit integers, it needs at
 *       least unsigned 16-bit integers
 * @ingroup mat_internal
 * @fixme should return the number of bytes written, but currently returns 0
 * @param mat MAT file pointer
 * @param data pointer to the slab of data
 * @param data_type data type of the data (enum matio_types)
 * @param dims dimensions of the dataset
 * @param start index to start writing the data in each dimension
 * @param stride write data every @c stride elements
 * @param edge number of elements to write in each dimension
 * @return number of byteswritten
 * @endif
 */
int
WriteCharDataSlab2(mat_t *mat,void *data,enum matio_types data_type,
    size_t *dims,int *start,int *stride,int *edge)
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

/** @brief Writes the data buffer to the file
 *
 * @param mat MAT file pointer
 * @param data pointer to the data to write
 * @param N number of elements to write
 * @param data_type data type of the data
 * @return number of bytes written
 */
int
WriteData(mat_t *mat,void *data,int N,enum matio_types data_type)
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
static size_t
WriteCompressedData(mat_t *mat,z_stream *z,void *data,int N,
    enum matio_types data_type)
{
    int nBytes = 0, data_size, data_tag[2], err, byteswritten = 0;
    int buf_size = 1024;
    mat_uint8_t   buf[1024], pad[8] = {0,};

    if ((mat == NULL) || (mat->fp == NULL))
        return 0;

    data_size = Mat_SizeOf(data_type);

    data_tag[0]  = data_type;
    data_tag[1]  = data_size*N;
    z->next_in   = ZLIB_BYTE_PTR(data_tag);
    z->avail_in  = 8;
    z->next_out  = buf;
    z->avail_out = buf_size;
    err = deflate(z,Z_NO_FLUSH);
    byteswritten += fwrite(buf,1,buf_size-z->avail_out,mat->fp);

    /* exit early if this is a empty data */
    if ( NULL == data || N < 1 )
        return byteswritten;

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

/** @brief Reads the next cell of the cell array in @c matvar
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar MAT variable pointer
 * @return Number of bytes read
 */
static int
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

            cells[i]->internal->fpos = ftell(mat->fp)-matvar->internal->z->avail_in;

            /* Read variable tag for cell */
            uncomp_buf[0] = 0;
            uncomp_buf[1] = 0;
            bytesread += InflateVarTag(mat,matvar,uncomp_buf);
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(uncomp_buf);
                (void)Mat_uint32Swap(uncomp_buf+1);
            }
            nbytes = uncomp_buf[1];
            if ( !nbytes ) {
                /* empty cell */
                continue;
            } else if ( uncomp_buf[0] != MAT_T_MATRIX ) {
                Mat_Critical("cells[%d], Uncompressed type not MAT_T_MATRIX",i);
                Mat_VarFree(cells[i]);
                cells[i] = NULL;
                break;
            }
            cells[i]->compression = 1;
            bytesread += InflateArrayFlags(mat,matvar,uncomp_buf);
            nbytes -= 16;
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(uncomp_buf);
                (void)Mat_uint32Swap(uncomp_buf+1);
                (void)Mat_uint32Swap(uncomp_buf+2);
                (void)Mat_uint32Swap(uncomp_buf+3);
            }
            /* Array Flags */
            if ( uncomp_buf[0] == MAT_T_UINT32 ) {
               array_flags = uncomp_buf[2];
               cells[i]->class_type  = CLASS_FROM_ARRAY_FLAGS(array_flags);
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
                bytesread+=InflateSkip(mat,matvar->internal->z,nbytes);
            }
            bytesread += InflateDimensions(mat,matvar,uncomp_buf);
            nbytes -= 8;
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(uncomp_buf);
                (void)Mat_uint32Swap(uncomp_buf+1);
            }
            /* Rank and Dimension */
            if ( uncomp_buf[0] == MAT_T_INT32 ) {
                int j = 0;

                cells[i]->rank = uncomp_buf[1];
                nbytes -= cells[i]->rank;
                cells[i]->rank /= 4;
                cells[i]->dims = malloc(cells[i]->rank*sizeof(*cells[i]->dims));
                if ( mat->byteswap ) {
                    for ( j = 0; j < cells[i]->rank; j++ )
                        cells[i]->dims[j] = Mat_uint32Swap(uncomp_buf+2+j);
                } else {
                    for ( j = 0; j < cells[i]->rank; j++ )
                        cells[i]->dims[j] = uncomp_buf[2+j];
                }
                if ( cells[i]->rank % 2 != 0 )
                    nbytes -= 4;
            }
            bytesread += InflateVarNameTag(mat,matvar,uncomp_buf);
            nbytes -= 8;
            cells[i]->internal->z = calloc(1,sizeof(z_stream));
            err = inflateCopy(cells[i]->internal->z,matvar->internal->z);
            if ( err != Z_OK )
                Mat_Critical("inflateCopy returned error %d",err);
            cells[i]->internal->datapos = ftell(mat->fp)-matvar->internal->z->avail_in;
            if ( cells[i]->class_type == MAT_C_STRUCT )
                bytesread+=ReadNextStructField(mat,cells[i]);
            else if ( cells[i]->class_type == MAT_C_CELL )
                bytesread+=ReadNextCell(mat,cells[i]);
            fseek(mat->fp,cells[i]->internal->datapos,SEEK_SET);
            bytesread+=InflateSkip(mat,matvar->internal->z,nbytes);
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
            int cell_bytes_read;
            cells[i] = Mat_VarCalloc();
            if ( !cells[i] ) {
                Mat_Critical("Couldn't allocate memory for cell %d", i);
                continue;
            }

            cells[i]->internal->fpos = ftell(mat->fp);

            /* Read variable tag for cell */
            cell_bytes_read = fread(buf,4,2,mat->fp);

            /* Empty cells at the end of a file may cause an EOF */
            if ( !cell_bytes_read )
                continue;
            bytesread += cell_bytes_read;
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(buf);
                (void)Mat_uint32Swap(buf+1);
            }
            nBytes = buf[1];
            if ( !nBytes ) {
                /* empty cell */
                continue;
            } else if ( buf[0] != MAT_T_MATRIX ) {
                Mat_Critical("cells[%d] not MAT_T_MATRIX, fpos = %ld",i,ftell(mat->fp));
                Mat_VarFree(cells[i]);
                cells[i] = NULL;
                break;
            }
            cells[i]->compression = 0;
#if defined(HAVE_ZLIB)
            cells[i]->internal->z = NULL;
#endif

            /* Read Array Flags and The Dimensions Tag */
            bytesread  += fread(buf,4,6,mat->fp);
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(buf);
                (void)Mat_uint32Swap(buf+1);
                (void)Mat_uint32Swap(buf+2);
                (void)Mat_uint32Swap(buf+3);
                (void)Mat_uint32Swap(buf+4);
                (void)Mat_uint32Swap(buf+5);
            }
            nBytes-=24;
            /* Array Flags */
            if ( buf[0] == MAT_T_UINT32 ) {
               array_flags = buf[2];
               cells[i]->class_type  = CLASS_FROM_ARRAY_FLAGS(array_flags);
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
                cells[i]->dims = malloc(cells[i]->rank*sizeof(*cells[i]->dims));

                /* Assumes rank <= 16 */
                if ( cells[i]->rank % 2 != 0 ) {
                    bytesread+=fread(buf,4,cells[i]->rank+1,mat->fp);
                    nBytes-=4;
                } else
                    bytesread+=fread(buf,4,cells[i]->rank,mat->fp);

                if ( mat->byteswap ) {
                    for ( j = 0; j < cells[i]->rank; j++ )
                        cells[i]->dims[j] = Mat_uint32Swap(buf+j);
                } else {
                    for ( j = 0; j < cells[i]->rank; j++ )
                        cells[i]->dims[j] = buf[j];
                }
            }
            /* Variable Name Tag */
            bytesread+=fread(buf,1,8,mat->fp);
            nBytes-=8;
            cells[i]->internal->datapos = ftell(mat->fp);
            if ( cells[i]->class_type == MAT_C_STRUCT )
                bytesread+=ReadNextStructField(mat,cells[i]);
            if ( cells[i]->class_type == MAT_C_CELL )
                bytesread+=ReadNextCell(mat,cells[i]);
            fseek(mat->fp,cells[i]->internal->datapos+nBytes,SEEK_SET);
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
static int
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
            (void)Mat_uint32Swap(uncomp_buf);
            (void)Mat_uint32Swap(uncomp_buf+1);
        }
        if ( (uncomp_buf[0] & 0x0000ffff) == MAT_T_INT32 ) {
            fieldname_size = uncomp_buf[1];
        } else {
            Mat_Warning("Error getting fieldname size");
            return bytesread;
        }

        bytesread += InflateFieldNamesTag(mat,matvar,uncomp_buf);
        if ( mat->byteswap ) {
            (void)Mat_uint32Swap(uncomp_buf);
            (void)Mat_uint32Swap(uncomp_buf+1);
        }
        nfields = uncomp_buf[1];
        nfields = nfields / fieldname_size;
        matvar->data_size = sizeof(matvar_t *);

        if ( nfields*fieldname_size % 8 != 0 )
            i = 8-(nfields*fieldname_size % 8);
        else
            i = 0;
        ptr = malloc(nfields*fieldname_size+i);
        bytesread += InflateFieldNames(mat,matvar,ptr,nfields,fieldname_size,i);
        matvar->internal->num_fields = nfields;
        matvar->internal->fieldnames =
            calloc(nfields,sizeof(*matvar->internal->fieldnames));
        for ( i = 0; i < nfields; i++ ) {
            matvar->internal->fieldnames[i] = malloc(fieldname_size);
            memcpy(matvar->internal->fieldnames[i],ptr+i*fieldname_size,
                   fieldname_size);
            matvar->internal->fieldnames[i][fieldname_size-1] = '\0';
        }
        free(ptr);

        matvar->nbytes = nmemb*nfields*matvar->data_size;
        if ( !matvar->nbytes )
            return bytesread;

        matvar->data = malloc(matvar->nbytes);
        if ( !matvar->data )
            return bytesread;

        fields = matvar->data;
        for ( i = 0; i < nmemb; i++ ) {
            for ( j = 0; j < nfields; j++ ) {
                fields[i*nfields+j] = Mat_VarCalloc();
                fields[i*nfields+j]->name = strdup(matvar->internal->fieldnames[j]);
            }
        }

        for ( i = 0; i < nmemb*nfields; i++ ) {
            fields[i]->internal->fpos = ftell(mat->fp)-matvar->internal->z->avail_in;
            /* Read variable tag for struct field */
            bytesread += InflateVarTag(mat,matvar,uncomp_buf);
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(uncomp_buf);
                (void)Mat_uint32Swap(uncomp_buf+1);
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
            fields[i]->compression = MAT_COMPRESSION_ZLIB;
            bytesread += InflateArrayFlags(mat,matvar,uncomp_buf);
            nbytes -= 16;
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(uncomp_buf);
                (void)Mat_uint32Swap(uncomp_buf+1);
                (void)Mat_uint32Swap(uncomp_buf+2);
                (void)Mat_uint32Swap(uncomp_buf+3);
            }
            /* Array Flags */
            if ( uncomp_buf[0] == MAT_T_UINT32 ) {
               array_flags = uncomp_buf[2];
               fields[i]->class_type  = CLASS_FROM_ARRAY_FLAGS(array_flags);
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
                bytesread+=InflateSkip(mat,matvar->internal->z,nbytes);
            }
            bytesread += InflateDimensions(mat,matvar,uncomp_buf);
            nbytes -= 8;
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(uncomp_buf);
                (void)Mat_uint32Swap(uncomp_buf+1);
            }
            /* Rank and Dimension */
            if ( uncomp_buf[0] == MAT_T_INT32 ) {
                int j = 0;

                fields[i]->rank = uncomp_buf[1];
                nbytes -= fields[i]->rank;
                fields[i]->rank /= 4;
                fields[i]->dims = malloc(fields[i]->rank*
                                         sizeof(*fields[i]->dims));
                if ( mat->byteswap ) {
                    for ( j = 0; j < fields[i]->rank; j++ )
                        fields[i]->dims[j] = Mat_uint32Swap(uncomp_buf+2+j);
                } else {
                    for ( j = 0; j < fields[i]->rank; j++ )
                        fields[i]->dims[j] = uncomp_buf[2+j];
                }
                if ( fields[i]->rank % 2 != 0 )
                    nbytes -= 4;
            }
            bytesread += InflateVarNameTag(mat,matvar,uncomp_buf);
            nbytes -= 8;
            fields[i]->internal->z = calloc(1,sizeof(z_stream));
            err = inflateCopy(fields[i]->internal->z,matvar->internal->z);
            if ( err != Z_OK ) {
                Mat_Critical("inflateCopy returned error %d",err);
            }
            fields[i]->internal->datapos = ftell(mat->fp)-matvar->internal->z->avail_in;
            if ( fields[i]->class_type == MAT_C_STRUCT )
                bytesread+=ReadNextStructField(mat,fields[i]);
            else if ( fields[i]->class_type == MAT_C_CELL )
                bytesread+=ReadNextCell(mat,fields[i]);
            fseek(mat->fp,fields[i]->internal->datapos,SEEK_SET);
            bytesread+=InflateSkip(mat,matvar->internal->z,nbytes);
        }
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
            (void)Mat_uint32Swap(buf);
            (void)Mat_uint32Swap(buf+1);
        }
        if ( (buf[0] & 0x0000ffff) == MAT_T_INT32 ) {
            fieldname_size = buf[1];
        } else {
            Mat_Warning("Error getting fieldname size");
            return bytesread;
        }
        bytesread+=fread(buf,4,2,mat->fp);
        if ( mat->byteswap ) {
            (void)Mat_uint32Swap(buf);
            (void)Mat_uint32Swap(buf+1);
        }
        nfields = buf[1];
        nfields = nfields / fieldname_size;
        matvar->data_size = sizeof(matvar_t *);

        matvar->internal->num_fields = nfields;
        matvar->internal->fieldnames =
            calloc(nfields,sizeof(*matvar->internal->fieldnames));
        for ( i = 0; i < nfields; i++ ) {
            matvar->internal->fieldnames[i] = malloc(fieldname_size);
            bytesread+=fread(matvar->internal->fieldnames[i],1,fieldname_size,mat->fp);
            matvar->internal->fieldnames[i][fieldname_size-1] = '\0';
        }

        if ( (nfields*fieldname_size) % 8 ) {
            fseek(mat->fp,8-((nfields*fieldname_size) % 8),SEEK_CUR);
            bytesread+=8-((nfields*fieldname_size) % 8);
        }

        matvar->nbytes = nmemb*nfields*matvar->data_size;
        if ( !matvar->nbytes )
            return bytesread;

        matvar->data = malloc(matvar->nbytes);
        if ( !matvar->data )
            return bytesread;

        fields = matvar->data;
        for ( i = 0; i < nmemb; i++ ) {
            for ( j = 0; j < nfields; j++ ) {
                fields[i*nfields+j] = Mat_VarCalloc();
                fields[i*nfields+j]->name = strdup(matvar->internal->fieldnames[j]);
            }
        }

        for ( i = 0; i < nmemb*nfields; i++ ) {

            fields[i]->internal->fpos = ftell(mat->fp);

            /* Read variable tag for struct field */
            bytesread += fread(buf,4,2,mat->fp);
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(buf);
                (void)Mat_uint32Swap(buf+1);
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
            fields[i]->internal->z = NULL;
#endif

            /* Read Array Flags and The Dimensions Tag */
            bytesread  += fread(buf,4,6,mat->fp);
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(buf);
                (void)Mat_uint32Swap(buf+1);
                (void)Mat_uint32Swap(buf+2);
                (void)Mat_uint32Swap(buf+3);
                (void)Mat_uint32Swap(buf+4);
                (void)Mat_uint32Swap(buf+5);
            }
            nBytes-=24;
            /* Array Flags */
            if ( buf[0] == MAT_T_UINT32 ) {
               array_flags = buf[2];
               fields[i]->class_type  = CLASS_FROM_ARRAY_FLAGS(array_flags);
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
                fields[i]->dims = malloc(fields[i]->rank*
                                         sizeof(*fields[i]->dims));

                /* Assumes rank <= 16 */
                if ( fields[i]->rank % 2 != 0 ) {
                    bytesread+=fread(buf,4,fields[i]->rank+1,mat->fp);
                    nBytes-=4;
                } else
                    bytesread+=fread(buf,4,fields[i]->rank,mat->fp);

                if ( mat->byteswap ) {
                    for ( j = 0; j < fields[i]->rank; j++ )
                        fields[i]->dims[j] = Mat_uint32Swap(buf+j);
                } else {
                    for ( j = 0; j < fields[i]->rank; j++ )
                        fields[i]->dims[j] = buf[j];
                }
            }
            /* Variable Name Tag */
            bytesread+=fread(buf,1,8,mat->fp);
            nBytes-=8;
            fields[i]->internal->datapos = ftell(mat->fp);
            if ( fields[i]->class_type == MAT_C_STRUCT )
                bytesread+=ReadNextStructField(mat,fields[i]);
            else if ( fields[i]->class_type == MAT_C_CELL )
                bytesread+=ReadNextCell(mat,fields[i]);
            fseek(mat->fp,fields[i]->internal->datapos+nBytes,SEEK_SET);
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
static int
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

/** @brief Writes the header and blank data for a cell array
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @return number of bytes written
 */
static int
WriteCellArrayFieldInfo(mat_t *mat,matvar_t *matvar)
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
    array_flags = matvar->class_type & CLASS_TYPE_MASK;
    if ( matvar->isComplex )
        array_flags |= MAT_F_COMPLEX;
    if ( matvar->isGlobal )
        array_flags |= MAT_F_GLOBAL;
    if ( matvar->isLogical )
        array_flags |= MAT_F_LOGICAL;

    if ( mat->byteswap )
        array_flags = Mat_int32Swap((mat_int32_t*)&array_flags);
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

    matvar->internal->datapos = ftell(mat->fp);
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
            int        ncells;
            matvar_t **cells = (matvar_t **)matvar->data;

            /* Check for an empty cell array */
            if ( matvar->nbytes == 0 || matvar->data_size == 0 ||
                 matvar->data   == NULL )
                break;
            ncells  = matvar->nbytes / matvar->data_size;

            for ( i = 0; i < ncells; i++ )
                WriteCellArrayFieldInfo(mat,cells[i]);
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

/** @brief Writes the header and data for an element of a cell array
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @retval 0 on success
 */
static int
WriteCellArrayField(mat_t *mat,matvar_t *matvar )
{
    mat_uint32_t array_flags = 0x0;
    mat_int16_t  array_name_type = MAT_T_INT8,fieldname_type = MAT_T_INT32,fieldname_data_size=4;
    int      array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int      array_flags_size = 8, pad4 = 0, matrix_type = MAT_T_MATRIX;
    mat_int8_t   pad1 = 0;
    int      nBytes, i, nmemb = 1, nzmax = 0;
    long     start = 0, end = 0;

    if ((matvar == NULL) || (mat == NULL))
        return 1;

#if 0
    nBytes = GetMatrixMaxBufSize(matvar);
#endif

    fwrite(&matrix_type,4,1,mat->fp);
    fwrite(&pad4,4,1,mat->fp);
    start = ftell(mat->fp);

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
        array_flags = Mat_int32Swap((mat_int32_t*)&array_flags);
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
        case MAT_C_INT64:
        case MAT_C_UINT64:
        case MAT_C_INT32:
        case MAT_C_UINT32:
        case MAT_C_INT16:
        case MAT_C_UINT16:
        case MAT_C_INT8:
        case MAT_C_UINT8:
        {
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = matvar->data;

                if ( NULL == matvar->data )
                    complex_data = &null_complex_data;

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
            int        ncells;
            matvar_t **cells = (matvar_t **)matvar->data;

            /* Check for an empty cell array */
            if ( matvar->nbytes == 0 || matvar->data_size == 0 ||
                 matvar->data   == NULL )
                break;
            ncells  = matvar->nbytes / matvar->data_size;

            for ( i = 0; i < ncells; i++ )
                WriteCellArrayField(mat,cells[i]);
            break;
        }
        case MAT_C_STRUCT:
        {
            char **fieldnames, *padzero;
            int    fieldname_size, nfields;
            size_t maxlen = 0;
            matvar_t **fields = (matvar_t **)matvar->data;
            unsigned fieldname;

            if ( nmemb && matvar->data_size )
                nfields = matvar->nbytes / (nmemb*matvar->data_size);
            else if ( matvar->data_size )
                nfields = matvar->nbytes / matvar->data_size;
            else
                break;

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
            free(padzero);
            for ( i = 0; i < nmemb*nfields; i++ )
                WriteStructField(mat,fields[i]);
            break;
        }
        case MAT_C_SPARSE:
        {
            mat_sparse_t *sparse = matvar->data;

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

#if defined(HAVE_ZLIB)
/** @brief Writes the header and data for a field of a compressed cell array
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @return number of bytes written to the MAT file
 */
static size_t
WriteCompressedCellArrayField(mat_t *mat,matvar_t *matvar,z_stream *z)
{
    mat_uint32_t array_flags = 0x0;
    mat_int16_t  array_name_type     = MAT_T_INT8;
    mat_int16_t  fieldname_type      = MAT_T_INT32;
    mat_int16_t  fieldname_data_size = 4;
    int      array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int      array_flags_size = 8, pad4 = 0;
    int      nBytes, i, nmemb = 1, nzmax = 0;
    long     start = 0;

    mat_uint32_t comp_buf[512];
    mat_uint32_t uncomp_buf[512] = {0,};
    int buf_size = 512, err;
    size_t byteswritten = 0;

    if ( NULL == matvar || NULL == mat || NULL == z)
        return 0;

    start = ftell(mat->fp);

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

    uncomp_buf[0] = MAT_T_MATRIX;
    uncomp_buf[1] = (int)GetCellArrayFieldBufSize(matvar);
    z->next_out  = ZLIB_BYTE_PTR(comp_buf);
    z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
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

    z->next_out  = ZLIB_BYTE_PTR(comp_buf);
    z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
    z->avail_out = buf_size*sizeof(*comp_buf);
    z->avail_in  = (6+i)*sizeof(*uncomp_buf);
    err = deflate(z,Z_NO_FLUSH);
    byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-z->avail_out,
        mat->fp);
    /* Name of variable */
    uncomp_buf[0] = array_name_type;
    uncomp_buf[1] = 0;
    z->next_out  = ZLIB_BYTE_PTR(comp_buf);
    z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
    z->avail_out = buf_size*sizeof(*comp_buf);
    z->avail_in  = 8;
    err = deflate(z,Z_NO_FLUSH);
    byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-z->avail_out,
        mat->fp);

    matvar->internal->datapos = ftell(mat->fp);
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
        {
            /* WriteCompressedData makes sure uncomressed data is aligned
             * on an 8-byte boundary */
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = matvar->data;

                if ( NULL == matvar->data )
                    complex_data = &null_complex_data;

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
        case MAT_C_CHAR:
        {
            byteswritten += WriteCompressedCharData(mat,z,matvar->data,
                nmemb,matvar->data_type);
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
                WriteCompressedCellArrayField(mat,cells[i],z);
            break;
        }
        case MAT_C_STRUCT:
        {
            char     **fieldnames;
            unsigned char *padzero;
            int        fieldname_size, nfields;
            size_t     maxlen = 0;
            mat_int32_t array_name_type = MAT_T_INT8;
            matvar_t **fields = (matvar_t **)matvar->data;

            /* Check for a structure with no fields */
            if ( matvar->nbytes == 0 || matvar->data_size == 0 ||
                 matvar->data   == NULL ) {
                fieldname_size = 1;
                uncomp_buf[0] = (fieldname_data_size << 16) |
                                 fieldname_type;
                uncomp_buf[1] = 1;
                uncomp_buf[2] = array_name_type;
                uncomp_buf[3] = 0;
                z->next_out  = ZLIB_BYTE_PTR(comp_buf);
                z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
                z->avail_out = buf_size*sizeof(*comp_buf);
                z->avail_in  = 16;
                err = deflate(z,Z_NO_FLUSH);
                byteswritten += fwrite(comp_buf,1,buf_size*
                    sizeof(*comp_buf)-z->avail_out,mat->fp);
                break;
            }
            if ( nmemb )
                nfields = matvar->nbytes / (nmemb*matvar->data_size);
            else /* matvar->data_size is checked above */
                nfields = matvar->nbytes / matvar->data_size;

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
            uncomp_buf[0] = (fieldname_data_size << 16) | fieldname_type;
            uncomp_buf[1] = fieldname_size;
            uncomp_buf[2] = array_name_type;
            uncomp_buf[3] = nfields*fieldname_size;

            padzero = calloc(fieldname_size,1);
            z->next_out  = ZLIB_BYTE_PTR(comp_buf);
            z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
            z->avail_out = buf_size*sizeof(*comp_buf);
            z->avail_in  = 16;
            err = deflate(z,Z_NO_FLUSH);
            byteswritten += fwrite(comp_buf,1,
                    buf_size*sizeof(*comp_buf)-z->avail_out,mat->fp);
            for ( i = 0; i < nfields; i++ ) {
                memset(padzero,'\0',fieldname_size);
                memcpy(padzero,fieldnames[i],strlen(fieldnames[i]));
                z->next_out  = ZLIB_BYTE_PTR(comp_buf);
                z->next_in   = ZLIB_BYTE_PTR(padzero);
                z->avail_out = buf_size*sizeof(*comp_buf);
                z->avail_in  = fieldname_size;
                err = deflate(z,Z_NO_FLUSH);
                byteswritten += fwrite(comp_buf,1,
                        buf_size*sizeof(*comp_buf)-z->avail_out,mat->fp);
            }
            free(fieldnames);
            free(padzero);
            for ( i = 0; i < nmemb*nfields; i++ )
                byteswritten +=
                    WriteCompressedStructField(mat,fields[i],z);
            break;
        }
        case MAT_C_SPARSE:
        {
            mat_sparse_t *sparse = matvar->data;

            byteswritten += WriteCompressedData(mat,z,sparse->ir,
                sparse->nir,MAT_T_INT32);
            byteswritten += WriteCompressedData(mat,z,sparse->jc,
                sparse->njc,MAT_T_INT32);
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = sparse->data;
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

    if ( mat == NULL )
        return 1;

    if ( NULL == matvar ) {
        size_t dims[2] = {0,0};
        Mat_WriteEmptyVariable5(mat, NULL, 2, dims);
        return 0;
    }

    fwrite(&matrix_type,4,1,mat->fp);
    fwrite(&pad4,4,1,mat->fp);
    start = ftell(mat->fp);

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
        array_flags = Mat_int32Swap((mat_int32_t*)&array_flags);
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
        case MAT_C_INT64:
        case MAT_C_UINT64:
        case MAT_C_INT32:
        case MAT_C_UINT32:
        case MAT_C_INT16:
        case MAT_C_UINT16:
        case MAT_C_INT8:
        case MAT_C_UINT8:
        {
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = matvar->data;

                if ( NULL == matvar->data )
                    complex_data = &null_complex_data;

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
            int        ncells;
            matvar_t **cells = (matvar_t **)matvar->data;

            /* Check for an empty cell array */
            if ( matvar->nbytes == 0 || matvar->data_size == 0 ||
                 matvar->data   == NULL )
                break;
            ncells  = matvar->nbytes / matvar->data_size;

            for ( i = 0; i < ncells; i++ )
                WriteCellArrayField(mat,cells[i]);
            break;
        }
        case MAT_C_STRUCT:
        {
            char  *padzero;
            int    fieldname_size, nfields = 0;
            size_t maxlen = 0;
            matvar_t **fields = (matvar_t **)matvar->data;
            unsigned fieldname;

            /* nmemb*matvar->data_size can be zero when saving a struct that
             * contains an empty struct in one of its fields
             * (e.g. x.y = struct('z', {})). If it's zero, we would divide
             * by zero.
             */
            nfields = matvar->internal->num_fields;

            for ( i = 0; i < nfields; i++ ) {
                size_t len = strlen(matvar->internal->fieldnames[i]);
                if ( len > maxlen )
                    maxlen = len;
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
                size_t len = strlen(matvar->internal->fieldnames[i]);
                fwrite(matvar->internal->fieldnames[i],1,len,mat->fp);
                fwrite(padzero,1,fieldname_size-len,mat->fp);
            }
            free(padzero);
            for ( i = 0; i < nmemb*nfields; i++ )
                WriteStructField(mat,fields[i]);
            break;
        }
        case MAT_C_SPARSE:
        {
            mat_sparse_t *sparse = matvar->data;

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
/** @brief Writes the header and data for a field of a compressed struct array
 *
 * @ingroup mat_internal
 * @fixme Currently does not work for cell arrays or sparse data
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @return number of bytes written to the MAT file
 */
static size_t
WriteCompressedStructField(mat_t *mat,matvar_t *matvar,z_stream *z)
{
    mat_uint32_t array_flags = 0x0;
    mat_int16_t  array_name_type     = MAT_T_INT8;
    mat_int16_t  fieldname_type      = MAT_T_INT32;
    mat_int16_t  fieldname_data_size = 4;
    int      array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int      array_flags_size = 8, pad4 = 0;
    int      nBytes, i, nmemb = 1, nzmax = 0;
    long     start = 0;

    mat_uint32_t comp_buf[512];
    mat_uint32_t uncomp_buf[512] = {0,};
    int buf_size = 512, err;
    size_t byteswritten = 0;

    if ( NULL == mat || NULL == z)
        return 1;

    if ( NULL == matvar ) {
        size_t dims[2] = {0,0};
        byteswritten = Mat_WriteCompressedEmptyVariable5(mat, NULL, 2, dims, z);
        return byteswritten;
    }
    start = ftell(mat->fp);

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

    uncomp_buf[0] = MAT_T_MATRIX;
    uncomp_buf[1] = (int)GetStructFieldBufSize(matvar);
    z->next_out  = ZLIB_BYTE_PTR(comp_buf);
    z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
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

    z->next_out  = ZLIB_BYTE_PTR(comp_buf);
    z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
    z->avail_out = buf_size*sizeof(*comp_buf);
    z->avail_in  = (6+i)*sizeof(*uncomp_buf);
    err = deflate(z,Z_NO_FLUSH);
    byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-z->avail_out,
        mat->fp);
    /* Name of variable */
    uncomp_buf[0] = array_name_type;
    uncomp_buf[1] = 0;
    z->next_out  = ZLIB_BYTE_PTR(comp_buf);
    z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
    z->avail_out = buf_size*sizeof(*comp_buf);
    z->avail_in  = 8;
    err = deflate(z,Z_NO_FLUSH);
    byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-z->avail_out,
        mat->fp);

    matvar->internal->datapos = ftell(mat->fp);
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
        {
            /* WriteCompressedData makes sure uncomressed data is aligned
             * on an 8-byte boundary */
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = matvar->data;

                if ( NULL == matvar->data )
                    complex_data = &null_complex_data;

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
        case MAT_C_CHAR:
        {
            byteswritten += WriteCompressedCharData(mat,z,matvar->data,
                nmemb,matvar->data_type);
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
                WriteCompressedCellArrayField(mat,cells[i],z);
            break;
        }
        case MAT_C_STRUCT:
        {
            unsigned char *padzero;
            int        fieldname_size, nfields;
            size_t     maxlen = 0;
            mat_int32_t array_name_type = MAT_T_INT8;
            matvar_t **fields = (matvar_t **)matvar->data;

            nfields = matvar->internal->num_fields;
            /* Check for a structure with no fields */
            if ( nfields < 1 ) {
                fieldname_size = 1;
                uncomp_buf[0] = (fieldname_data_size << 16) |
                                 fieldname_type;
                uncomp_buf[1] = 1;
                uncomp_buf[2] = array_name_type;
                uncomp_buf[3] = 0;
                z->next_out  = ZLIB_BYTE_PTR(comp_buf);
                z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
                z->avail_out = buf_size*sizeof(*comp_buf);
                z->avail_in  = 16;
                err = deflate(z,Z_NO_FLUSH);
                byteswritten += fwrite(comp_buf,1,buf_size*
                    sizeof(*comp_buf)-z->avail_out,mat->fp);
                break;
            }
            for ( i = 0; i < nfields; i++ ) {
                size_t len = strlen(matvar->internal->fieldnames[i]);
                if ( len > maxlen )
                    maxlen = len;
            }
            maxlen++;
            fieldname_size = maxlen;
            while ( nfields*fieldname_size % 8 != 0 )
                fieldname_size++;
            uncomp_buf[0] = (fieldname_data_size << 16) | fieldname_type;
            uncomp_buf[1] = fieldname_size;
            uncomp_buf[2] = array_name_type;
            uncomp_buf[3] = nfields*fieldname_size;

            padzero = calloc(fieldname_size,1);
            z->next_out  = ZLIB_BYTE_PTR(comp_buf);
            z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
            z->avail_out = buf_size*sizeof(*comp_buf);
            z->avail_in  = 16;
            err = deflate(z,Z_NO_FLUSH);
            byteswritten += fwrite(comp_buf,1,
                    buf_size*sizeof(*comp_buf)-z->avail_out,mat->fp);
            for ( i = 0; i < nfields; i++ ) {
                size_t len = strlen(matvar->internal->fieldnames[i]);
                memset(padzero,'\0',fieldname_size);
                memcpy(padzero,matvar->internal->fieldnames[i],len);
                z->next_out  = ZLIB_BYTE_PTR(comp_buf);
                z->next_in   = ZLIB_BYTE_PTR(padzero);
                z->avail_out = buf_size*sizeof(*comp_buf);
                z->avail_in  = fieldname_size;
                err = deflate(z,Z_NO_FLUSH);
                byteswritten += fwrite(comp_buf,1,
                        buf_size*sizeof(*comp_buf)-z->avail_out,mat->fp);
            }
            free(padzero);
            for ( i = 0; i < nmemb*nfields; i++ )
                byteswritten +=
                    WriteCompressedStructField(mat,fields[i],z);
            break;
        }
        case MAT_C_SPARSE:
        {
            mat_sparse_t *sparse = matvar->data;

            byteswritten += WriteCompressedData(mat,z,sparse->ir,
                sparse->nir,MAT_T_INT32);
            byteswritten += WriteCompressedData(mat,z,sparse->jc,
                sparse->njc,MAT_T_INT32);
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = sparse->data;
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

    return byteswritten;
}
#endif

static size_t
Mat_WriteEmptyVariable5(mat_t *mat,const char *name,int rank,size_t *dims)
{
    mat_uint32_t array_flags = 0x0;
    mat_int32_t  array_name_type = MAT_T_INT8, matrix_type = MAT_T_MATRIX;
    int          array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int          array_flags_size = 8, pad4 = 0, nBytes, i, nmemb = 1;
    mat_int8_t   pad1 = 0;
    size_t       byteswritten = 0;
    long         start = 0, end = 0;

    fwrite(&matrix_type,4,1,mat->fp);
    fwrite(&pad4,4,1,mat->fp);

    start = ftell(mat->fp);
    /* Array Flags */
    array_flags = MAT_C_DOUBLE;

    if ( mat->byteswap )
        array_flags = Mat_int32Swap((mat_int32_t*)&array_flags);
    byteswritten += fwrite(&array_flags_type,4,1,mat->fp);
    byteswritten += fwrite(&array_flags_size,4,1,mat->fp);
    byteswritten += fwrite(&array_flags,4,1,mat->fp);
    byteswritten += fwrite(&pad4,4,1,mat->fp);
    /* Rank and Dimension */
    nBytes = rank * 4;
    byteswritten += fwrite(&dims_array_type,4,1,mat->fp);
    byteswritten += fwrite(&nBytes,4,1,mat->fp);
    for ( i = 0; i < rank; i++ ) {
        mat_int32_t dim;
        dim = dims[i];
        nmemb *= dim;
        byteswritten += fwrite(&dim,4,1,mat->fp);
    }
    if ( rank % 2 != 0 )
        byteswritten += fwrite(&pad4,4,1,mat->fp);

    if ( NULL == name ) {
        /* Name of variable */
        byteswritten += fwrite(&array_name_type,4,1,mat->fp);
        byteswritten += fwrite(&pad4,4,1,mat->fp);
    } else {
        mat_int32_t  array_name_type = MAT_T_INT8;
        mat_int32_t  array_name_len   = strlen(name);
        /* Name of variable */
        if ( array_name_len <= 4 ) {
            mat_int8_t  pad1 = 0;
            array_name_type = (array_name_len << 16) | array_name_type;
            byteswritten += fwrite(&array_name_type,4,1,mat->fp);
            byteswritten += fwrite(name,1,array_name_len,mat->fp);
            for ( i = array_name_len; i < 4; i++ )
                byteswritten += fwrite(&pad1,1,1,mat->fp);
        } else {
            byteswritten += fwrite(&array_name_type,4,1,mat->fp);
            byteswritten += fwrite(&array_name_len,4,1,mat->fp);
            byteswritten += fwrite(name,1,array_name_len,mat->fp);
            if ( array_name_len % 8 )
                for ( i = array_name_len % 8; i < 8; i++ )
                    byteswritten += fwrite(&pad1,1,1,mat->fp);
        }
    }

    nBytes = WriteData(mat,NULL,0,MAT_T_DOUBLE);
    byteswritten += nBytes;
    if ( nBytes % 8 )
        for ( i = nBytes % 8; i < 8; i++ )
            byteswritten += fwrite(&pad1,1,1,mat->fp);

    end = ftell(mat->fp);
    nBytes = (int)(end-start);
    fseek(mat->fp,(long)-(nBytes+4),SEEK_CUR);
    fwrite(&nBytes,4,1,mat->fp);
    fseek(mat->fp,end,SEEK_SET);

    return byteswritten;
}

#if defined(HAVE_ZLIB)
static size_t
Mat_WriteCompressedEmptyVariable5(mat_t *mat,const char *name,int rank,
                                  size_t *dims,z_stream *z)
{
    mat_uint32_t array_flags = 0x0;
    mat_int16_t  array_name_type     = MAT_T_INT8;
    int      array_flags_type = MAT_T_UINT32, dims_array_type = MAT_T_INT32;
    int      array_flags_size = 8, pad4 = 0;
    int      nBytes, i, nmemb = 1;

    mat_uint32_t comp_buf[512];
    mat_uint32_t uncomp_buf[512] = {0,};
    int buf_size = 512, err;
    size_t byteswritten = 0, buf_size_bytes;

    if ( NULL == mat || NULL == z)
        return 1;

    buf_size_bytes = buf_size*sizeof(*comp_buf);

    /* Array Flags */
    array_flags = MAT_C_DOUBLE;

    uncomp_buf[0] = MAT_T_MATRIX;
    uncomp_buf[1] = (int)GetEmptyMatrixMaxBufSize(name,rank);
    z->next_out  = ZLIB_BYTE_PTR(comp_buf);
    z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
    z->avail_out = buf_size_bytes;
    z->avail_in  = 8;
    err = deflate(z,Z_NO_FLUSH);
    byteswritten += fwrite(comp_buf,1,buf_size_bytes-z->avail_out,mat->fp);
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
        nmemb *= dim;
        uncomp_buf[6+i] = dim;
    }
    if ( rank % 2 != 0 ) {
        uncomp_buf[6+i] = pad4;
        i++;
    }

    z->next_out  = ZLIB_BYTE_PTR(comp_buf);
    z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
    z->avail_out = buf_size_bytes;
    z->avail_in  = (6+i)*sizeof(*uncomp_buf);
    err = deflate(z,Z_NO_FLUSH);
    byteswritten += fwrite(comp_buf,1,buf_size_bytes-z->avail_out,mat->fp);
    /* Name of variable */
    if ( NULL == name ) {
        uncomp_buf[0] = array_name_type;
        uncomp_buf[1] = 0;
        z->next_out  = ZLIB_BYTE_PTR(comp_buf);
        z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
        z->avail_out = buf_size_bytes;
        z->avail_in  = 8;
        err = deflate(z,Z_NO_FLUSH);
        byteswritten += fwrite(comp_buf,1,buf_size_bytes-z->avail_out,mat->fp);
    } else {
        if ( strlen(name) <= 4 ) {
            mat_int16_t array_name_len = (mat_int16_t)strlen(name);
            mat_int16_t array_name_type = MAT_T_INT8;

            memset(uncomp_buf,0,8);
            uncomp_buf[0] = (array_name_len << 16) | array_name_type;
            memcpy(uncomp_buf+1,name,array_name_len);
            if ( array_name_len % 4 )
                array_name_len += 4-(array_name_len % 4);

            z->next_out  = ZLIB_BYTE_PTR(comp_buf);
            z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
            z->avail_out = buf_size_bytes;
            z->avail_in  = 8;
            err = deflate(z,Z_NO_FLUSH);
            byteswritten += fwrite(comp_buf,1,buf_size_bytes-z->avail_out,
                                   mat->fp);
        } else {
            mat_int32_t array_name_len = (mat_int32_t)strlen(name);
            mat_int32_t array_name_type = MAT_T_INT8;

            memset(uncomp_buf,0,buf_size*sizeof(*uncomp_buf));
            uncomp_buf[0] = array_name_type;
            uncomp_buf[1] = array_name_len;
            memcpy(uncomp_buf+2,name,array_name_len);
            if ( array_name_len % 8 )
                array_name_len += 8-(array_name_len % 8);
            z->next_out  = ZLIB_BYTE_PTR(comp_buf);
            z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
            z->avail_out = buf_size_bytes;
            z->avail_in  = 8+array_name_len;
            err = deflate(z,Z_NO_FLUSH);
            byteswritten += fwrite(comp_buf,1,buf_size_bytes-z->avail_out,
                                   mat->fp);
        }
    }

    byteswritten += WriteCompressedData(mat,z,NULL,0,MAT_T_DOUBLE);
    return byteswritten;
}
#endif

/** @if mat_devman
 * @brief Reads the data of a version 5 MAT variable
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar MAT variable pointer to read the data
 * @endif
 */
void
Read5(mat_t *mat, matvar_t *matvar)
{
    int nBytes, len = 0, i, byteswap, data_in_tag = 0;
    enum matio_types packed_type;
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
        case MAT_C_EMPTY:
            matvar->nbytes = 0;
            matvar->data_size = sizeof(double);
            matvar->data_type = MAT_T_DOUBLE;
            matvar->class_type = MAT_C_EMPTY;
            matvar->rank = 2;
            matvar->dims = malloc(matvar->rank*sizeof(*(matvar->dims)));
            matvar->dims[0] = 0;
            matvar->dims[1] = 0;
            break;
        case MAT_C_DOUBLE:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);

                matvar->internal->z->avail_in = 0;
                InflateDataType(mat,matvar->internal->z,tag);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);

                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar->internal->z,tag+1);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
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
                mat_complex_split_t *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                        (void)Mat_uint32Swap(tag);
                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadDoubleData(mat,complex_data->Im,packed_type,
                                            len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedDoubleData(mat,matvar->internal->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar->internal->z,tag);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag);

                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar->internal->z,tag+1);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedDoubleData(mat,matvar->internal->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
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
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedDoubleData(mat,matvar->internal->z,
                                 (double*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
#endif
                }
            }
            break;
        case MAT_C_SINGLE:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);

                matvar->internal->z->avail_in = 0;
                InflateDataType(mat,matvar->internal->z,tag);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);

                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar->internal->z,tag+1);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
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
                mat_complex_split_t *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                        (void)Mat_uint32Swap(tag);
                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadSingleData(mat,complex_data->Im,
                               packed_type,len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedSingleData(mat,matvar->internal->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar->internal->z,tag);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag);

                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar->internal->z,tag+1);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedSingleData(mat,matvar->internal->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
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
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedSingleData(mat,matvar->internal->z,
                                 (float*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
#endif
                }
            }
            break;
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);

                matvar->internal->z->avail_in = 0;
                InflateDataType(mat,matvar->internal->z,tag);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);

                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar->internal->z,tag+1);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
                    nBytes = tag[1];
                }
            }
            if ( nBytes == 0 ) {
                matvar->nbytes = 0;
                break;
            }
            for ( i = 0; i < matvar->rank; i++ )
                len *= matvar->dims[i];
            matvar->data_size = sizeof(mat_int64_t);
            matvar->data_type = MAT_T_INT64;
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
                    nBytes = ReadInt64Data(mat,complex_data->Re,
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
                        (void)Mat_uint32Swap(tag);
                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadInt64Data(mat,complex_data->Im,
                               packed_type,len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedInt64Data(mat,matvar->internal->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar->internal->z,tag);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag);

                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar->internal->z,tag+1);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedInt64Data(mat,matvar->internal->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
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
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
                    nBytes = ReadInt64Data(mat,(mat_int64_t*)matvar->data,
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
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt64Data(mat,matvar->internal->z,
                                 (mat_int64_t*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
#endif
                }
            }
            break;
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);

                matvar->internal->z->avail_in = 0;
                InflateDataType(mat,matvar->internal->z,tag);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar->internal->z,tag+1);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
                    nBytes = tag[1];
                }
            }
            if ( nBytes == 0 ) {
                matvar->nbytes = 0;
                break;
            }
            for ( i = 0; i < matvar->rank; i++ )
                len *= matvar->dims[i];
            matvar->data_size = sizeof(mat_uint64_t);
            matvar->data_type = MAT_T_UINT64;
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
                    nBytes = ReadInt64Data(mat,complex_data->Re,
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
                        (void)Mat_uint32Swap(tag);
                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadInt64Data(mat,complex_data->Im,
                               packed_type,len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedInt64Data(mat,matvar->internal->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar->internal->z,tag);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag);

                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar->internal->z,tag+1);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedInt64Data(mat,matvar->internal->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
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
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
                    nBytes = ReadInt64Data(mat,(mat_int64_t*)matvar->data,
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
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt64Data(mat,matvar->internal->z,
                                 (mat_int64_t*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
#endif
                }
            }
            break;
#endif /* HAVE_MAT_UINT64_T */
        case MAT_C_INT32:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);

                matvar->internal->z->avail_in = 0;
                InflateDataType(mat,matvar->internal->z,tag);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);

                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar->internal->z,tag+1);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
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
                mat_complex_split_t *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                        (void)Mat_uint32Swap(tag);
                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadInt32Data(mat,complex_data->Im,
                               packed_type,len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedInt32Data(mat,matvar->internal->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar->internal->z,tag);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag);

                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar->internal->z,tag+1);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedInt32Data(mat,matvar->internal->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
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
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt32Data(mat,matvar->internal->z,
                                 (mat_int32_t*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
#endif
                }
            }
            break;
        case MAT_C_UINT32:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);

                matvar->internal->z->avail_in = 0;
                InflateDataType(mat,matvar->internal->z,tag);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar->internal->z,tag+1);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
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
                mat_complex_split_t *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                        (void)Mat_uint32Swap(tag);
                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadInt32Data(mat,complex_data->Im,
                               packed_type,len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedInt32Data(mat,matvar->internal->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar->internal->z,tag);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag);

                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar->internal->z,tag+1);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedInt32Data(mat,matvar->internal->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
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
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt32Data(mat,matvar->internal->z,
                                 (mat_int32_t*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
#endif
                }
            }
            break;
        case MAT_C_INT16:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);

                matvar->internal->z->avail_in = 0;
                InflateDataType(mat,matvar->internal->z,tag);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar->internal->z,tag+1);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
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
                mat_complex_split_t *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                        (void)Mat_uint32Swap(tag);
                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadInt16Data(mat,complex_data->Im,
                               packed_type,len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedInt16Data(mat,matvar->internal->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar->internal->z,tag);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag);

                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar->internal->z,tag+1);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedInt16Data(mat,matvar->internal->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
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
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt16Data(mat,matvar->internal->z,
                                 (mat_int16_t*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
#endif
                }
            }
            break;
        case MAT_C_UINT16:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);

                matvar->internal->z->avail_in = 0;
                InflateDataType(mat,matvar->internal->z,tag);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar->internal->z,tag+1);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
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
                mat_complex_split_t *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                        (void)Mat_uint32Swap(tag);
                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadInt16Data(mat,complex_data->Im,
                               packed_type,len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedInt16Data(mat,matvar->internal->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar->internal->z,tag);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag);

                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar->internal->z,tag+1);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedInt16Data(mat,matvar->internal->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
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
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt16Data(mat,matvar->internal->z,
                                 (mat_int16_t*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
#endif
                }
            }
            break;
        case MAT_C_INT8:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);

                matvar->internal->z->avail_in = 0;
                InflateDataType(mat,matvar->internal->z,tag);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar->internal->z,tag+1);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
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
                mat_complex_split_t *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                        (void)Mat_uint32Swap(tag);
                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadInt8Data(mat,complex_data->Im,
                               packed_type,len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedInt8Data(mat,matvar->internal->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar->internal->z,tag);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag);

                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar->internal->z,tag+1);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedInt8Data(mat,matvar->internal->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
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
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt8Data(mat,matvar->internal->z,
                                 (mat_int8_t*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
#endif
                }
            }
            break;
        case MAT_C_UINT8:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);

                matvar->internal->z->avail_in = 0;
                InflateDataType(mat,matvar->internal->z,tag);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar->internal->z,tag+1);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
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
                mat_complex_split_t *complex_data;

                matvar->nbytes = len*matvar->data_size;
                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL == complex_data || NULL == complex_data->Re ||
                     NULL == complex_data->Im ) {
                    Mat_Critical("Failed to allocate %d bytes",2*matvar->nbytes);
                    break;
                }
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                        (void)Mat_uint32Swap(tag);
                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        fread(tag+1,4,1,mat->fp);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadInt8Data(mat,complex_data->Im,
                               packed_type,len);
#if defined(HAVE_ZLIB)
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
                    nBytes = ReadCompressedInt8Data(mat,matvar->internal->z,
                                 complex_data->Re,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));

                    /* Complex Data Tag */
                    InflateDataType(mat,matvar->internal->z,tag);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag);

                    packed_type = TYPE_FROM_TAG(tag[0]);
                    if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                        data_in_tag = 1;
                        nBytes = (tag[0] & 0xffff0000) >> 16;
                    } else {
                        data_in_tag = 0;
                        InflateDataType(mat,matvar->internal->z,tag+1);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag+1);
                        nBytes = tag[1];
                    }
                    nBytes = ReadCompressedInt8Data(mat,matvar->internal->z,
                                 complex_data->Im,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
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
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt8Data(mat,matvar->internal->z,
                                 (mat_int8_t*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
#endif
                }
            }
            break;
            break;
        case MAT_C_CHAR:
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);

                matvar->internal->z->avail_in = 0;
                InflateDataType(mat,matvar->internal->z,tag);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    InflateDataType(mat,matvar->internal->z,tag+1);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
                    nBytes = tag[1];
                }
#endif
            } else {
                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);
                fread(tag,4,1,mat->fp);
                if ( byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    nBytes = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(tag+1,4,1,mat->fp);
                    if ( byteswap )
                        (void)Mat_uint32Swap(tag+1);
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
            if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
            } else if ( matvar->compression == MAT_COMPRESSION_ZLIB) {
                nBytes = ReadCompressedCharData(mat,matvar->internal->z,
                             (char*)matvar->data,packed_type,len);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                if ( (nBytes % 8) != 0 )
                    InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
#endif
            }
            break;
        case MAT_C_STRUCT:
        {
            matvar_t **fields;
            int nfields = 0;

            matvar->data_type = MAT_T_STRUCT;
            if ( !matvar->nbytes || !matvar->data_size || NULL == matvar->data )
                break;
            len = 1;
            for ( i = 0; i < matvar->rank; i++ )
                len *= matvar->dims[i];
            nfields = matvar->internal->num_fields;
            fields = (matvar_t **)matvar->data;
            for ( i = 0; i < len*nfields; i++ ) {
                fields[i]->internal->fp = mat;
                Read5(mat,fields[i]);
            }
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
                cells[i]->internal->fp = mat;
                Read5(mat,cells[i]);
            }
            /* FIXME: */
            matvar->data_type = MAT_T_CELL;
            break;
        }
        case MAT_C_SPARSE:
        {
            int N;
            mat_sparse_t *data;

            matvar->data_size = sizeof(mat_sparse_t);
            matvar->data      = malloc(matvar->data_size);
            if ( matvar->data == NULL ) {
                Mat_Critical("ReadData: Allocation of data pointer failed");
                break;
            }
            data = matvar->data;
            data->nzmax  = matvar->nbytes;
            fseek(mat->fp,matvar->internal->datapos,SEEK_SET);
            /*  Read ir    */
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                matvar->internal->z->avail_in = 0;
                InflateDataType(mat,matvar->internal->z,tag);
                if ( mat->byteswap )
                    (void)Mat_uint32Swap(tag);

                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    N = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    (void)ReadCompressedInt32Data(mat,matvar->internal->z,
                             (mat_int32_t*)&N,MAT_T_INT32,1);
                }
#endif
            } else {
                fread(tag,4,1,mat->fp);
                if ( mat->byteswap )
                    (void)Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    N = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(&N,4,1,mat->fp);
                    if ( mat->byteswap )
                        Mat_int32Swap(&N);
                }
            }
            data->nir = N / 4;
            data->ir = malloc(data->nir*sizeof(mat_int32_t));
            if ( data->ir != NULL ) {
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt32Data(mat,matvar->internal->z,
                                 data->ir,packed_type,data->nir);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
#endif
                }
            } else {
                Mat_Critical("ReadData: Allocation of ir pointer failed");
                break;
            }
            /*  Read jc    */
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                matvar->internal->z->avail_in = 0;
                InflateDataType(mat,matvar->internal->z,tag);
                if ( mat->byteswap )
                    Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    N = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    (void)ReadCompressedInt32Data(mat,matvar->internal->z,
                             (mat_int32_t*)&N,MAT_T_INT32,1);
                }
#endif
            } else {
                fread(tag,4,1,mat->fp);
                if ( mat->byteswap )
                    Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    N = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(&N,4,1,mat->fp);
                    if ( mat->byteswap )
                        Mat_int32Swap(&N);
                }
            }
            data->njc = N / 4;
            data->jc = malloc(data->njc*sizeof(mat_int32_t));
            if ( data->jc != NULL ) {
                if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                } else if ( matvar->compression == MAT_COMPRESSION_ZLIB) {
                    nBytes = ReadCompressedInt32Data(mat,matvar->internal->z,
                                 data->jc,packed_type,data->njc);
                    /*
                     * If the data was in the tag we started on a 4-byte
                     * boundary so add 4 to make it an 8-byte
                     */
                    if ( data_in_tag )
                        nBytes+=4;
                    if ( (nBytes % 8) != 0 )
                        InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
#endif
                }
            } else {
                Mat_Critical("ReadData: Allocation of jc pointer failed");
                break;
            }
            /*  Read data    */
            if ( matvar->compression ) {
#if defined(HAVE_ZLIB)
                matvar->internal->z->avail_in = 0;
                InflateDataType(mat,matvar->internal->z,tag);
                if ( mat->byteswap )
                    Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    N = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    (void)ReadCompressedInt32Data(mat,matvar->internal->z,
                             (mat_int32_t*)&N,MAT_T_INT32,1);
                }
#endif
            } else {
                fread(tag,4,1,mat->fp);
                if ( mat->byteswap )
                    Mat_uint32Swap(tag);
                packed_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                    data_in_tag = 1;
                    N = (tag[0] & 0xffff0000) >> 16;
                } else {
                    data_in_tag = 0;
                    fread(&N,4,1,mat->fp);
                    if ( mat->byteswap )
                        Mat_int32Swap(&N);
                }
            }
#if defined(EXTENDED_SPARSE)
            matvar->data_type = packed_type;
#else
            matvar->data_type = MAT_T_DOUBLE;
#endif
            data->ndata = N / Mat_SizeOf(packed_type);
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data;

                complex_data = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(data->ndata*
                                          Mat_SizeOf(matvar->data_type));
                complex_data->Im = malloc(data->ndata*
                                          Mat_SizeOf(matvar->data_type));
                if ( NULL != complex_data && NULL != complex_data->Re &&
                     NULL != complex_data->Im ) {
                    if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                            (void)Mat_uint32Swap(tag);
                        packed_type = TYPE_FROM_TAG(tag[0]);
                        if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                            data_in_tag = 1;
                            nBytes = (tag[0] & 0xffff0000) >> 16;
                        } else {
                            data_in_tag = 0;
                            fread(tag+1,4,1,mat->fp);
                            if ( byteswap )
                                (void)Mat_uint32Swap(tag+1);
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
                    } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
#if defined(EXTENDED_SPARSE)
                        switch ( matvar->data_type ) {
                            case MAT_T_DOUBLE:
                                nBytes = ReadCompressedDoubleData(mat,matvar->internal->z,
                                     complex_data->Re,packed_type,data->ndata);
                                break;
                            case MAT_T_SINGLE:
                                nBytes = ReadCompressedSingleData(mat,matvar->internal->z,
                                     complex_data->Re,packed_type,data->ndata);
                                break;
                            case MAT_T_INT32:
                                nBytes = ReadCompressedInt32Data(mat,matvar->internal->z,
                                     complex_data->Re,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT32:
                                nBytes = ReadCompressedInt32Data(mat,matvar->internal->z,
                                     complex_data->Re,packed_type,data->ndata);
                                break;
                            case MAT_T_INT16:
                                nBytes = ReadCompressedInt16Data(mat,matvar->internal->z,
                                     complex_data->Re,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT16:
                                nBytes = ReadCompressedInt16Data(mat,matvar->internal->z,
                                     complex_data->Re,packed_type,data->ndata);
                                break;
                            case MAT_T_INT8:
                                nBytes = ReadCompressedInt8Data(mat,matvar->internal->z,
                                     complex_data->Re,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT8:
                                nBytes = ReadCompressedInt8Data(mat,matvar->internal->z,
                                     complex_data->Re,packed_type,data->ndata);
                                break;
                        }
#else    /* EXTENDED_SPARSE */
                        nBytes = ReadCompressedDoubleData(mat,matvar->internal->z,
                                     complex_data->Re,packed_type,data->ndata);
#endif    /* EXTENDED_SPARSE */
                        if ( data_in_tag )
                            nBytes+=4;
                        if ( (nBytes % 8) != 0 )
                            InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));

                        /* Complex Data Tag */
                        InflateDataType(mat,matvar->internal->z,tag);
                        if ( byteswap )
                            (void)Mat_uint32Swap(tag);

                        packed_type = TYPE_FROM_TAG(tag[0]);
                        if ( tag[0] & 0xffff0000 ) { /* Data is in the tag */
                            data_in_tag = 1;
                            nBytes = (tag[0] & 0xffff0000) >> 16;
                        } else {
                            data_in_tag = 0;
                            InflateDataType(mat,matvar->internal->z,tag+1);
                            if ( byteswap )
                                (void)Mat_uint32Swap(tag+1);
                            nBytes = tag[1];
                        }
#if defined(EXTENDED_SPARSE)
                        switch ( matvar->data_type ) {
                            case MAT_T_DOUBLE:
                                nBytes = ReadCompressedDoubleData(mat,matvar->internal->z,
                                     complex_data->Im,packed_type,data->ndata);
                                break;
                            case MAT_T_SINGLE:
                                nBytes = ReadCompressedSingleData(mat,matvar->internal->z,
                                     complex_data->Im,packed_type,data->ndata);
                                break;
                            case MAT_T_INT32:
                                nBytes = ReadCompressedInt32Data(mat,matvar->internal->z,
                                     complex_data->Im,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT32:
                                nBytes = ReadCompressedUInt32Data(mat,matvar->internal->z,
                                     complex_data->Im,packed_type,data->ndata);
                                break;
                            case MAT_T_INT16:
                                nBytes = ReadCompressedInt16Data(mat,matvar->internal->z,
                                     complex_data->Im,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT16:
                                nBytes = ReadCompressedUInt16Data(mat,matvar->internal->z,
                                     complex_data->Im,packed_type,data->ndata);
                                break;
                            case MAT_T_INT8:
                                nBytes = ReadCompressedInt8Data(mat,matvar->internal->z,
                                     complex_data->Im,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT8:
                                nBytes = ReadCompressedUInt8Data(mat,matvar->internal->z,
                                     complex_data->Im,packed_type,data->ndata);
                                break;
                        }
#else    /* EXTENDED_SPARSE */
                        nBytes = ReadCompressedDoubleData(mat,matvar->internal->z,
                                     complex_data->Im,packed_type,data->ndata);
#endif    /* EXTENDED_SPARSE */
                        if ( data_in_tag )
                            nBytes+=4;
                        if ( (nBytes % 8) != 0 )
                            InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
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
                    if ( matvar->compression == MAT_COMPRESSION_NONE) {
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
                    } else if ( matvar->compression == MAT_COMPRESSION_ZLIB) {
#if defined(EXTENDED_SPARSE)
                        switch ( matvar->data_type ) {
                            case MAT_T_DOUBLE:
                                nBytes = ReadCompressedDoubleData(mat,matvar->internal->z,
                                     data->data,packed_type,data->ndata);
                                break;
                            case MAT_T_SINGLE:
                                nBytes = ReadCompressedSingleData(mat,matvar->internal->z,
                                     data->data,packed_type,data->ndata);
                                break;
                            case MAT_T_INT32:
                                nBytes = ReadCompressedInt32Data(mat,matvar->internal->z,
                                     data->data,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT32:
                                nBytes = ReadCompressedInt32Data(mat,matvar->internal->z,
                                     data->data,packed_type,data->ndata);
                                break;
                            case MAT_T_INT16:
                                nBytes = ReadCompressedInt16Data(mat,matvar->internal->z,
                                     data->data,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT16:
                                nBytes = ReadCompressedInt16Data(mat,matvar->internal->z,
                                     data->data,packed_type,data->ndata);
                                break;
                            case MAT_T_INT8:
                                nBytes = ReadCompressedInt8Data(mat,matvar->internal->z,
                                     data->data,packed_type,data->ndata);
                                break;
                            case MAT_T_UINT8:
                                nBytes = ReadCompressedInt8Data(mat,matvar->internal->z,
                                     data->data,packed_type,data->ndata);
                                break;
                        }
#else   /* EXTENDED_SPARSE */
                        nBytes = ReadCompressedDoubleData(mat,matvar->internal->z,
                                     data->data,packed_type,data->ndata);
#endif   /* EXTENDED_SPARSE */
                        if ( data_in_tag )
                            nBytes+=4;
                        if ( (nBytes % 8) != 0 )
                            InflateSkip(mat,matvar->internal->z,8-(nBytes % 8));
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
                functions[i]->internal->fp = mat;
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
ReadData5(mat_t *mat,matvar_t *matvar,void *data,
    int *start,int *stride,int *edge)
{
    int err = 0,real_bytes;
    mat_int32_t tag[2];
#if defined(HAVE_ZLIB)
    z_stream z;
#endif

    fseek(mat->fp,matvar->internal->datapos,SEEK_SET);
    if ( matvar->compression == MAT_COMPRESSION_NONE ) {
        fread(tag,4,2,mat->fp);
        if ( mat->byteswap ) {
            Mat_int32Swap(tag);
            Mat_int32Swap(tag+1);
        }
        matvar->data_type = TYPE_FROM_TAG(tag[0]);
        if ( tag[0] & 0xffff0000 ) { /* Data is packed in the tag */
            fseek(mat->fp,-4,SEEK_CUR);
            real_bytes = 4+(tag[0] >> 16);
        } else {
            real_bytes = 8+tag[1];
        }
#if defined(HAVE_ZLIB)
    } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
        err = inflateCopy(&z,matvar->internal->z);
        z.avail_in = 0;
        InflateDataType(mat,&z,tag);
        if ( mat->byteswap ) {
            Mat_int32Swap(tag);
        }
        matvar->data_type = TYPE_FROM_TAG(tag[0]);
        if ( !(tag[0] & 0xffff0000) ) {/* Data is NOT packed in the tag */
            /* We're cheating, but InflateDataType just inflates 4 bytes */
            InflateDataType(mat,&z,tag+1);
            if ( mat->byteswap ) {
                Mat_int32Swap(tag+1);
            }
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
        else if ( matvar->compression == MAT_COMPRESSION_NONE ) {
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = data;

                ReadDataSlab2(mat,complex_data->Re,matvar->class_type,
                    matvar->data_type,matvar->dims,start,stride,edge);
                fseek(mat->fp,matvar->internal->datapos+real_bytes,SEEK_SET);
                fread(tag,4,2,mat->fp);
                if ( mat->byteswap ) {
                    Mat_int32Swap(tag);
                    Mat_int32Swap(tag+1);
                }
                matvar->data_type = TYPE_FROM_TAG(tag[0]);
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
        else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = data;

                ReadCompressedDataSlab2(mat,&z,complex_data->Re,
                    matvar->class_type,matvar->data_type,matvar->dims,
                    start,stride,edge);

                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);

                /* Reset zlib knowledge to before reading real tag */
                inflateEnd(&z);
                err = inflateCopy(&z,matvar->internal->z);
                InflateSkip(mat,&z,real_bytes);
                z.avail_in = 0;
                InflateDataType(mat,&z,tag);
                if ( mat->byteswap ) {
                    Mat_int32Swap(tag);
                }
                matvar->data_type = TYPE_FROM_TAG(tag[0]);
                if ( !(tag[0] & 0xffff0000) ) {/*Data is NOT packed in the tag*/
                    InflateSkip(mat,&z,4);
                }
                ReadCompressedDataSlab2(mat,&z,complex_data->Im,
                    matvar->class_type,matvar->data_type,matvar->dims,
                    start,stride,edge);
                inflateEnd(&z);
            } else {
                ReadCompressedDataSlab2(mat,&z,data,matvar->class_type,
                    matvar->data_type,matvar->dims,start,stride,edge);
            }
        }
#endif
    } else {
        if ( matvar->compression == MAT_COMPRESSION_NONE ) {
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = data;

                ReadDataSlabN(mat,complex_data->Re,matvar->class_type,
                    matvar->data_type,matvar->rank,matvar->dims,
                    start,stride,edge);

                fseek(mat->fp,matvar->internal->datapos+real_bytes,SEEK_SET);
                fread(tag,4,2,mat->fp);
                if ( mat->byteswap ) {
                    Mat_int32Swap(tag);
                    Mat_int32Swap(tag+1);
                }
                matvar->data_type = TYPE_FROM_TAG(tag[0]);
                if ( tag[0] & 0xffff0000 ) { /* Data is packed in the tag */
                    fseek(mat->fp,-4,SEEK_CUR);
                }
                ReadDataSlabN(mat,complex_data->Im,matvar->class_type,
                    matvar->data_type,matvar->rank,matvar->dims,
                    start,stride,edge);
            } else {
                ReadDataSlabN(mat,data,matvar->class_type,matvar->data_type,
                    matvar->rank,matvar->dims,start,stride,edge);
            }
        }
#if defined(HAVE_ZLIB)
        else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = data;

                ReadCompressedDataSlabN(mat,&z,complex_data->Re,
                    matvar->class_type,matvar->data_type,matvar->rank,
                    matvar->dims,start,stride,edge);

                fseek(mat->fp,matvar->internal->datapos,SEEK_SET);
                /* Reset zlib knowledge to before reading real tag */
                inflateEnd(&z);
                err = inflateCopy(&z,matvar->internal->z);
                InflateSkip(mat,&z,real_bytes);
                z.avail_in = 0;
                InflateDataType(mat,&z,tag);
                if ( mat->byteswap ) {
                    Mat_int32Swap(tag);
                }
                matvar->data_type = TYPE_FROM_TAG(tag[0]);
                if ( !(tag[0] & 0xffff0000) ) {/*Data is NOT packed in the tag*/
                    InflateSkip(mat,&z,4);
                }
                ReadCompressedDataSlabN(mat,&z,complex_data->Im,
                    matvar->class_type,matvar->data_type,matvar->rank,
                    matvar->dims,start,stride,edge);
                inflateEnd(&z);
            } else {
                ReadCompressedDataSlabN(mat,&z,data,matvar->class_type,
                    matvar->data_type,matvar->rank,matvar->dims,
                    start,stride,edge);
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
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
            matvar->data_type = MAT_T_INT64;
            matvar->data_size = sizeof(mat_int64_t);
            break;
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
            matvar->data_type = MAT_T_UINT64;
            matvar->data_size = sizeof(mat_uint64_t);
            break;
#endif /* HAVE_MAT_UINT64_T */
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
Mat_VarWrite5(mat_t *mat,matvar_t *matvar,int compress)
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

#if !defined(HAVE_ZLIB)
    compress = MAT_COMPRESSION_NONE;
#endif

    if ( compress == MAT_COMPRESSION_NONE ) {
        fwrite(&matrix_type,4,1,mat->fp);
        fwrite(&pad4,4,1,mat->fp);
        start = ftell(mat->fp);

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
            mat_int32_t  array_name_type = MAT_T_INT8;
            mat_int32_t array_name_len   = strlen(matvar->name);
            mat_int8_t  pad1 = 0;
#if 0
            fwrite(&array_name_type,2,1,mat->fp);
            fwrite(&array_name_len,2,1,mat->fp);
#else
            array_name_type = (array_name_len << 16) | array_name_type;
            fwrite(&array_name_type,4,1,mat->fp);
#endif
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

        matvar->internal->datapos = ftell(mat->fp);
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
            {
                if ( matvar->isComplex ) {
                    mat_complex_split_t *complex_data = matvar->data;

                    if ( NULL == complex_data )
                        complex_data = &null_complex_data;

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
                    WriteCellArrayField(mat,cells[i]);
                break;
            }
            case MAT_C_STRUCT:
            {
                char      *padzero;
                int        fieldname_size, nfields;
                size_t     maxlen = 0;
                matvar_t **fields = (matvar_t **)matvar->data;
                mat_int32_t array_name_type = MAT_T_INT8;
                unsigned   fieldname;

                /* Check for a structure with no fields */
                if ( matvar->internal->num_fields < 1 ) {
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
                nfields = matvar->internal->num_fields;
                for ( i = 0; i < nfields; i++ ) {
                    size_t len = strlen(matvar->internal->fieldnames[i]);
                    if ( len > maxlen )
                        maxlen = len;
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
                    size_t len = strlen(matvar->internal->fieldnames[i]);
                    fwrite(matvar->internal->fieldnames[i],1,len,mat->fp);
                    fwrite(padzero,1,fieldname_size-len,mat->fp);
                }
                free(padzero);
                for ( i = 0; i < nmemb*nfields; i++ )
                    WriteStructField(mat,fields[i]);
                break;
            }
            case MAT_C_SPARSE:
            {
                mat_sparse_t *sparse = matvar->data;

                nBytes = WriteData(mat,sparse->ir,sparse->nir,MAT_T_INT32);
                if ( nBytes % 8 )
                    for ( i = nBytes % 8; i < 8; i++ )
                        fwrite(&pad1,1,1,mat->fp);
                nBytes = WriteData(mat,sparse->jc,sparse->njc,MAT_T_INT32);
                if ( nBytes % 8 )
                    for ( i = nBytes % 8; i < 8; i++ )
                        fwrite(&pad1,1,1,mat->fp);
                if ( matvar->isComplex ) {
                    mat_complex_split_t *complex_data = sparse->data;
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
    } else if ( compress == MAT_COMPRESSION_ZLIB ) {
        mat_uint32_t comp_buf[512];
        mat_uint32_t uncomp_buf[512] = {0,};
        int buf_size = 512, err;
        size_t byteswritten = 0;

        matvar->internal->z         = calloc(1,sizeof(*matvar->internal->z));
        matvar->internal->z->zalloc = Z_NULL;
        matvar->internal->z->zfree  = Z_NULL;
        err = deflateInit(matvar->internal->z,Z_DEFAULT_COMPRESSION);

        matrix_type = MAT_T_COMPRESSED;
        fwrite(&matrix_type,4,1,mat->fp);
        fwrite(&pad4,4,1,mat->fp);
        start = ftell(mat->fp);

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

        uncomp_buf[0] = MAT_T_MATRIX;
        uncomp_buf[1] = (int)GetMatrixMaxBufSize(matvar);
        matvar->internal->z->next_out  = ZLIB_BYTE_PTR(comp_buf);
        matvar->internal->z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
        matvar->internal->z->avail_out = buf_size*sizeof(*comp_buf);
        matvar->internal->z->avail_in  = 8;
        err = deflate(matvar->internal->z,Z_NO_FLUSH);
        byteswritten += fwrite(comp_buf,1,
            buf_size*sizeof(*comp_buf)-matvar->internal->z->avail_out,mat->fp);
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

        matvar->internal->z->next_out  = ZLIB_BYTE_PTR(comp_buf);
        matvar->internal->z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
        matvar->internal->z->avail_out = buf_size*sizeof(*comp_buf);
        matvar->internal->z->avail_in  = (6+i)*sizeof(*uncomp_buf);
        err = deflate(matvar->internal->z,Z_NO_FLUSH);
        byteswritten += fwrite(comp_buf,1,
                buf_size*sizeof(*comp_buf)-matvar->internal->z->avail_out,mat->fp);
        /* Name of variable */
        if ( strlen(matvar->name) <= 4 ) {
            mat_int16_t array_name_len = (mat_int16_t)strlen(matvar->name);
            mat_int16_t array_name_type = MAT_T_INT8;

            memset(uncomp_buf,0,8);
            uncomp_buf[0] = (array_name_len << 16) | array_name_type;
            memcpy(uncomp_buf+1,matvar->name,array_name_len);
            if ( array_name_len % 4 )
                array_name_len += 4-(array_name_len % 4);

            matvar->internal->z->next_out  = ZLIB_BYTE_PTR(comp_buf);
            matvar->internal->z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
            matvar->internal->z->avail_out = buf_size*sizeof(*comp_buf);
            matvar->internal->z->avail_in  = 8;
            err = deflate(matvar->internal->z,Z_NO_FLUSH);
            byteswritten += fwrite(comp_buf,1,
                    buf_size*sizeof(*comp_buf)-matvar->internal->z->avail_out,mat->fp);
        } else {
            mat_int32_t array_name_len = (mat_int32_t)strlen(matvar->name);
            mat_int32_t array_name_type = MAT_T_INT8;

            memset(uncomp_buf,0,buf_size*sizeof(*uncomp_buf));
            uncomp_buf[0] = array_name_type;
            uncomp_buf[1] = array_name_len;
            memcpy(uncomp_buf+2,matvar->name,array_name_len);
            if ( array_name_len % 8 )
                array_name_len += 8-(array_name_len % 8);
            matvar->internal->z->next_out  = ZLIB_BYTE_PTR(comp_buf);
            matvar->internal->z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
            matvar->internal->z->avail_out = buf_size*sizeof(*comp_buf);
            matvar->internal->z->avail_in  = 8+array_name_len;
            err = deflate(matvar->internal->z,Z_NO_FLUSH);
            byteswritten += fwrite(comp_buf,1,
                    buf_size*sizeof(*comp_buf)-matvar->internal->z->avail_out,mat->fp);
        }
        matvar->internal->datapos = ftell(mat->fp);
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
            {
                /* WriteCompressedData makes sure uncomressed data is aligned
                 * on an 8-byte boundary */
                if ( matvar->isComplex ) {
                    mat_complex_split_t *complex_data = matvar->data;

                    if ( NULL == matvar->data )
                        complex_data = &null_complex_data;

                    byteswritten += WriteCompressedData(mat,matvar->internal->z,
                        complex_data->Re,nmemb,matvar->data_type);
                    byteswritten += WriteCompressedData(mat,matvar->internal->z,
                        complex_data->Im,nmemb,matvar->data_type);
                } else {
                    byteswritten += WriteCompressedData(mat,matvar->internal->z,
                        matvar->data,nmemb,matvar->data_type);
                }
                break;
            }
            case MAT_C_CHAR:
            {
                byteswritten += WriteCompressedCharData(mat,matvar->internal->z,
                    matvar->data,nmemb,matvar->data_type);
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
                    WriteCompressedCellArrayField(mat,cells[i],matvar->internal->z);
                break;
            }
            case MAT_C_STRUCT:
            {
                unsigned char *padzero;
                int        fieldname_size, nfields;
                size_t     maxlen = 0;
                mat_int32_t array_name_type = MAT_T_INT8;
                matvar_t **fields = (matvar_t **)matvar->data;

                /* Check for a structure with no fields */
                if ( matvar->internal->num_fields < 1 ) {
                    fieldname_size = 1;
                    uncomp_buf[0] = (fieldname_data_size << 16) |
                                     fieldname_type;
                    uncomp_buf[1] = 1;
                    uncomp_buf[2] = array_name_type;
                    uncomp_buf[3] = 0;
                    matvar->internal->z->next_out  = ZLIB_BYTE_PTR(comp_buf);
                    matvar->internal->z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
                    matvar->internal->z->avail_out = buf_size*sizeof(*comp_buf);
                    matvar->internal->z->avail_in  = 16;
                    err = deflate(matvar->internal->z,Z_NO_FLUSH);
                    byteswritten += fwrite(comp_buf,1,buf_size*
                        sizeof(*comp_buf)-matvar->internal->z->avail_out,mat->fp);
                    break;
                }
                nfields = matvar->internal->num_fields;
                for ( i = 0; i < nfields; i++ ) {
                    size_t len = strlen(matvar->internal->fieldnames[i]);
                    if ( len > maxlen )
                        maxlen = len;
                }
                maxlen++;
                fieldname_size = maxlen;
                while ( nfields*fieldname_size % 8 != 0 )
                    fieldname_size++;
                uncomp_buf[0] = (fieldname_data_size << 16) | fieldname_type;
                uncomp_buf[1] = fieldname_size;
                uncomp_buf[2] = array_name_type;
                uncomp_buf[3] = nfields*fieldname_size;

                padzero = calloc(fieldname_size,1);
                matvar->internal->z->next_out  = ZLIB_BYTE_PTR(comp_buf);
                matvar->internal->z->next_in   = ZLIB_BYTE_PTR(uncomp_buf);
                matvar->internal->z->avail_out = buf_size*sizeof(*comp_buf);
                matvar->internal->z->avail_in  = 16;
                err = deflate(matvar->internal->z,Z_NO_FLUSH);
                byteswritten += fwrite(comp_buf,1,
                        buf_size*sizeof(*comp_buf)-matvar->internal->z->avail_out,mat->fp);
                for ( i = 0; i < nfields; i++ ) {
                    size_t len = strlen(matvar->internal->fieldnames[i]);
                    memset(padzero,'\0',fieldname_size);
                    memcpy(padzero,matvar->internal->fieldnames[i],len);
                    matvar->internal->z->next_out  = ZLIB_BYTE_PTR(comp_buf);
                    matvar->internal->z->next_in   = ZLIB_BYTE_PTR(padzero);
                    matvar->internal->z->avail_out = buf_size*sizeof(*comp_buf);
                    matvar->internal->z->avail_in  = fieldname_size;
                    err = deflate(matvar->internal->z,Z_NO_FLUSH);
                    byteswritten += fwrite(comp_buf,1,
                            buf_size*sizeof(*comp_buf)-matvar->internal->z->avail_out,
                            mat->fp);
                }
                free(padzero);
                for ( i = 0; i < nmemb*nfields; i++ )
                    byteswritten +=
                        WriteCompressedStructField(mat,fields[i],matvar->internal->z);
                break;
            }
            case MAT_C_SPARSE:
            {
                mat_sparse_t *sparse = matvar->data;

                byteswritten += WriteCompressedData(mat,matvar->internal->z,sparse->ir,
                    sparse->nir,MAT_T_INT32);
                byteswritten += WriteCompressedData(mat,matvar->internal->z,sparse->jc,
                    sparse->njc,MAT_T_INT32);
                if ( matvar->isComplex ) {
                    mat_complex_split_t *complex_data = sparse->data;
                    byteswritten += WriteCompressedData(mat,matvar->internal->z,
                        complex_data->Re,sparse->ndata,matvar->data_type);
                    byteswritten += WriteCompressedData(mat,matvar->internal->z,
                        complex_data->Im,sparse->ndata,matvar->data_type);
                } else {
                    byteswritten += WriteCompressedData(mat,matvar->internal->z,
                        sparse->data,sparse->ndata,matvar->data_type);
                }
                break;
            }
        }
        matvar->internal->z->avail_in  = 0;
        matvar->internal->z->next_in   = NULL;
        matvar->internal->z->next_out  = ZLIB_BYTE_PTR(comp_buf);
        matvar->internal->z->avail_out = buf_size*sizeof(*comp_buf);

        err = deflate(matvar->internal->z,Z_FINISH);
        byteswritten += fwrite(comp_buf,1,
            buf_size*sizeof(*comp_buf)-matvar->internal->z->avail_out,mat->fp);
        while ( err != Z_STREAM_END && !matvar->internal->z->avail_out ) {
            matvar->internal->z->next_out  = ZLIB_BYTE_PTR(comp_buf);
            matvar->internal->z->avail_out = buf_size*sizeof(*comp_buf);

            err = deflate(matvar->internal->z,Z_FINISH);
            byteswritten += fwrite(comp_buf,1,
                buf_size*sizeof(*comp_buf)-matvar->internal->z->avail_out,mat->fp);
        }
        /* End the compression and set to NULL so Mat_VarFree doesn't try
         * to free matvar->internal->z with inflateEnd
         */
#if 0
        if ( byteswritten % 8 )
            for ( i = 0; i < 8-(byteswritten % 8); i++ )
                fwrite(&pad1,1,1,mat->fp);
#endif
        err = deflateEnd(matvar->internal->z);
        free(matvar->internal->z);
        matvar->internal->z = NULL;
#endif
    }
    end = ftell(mat->fp);
    nBytes = (int)(end-start);
    fseek(mat->fp,(long)-(nBytes+4),SEEK_CUR);
    fwrite(&nBytes,4,1,mat->fp);
    fseek(mat->fp,end,SEEK_SET);

    return 0;
}

/** @if mat_devman
 * @brief Writes the variable information and empty data
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @endif
 */
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


    if ( matvar->compression == MAT_COMPRESSION_NONE ) {
        fwrite(&matrix_type,4,1,mat->fp);
        fwrite(&pad4,4,1,mat->fp);
        start = ftell(mat->fp);

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

        matvar->internal->datapos = ftell(mat->fp);
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
                int        ncells;
                matvar_t **cells = (matvar_t **)matvar->data;

                /* Check for an empty cell array */
                if ( matvar->nbytes == 0 || matvar->data_size == 0 ||
                     matvar->data   == NULL )
                    break;
                ncells  = matvar->nbytes / matvar->data_size;

                for ( i = 0; i < ncells; i++ )
                    WriteCellArrayFieldInfo(mat,cells[i]);
                break;
            }
            case MAT_C_STRUCT:
            {
                char *padzero;
                int maxlen = 0, fieldname_size;
                int nfields = matvar->internal->num_fields;
                matvar_t **fields = (matvar_t **)matvar->data;
                mat_int32_t  array_name_type = MAT_T_INT8;
                unsigned fieldname;

                for ( i = 0; i < nfields; i++ ) {
                    size_t len = strlen(matvar->internal->fieldnames[i]);
                    if ( len > maxlen )
                        maxlen = len;
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
                    size_t len = strlen(matvar->internal->fieldnames[i]);
                    fwrite(matvar->internal->fieldnames[i],1,len,mat->fp);
                    fwrite(padzero,1,fieldname_size-len,mat->fp);
                }
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
    } else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
#if defined(HAVE_ZLIB)
        mat_uint32_t comp_buf[512];
        mat_uint32_t uncomp_buf[512] = {0,};
        int buf_size = 512, err;
        size_t byteswritten = 0;

        matvar->internal->z         = malloc(sizeof(*matvar->internal->z));
        matvar->internal->z->zalloc = Z_NULL;
        matvar->internal->z->zfree  = Z_NULL;
        err = deflateInit(matvar->internal->z,Z_DEFAULT_COMPRESSION);

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
        matvar->internal->z->next_out  = comp_buf;
        matvar->internal->z->next_in   = uncomp_buf;
        matvar->internal->z->avail_out = buf_size*sizeof(*comp_buf);
        matvar->internal->z->avail_in  = 8;
        err = deflate(matvar->internal->z,Z_SYNC_FLUSH);
        byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-matvar->internal->z->avail_out,mat->fp);
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

        matvar->internal->z->next_out  = comp_buf;
        matvar->internal->z->next_in   = uncomp_buf;
        matvar->internal->z->avail_out = buf_size*sizeof(*comp_buf);
        matvar->internal->z->avail_in  = (6+i)*sizeof(*uncomp_buf);
        err = deflate(matvar->internal->z,Z_NO_FLUSH);
        byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-matvar->internal->z->avail_out,mat->fp);
        /* Name of variable */
        if ( strlen(matvar->name) <= 4 ) {
#if 0
            mat_int16_t array_name_len = (mat_int16_t)strlen(matvar->name);
            mat_int8_t  pad1 = 0;

            uncomp_buf[0] = (array_name_type << 16) | array_name_len;
            memcpy(uncomp_buf+1,matvar->name,array_name_len);

            matvar->internal->z->next_out  = comp_buf;
            matvar->internal->z->next_in   = uncomp_buf;
            matvar->internal->z->avail_out = buf_size*sizeof(*comp_buf);
            matvar->internal->z->avail_in  = 8;
            err = deflate(matvar->internal->z,Z_NO_FLUSH);
            byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-matvar->internal->z->avail_out,mat->fp);
        } else {
#endif
            mat_int32_t array_name_len = (mat_int32_t)strlen(matvar->name);

            memset(uncomp_buf,0,buf_size*sizeof(*uncomp_buf));
            uncomp_buf[0] = array_name_type;
            uncomp_buf[1] = array_name_len;
            memcpy(uncomp_buf+2,matvar->name,array_name_len);
            if ( array_name_len % 8 )
                array_name_len += array_name_len % 8;
            matvar->internal->z->next_out  = comp_buf;
            matvar->internal->z->next_in   = uncomp_buf;
            matvar->internal->z->avail_out = buf_size*sizeof(*comp_buf);
            matvar->internal->z->avail_in  = 8+array_name_len;
            err = deflate(matvar->internal->z,Z_FULL_FLUSH);
            byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-matvar->internal->z->avail_out,mat->fp);
        }
        matvar->internal->datapos = ftell(mat->fp);
        deflateCopy(&z_save,matvar->internal->z);
        switch ( matvar->class_type ) {
            case MAT_C_DOUBLE:
            case MAT_C_SINGLE:
            case MAT_C_INT32:
            case MAT_C_UINT32:
            case MAT_C_INT16:
            case MAT_C_UINT16:
            case MAT_C_INT8:
            case MAT_C_UINT8:
                byteswritten += WriteCompressedEmptyData(mat,matvar->internal->z,nmemb,matvar->data_type);
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
        matvar->internal->z->next_out  = comp_buf;
        matvar->internal->z->next_in   = NULL;
        matvar->internal->z->avail_out = buf_size*sizeof(*comp_buf);
        matvar->internal->z->avail_in  = 0;

        err = deflate(matvar->internal->z,Z_FINISH);
        byteswritten += fwrite(comp_buf,1,buf_size*sizeof(*comp_buf)-matvar->internal->z->avail_out,mat->fp);
                if ( byteswritten % 8 )
                    for ( i = byteswritten % 8; i < 8; i++ )
                        fwrite(&pad1,1,1,mat->fp);
        fprintf(stderr,"deflate Z_FINISH: err = %d,byteswritten = %u\n",err,byteswritten);

        err = deflateEnd(matvar->internal->z);
        fprintf(stderr,"deflateEnd: err = %d\n",err);
#if 1
        err = deflateEnd(matvar->internal->z);
        free(matvar->internal->z);
        matvar->internal->z = NULL;
#else
        memcpy(matvar->internal->z,&z_save,sizeof(*matvar->internal->z));
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

/** @if mat_devman
 * @brief Reads the header information for the next MAT variable
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @retuen pointer to the MAT variable or NULL
 * @endif
 */
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
        Mat_int32Swap(&data_type);
        Mat_int32Swap(&nBytes);
    }
    switch ( data_type ) {
#if defined(HAVE_ZLIB)
        case MAT_T_COMPRESSED:
        {
            mat_uint32_t uncomp_buf[16] = {0,};
            int      nbytes;

            matvar               = Mat_VarCalloc();
            matvar->name         = NULL;
            matvar->data         = NULL;
            matvar->dims         = NULL;
            matvar->nbytes       = 0;
            matvar->data_type    = MAT_T_UNKNOWN;
            matvar->class_type   = MAT_C_EMPTY;
            matvar->data_size    = 0;
            matvar->mem_conserve = 0;
            matvar->compression  = 1;

            matvar->internal->fp = mat;
            matvar->internal->fpos         = fpos;
            matvar->internal->z = calloc(1,sizeof(z_stream));
            matvar->internal->z->zalloc    = NULL;
            matvar->internal->z->zfree     = NULL;
            matvar->internal->z->opaque    = NULL;
            matvar->internal->z->next_in   = NULL;
            matvar->internal->z->next_out  = NULL;
            matvar->internal->z->avail_in  = 0;
            matvar->internal->z->avail_out = 0;
            err = inflateInit(matvar->internal->z);
            if ( err != Z_OK ) {
                Mat_Critical("inflateInit2 returned %d",err);
                Mat_VarFree(matvar);
                break;
            }

            /* Read Variable tag */
            bytesread += InflateVarTag(mat,matvar,uncomp_buf);
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(uncomp_buf);
                (void)Mat_uint32Swap(uncomp_buf+1);
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
                (void)Mat_uint32Swap(uncomp_buf);
                (void)Mat_uint32Swap(uncomp_buf+2);
                (void)Mat_uint32Swap(uncomp_buf+3);
            }
            /* Array Flags */
            if ( uncomp_buf[0] == MAT_T_UINT32 ) {
               array_flags = uncomp_buf[2];
               matvar->class_type  = CLASS_FROM_ARRAY_FLAGS(array_flags);
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
                (void)Mat_uint32Swap(uncomp_buf);
                (void)Mat_uint32Swap(uncomp_buf+1);
            }
            /* Rank and Dimension */
            if ( uncomp_buf[0] == MAT_T_INT32 ) {
                nbytes = uncomp_buf[1];
                matvar->rank = nbytes / 4;
                matvar->dims = malloc(matvar->rank*sizeof(*matvar->dims));
                if ( mat->byteswap ) {
                    for ( i = 0; i < matvar->rank; i++ )
                        matvar->dims[i] = Mat_uint32Swap(&(uncomp_buf[2+i]));
                } else {
                    for ( i = 0; i < matvar->rank; i++ )
                        matvar->dims[i] = uncomp_buf[2+i];
                }
            }
            /* Inflate variable name tag */
            bytesread += InflateVarNameTag(mat,matvar,uncomp_buf);
            if ( mat->byteswap )
                (void)Mat_uint32Swap(uncomp_buf);
            /* Name of variable */
            if ( uncomp_buf[0] == MAT_T_INT8 ) {    /* Name not in tag */
                int len;
                if ( mat->byteswap )
                    len = Mat_uint32Swap(uncomp_buf+1);
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
            fseek(mat->fp,-(int)matvar->internal->z->avail_in,SEEK_CUR);
            matvar->internal->datapos = ftell(mat->fp);
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
            matvar->internal->fpos = fpos;
            matvar->internal->fp   = mat;

            /* Read Array Flags and The Dimensions Tag */
            bytesread  += fread(buf,4,6,mat->fp);
            if ( mat->byteswap ) {
                (void)Mat_uint32Swap(buf);
                (void)Mat_uint32Swap(buf+1);
                (void)Mat_uint32Swap(buf+2);
                (void)Mat_uint32Swap(buf+3);
                (void)Mat_uint32Swap(buf+4);
                (void)Mat_uint32Swap(buf+5);
            }
            /* Array Flags */
            if ( buf[0] == MAT_T_UINT32 ) {
               array_flags = buf[2];
               matvar->class_type  = CLASS_FROM_ARRAY_FLAGS(array_flags);
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
                matvar->dims = malloc(matvar->rank*sizeof(*matvar->dims));

                /* Assumes rank <= 16 */
                if ( matvar->rank % 2 != 0 )
                    bytesread+=fread(buf,4,matvar->rank+1,mat->fp);
                else
                    bytesread+=fread(buf,4,matvar->rank,mat->fp);

                if ( mat->byteswap ) {
                    for ( i = 0; i < matvar->rank; i++ )
                        matvar->dims[i] = Mat_uint32Swap(buf+i);
                } else {
                    for ( i = 0; i < matvar->rank; i++ )
                        matvar->dims[i] = buf[i];
                }
            }
            /* Variable Name Tag */
            bytesread+=fread(buf,4,2,mat->fp);
            if ( mat->byteswap )
                (void)Mat_uint32Swap(buf);
            /* Name of variable */
            if ( buf[0] == MAT_T_INT8 ) {    /* Name not in tag */
                int len;

                if ( mat->byteswap )
                    len = Mat_uint32Swap(buf+1);
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
            matvar->internal->datapos = ftell(mat->fp);
            fseek(mat->fp,nBytes+8+fpos,SEEK_SET);
            break;
        }
        default:
            Mat_Message("%d is not valid (MAT_T_MATRIX or MAT_T_COMPRESSED", data_type);
            return NULL;
    }

    return matvar;
}
