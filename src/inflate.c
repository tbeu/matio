/** @file inflate.c
 * @brief Functions to inflate data/tags
 * @ingroup MAT
 */
/*
 * Copyright (c) 2005-2020, Christopher C. Hulbert
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

#include <stdlib.h>
#include "matio_private.h"

#if HAVE_ZLIB

/** @cond mat_devman */

/** @brief Inflate the data until @c nBytes of uncompressed data has been
 *         inflated
 *
 * @ingroup mat_internal
 * @param mat Pointer to the MAT file
 * @param z zlib compression stream
 * @param nBytes Number of uncompressed bytes to skip
 * @return Number of bytes read from the file
 */
size_t
InflateSkip(mat_t *mat, z_streamp z, int nBytes)
{
    mat_uint8_t comp_buf[READ_BLOCK_SIZE], uncomp_buf[READ_BLOCK_SIZE];
    int    n, err, cnt = 0;
    size_t bytesread = 0;

    if ( nBytes < 1 )
        return 0;

    n = nBytes < READ_BLOCK_SIZE ? nBytes : READ_BLOCK_SIZE;
    if ( !z->avail_in ) {
        size_t nbytes = fread(comp_buf, 1, n, (FILE*)mat->fp);
        if ( 0 == nbytes ) {
            return bytesread;
        }
        bytesread += nbytes;
        z->avail_in = (uInt)nbytes;
        z->next_in = comp_buf;
    }
    z->avail_out = n;
    z->next_out  = uncomp_buf;
    err = inflate(z,Z_FULL_FLUSH);
    if ( err == Z_STREAM_END ) {
        return bytesread;
    } else if ( err != Z_OK ) {
        Mat_Critical("InflateSkip: inflate returned %s",zError(err == Z_NEED_DICT ? Z_DATA_ERROR : err));
        return bytesread;
    }
    if ( !z->avail_out ) {
        cnt += n;
        n = nBytes - cnt < READ_BLOCK_SIZE ? nBytes - cnt : READ_BLOCK_SIZE;
        z->avail_out = n;
        z->next_out  = uncomp_buf;
    }
    while ( cnt < nBytes ) {
        if ( !z->avail_in ) {
            size_t nbytes = fread(comp_buf, 1, n, (FILE*)mat->fp);
            bytesread += nbytes;
            z->avail_in = (uInt)nbytes;
            if ( 0 == nbytes ) {
                break;
            }
            z->next_in = comp_buf;
        }
        err = inflate(z,Z_FULL_FLUSH);
        if ( err == Z_STREAM_END ) {
            break;
        } else if ( err != Z_OK ) {
            Mat_Critical("InflateSkip: inflate returned %s",zError(err == Z_NEED_DICT ? Z_DATA_ERROR : err));
            break;
        }
        if ( !z->avail_out ) {
            cnt += n;
            n = nBytes - cnt < READ_BLOCK_SIZE ? nBytes - cnt : READ_BLOCK_SIZE;
            z->avail_out = n;
            z->next_out  = uncomp_buf;
        }
    }

    if ( z->avail_in ) {
        long offset = -(long)z->avail_in;
        (void)fseek((FILE*)mat->fp,offset,SEEK_CUR);
        bytesread -= z->avail_in;
        z->avail_in = 0;
    }

    return bytesread;
}

/** @brief Inflate the data until @c len elements of compressed data with data
 *         type @c data_type has been inflated
 *
 * @ingroup mat_internal
 * @param mat Pointer to the MAT file
 * @param z zlib compression stream
 * @param data_type Data type (matio_types enumerations)
 * @param len Number of elements of datatype @c data_type to skip
 * @return Number of bytes read from the file
 */
size_t
InflateSkipData(mat_t *mat, z_streamp z, enum matio_types data_type, int len)
{
    if ( mat == NULL || z == NULL || len < 1 )
        return 0;

    switch ( data_type ) {
        case MAT_T_UTF8:
        case MAT_T_UTF16:
        case MAT_T_UTF32:
            return 0;
        default:
            break;
    }

    InflateSkip(mat, z, (unsigned int)Mat_SizeOf(data_type)*len);
    return len;
}

/** @brief Inflates the dimensions tag and the dimensions data
 *
 * @c buf must hold at least (8+4*rank) bytes where rank is the number of
 * dimensions. If the end of the dimensions data is not aligned on an 8-byte
 * boundary, this function eats up those bytes and stores then in @c buf.
 * @ingroup mat_internal
 * @param mat Pointer to the MAT file
 * @param z zlib compression stream
 * @param buf Pointer to store the dimensions flag and data
 * @param nBytes Size of buf in bytes
 * @param dims Output buffer to be allocated if (8+4*rank) > nBytes
 * @return Number of bytes read from the file
 */
size_t
InflateRankDims(mat_t *mat, z_streamp z, void *buf, size_t nBytes, mat_uint32_t** dims)
{
    mat_int32_t tag[2];
    int rank, i;
    size_t bytesread = 0;

    if ( buf == NULL )
        return 0;

    bytesread += Inflate(mat, z, buf, 8);
    tag[0] = *(int *)buf;
    tag[1] = *((int *)buf+1);
    if ( mat->byteswap ) {
        Mat_int32Swap(tag);
        Mat_int32Swap(tag+1);
    }
    if ( (tag[0] & 0x0000ffff) != MAT_T_INT32 ) {
        Mat_Critical("InflateRankDims: Reading dimensions expected type MAT_T_INT32");
        return bytesread;
    }
    rank = tag[1];
    if ( rank % 8 != 0 )
        i = 8-(rank %8);
    else
        i = 0;
    rank+=i;

    if ( sizeof(mat_uint32_t)*(rank + 2) <= nBytes ) {
        bytesread += Inflate(mat, z, (mat_int32_t *)buf+2, (unsigned int)rank);
    } else {
        /* Cannot use too small buf, but can allocate output buffer dims */
        *dims = (mat_uint32_t*)calloc(rank, sizeof(mat_uint32_t));
        if ( NULL != *dims ) {
            bytesread += Inflate(mat, z, *dims, (unsigned int)rank);
        } else {
            *((mat_int32_t *)buf+1) = 0;
            Mat_Critical("Error allocating memory for dims");
            return bytesread;
        }
    }

    return bytesread;
}

/** @brief Inflates the data
 *
 * @ingroup mat_internal
 * @param mat Pointer to the MAT file
 * @param z zlib compression stream
 * @param buf Pointer to store the variables name
 * @param nBytes Number of characters in the name
 * @return Number of bytes read from the file
 */
size_t
Inflate(mat_t *mat, z_streamp z, void *buf, unsigned int nBytes)
{
    mat_uint8_t comp_buf[32];
    int    err;
    size_t bytesread = 0;

    if ( buf == NULL )
        return 0;

    if ( !z->avail_in ) {
        size_t nbytes = fread(comp_buf, 1, 1, (FILE*)mat->fp);
        if ( 0 == nbytes ) {
            return bytesread;
        }
        bytesread += nbytes;
        z->avail_in = (uInt)nbytes;
        z->next_in = comp_buf;
    }
    z->avail_out = nBytes;
    z->next_out = ZLIB_BYTE_PTR(buf);
    err = inflate(z,Z_NO_FLUSH);
    if ( err != Z_OK ) {
        Mat_Critical("Inflate: inflate returned %s",zError(err == Z_NEED_DICT ? Z_DATA_ERROR : err));
        return bytesread;
    }
    while ( z->avail_out && !z->avail_in ) {
        size_t nbytes = fread(comp_buf, 1, 1, (FILE*)mat->fp);
        bytesread += nbytes;
        z->avail_in = (uInt)nbytes;
        if ( 0 == nbytes ) {
            break;
        }
        z->next_in = comp_buf;
        err = inflate(z,Z_NO_FLUSH);
        if ( err != Z_OK ) {
            Mat_Critical("Inflate: inflate returned %s",zError(err == Z_NEED_DICT ? Z_DATA_ERROR : err));
            return bytesread;
        }
    }

    if ( z->avail_in ) {
        (void)fseek((FILE*)mat->fp,-(int)z->avail_in,SEEK_CUR);
        bytesread -= z->avail_in;
        z->avail_in = 0;
    }

    if ( z->avail_out && feof((FILE*)mat->fp) ) {
        Mat_Critical("Inflate: Read beyond EOF error: Read %u bytes, expected %u bytes",
            nBytes - z->avail_out, nBytes);
    }

    return bytesread;
}

/** @brief Inflates the data in blocks
 *
 * buf must hold at least @c nBytes bytes
 * @ingroup mat_internal
 * @param mat Pointer to the MAT file
 * @param z zlib compression stream
 * @param buf Pointer to store the data type
 * @param nBytes Number of bytes to inflate
 * @return Number of bytes read from the file
 */
size_t
InflateData(mat_t *mat, z_streamp z, void *buf, unsigned int nBytes)
{
    mat_uint8_t comp_buf[READ_BLOCK_SIZE];
    int err;
    unsigned int n;
    size_t bytesread = 0;

    if ( buf == NULL )
        return 0;
    if ( nBytes == 0 ) {
        return bytesread;
    }

    n = nBytes < READ_BLOCK_SIZE ? nBytes : READ_BLOCK_SIZE;
    if ( !z->avail_in ) {
        size_t nbytes = fread(comp_buf, 1, n, (FILE*)mat->fp);
        if ( 0 == nbytes ) {
            return bytesread;
        }
        bytesread += nbytes;
        z->avail_in = (uInt)nbytes;
        z->next_in = comp_buf;
    }
    z->avail_out = nBytes;
    z->next_out = ZLIB_BYTE_PTR(buf);
    err = inflate(z,Z_FULL_FLUSH);
    if ( err == Z_STREAM_END ) {
        return bytesread;
    } else if ( err != Z_OK ) {
        Mat_Critical("InflateData: inflate returned %s",zError( err == Z_NEED_DICT ? Z_DATA_ERROR : err ));
        return bytesread;
    }
    while ( z->avail_out && !z->avail_in ) {
        size_t nbytes;
        if ( nBytes > READ_BLOCK_SIZE + bytesread ) {
            nbytes = fread(comp_buf, 1, READ_BLOCK_SIZE, (FILE*)mat->fp);
        } else if ( nBytes < 1 + bytesread ) { /* Read a byte at a time */
            nbytes = fread(comp_buf, 1, 1, (FILE*)mat->fp);
        } else {
            nbytes = fread(comp_buf, 1, nBytes - bytesread, (FILE*)mat->fp);
        }
        bytesread += nbytes;
        z->avail_in = (uInt)nbytes;
        if ( 0 == nbytes ) {
            break;
        }
        z->next_in = comp_buf;
        err = inflate(z,Z_FULL_FLUSH);
        if ( err == Z_STREAM_END ) {
            break;
        } else if ( err != Z_OK && err != Z_BUF_ERROR ) {
            Mat_Critical("InflateData: inflate returned %s",zError(err == Z_NEED_DICT ? Z_DATA_ERROR : err));
            break;
        }
    }

    if ( z->avail_in ) {
        const long offset = -(long)z->avail_in;
        (void)fseek((FILE*)mat->fp,offset,SEEK_CUR);
        bytesread -= z->avail_in;
        z->avail_in = 0;
    }

    if ( z->avail_out && feof((FILE*)mat->fp) ) {
        Mat_Critical("InflateData: Read beyond EOF error: Read %u bytes, expected %u bytes",
            nBytes - z->avail_out, nBytes);
    }

    return bytesread;
}

/** @endcond */

#endif
