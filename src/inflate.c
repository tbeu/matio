/** @file inflate.c
 * @brief Functions to inflate data/tags
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
#include <stdlib.h>
#include "matio.h"
#include "matio_private.h"

/* InflateSkip
 * Inflate the data until nbytes of uncompressed data has been inflated
 */
int 
InflateSkip(mat_t *mat, z_stream *z, int nbytes)
{
    mat_uint8_t comp_buf[32],uncomp_buf[32];
    int     bytesread = 0, err, cnt = 0;

    if ( nbytes < 1 )
        return 0;

    if ( !z->avail_in ) {
        z->avail_in = 1;
        z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
    }
    z->avail_out = 1;
    z->next_out = uncomp_buf;
    err = inflate(z,Z_NO_FLUSH);
    if ( err == Z_STREAM_END ) {
        return bytesread;
    } else if ( err != Z_OK ) {
        Mat_Critical("InflateSkip: inflate returned %d",err);
        return bytesread;
    }
    if ( !z->avail_out ) {
        z->avail_out = 1;
        z->next_out = uncomp_buf;
            cnt++;
    }
    while ( cnt < nbytes ) {
        if ( !z->avail_in ) {
            z->avail_in = 1;
            z->next_in = comp_buf;
            bytesread += fread(comp_buf,1,1,mat->fp);
        }
        err = inflate(z,Z_NO_FLUSH);
        if ( err == Z_STREAM_END ) {
            break;
        } else if ( err != Z_OK ) {
            Mat_Critical("InflateSkip: inflate returned %d",err);
            return bytesread;
        }
        if ( !z->avail_out ) {
            z->avail_out = 1;
            z->next_out = uncomp_buf;
            cnt++;
        }
    }

    if ( z->avail_in ) {
        fseek(mat->fp,-(int)z->avail_in,SEEK_CUR);
        bytesread -= z->avail_in;
        z->avail_in = 0;
    }

    return bytesread;
}

/*
 * Inflate the data until nbytes of compressed data has been inflated
 */
int
InflateSkip2(mat_t *mat, matvar_t *matvar, int nbytes)
{
    mat_uint8_t comp_buf[32],uncomp_buf[32];
    int     bytesread = 0, err, cnt = 0;

    if ( !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
    }
    matvar->z->avail_out = 1;
    matvar->z->next_out = uncomp_buf;
    err = inflate(matvar->z,Z_NO_FLUSH);
    if ( err != Z_OK ) {
        Mat_Critical("InflateSkip2: %s - inflate returned %d",matvar->name,err);
        return bytesread;
    }
    if ( !matvar->z->avail_out ) {
        matvar->z->avail_out = 1;
        matvar->z->next_out = uncomp_buf;
    }
    while ( cnt < nbytes ) {
        if ( !matvar->z->avail_in ) {
            matvar->z->avail_in = 1;
            matvar->z->next_in = comp_buf;
            bytesread += fread(comp_buf,1,1,mat->fp);
            cnt++;
        }
        err = inflate(matvar->z,Z_NO_FLUSH);
        if ( err != Z_OK ) {
            Mat_Critical("InflateSkip2: %s - inflate returned %d",matvar->name,err);
            return bytesread;
        }
        if ( !matvar->z->avail_out ) {
            matvar->z->avail_out = 1;
            matvar->z->next_out = uncomp_buf;
        }
    }

    if ( matvar->z->avail_in ) {
        fseek(mat->fp,-(int)matvar->z->avail_in,SEEK_CUR);
        bytesread -= matvar->z->avail_in;
        matvar->z->avail_in = 0;
    }

    return bytesread;
}

int
InflateSkipData(mat_t *mat,z_stream *z,int data_type,int len)
{
    int data_size = 0;

    if ( (mat == NULL) || (z == NULL) )
        return 0;
    else if ( len < 1 )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
            data_size = sizeof(double);
            break;
        case MAT_T_SINGLE:
            data_size = sizeof(float);
            break;
        case MAT_T_INT32:
            data_size = sizeof(mat_int32_t);
            break;
        case MAT_T_UINT32:
            data_size = sizeof(mat_uint32_t);
            break;
        case MAT_T_INT16:
            data_size = sizeof(mat_int16_t);
            break;
        case MAT_T_UINT16:
            data_size = sizeof(mat_uint16_t);
            break;
        case MAT_T_UINT8:
            data_size = sizeof(mat_uint8_t);
            break;
        case MAT_T_INT8:
            data_size = sizeof(mat_int8_t);
            break;
    }
    InflateSkip(mat,z,len*data_size);
    return len;
}

/*
 * Inflates the variable's tag.  buf must hold at least 8 bytes
 */
int
InflateVarTag(mat_t *mat, matvar_t *matvar, void *buf)
{
    mat_uint8_t comp_buf[32];
    int     bytesread = 0, err;

    if (buf == NULL)
        return 0;

    if ( !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
    }
    matvar->z->avail_out = 8;
    matvar->z->next_out = buf;
    err = inflate(matvar->z,Z_NO_FLUSH);
    if ( err != Z_OK ) {
        Mat_Critical("InflateVarTag: inflate returned %d",err);
        return bytesread;
    }
    while ( matvar->z->avail_out && !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
        err = inflate(matvar->z,Z_NO_FLUSH);
        if ( err != Z_OK ) {
            Mat_Critical("InflateVarTag: inflate returned %d",err);
            return bytesread;
        }
    }

    if ( matvar->z->avail_in ) {
        fseek(mat->fp,-(int)matvar->z->avail_in,SEEK_CUR);
        bytesread -= matvar->z->avail_in;
        matvar->z->avail_in = 0;
    }

    return bytesread;
}

/*
 * Inflates the Array Flags Tag and the Array Flags data.  buf must hold at
 * least 16 bytes
 */
int
InflateArrayFlags(mat_t *mat, matvar_t *matvar, void *buf)
{
    mat_uint8_t comp_buf[32];
    int     bytesread = 0, err;

    if (buf == NULL) return 0;

    if ( !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
    }
    matvar->z->avail_out = 16;
    matvar->z->next_out = buf;
    err = inflate(matvar->z,Z_NO_FLUSH);
    if ( err != Z_OK ) {
        Mat_Critical("InflateArrayFlags: inflate returned %d",err);
        return bytesread;
    }
    while ( matvar->z->avail_out && !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
        err = inflate(matvar->z,Z_NO_FLUSH);
        if ( err != Z_OK ) {
            Mat_Critical("InflateArrayFlags: inflate returned %d",err);
            return bytesread;
        }
    }

    if ( matvar->z->avail_in ) {
        fseek(mat->fp,-(int)matvar->z->avail_in,SEEK_CUR);
        bytesread -= matvar->z->avail_in;
        matvar->z->avail_in = 0;
    }

    return bytesread;
}

/*
 * Inflates the Dimensions Tag and the Dimensions data.  buf must hold at
 * least (8+4*rank) bytes.  If the end of the dimensions data is not aligned
 * on an 8-byte boundary, this function eats up those bytes and are stored in
 * buf.
 */
int
InflateDimensions(mat_t *mat, matvar_t *matvar, void *buf)
{
    mat_uint8_t comp_buf[32];
    mat_int32_t tag[2];
    int     bytesread = 0, err, rank, i;

    if ( buf == NULL )
        return 0;

    if ( !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
    }
    matvar->z->avail_out = 8;
    matvar->z->next_out = buf;
    err = inflate(matvar->z,Z_NO_FLUSH);
    if ( err != Z_OK ) {
        Mat_Critical("InflateDimensions: inflate returned %d",err);
        return bytesread;
    }
    while ( matvar->z->avail_out && !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
        err = inflate(matvar->z,Z_NO_FLUSH);
        if ( err != Z_OK ) {
            Mat_Critical("InflateDimensions: inflate returned %d",err);
            return bytesread;
        }
    }
    tag[0] = *(int *)buf;
    tag[1] = *((int *)buf+1);
    if ( mat->byteswap ) {
        int32Swap(tag);
        int32Swap(tag+1);
    }
    if ( (tag[0] & 0x0000ffff) != MAT_T_INT32 ) {
        Mat_Critical("InflateDimensions: Reading dimensions expected type MAT_T_INT32");
        return bytesread;
    }
    rank = tag[1];
    if ( rank % 8 != 0 )
        i = 8-(rank %8);
    else
        i = 0;
    rank+=i;

    if ( !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
    }
    matvar->z->avail_out = rank;
    matvar->z->next_out = (void *)((mat_int32_t *)buf+2);
    err = inflate(matvar->z,Z_NO_FLUSH);
    if ( err != Z_OK ) {
        Mat_Critical("InflateDimensions: inflate returned %d",err);
        return bytesread;
    }
    while ( matvar->z->avail_out && !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
        err = inflate(matvar->z,Z_NO_FLUSH);
        if ( err != Z_OK ) {
            Mat_Critical("InflateDimensions: inflate returned %d",err);
            return bytesread;
        }
    }

    if ( matvar->z->avail_in ) {
        fseek(mat->fp,-(int)matvar->z->avail_in,SEEK_CUR);
        bytesread -= matvar->z->avail_in;
        matvar->z->avail_in = 0;
    }

    return bytesread;
}

int
InflateVarNameTag(mat_t *mat, matvar_t *matvar, void *buf)
{
    mat_uint8_t comp_buf[32];
    int     bytesread = 0, err;

    if ( buf == NULL )
        return 0;

    if ( !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
    }
    matvar->z->avail_out = 8;
    matvar->z->next_out = buf;
    err = inflate(matvar->z,Z_NO_FLUSH);
    if ( err != Z_OK ) {
        Mat_Critical("InflateVarNameTag: inflate returned %d",err);
        return bytesread;
    }
    while ( matvar->z->avail_out && !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
        err = inflate(matvar->z,Z_NO_FLUSH);
        if ( err != Z_OK ) {
            Mat_Critical("InflateVarNameTag: inflate returned %d",err);
            return bytesread;
        }
    }

    if ( matvar->z->avail_in ) {
        fseek(mat->fp,-(int)matvar->z->avail_in,SEEK_CUR);
        bytesread -= matvar->z->avail_in;
        matvar->z->avail_in = 0;
    }

    return bytesread;
}

int
InflateVarName(mat_t *mat, matvar_t *matvar, void *buf, int N)
{
    mat_uint8_t comp_buf[32];
    int     bytesread = 0, err;

    if ( buf == NULL )
        return 0;

    if ( !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
    }
    matvar->z->avail_out = N;
    matvar->z->next_out = buf;
    err = inflate(matvar->z,Z_NO_FLUSH);
    if ( err != Z_OK ) {
        Mat_Critical("InflateVarName: inflate returned %d",err);
        return bytesread;
    }
    while ( matvar->z->avail_out && !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
        err = inflate(matvar->z,Z_NO_FLUSH);
        if ( err != Z_OK ) {
            Mat_Critical("InflateVarName: inflate returned %d",err);
            return bytesread;
        }
    }

    if ( matvar->z->avail_in ) {
        fseek(mat->fp,-(int)matvar->z->avail_in,SEEK_CUR);
        bytesread -= matvar->z->avail_in;
        matvar->z->avail_in = 0;
    }

    return bytesread;
}

/*
 * Inflates the data's tag.  buf must hold at least 8 bytes
 */
int
InflateDataTag(mat_t *mat, matvar_t *matvar, void *buf)
{
    mat_uint8_t comp_buf[32];
    int     bytesread = 0, err;

    if ( buf == NULL )
        return 0;

   if ( !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
    }
    matvar->z->avail_out = 8;
    matvar->z->next_out = buf;
    err = inflate(matvar->z,Z_NO_FLUSH);
    if ( err == Z_STREAM_END ) {
        return bytesread;
    } else if ( err != Z_OK ) {
        Mat_Critical("InflateDataTag: %s - inflate returned %d",matvar->name,err);
        return bytesread;
    }
    while ( matvar->z->avail_out && !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
        err = inflate(matvar->z,Z_NO_FLUSH);
        if ( err == Z_STREAM_END ) {
            break;
        } else if ( err != Z_OK ) {
            Mat_Critical("InflateDataTag: %s - inflate returned %d",matvar->name,err);
            return bytesread;
        }
    }

    if ( matvar->z->avail_in ) {
        fseek(mat->fp,-(int)matvar->z->avail_in,SEEK_CUR);
        bytesread -= matvar->z->avail_in;
        matvar->z->avail_in = 0;
    }

    return bytesread;
}

/*
 * Inflates the data's type.  buf must hold at least 4 bytes
 */
int
InflateDataType(mat_t *mat, matvar_t *matvar, void *buf)
{
    mat_uint8_t comp_buf[32];
    int     bytesread = 0, err;

    if ( buf == NULL )
        return 0;

    if ( !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
    }
    matvar->z->avail_out = 4;
    matvar->z->next_out = buf;
    err = inflate(matvar->z,Z_NO_FLUSH);
    if ( err != Z_OK ) {
        Mat_Critical("InflateDataTag: %s - inflate returned %d",matvar->name,err);
        return bytesread;
    }
    while ( matvar->z->avail_out && !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
        err = inflate(matvar->z,Z_NO_FLUSH);
        if ( err != Z_OK ) {
            Mat_Critical("InflateDataTag: %s - inflate returned %d",matvar->name,err);
            return bytesread;
        }
    }

    if ( matvar->z->avail_in ) {
        fseek(mat->fp,-(int)matvar->z->avail_in,SEEK_CUR);
        bytesread -= matvar->z->avail_in;
        matvar->z->avail_in = 0;
    }

    return bytesread;
}

/*
 * Inflates the Dimensions Tag and the Dimensions data.  buf must hold at
 * least nBytes bytes
 */
int
InflateData(mat_t *mat, z_stream *z, void *buf, int nBytes)
{
    mat_uint8_t comp_buf[1024];
    int     bytesread = 0, err;

    if ( buf == NULL )
        return 0;
    if ( nBytes < 1 ) {
        Mat_Critical("InflateData: nBytes must be > 0");
        return bytesread;
    }

    if ( !z->avail_in ) {
        if ( nBytes > 1024 ) {
            z->avail_in = fread(comp_buf,1,1024,mat->fp);
            bytesread += z->avail_in;
            z->next_in = comp_buf;
        } else {
            z->avail_in = fread(comp_buf,1,nBytes,mat->fp);
            bytesread  += z->avail_in;
            z->next_in  = comp_buf;
        }
    }
    z->avail_out = nBytes;
    z->next_out = buf;
    err = inflate(z,Z_FULL_FLUSH);
    if ( err == Z_STREAM_END ) {
        return bytesread;
    } else if ( err != Z_OK ) {
        Mat_Critical("InflateData: inflate returned %d",err);
        return bytesread;
    }
    while ( z->avail_out && !z->avail_in ) {
        if ( (nBytes-bytesread) > 1024 ) {
            z->avail_in = fread(comp_buf,1,1024,mat->fp);
            bytesread += z->avail_in;
            z->next_in = comp_buf;
        } else if ( (nBytes-bytesread) < 1 ) { /* Read a byte at a time */
            z->avail_in = fread(comp_buf,1,1,mat->fp);
            bytesread  += z->avail_in;
            z->next_in  = comp_buf;
        } else {
            z->avail_in = fread(comp_buf,1,nBytes,mat->fp);
            bytesread  += z->avail_in;
            z->next_in  = comp_buf;
        }
        err = inflate(z,Z_FULL_FLUSH);
        if ( err == Z_STREAM_END ) {
            break;
        } else if ( err != Z_OK && err != Z_BUF_ERROR ) {
            Mat_Critical("InflateData: inflate returned %d",err);
            return bytesread;
        }
    }

    if ( z->avail_in ) {
        long offset = -(long)z->avail_in;
        fseek(mat->fp,offset,SEEK_CUR);
        bytesread -= z->avail_in;
        z->avail_in = 0;
    }

    return bytesread;
}

/*
 * Inflates the structure's fieldname length.  buf must hold at least 8 bytes
 */
int
InflateFieldNameLength(mat_t *mat, matvar_t *matvar, void *buf)
{
    mat_uint8_t comp_buf[32];
    int     bytesread = 0, err;

    if ( buf == NULL )
        return 0;

    if ( !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
    }
    matvar->z->avail_out = 8;
    matvar->z->next_out = buf;
    err = inflate(matvar->z,Z_NO_FLUSH);
    if ( err != Z_OK ) {
        Mat_Critical("InflateFieldNameLength: inflate returned %d",err);
        return bytesread;
    }
    while ( matvar->z->avail_out && !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
        err = inflate(matvar->z,Z_NO_FLUSH);
        if ( err != Z_OK ) {
            Mat_Critical("InflateFieldNameLength: inflate returned %d",err);
            return bytesread;
        }
    }

    if ( matvar->z->avail_in ) {
        fseek(mat->fp,-(int)matvar->z->avail_in,SEEK_CUR);
        bytesread -= matvar->z->avail_in;
        matvar->z->avail_in = 0;
    }

    return bytesread;
}

/*
 * Inflates the structure's fieldname tag.  buf must hold at least 8 bytes
 */
int
InflateFieldNamesTag(mat_t *mat, matvar_t *matvar, void *buf)
{
    mat_uint8_t comp_buf[32];
    int     bytesread = 0, err;

    if ( buf == NULL )
        return 0;

    if ( !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
    }
    matvar->z->avail_out = 8;
    matvar->z->next_out = buf;
    err = inflate(matvar->z,Z_NO_FLUSH);
    if ( err != Z_OK ) {
        Mat_Critical("InflateFieldNamesTag: inflate returned %d",err);
        return bytesread;
    }
    while ( matvar->z->avail_out && !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
        err = inflate(matvar->z,Z_NO_FLUSH);
        if ( err != Z_OK ) {
            Mat_Critical("InflateFieldNamesTag: inflate returned %d",err);
            return bytesread;
        }
    }

    if ( matvar->z->avail_in ) {
        fseek(mat->fp,-(int)matvar->z->avail_in,SEEK_CUR);
        bytesread -= matvar->z->avail_in;
        matvar->z->avail_in = 0;
    }

    return bytesread;
}

/*
 * Inflates the structure's fieldname length.  buf must hold at least
 * nfields*fieldname_length bytes
 */
int
InflateFieldNames(mat_t *mat,matvar_t *matvar,void *buf,int nfields,
                  int fieldname_length,int padding)
{
    mat_uint8_t comp_buf[32];
    int     bytesread = 0, err;

    if ( buf == NULL )
        return 0;

    if ( !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
    }
    matvar->z->avail_out = nfields*fieldname_length+padding;
    matvar->z->next_out = buf;
    err = inflate(matvar->z,Z_NO_FLUSH);
    if ( err != Z_OK ) {
        Mat_Critical("InflateFieldNames: inflate returned %d",err);
        return bytesread;
    }
    while ( matvar->z->avail_out && !matvar->z->avail_in ) {
        matvar->z->avail_in = 1;
        matvar->z->next_in = comp_buf;
        bytesread += fread(comp_buf,1,1,mat->fp);
        err = inflate(matvar->z,Z_NO_FLUSH);
        if ( err != Z_OK ) {
            Mat_Critical("InflateFieldNames: inflate returned %d",err);
            return bytesread;
        }
    }

    if ( matvar->z->avail_in ) {
        fseek(mat->fp,-(int)matvar->z->avail_in,SEEK_CUR);
        bytesread -= matvar->z->avail_in;
        matvar->z->avail_in = 0;
    }

    return bytesread;
}
