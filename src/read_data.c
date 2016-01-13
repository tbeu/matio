/** @file read_data.c
 * Matlab MAT version 5 file functions
 * @ingroup MAT
 */
/*
 * Copyright (C) 2005-2016   Christopher C. Hulbert
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
#if defined(HAVE_ZLIB)
#   include <zlib.h>
#endif

/*
 * --------------------------------------------------------------------------
 *    Routines to read data of any type into arrays of a specific type
 * --------------------------------------------------------------------------
 */

/** @cond mat_devman */

/** @brief Reads data of type @c data_type into a double type
 *
 * Reads from the MAT file @c len elements of data type @c data_type storing
 * them as double's in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param data Pointer to store the output double values (len*sizeof(double))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadDoubleData(mat_t *mat,double *data,enum matio_types data_type,int len)
{
    int bytesread = 0, data_size = 0, i;

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            data_size = sizeof(double);
            if ( mat->byteswap ) {
                bytesread += fread(data,data_size,len,mat->fp);
                for ( i = 0; i < len; i++ ) {
                    (void)Mat_doubleSwap(data+i);
                }
            } else {
                bytesread += fread(data,data_size,len,mat->fp);
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&i8,data_size,1,mat->fp);
	      data[i] = i8;
	    }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&ui8,data_size,1,mat->fp);
	      data[i] = ui8;
	    }
            break;
        }
        default:
            return 0;
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
/** @brief Reads data of type @c data_type into a double type
 *
 * Reads from the MAT file @c len compressed elements of data type @c data_type
 * storing them as double's in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param z Pointer to the zlib stream for inflation
 * @param data Pointer to store the output double values (len*sizeof(double))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadCompressedDoubleData(mat_t *mat,z_stream *z,double *data,
    enum matio_types data_type,int len)
{
    int nBytes = 0, data_size = 0, i;
    union _buf {
#if SIZEOF_DOUBLE == 8
        double          d[128];
#elif SIZEOF_DOUBLE == 16
        double          d[64];
#endif
        float           f[256];
        mat_int32_t   i32[256];
        mat_uint32_t ui32[256];
        mat_int16_t   i16[512];
        mat_uint16_t ui16[512];
        mat_int8_t     i8[1024];
        mat_uint8_t   ui8[1024];
    } buf;


    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            data_size = sizeof(double);
            if ( mat->byteswap ) {
                InflateData(mat,z,data,len*data_size);
                for ( i = 0; i < len; i++ )
                    (void)Mat_doubleSwap(data+i);
            } else {
                InflateData(mat,z,data,len*data_size);
            }
            break;
        }
        case MAT_T_INT32:
        {
            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                if ( len <= 256 ){
                    InflateData(mat,z,buf.i32,len*data_size);
                    for ( i = 0; i < len; i++ )
                        data[i] = Mat_int32Swap(buf.i32+i);
                } else {
                    int j;
                    len -= 256;
                    for ( i = 0; i < len; i+=256 ) {
                        InflateData(mat,z,buf.i32,256*data_size);
                        for ( j = 0; j < 256; j++ )
                            data[i+j] = Mat_int32Swap(buf.i32+j);
                    }
                    len = len-(i-256);
                    InflateData(mat,z,buf.i32,len*data_size);
                    for ( j = 0; j < len; j++ )
                        data[i+j] = Mat_int32Swap(buf.i32+j);
                }
            } else {
                if ( len <= 256 ){
                    InflateData(mat,z,buf.i32,len*data_size);
                    for ( i = 0; i < len; i++ )
                        data[i] = buf.i32[i];
                } else {
                    int j;
                    len -= 256;
                    for ( i = 0; i < len; i+=256 ) {
                        InflateData(mat,z,buf.i32,256*data_size);
                        for ( j = 0; j < 256; j++ )
                            data[i+j] = buf.i32[j];
                    }
                    len = len-(i-256);
                    InflateData(mat,z,buf.i32,len*data_size);
                    for ( j = 0; j < len; j++ )
                        data[i+j] = buf.i32[j];
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                if ( len <= 256 ){
                    InflateData(mat,z,buf.ui32,len*data_size);
                    for ( i = 0; i < len; i++ )
                        data[i] = Mat_uint32Swap(buf.ui32+i);
                } else {
                    int j;
                    len -= 256;
                    for ( i = 0; i < len; i+=256 ) {
                        InflateData(mat,z,buf.ui32,256*data_size);
                        for ( j = 0; j < 256; j++ )
                            data[i+j] = Mat_uint32Swap(buf.ui32+j);
                    }
                    len = len-(i-256);
                    InflateData(mat,z,buf.ui32,len*data_size);
                    for ( j = 0; j < len; j++ )
                        data[i+j] = Mat_uint32Swap(buf.ui32+j);
                }
            } else {
                if ( len <= 256 ) {
                    InflateData(mat,z,buf.ui32,len*data_size);
                    for ( i = 0; i < len; i++ )
                        data[i] = buf.ui32[i];
                } else {
                    int j;
                    len -= 256;
                    for ( i = 0; i < len; i+=256 ) {
                        InflateData(mat,z,buf.ui32,256*data_size);
                        for ( j = 0; j < 256; j++ )
                            data[i+j] = buf.ui32[j];
                    }
                    len = len-(i-256);
                    InflateData(mat,z,buf.ui32,len*data_size);
                    for ( j = 0; j < len; j++ )
                        data[i+j] = buf.ui32[j];
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                if ( len <= 512 ){
                    InflateData(mat,z,buf.i16,len*data_size);
                    for ( i = 0; i < len; i++ )
                        data[i] = Mat_int16Swap(buf.i16+i);
                } else {
                    int j;
                    len -= 512;
                    for ( i = 0; i < len; i+=512 ) {
                        InflateData(mat,z,buf.i16,512*data_size);
                        for ( j = 0; j < 512; j++ )
                            data[i+j] = Mat_int16Swap(buf.i16+j);
                    }
                    len = len-(i-512);
                    InflateData(mat,z,buf.i16,len*data_size);
                    for ( j = 0; j < len; j++ )
                        data[i+j] = Mat_int16Swap(buf.i16+j);
                }
            } else {
                if ( len <= 512 ) {
                    InflateData(mat,z,buf.i16,len*data_size);
                    for ( i = 0; i < len; i++ )
                        data[i] = buf.i16[i];
                } else {
                    int j;
                    len -= 512;
                    for ( i = 0; i < len; i+=512 ) {
                        InflateData(mat,z,buf.i16,512*data_size);
                        for ( j = 0; j < 512; j++ )
                            data[i+j] = buf.i16[j];
                    }
                    len = len-(i-512);
                    InflateData(mat,z,buf.i16,len*data_size);
                    for ( j = 0; j < len; j++ )
                        data[i+j] = buf.i16[j];
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                if ( len <= 512 ){
                    InflateData(mat,z,buf.ui16,len*data_size);
                    for ( i = 0; i < len; i++ )
                        data[i] = Mat_uint16Swap(buf.ui16+i);
                } else {
                    int j;
                    len -= 512;
                    for ( i = 0; i < len; i+=512 ) {
                        InflateData(mat,z,buf.ui16,512*data_size);
                        for ( j = 0; j < 512; j++ )
                            data[i+j] = Mat_uint16Swap(buf.ui16+j);
                    }
                    len = len-(i-512);
                    InflateData(mat,z,buf.ui16,len*data_size);
                    for ( j = 0; j < len; j++ )
                        data[i+j] = Mat_uint16Swap(buf.ui16+j);
                }
            } else {
                if ( len <= 512 ) {
                    InflateData(mat,z,buf.ui16,len*data_size);
                    for ( i = 0; i < len; i++ )
                        data[i] = buf.ui16[i];
                } else {
                    int j;
                    len -= 512;
                    for ( i = 0; i < len; i+=512 ) {
                        InflateData(mat,z,buf.ui16,512*data_size);
                        for ( j = 0; j < 512; j++ )
                            data[i+j] = buf.ui16[j];
                    }
                    len = len-(i-512);
                    InflateData(mat,z,buf.ui16,len*data_size);
                    for ( j = 0; j < len; j++ )
                        data[i+j] = buf.ui16[j];
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            data_size = sizeof(mat_uint8_t);
            if ( len <= 1024 ) {
                InflateData(mat,z,buf.ui8,len*data_size);
                for ( i = 0; i < len; i++ )
                    data[i] = buf.ui8[i];
            } else {
                int j;
                len -= 1024;
                for ( i = 0; i < len; i+=1024 ) {
                    InflateData(mat,z,buf.ui8,1024*data_size);
                    for ( j = 0; j < 1024; j++ )
                        data[i+j] = buf.ui8[j];
                }
                len = len-(i-1024);
                InflateData(mat,z,buf.ui8,len*data_size);
                for ( j = 0; j < len; j++ )
                    data[i+j] = buf.ui8[j];
            }
            break;
        }
        case MAT_T_INT8:
        {
            data_size = sizeof(mat_int8_t);
            if ( len <= 1024 ) {
                InflateData(mat,z,buf.i8,len*data_size);
                for ( i = 0; i < len; i++ )
                    data[i] = buf.i8[i];
            } else {
                int j;
                len -= 1024;
                for ( i = 0; i < len; i+=1024 ) {
                    InflateData(mat,z,buf.i8,1024*data_size);
                    for ( j = 0; j < 1024; j++ )
                        data[i+j] = buf.i8[j];
                }
                len = len-(i-1024);
                InflateData(mat,z,buf.i8,len*data_size);
                for ( j = 0; j < len; j++ )
                    data[i+j] = buf.i8[j];
            }
            break;
        }
        default:
            return 0;
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

/** @brief Reads data of type @c data_type into a float type
 *
 * Reads from the MAT file @c len elements of data type @c data_type storing
 * them as float's in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param data Pointer to store the output float values (len*sizeof(float))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadSingleData(mat_t *mat,float *data,enum matio_types data_type,int len)
{
    int bytesread = 0, data_size = 0, i;

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&i8,data_size,1,mat->fp);
	      data[i] = i8;
	    }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&ui8,data_size,1,mat->fp);
	      data[i] = ui8;
	    }
            break;
        }
        default:
            return 0;
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
/** @brief Reads data of type @c data_type into a float type
 *
 * Reads from the MAT file @c len compressed elements of data type @c data_type
 * storing them as float's in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param z Pointer to the zlib stream for inflation
 * @param data Pointer to store the output float values (len*sizeof(float))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadCompressedSingleData(mat_t *mat,z_stream *z,float *data,
    enum matio_types data_type,int len)
{
    int nBytes = 0, data_size = 0, i;

    if ( (mat == NULL) || (data == NULL) || (z == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,data+i,data_size);
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&ui8,data_size);
                data[i] = ui8;
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&i8,data_size);
                data[i] = i8;
            }
            break;
        }
        default:
            return 0;
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

#ifdef HAVE_MAT_INT64_T
/** @brief Reads data of type @c data_type into a signed 64-bit integer type
 *
 * Reads from the MAT file @c len elements of data type @c data_type storing
 * them as signed 64-bit integers in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param data Pointer to store the output signed 64-bit integer values
 *             (len*sizeof(mat_int64_t))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadInt64Data(mat_t *mat,mat_int64_t *data,enum matio_types data_type,int len)
{
    int bytesread = 0, data_size = 0, i;

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT64:
        {
            mat_int64_t i64;

            data_size = sizeof(mat_int64_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i64,data_size,1,mat->fp);
                    data[i] = Mat_int64Swap(&i64);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i64,data_size,1,mat->fp);
                    data[i] = i64;
                }
            }
            break;
        }
#ifdef HAVE_MAT_UINT64_T
        case MAT_T_UINT64:
        {
            mat_uint64_t ui64;

            data_size = sizeof(mat_uint64_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui64,data_size,1,mat->fp);
                    data[i] = Mat_uint64Swap(&ui64);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui64,data_size,1,mat->fp);
                    data[i] = ui64;
                }
            }
            break;
        }
#endif /* HAVE_MAT_UINT64_T */
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&i8,data_size,1,mat->fp);
	      data[i] = i8;
	    }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&ui8,data_size,1,mat->fp);
	      data[i] = ui8;
	    }
            break;
        }
        default:
            return 0;
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
/** @brief Reads data of type @c data_type into a signed 64-bit integer type
 *
 * Reads from the MAT file @c len compressed elements of data type @c data_type
 * storing them as signed 64-bit integers in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param z Pointer to the zlib stream for inflation
 * @param data Pointer to store the output signed 64-bit integer values
 *             (len*sizeof(mat_int64_t))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadCompressedInt64Data(mat_t *mat,z_stream *z,mat_int64_t *data,
    enum matio_types data_type,int len)
{
    int nBytes = 0, data_size = 0, i;

    if ( (mat == NULL) || (data == NULL) || (z == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT64:
        {
            mat_int64_t i64;

            data_size = sizeof(mat_int64_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i64,data_size);
                    data[i] = Mat_int64Swap(&i64);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i64,data_size);
                    data[i] = i64;
                }
            }
            break;
        }
        case MAT_T_UINT64:
        {
            mat_uint64_t ui64;

            data_size = sizeof(mat_uint64_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui64,data_size);
                    data[i] = Mat_uint64Swap(&ui64);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui64,data_size);
                    data[i] = ui64;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&ui8,data_size);
                data[i] = ui8;
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&i8,data_size);
                data[i] = i8;
            }
            break;
        }
        default:
            return 0;
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif
#endif /* HAVE_MAT_INT64_T */

#ifdef HAVE_MAT_UINT64_T
/** @brief Reads data of type @c data_type into an unsigned 64-bit integer type
 *
 * Reads from the MAT file @c len elements of data type @c data_type storing
 * them as unsigned 64-bit integers in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param data Pointer to store the output unsigned 64-bit integer values
 *             (len*sizeof(mat_uint64_t))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadUInt64Data(mat_t *mat,mat_uint64_t *data,enum matio_types data_type,int len)
{
    int bytesread = 0, data_size = 0, i;

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = f;
                }
            }
            break;
        }
#ifdef HAVE_MAT_INT64_T
        case MAT_T_INT64:
        {
            mat_int64_t i64;

            data_size = sizeof(mat_int64_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i64,data_size,1,mat->fp);
                    data[i] = Mat_int64Swap(&i64);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i64,data_size,1,mat->fp);
                    data[i] = i64;
                }
            }
            break;
        }
#endif /* HAVE_MAT_INT64_T */
        case MAT_T_UINT64:
        {
            mat_uint64_t ui64;

            data_size = sizeof(mat_uint64_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui64,data_size,1,mat->fp);
                    data[i] = Mat_uint64Swap(&ui64);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui64,data_size,1,mat->fp);
                    data[i] = ui64;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&i8,data_size,1,mat->fp);
	      data[i] = i8;
	    }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&ui8,data_size,1,mat->fp);
	      data[i] = ui8;
	    }
            break;
        }
        default:
            return 0;
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
/** @brief Reads data of type @c data_type into an unsigned 64-bit integer type
 *
 * Reads from the MAT file @c len compressed elements of data type @c data_type
 * storing them as unsigned 64-bit integers in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param z Pointer to the zlib stream for inflation
 * @param data Pointer to store the output unsigned 64-bit integer values
 *             (len*sizeof(mat_uint64_t))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadCompressedUInt64Data(mat_t *mat,z_stream *z,mat_uint64_t *data,
    enum matio_types data_type,int len)
{
    int nBytes = 0, data_size = 0, i;

    if ( (mat == NULL) || (data == NULL) || (z == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT64:
        {
            mat_int64_t i64;

            data_size = sizeof(mat_int64_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i64,data_size);
                    data[i] = Mat_int64Swap(&i64);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i64,data_size);
                    data[i] = i64;
                }
            }
            break;
        }
        case MAT_T_UINT64:
        {
            mat_uint64_t ui64;

            data_size = sizeof(mat_uint64_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui64,data_size);
                    data[i] = Mat_uint64Swap(&ui64);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui64,data_size);
                    data[i] = ui64;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&ui8,data_size);
                data[i] = ui8;
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&i8,data_size);
                data[i] = i8;
            }
            break;
        }
        default:
            return 0;
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif /* HAVE_ZLIB */
#endif /* HAVE_MAT_UINT64_T */

/** @brief Reads data of type @c data_type into a signed 32-bit integer type
 *
 * Reads from the MAT file @c len elements of data type @c data_type storing
 * them as signed 32-bit integers in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param data Pointer to store the output signed 32-bit integer values
 *             (len*sizeof(mat_int32_t))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadInt32Data(mat_t *mat,mat_int32_t *data,enum matio_types data_type,int len)
{
    int bytesread = 0, data_size = 0, i;

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&i8,data_size,1,mat->fp);
	      data[i] = i8;
	    }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&ui8,data_size,1,mat->fp);
	      data[i] = ui8;
	    }
            break;
        }
        default:
            return 0;
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
/** @brief Reads data of type @c data_type into a signed 32-bit integer type
 *
 * Reads from the MAT file @c len compressed elements of data type @c data_type
 * storing them as signed 32-bit integers in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param z Pointer to the zlib stream for inflation
 * @param data Pointer to store the output signed 32-bit integer values
 *             (len*sizeof(mat_int32_t))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadCompressedInt32Data(mat_t *mat,z_stream *z,mat_int32_t *data,
    enum matio_types data_type,int len)
{
    int nBytes = 0, data_size = 0, i;

    if ( (mat == NULL) || (data == NULL) || (z == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&ui8,data_size);
                data[i] = ui8;
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&i8,data_size);
                data[i] = i8;
            }
            break;
        }
        default:
            return 0;
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

/** @brief Reads data of type @c data_type into an unsigned 32-bit integer type
 *
 * Reads from the MAT file @c len elements of data type @c data_type storing
 * them as unsigned 32-bit integers in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param data Pointer to store the output unsigned 32-bit integer values
 *             (len*sizeof(mat_uint32_t))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadUInt32Data(mat_t *mat,mat_uint32_t *data,enum matio_types data_type,int len)
{
    int bytesread = 0, data_size = 0, i;

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&i8,data_size,1,mat->fp);
	      data[i] = i8;
	    }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&ui8,data_size,1,mat->fp);
	      data[i] = ui8;
	    }
            break;
        }
        default:
            return 0;
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
/** @brief Reads data of type @c data_type into an unsigned 32-bit integer type
 *
 * Reads from the MAT file @c len compressed elements of data type @c data_type
 * storing them as unsigned 32-bit integers in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param z Pointer to the zlib stream for inflation
 * @param data Pointer to store the output unsigned 32-bit integer values
 *             (len*sizeof(mat_uint32_t))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadCompressedUInt32Data(mat_t *mat,z_stream *z,mat_uint32_t *data,
    enum matio_types data_type,int len)
{
    int nBytes = 0, data_size = 0, i;

    if ( (mat == NULL) || (data == NULL) || (z == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&ui8,data_size);
                data[i] = ui8;
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&i8,data_size);
                data[i] = i8;
            }
            break;
        }
        default:
            return 0;
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

/** @brief Reads data of type @c data_type into a signed 16-bit integer type
 *
 * Reads from the MAT file @c len elements of data type @c data_type storing
 * them as signed 16-bit integers in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param data Pointer to store the output signed 16-bit integer values
 *             (len*sizeof(mat_int16_t))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadInt16Data(mat_t *mat,mat_int16_t *data,enum matio_types data_type,int len)
{
    int bytesread = 0, data_size = 0, i;

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&i8,data_size,1,mat->fp);
	      data[i] = i8;
	    }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&ui8,data_size,1,mat->fp);
	      data[i] = ui8;
	    }
            break;
        }
        default:
            return 0;
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
/** @brief Reads data of type @c data_type into a signed 16-bit integer type
 *
 * Reads from the MAT file @c len compressed elements of data type @c data_type
 * storing them as signed 16-bit integers in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param z Pointer to the zlib stream for inflation
 * @param data Pointer to store the output signed 16-bit integer values
 *             (len*sizeof(mat_int16_t))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadCompressedInt16Data(mat_t *mat,z_stream *z,mat_int16_t *data,
    enum matio_types data_type,int len)
{
    int nBytes = 0, data_size = 0, i;

    if ( (mat == NULL) || (data == NULL) || (z == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&ui8,data_size);
                data[i] = ui8;
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&i8,data_size);
                data[i] = i8;
            }
            break;
        }
        default:
            return 0;
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

/** @brief Reads data of type @c data_type into an unsigned 16-bit integer type
 *
 * Reads from the MAT file @c len elements of data type @c data_type storing
 * them as unsigned 16-bit integers in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param data Pointer to store the output unsigned 16-bit integer values
 *             (len*sizeof(mat_uint16_t))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadUInt16Data(mat_t *mat,mat_uint16_t *data,enum matio_types data_type,int len)
{
    int bytesread = 0, data_size = 0, i;

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&i8,data_size,1,mat->fp);
	      data[i] = i8;
	    }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&ui8,data_size,1,mat->fp);
	      data[i] = ui8;
	    }
            break;
        }
        default:
            return 0;
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
/** @brief Reads data of type @c data_type into an unsigned 16-bit integer type
 *
 * Reads from the MAT file @c len compressed elements of data type @c data_type
 * storing them as unsigned 16-bit integers in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param z Pointer to the zlib stream for inflation
 * @param data Pointer to store the output n unsigned 16-bit integer values
 *             (len*sizeof(mat_uint16_t))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadCompressedUInt16Data(mat_t *mat,z_stream *z,mat_uint16_t *data,
    enum matio_types data_type,int len)
{
    int nBytes = 0, data_size = 0, i;

    if ( (mat == NULL) || (data == NULL) || (z == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&ui8,data_size);
                data[i] = ui8;
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&i8,data_size);
                data[i] = i8;
            }
            break;
        }
        default:
            return 0;
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

/** @brief Reads data of type @c data_type into a signed 8-bit integer type
 *
 * Reads from the MAT file @c len elements of data type @c data_type storing
 * them as signed 8-bit integers in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param data Pointer to store the output signed 8-bit integer values
 *             (len*sizeof(mat_int8_t))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadInt8Data(mat_t *mat,mat_int8_t *data,enum matio_types data_type,int len)
{
    int bytesread = 0, data_size = 0, i;

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&i8,data_size,1,mat->fp);
	      data[i] = i8;
	    }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&ui8,data_size,1,mat->fp);
	      data[i] = ui8;
	    }
            break;
        }
        default:
            return 0;
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
/** @brief Reads data of type @c data_type into a signed 8-bit integer type
 *
 * Reads from the MAT file @c len compressed elements of data type @c data_type
 * storing them as signed 8-bit integers in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param z Pointer to the zlib stream for inflation
 * @param data Pointer to store the output signed 8-bit integer values
 *             (len*sizeof(mat_int8_t))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadCompressedInt8Data(mat_t *mat,z_stream *z,mat_int8_t *data,
    enum matio_types data_type,int len)
{
    int nBytes = 0, data_size = 0, i;

    if ( (mat == NULL) || (data == NULL) || (z == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&ui8,data_size);
                data[i] = ui8;
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&i8,data_size);
                data[i] = i8;
            }
            break;
        }
        default:
            return 0;
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

/** @brief Reads data of type @c data_type into an unsigned 8-bit integer type
 *
 * Reads from the MAT file @c len elements of data type @c data_type storing
 * them as unsigned 8-bit integers in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param data Pointer to store the output unsigned 8-bit integer values
 *             (len*sizeof(mat_uint8_t))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadUInt8Data(mat_t *mat,mat_uint8_t *data,enum matio_types data_type,int len)
{
    int bytesread = 0, data_size = 0, i;

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&d,data_size,1,mat->fp);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&f,data_size,1,mat->fp);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i32,data_size,1,mat->fp);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui32,data_size,1,mat->fp);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,data_size,1,mat->fp);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui16,data_size,1,mat->fp);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&i8,data_size,1,mat->fp);
	      data[i] = i8;
	    }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
	    for ( i = 0; i < len; i++ ) {
	      bytesread += fread(&ui8,data_size,1,mat->fp);
	      data[i] = ui8;
	    }
            break;
        }
        default:
            return 0;
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
/** @brief Reads data of type @c data_type into an unsigned 8-bit integer type
 *
 * Reads from the MAT file @c len compressed elements of data type @c data_type
 * storing them as unsigned 8-bit integers in @c data.
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param z Pointer to the zlib stream for inflation
 * @param data Pointer to store the output 8-bit integer values
 *             (len*sizeof(mat_uint8_t))
 * @param data_type one of the @c matio_types enumerations which is the source
 *                  data type in the file
 * @param len Number of elements of type @c data_type to read from the file
 * @retval Number of bytes read from the file
 */
int
ReadCompressedUInt8Data(mat_t *mat,z_stream *z,mat_uint8_t *data,
    enum matio_types data_type,int len)
{
    int nBytes = 0, data_size = 0, i;

    if ( (mat == NULL) || (data == NULL) || (z == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_DOUBLE:
        {
            double d;

            data_size = sizeof(double);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = Mat_doubleSwap(&d);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&d,data_size);
                    data[i] = d;
                }
            }
            break;
        }
        case MAT_T_SINGLE:
        {
            float f;

            data_size = sizeof(float);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = Mat_floatSwap(&f);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&f,data_size);
                    data[i] = f;
                }
            }
            break;
        }
        case MAT_T_INT32:
        {
            mat_int32_t i32;

            data_size = sizeof(mat_int32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = Mat_int32Swap(&i32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i32,data_size);
                    data[i] = i32;
                }
            }
            break;
        }
        case MAT_T_UINT32:
        {
            mat_uint32_t ui32;

            data_size = sizeof(mat_uint32_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = Mat_uint32Swap(&ui32);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui32,data_size);
                    data[i] = ui32;
                }
            }
            break;
        }
        case MAT_T_INT16:
        {
            mat_int16_t i16;

            data_size = sizeof(mat_int16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = Mat_int16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = i16;
                }
            }
            break;
        }
        case MAT_T_UINT16:
        {
            mat_uint16_t ui16;

            data_size = sizeof(mat_uint16_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = Mat_uint16Swap(&ui16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&ui16,data_size);
                    data[i] = ui16;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&ui8,data_size);
                data[i] = ui8;
            }
            break;
        }
        case MAT_T_INT8:
        {
            mat_int8_t i8;

            data_size = sizeof(mat_int8_t);
            for ( i = 0; i < len; i++ ) {
                InflateData(mat,z,&i8,data_size);
                data[i] = i8;
            }
            break;
        }
        default:
            return 0;
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

#if defined(HAVE_ZLIB)
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
ReadCompressedCharData(mat_t *mat,z_stream *z,char *data,
    enum matio_types data_type,int len)
{
    int nBytes = 0, data_size = 0, i;

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_UTF8:
            data_size = 1;
            for ( i = 0; i < len; i++ )
                InflateData(mat,z,data+i,data_size);
            break;
        case MAT_T_INT8:
        case MAT_T_UINT8:
            data_size = 1;
            for ( i = 0; i < len; i++ )
                InflateData(mat,z,data+i,data_size);
            break;
        case MAT_T_INT16:
        case MAT_T_UINT16:
        {
            mat_uint16_t i16;

            data_size = 2;
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = Mat_uint16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    InflateData(mat,z,&i16,data_size);
                    data[i] = i16;
                }
            }
            break;
        }
        default:
            printf("Character data not supported type: %d",data_type);
            break;
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

int
ReadCharData(mat_t *mat,char *data,enum matio_types data_type,int len)
{
    int bytesread = 0, data_size = 0, i;

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) )
        return 0;

    switch ( data_type ) {
        case MAT_T_UTF8:
            for ( i = 0; i < len; i++ )
                bytesread += fread(data+i,1,1,mat->fp);
            break;
        case MAT_T_INT8:
        case MAT_T_UINT8:
            for ( i = 0; i < len; i++ )
                bytesread += fread(data+i,1,1,mat->fp);
            break;
        case MAT_T_INT16:
        case MAT_T_UINT16:
        {
            mat_uint16_t i16;

            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,2,1,mat->fp);
                    data[i] = Mat_uint16Swap(&i16);
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i16,2,1,mat->fp);
                    data[i] = i16;
                }
            }
            break;
        }
        default:
            printf("Character data not supported type: %d",data_type);
            break;
    }
    bytesread *= data_size;
    return bytesread;
}

/*
 *-------------------------------------------------------------------
 *  Routines to read "slabs" of data
 *-------------------------------------------------------------------
 */

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
ReadDataSlabN(mat_t *mat,void *data,enum matio_classes class_type,
    enum matio_types data_type,int rank,size_t *dims,int *start,int *stride,
    int *edge)
{
    int nBytes = 0, i, j, N, I = 0;
    int inc[10] = {0,}, cnt[10] = {0,}, dimp[10] = {0,};
    size_t data_size;

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) ||
         (start == NULL) || (stride == NULL) || (edge    == NULL) ) {
        return -1;
    } else if ( rank > 10 ) {
        return -1;
    }

    data_size = Mat_SizeOf(data_type);
    switch ( class_type ) {
        case MAT_C_DOUBLE:
        {
            double *ptr = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0; /* start[0]; */
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            (void)fseek(mat->fp,I*data_size,SEEK_CUR);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    ReadDoubleData(mat,ptr+i,data_type,edge[0]);
                    I += dims[0]-start[0];
                    (void)fseek(mat->fp,data_size*(dims[0]-edge[0]-start[0]),
                          SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j])+
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]; j++ ) {
                        ReadDoubleData(mat,ptr+i+j,data_type,1);
                        (void)fseek(mat->fp,data_size*(stride[0]-1),SEEK_CUR);
                        I += stride[0];
                    }
                    I += dims[0]-edge[0]*stride[0]-start[0];
                    (void)fseek(mat->fp,data_size*
                          (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j]) +
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            }
            break;
        }
        case MAT_C_SINGLE:
        {
            float *ptr = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0; /* start[0]; */
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            (void)fseek(mat->fp,I*data_size,SEEK_CUR);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    ReadSingleData(mat,ptr+i,data_type,edge[0]);
                    I += dims[0]-start[0];
                    (void)fseek(mat->fp,data_size*(dims[0]-edge[0]-start[0]),
                          SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j])+
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]; j++ ) {
                        ReadSingleData(mat,ptr+i+j,data_type,1);
                        (void)fseek(mat->fp,data_size*(stride[0]-1),SEEK_CUR);
                        I += stride[0];
                    }
                    I += dims[0]-edge[0]*stride[0]-start[0];
                    (void)fseek(mat->fp,data_size*
                          (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j]) +
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            }
            break;
        }
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
        {
            mat_int64_t *ptr = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0; /* start[0]; */
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            (void)fseek(mat->fp,I*data_size,SEEK_CUR);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    ReadInt64Data(mat,ptr+i,data_type,edge[0]);
                    I += dims[0]-start[0];
                    (void)fseek(mat->fp,data_size*(dims[0]-edge[0]-start[0]),
                          SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j])+
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]; j++ ) {
                        ReadInt64Data(mat,ptr+i+j,data_type,1);
                        (void)fseek(mat->fp,data_size*(stride[0]-1),SEEK_CUR);
                        I += stride[0];
                    }
                    I += dims[0]-edge[0]*stride[0]-start[0];
                    (void)fseek(mat->fp,data_size*
                          (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j]) +
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            }
            break;
        }
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
        {
            mat_uint64_t *ptr = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0; /* start[0]; */
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            (void)fseek(mat->fp,I*data_size,SEEK_CUR);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    ReadUInt64Data(mat,ptr+i,data_type,edge[0]);
                    I += dims[0]-start[0];
                    (void)fseek(mat->fp,data_size*(dims[0]-edge[0]-start[0]),
                          SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j])+
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]; j++ ) {
                        ReadUInt64Data(mat,ptr+i+j,data_type,1);
                        (void)fseek(mat->fp,data_size*(stride[0]-1),SEEK_CUR);
                        I += stride[0];
                    }
                    I += dims[0]-edge[0]*stride[0]-start[0];
                    (void)fseek(mat->fp,data_size*
                          (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j]) +
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            }
            break;
        }
#endif /* HAVE_MAT_UINT64_T */
        case MAT_C_INT32:
        {
            mat_int32_t *ptr = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0; /* start[0]; */
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            (void)fseek(mat->fp,I*data_size,SEEK_CUR);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    ReadInt32Data(mat,ptr+i,data_type,edge[0]);
                    I += dims[0]-start[0];
                    (void)fseek(mat->fp,data_size*(dims[0]-edge[0]-start[0]),
                          SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j])+
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]; j++ ) {
                        ReadInt32Data(mat,ptr+i+j,data_type,1);
                        (void)fseek(mat->fp,data_size*(stride[0]-1),SEEK_CUR);
                        I += stride[0];
                    }
                    I += dims[0]-edge[0]*stride[0]-start[0];
                    (void)fseek(mat->fp,data_size*
                          (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j]) +
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            }
            break;
        }
        case MAT_C_UINT32:
        {
            mat_uint32_t *ptr = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0; /* start[0]; */
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            (void)fseek(mat->fp,I*data_size,SEEK_CUR);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    ReadUInt32Data(mat,ptr+i,data_type,edge[0]);
                    I += dims[0]-start[0];
                    (void)fseek(mat->fp,data_size*(dims[0]-edge[0]-start[0]),
                          SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j])+
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]; j++ ) {
                        ReadUInt32Data(mat,ptr+i+j,data_type,1);
                        (void)fseek(mat->fp,data_size*(stride[0]-1),SEEK_CUR);
                        I += stride[0];
                    }
                    I += dims[0]-edge[0]*stride[0]-start[0];
                    (void)fseek(mat->fp,data_size*
                          (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j]) +
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            }
            break;
        }
        case MAT_C_INT16:
        {
            mat_int16_t *ptr = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0; /* start[0]; */
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            (void)fseek(mat->fp,I*data_size,SEEK_CUR);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    ReadInt16Data(mat,ptr+i,data_type,edge[0]);
                    I += dims[0]-start[0];
                    (void)fseek(mat->fp,data_size*(dims[0]-edge[0]-start[0]),
                          SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j])+
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]; j++ ) {
                        ReadInt16Data(mat,ptr+i+j,data_type,1);
                        (void)fseek(mat->fp,data_size*(stride[0]-1),SEEK_CUR);
                        I += stride[0];
                    }
                    I += dims[0]-edge[0]*stride[0]-start[0];
                    (void)fseek(mat->fp,data_size*
                          (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j]) +
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            }
            break;
        }
        case MAT_C_UINT16:
        {
            mat_uint16_t *ptr = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0; /* start[0]; */
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            (void)fseek(mat->fp,I*data_size,SEEK_CUR);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    ReadUInt16Data(mat,ptr+i,data_type,edge[0]);
                    I += dims[0]-start[0];
                    (void)fseek(mat->fp,data_size*(dims[0]-edge[0]-start[0]),
                          SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j])+
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]; j++ ) {
                        ReadUInt16Data(mat,ptr+i+j,data_type,1);
                        (void)fseek(mat->fp,data_size*(stride[0]-1),SEEK_CUR);
                        I += stride[0];
                    }
                    I += dims[0]-edge[0]*stride[0]-start[0];
                    (void)fseek(mat->fp,data_size*
                          (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j]) +
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            }
            break;
        }
        case MAT_C_INT8:
        {
            mat_int8_t *ptr = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0; /* start[0]; */
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            (void)fseek(mat->fp,I*data_size,SEEK_CUR);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    ReadInt8Data(mat,ptr+i,data_type,edge[0]);
                    I += dims[0]-start[0];
                    (void)fseek(mat->fp,data_size*(dims[0]-edge[0]-start[0]),
                          SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j])+
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]; j++ ) {
                        ReadInt8Data(mat,ptr+i+j,data_type,1);
                        (void)fseek(mat->fp,data_size*(stride[0]-1),SEEK_CUR);
                        I += stride[0];
                    }
                    I += dims[0]-edge[0]*stride[0]-start[0];
                    (void)fseek(mat->fp,data_size*
                          (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j]) +
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            }
            break;
        }
        case MAT_C_UINT8:
        {
            mat_uint8_t *ptr = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0; /* start[0]; */
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            (void)fseek(mat->fp,I*data_size,SEEK_CUR);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    ReadUInt8Data(mat,ptr+i,data_type,edge[0]);
                    I += dims[0]-start[0];
                    (void)fseek(mat->fp,data_size*(dims[0]-edge[0]-start[0]),
                          SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j])+
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]; j++ ) {
                        ReadUInt8Data(mat,ptr+i+j,data_type,1);
                        (void)fseek(mat->fp,data_size*(stride[0]-1),SEEK_CUR);
                        I += stride[0];
                    }
                    I += dims[0]-edge[0]*stride[0]-start[0];
                    (void)fseek(mat->fp,data_size*
                          (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                (void)fseek(mat->fp,data_size*
                                      (dimp[j]-(I % dimp[j]) +
                                       dimp[j-1]*start[j]),SEEK_CUR);
                                I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                            } else if ( start[j] ) {
                                (void)fseek(mat->fp,data_size*(dimp[j-1]*start[j]),
                                      SEEK_CUR);
                                I += dimp[j-1]*start[j];
                            }
                        } else {
                            I += inc[j];
                            (void)fseek(mat->fp,data_size*inc[j],SEEK_CUR);
                            break;
                        }
                    }
                }
            }
            break;
        }
        default:
            nBytes = 0;
    }
    return nBytes;
}

#if defined(HAVE_ZLIB)
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
ReadCompressedDataSlabN(mat_t *mat,z_stream *z,void *data,
    enum matio_classes class_type,enum matio_types data_type,int rank,
    size_t *dims,int *start,int *stride,int *edge)
{
    int nBytes = 0, i, j, N, I = 0;
    int inc[10] = {0,}, cnt[10] = {0,}, dimp[10] = {0,};
    z_stream z_copy = {0,};

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) ||
         (start == NULL) || (stride == NULL) || (edge    == NULL) ) {
        return 1;
    } else if ( rank > 10 ) {
        return 1;
    }

    i = inflateCopy(&z_copy,z);
    switch ( class_type ) {
        case MAT_C_DOUBLE:
        {
            double *ptr;

            ptr     = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0;
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            /* Skip all data to the starting indices */
            InflateSkipData(mat,&z_copy,data_type,I);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    ReadCompressedDoubleData(mat,&z_copy,ptr+i,data_type,edge[0]);
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-start[0]-edge[0]);
                    I += dims[0]-start[0];
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]-1; j++ ) {
                        ReadCompressedDoubleData(mat,&z_copy,ptr+i+j,data_type,1);
                        InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                        I += stride[0];
                    }
                    ReadCompressedDoubleData(mat,&z_copy,ptr+i+j,data_type,1);
                    I += dims[0]-(edge[0]-1)*stride[0]-start[0];
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-(edge[0]-1)*stride[0]-start[0]-1);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
#if 0
                        I += dims[0]-edge[0]*stride[0]-start[0];
                        InflateSkipData(mat,&z_copy,data_type,
                              dims[0]-edge[0]*stride[0]-start[0]);
#endif
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            }
            break;
        }
        case MAT_C_SINGLE:
        {
            float *ptr;

            ptr     = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0;
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            /* Skip all data to the starting indices */
            InflateSkipData(mat,&z_copy,data_type,I);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    ReadCompressedSingleData(mat,&z_copy,ptr+i,data_type,edge[0]);
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-start[0]-edge[0]);
                    I += dims[0]-start[0];
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]-1; j++ ) {
                        ReadCompressedSingleData(mat,&z_copy,ptr+i+j,data_type,1);
                        InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                        I += stride[0];
                    }
                    ReadCompressedSingleData(mat,&z_copy,ptr+i+j,data_type,1);
                    I += dims[0]-(edge[0]-1)*stride[0]-start[0];
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-(edge[0]-1)*stride[0]-start[0]-1);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            }
            break;
        }
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
        {
            mat_int64_t *ptr;

            ptr     = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0;
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            /* Skip all data to the starting indices */
            InflateSkipData(mat,&z_copy,data_type,I);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    ReadCompressedInt64Data(mat,&z_copy,ptr+i,data_type,edge[0]);
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-start[0]-edge[0]);
                    I += dims[0]-start[0];
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]-1; j++ ) {
                        ReadCompressedInt64Data(mat,&z_copy,ptr+i+j,data_type,1);
                        InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                        I += stride[0];
                    }
                    ReadCompressedInt64Data(mat,&z_copy,ptr+i+j,data_type,1);
                    I += dims[0]-(edge[0]-1)*stride[0]-start[0];
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-(edge[0]-1)*stride[0]-start[0]-1);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            }
            break;
        }
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
        {
            mat_uint64_t *ptr;

            ptr     = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0;
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            /* Skip all data to the starting indices */
            InflateSkipData(mat,&z_copy,data_type,I);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    ReadCompressedUInt64Data(mat,&z_copy,ptr+i,data_type,edge[0]);
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-start[0]-edge[0]);
                    I += dims[0]-start[0];
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]-1; j++ ) {
                        ReadCompressedUInt64Data(mat,&z_copy,ptr+i+j,data_type,1);
                        InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                        I += stride[0];
                    }
                    ReadCompressedUInt64Data(mat,&z_copy,ptr+i+j,data_type,1);
                    I += dims[0]-(edge[0]-1)*stride[0]-start[0];
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-(edge[0]-1)*stride[0]-start[0]-1);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            }
            break;
        }
#endif /* HAVE_MAT_UINT64_T */
        case MAT_C_INT32:
        {
            mat_int32_t *ptr;

            ptr     = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0;
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            /* Skip all data to the starting indices */
            InflateSkipData(mat,&z_copy,data_type,I);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    ReadCompressedInt32Data(mat,&z_copy,ptr+i,data_type,edge[0]);
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-start[0]-edge[0]);
                    I += dims[0]-start[0];
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]-1; j++ ) {
                        ReadCompressedInt32Data(mat,&z_copy,ptr+i+j,data_type,1);
                        InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                        I += stride[0];
                    }
                    ReadCompressedInt32Data(mat,&z_copy,ptr+i+j,data_type,1);
                    I += dims[0]-(edge[0]-1)*stride[0]-start[0];
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-(edge[0]-1)*stride[0]-start[0]-1);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            }
            break;
        }
        case MAT_C_UINT32:
        {
            mat_uint32_t *ptr;

            ptr     = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0;
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            /* Skip all data to the starting indices */
            InflateSkipData(mat,&z_copy,data_type,I);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    ReadCompressedUInt32Data(mat,&z_copy,ptr+i,data_type,edge[0]);
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-start[0]-edge[0]);
                    I += dims[0]-start[0];
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]-1; j++ ) {
                        ReadCompressedUInt32Data(mat,&z_copy,ptr+i+j,data_type,1);
                        InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                        I += stride[0];
                    }
                    ReadCompressedUInt32Data(mat,&z_copy,ptr+i+j,data_type,1);
                    I += dims[0]-(edge[0]-1)*stride[0]-start[0];
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-(edge[0]-1)*stride[0]-start[0]-1);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            }
            break;
        }
        case MAT_C_INT16:
        {
            mat_int16_t *ptr;

            ptr     = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0;
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            /* Skip all data to the starting indices */
            InflateSkipData(mat,&z_copy,data_type,I);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    ReadCompressedInt16Data(mat,&z_copy,ptr+i,data_type,edge[0]);
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-start[0]-edge[0]);
                    I += dims[0]-start[0];
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]-1; j++ ) {
                        ReadCompressedInt16Data(mat,&z_copy,ptr+i+j,data_type,1);
                        InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                        I += stride[0];
                    }
                    ReadCompressedInt16Data(mat,&z_copy,ptr+i+j,data_type,1);
                    I += dims[0]-(edge[0]-1)*stride[0]-start[0];
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-(edge[0]-1)*stride[0]-start[0]-1);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            }
            break;
        }
        case MAT_C_UINT16:
        {
            mat_uint16_t *ptr;

            ptr     = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0;
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            /* Skip all data to the starting indices */
            InflateSkipData(mat,&z_copy,data_type,I);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    ReadCompressedUInt16Data(mat,&z_copy,ptr+i,data_type,edge[0]);
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-start[0]-edge[0]);
                    I += dims[0]-start[0];
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]-1; j++ ) {
                        ReadCompressedUInt16Data(mat,&z_copy,ptr+i+j,data_type,1);
                        InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                        I += stride[0];
                    }
                    ReadCompressedUInt16Data(mat,&z_copy,ptr+i+j,data_type,1);
                    I += dims[0]-(edge[0]-1)*stride[0]-start[0];
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-(edge[0]-1)*stride[0]-start[0]-1);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            }
            break;
        }
        case MAT_C_INT8:
        {
            mat_int8_t *ptr;

            ptr     = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0;
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            /* Skip all data to the starting indices */
            InflateSkipData(mat,&z_copy,data_type,I);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    ReadCompressedInt8Data(mat,&z_copy,ptr+i,data_type,edge[0]);
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-start[0]-edge[0]);
                    I += dims[0]-start[0];
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]-1; j++ ) {
                        ReadCompressedInt8Data(mat,&z_copy,ptr+i+j,data_type,1);
                        InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                        I += stride[0];
                    }
                    ReadCompressedInt8Data(mat,&z_copy,ptr+i+j,data_type,1);
                    I += dims[0]-(edge[0]-1)*stride[0]-start[0];
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-(edge[0]-1)*stride[0]-start[0]-1);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            }
            break;
        }
        case MAT_C_UINT8:
        {
            mat_uint8_t *ptr;

            ptr     = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N       = edge[0];
            I       = 0;
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                I += dimp[i-1]*start[i];
            }
            /* Skip all data to the starting indices */
            InflateSkipData(mat,&z_copy,data_type,I);
            if ( stride[0] == 1 ) {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    ReadCompressedUInt8Data(mat,&z_copy,ptr+i,data_type,edge[0]);
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-start[0]-edge[0]);
                    I += dims[0]-start[0];
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            } else {
                for ( i = 0; i < N; i+=edge[0] ) {
                    if ( start[0] ) {
                        InflateSkipData(mat,&z_copy,data_type,start[0]);
                        I += start[0];
                    }
                    for ( j = 0; j < edge[0]-1; j++ ) {
                        ReadCompressedUInt8Data(mat,&z_copy,ptr+i+j,data_type,1);
                        InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                        I += stride[0];
                    }
                    ReadCompressedUInt8Data(mat,&z_copy,ptr+i+j,data_type,1);
                    I += dims[0]-(edge[0]-1)*stride[0]-start[0];
                    InflateSkipData(mat,&z_copy,data_type,dims[0]-(edge[0]-1)*stride[0]-start[0]-1);
                    for ( j = 1; j < rank; j++ ) {
                        cnt[j]++;
                        if ( (cnt[j] % edge[j]) == 0 ) {
                            cnt[j] = 0;
                            if ( (I % dimp[j]) != 0 ) {
                                InflateSkipData(mat,&z_copy,data_type,
                                      dimp[j]-(I % dimp[j])+dimp[j-1]*start[j]);
                                    I += dimp[j]-(I % dimp[j]) + dimp[j-1]*start[j];
                                } else if ( start[j] ) {
                                    InflateSkipData(mat,&z_copy,data_type,
                                        dimp[j-1]*start[j]);
                                    I += dimp[j-1]*start[j];
                                }
                        } else {
                            if ( inc[j] ) {
                                I += inc[j];
                                InflateSkipData(mat,&z_copy,data_type,inc[j]);
                            }
                            break;
                        }
                    }
                }
            }
            break;
        }
        default:
            nBytes = 0;
    }
    inflateEnd(&z_copy);
    return nBytes;
}
#endif

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
 * @return Number of bytes read from the file
 */
int
ReadDataSlab1(mat_t *mat,void *data,enum matio_classes class_type,
    enum matio_types data_type,int start,int stride,int edge)
{
    int i;
    size_t data_size;
    int    bytesread = 0;

    data_size = Mat_SizeOf(data_type);
    (void)fseek(mat->fp,start*data_size,SEEK_CUR);

    stride = data_size*(stride-1);
    switch(class_type) {
        case MAT_C_DOUBLE:
            if ( !stride ) {
                bytesread+=ReadDoubleData(mat,data,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    bytesread+=ReadDoubleData(mat,(double*)data+i,data_type,1);
                    (void)fseek(mat->fp,stride,SEEK_CUR);
                }
            }
            break;
        case MAT_C_SINGLE:
            if ( !stride ) {
                bytesread+=ReadSingleData(mat,data,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    bytesread+=ReadSingleData(mat,(float*)data+i,data_type,1);
                    (void)fseek(mat->fp,stride,SEEK_CUR);
                }
            }
            break;
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
            if ( !stride ) {
                bytesread+=ReadInt64Data(mat,data,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    bytesread+=ReadInt64Data(mat,(mat_int64_t*)data+i,data_type,1);
                    (void)fseek(mat->fp,stride,SEEK_CUR);
                }
            }
            break;
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
            if ( !stride ) {
                bytesread+=ReadUInt64Data(mat,data,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    bytesread+=ReadUInt64Data(mat,(mat_uint64_t*)data+i,data_type,1);
                    (void)fseek(mat->fp,stride,SEEK_CUR);
                }
            }
            break;
#endif /* HAVE_MAT_UINT64_T */
        case MAT_C_INT32:
            if ( !stride ) {
                bytesread+=ReadInt32Data(mat,data,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    bytesread+=ReadInt32Data(mat,(mat_int32_t*)data+i,data_type,1);
                    (void)fseek(mat->fp,stride,SEEK_CUR);
                }
            }
            break;
        case MAT_C_UINT32:
            if ( !stride ) {
                bytesread+=ReadUInt32Data(mat,data,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    bytesread+=ReadUInt32Data(mat,(mat_uint32_t*)data+i,data_type,1);
                    (void)fseek(mat->fp,stride,SEEK_CUR);
                }
            }
            break;
        case MAT_C_INT16:
            if ( !stride ) {
                bytesread+=ReadInt16Data(mat,data,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    bytesread+=ReadInt16Data(mat,(mat_int16_t*)data+i,data_type,1);
                    (void)fseek(mat->fp,stride,SEEK_CUR);
                }
            }
            break;
        case MAT_C_UINT16:
            if ( !stride ) {
                bytesread+=ReadUInt16Data(mat,data,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    bytesread+=ReadUInt16Data(mat,(mat_uint16_t*)data+i,data_type,1);
                    (void)fseek(mat->fp,stride,SEEK_CUR);
                }
            }
            break;
        case MAT_C_INT8:
            if ( !stride ) {
                bytesread+=ReadInt8Data(mat,data,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    bytesread+=ReadInt8Data(mat,(mat_int8_t*)data+i,data_type,1);
                    (void)fseek(mat->fp,stride,SEEK_CUR);
                }
            }
            break;
        case MAT_C_UINT8:
            if ( !stride ) {
                bytesread+=ReadUInt8Data(mat,data,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    bytesread+=ReadUInt8Data(mat,(mat_uint8_t*)data+i,data_type,1);
                    (void)fseek(mat->fp,stride,SEEK_CUR);
                }
            }
            break;
        default:
            return 0;
    }

    return bytesread;
}

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
ReadDataSlab2(mat_t *mat,void *data,enum matio_classes class_type,
    enum matio_types data_type,size_t *dims,int *start,int *stride,int *edge)
{
    int nBytes = 0, data_size, i, j;
    long pos, row_stride, col_stride;

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) ||
         (start == NULL) || (stride == NULL) || (edge    == NULL) ) {
        return 0;
    }

    data_size = Mat_SizeOf(data_type);

    switch ( class_type ) {
        case MAT_C_DOUBLE:
        {
            double *ptr;

            ptr = (double *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;
            pos = ftell(mat->fp);
            (void)fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadDoubleData(mat,ptr++,data_type,1);
                    (void)fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                (void)fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        case MAT_C_SINGLE:
        {
            float *ptr;

            ptr = (float *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;
            pos = ftell(mat->fp);
            (void)fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadSingleData(mat,ptr++,data_type,1);
                    (void)fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                (void)fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
        {
            mat_int64_t *ptr;

            ptr = (mat_int64_t *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;
            pos = ftell(mat->fp);
            (void)fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadInt64Data(mat,ptr++,data_type,1);
                    (void)fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                (void)fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
        {
            mat_uint64_t *ptr;

            ptr = (mat_uint64_t *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;
            pos = ftell(mat->fp);
            (void)fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadUInt64Data(mat,ptr++,data_type,1);
                    (void)fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                (void)fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
#endif /* HAVE_MAT_UINT64_T */
        case MAT_C_INT32:
        {
            mat_int32_t *ptr;

            ptr = (mat_int32_t *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;
            pos = ftell(mat->fp);
            (void)fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadInt32Data(mat,ptr++,data_type,1);
                    (void)fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                (void)fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        case MAT_C_UINT32:
        {
            mat_uint32_t *ptr;

            ptr = (mat_uint32_t *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;
            pos = ftell(mat->fp);
            (void)fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadUInt32Data(mat,ptr++,data_type,1);
                    (void)fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                (void)fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        case MAT_C_INT16:
        {
            mat_int16_t *ptr;

            ptr = (mat_int16_t *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;
            pos = ftell(mat->fp);
            (void)fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadInt16Data(mat,ptr++,data_type,1);
                    (void)fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                (void)fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        case MAT_C_UINT16:
        {
            mat_uint16_t *ptr;

            ptr = (mat_uint16_t *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;
            pos = ftell(mat->fp);
            (void)fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadUInt16Data(mat,ptr++,data_type,1);
                    (void)fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                (void)fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        case MAT_C_INT8:
        {
            mat_int8_t *ptr;

            ptr = (mat_int8_t *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;
            pos = ftell(mat->fp);
            (void)fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadInt8Data(mat,ptr++,data_type,1);
                    (void)fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                (void)fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        case MAT_C_UINT8:
        {
            mat_uint8_t *ptr;

            ptr = (mat_uint8_t *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;
            pos = ftell(mat->fp);
            (void)fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                (void)fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadUInt8Data(mat,ptr++,data_type,1);
                    (void)fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                (void)fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        default:
            nBytes = 0;
    }
    return nBytes;
}

#if defined(HAVE_ZLIB)
/** @brief Reads data of type @c data_type by user-defined dimensions for 1-D
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
ReadCompressedDataSlab1(mat_t *mat,z_stream *z,void *data,
    enum matio_classes class_type,enum matio_types data_type,int start,
    int stride,int edge)
{
    int nBytes = 0, i, err;
    z_stream z_copy = {0,};

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) )
        return 0;

    stride--;
    err = inflateCopy(&z_copy,z);
    InflateSkipData(mat,&z_copy,data_type,start);
    switch ( class_type ) {
        case MAT_C_DOUBLE:
        {
            double *ptr = data;
            if ( !stride ) {
                nBytes+=ReadCompressedDoubleData(mat,&z_copy,ptr,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    nBytes+=ReadCompressedDoubleData(mat,&z_copy,ptr+i,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride);
                }
            }
            break;
        }
        case MAT_C_SINGLE:
        {
            float *ptr = data;
            if ( !stride ) {
                nBytes+=ReadCompressedSingleData(mat,&z_copy,ptr,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    nBytes+=ReadCompressedSingleData(mat,&z_copy,ptr+i,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride);
                }
            }
            break;
        }
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
        {
            mat_int64_t *ptr = data;
            if ( !stride ) {
                nBytes+=ReadCompressedInt64Data(mat,&z_copy,ptr,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    nBytes+=ReadCompressedInt64Data(mat,&z_copy,ptr+i,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride);
                }
            }
            break;
        }
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
        {
            mat_uint64_t *ptr = data;
            if ( !stride ) {
                nBytes+=ReadCompressedUInt64Data(mat,&z_copy,ptr,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    nBytes+=ReadCompressedUInt64Data(mat,&z_copy,ptr+i,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride);
                }
            }
            break;
        }
#endif /* HAVE_MAT_UINT64_T */
        case MAT_C_INT32:
        {
            mat_int32_t *ptr = data;
            if ( !stride ) {
                nBytes+=ReadCompressedInt32Data(mat,&z_copy,ptr,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    nBytes+=ReadCompressedInt32Data(mat,&z_copy,ptr+i,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride);
                }
            }
            break;
        }
        case MAT_C_UINT32:
        {
            mat_uint32_t *ptr = data;
            if ( !stride ) {
                nBytes+=ReadCompressedUInt32Data(mat,&z_copy,ptr,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    nBytes+=ReadCompressedUInt32Data(mat,&z_copy,ptr+i,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride);
                }
            }
            break;
        }
        case MAT_C_INT16:
        {
            mat_int16_t *ptr = data;
            if ( !stride ) {
                nBytes+=ReadCompressedInt16Data(mat,&z_copy,ptr,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    nBytes+=ReadCompressedInt16Data(mat,&z_copy,ptr+i,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride);
                }
            }
            break;
        }
        case MAT_C_UINT16:
        {
            mat_uint16_t *ptr = data;
            if ( !stride ) {
                nBytes+=ReadCompressedUInt16Data(mat,&z_copy,ptr,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    nBytes+=ReadCompressedUInt16Data(mat,&z_copy,ptr+i,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride);
                }
            }
            break;
        }
        case MAT_C_INT8:
        {
            mat_int8_t *ptr = data;
            if ( !stride ) {
                nBytes+=ReadCompressedInt8Data(mat,&z_copy,ptr,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    nBytes+=ReadCompressedInt8Data(mat,&z_copy,ptr+i,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride);
                }
            }
            break;
        }
        case MAT_C_UINT8:
        {
            mat_uint8_t *ptr = data;
            if ( !stride ) {
                nBytes+=ReadCompressedUInt8Data(mat,&z_copy,ptr,data_type,edge);
            } else {
                for ( i = 0; i < edge; i++ ) {
                    nBytes+=ReadCompressedUInt8Data(mat,&z_copy,ptr+i,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride);
                }
            }
            break;
        }
        default:
            break;
    }
    inflateEnd(&z_copy);
    return nBytes;
}

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
ReadCompressedDataSlab2(mat_t *mat,z_stream *z,void *data,
    enum matio_classes class_type,enum matio_types data_type,size_t *dims,
    int *start,int *stride,int *edge)
{
    int nBytes = 0, data_size, i, j, err;
    int pos, row_stride, col_stride;
    z_stream z_copy = {0,};

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) ||
         (start == NULL) || (stride == NULL) || (edge    == NULL) ) {
        return 0;
    }

    err = inflateCopy(&z_copy,z);
    switch ( class_type ) {
        case MAT_C_DOUBLE:
        {
            double *ptr;

            data_size = sizeof(double);
            ptr = data;
            row_stride = (stride[0]-1);
            col_stride = (stride[1]-1)*dims[0];
            InflateSkipData(mat,&z_copy,data_type,start[1]*dims[0]);
            /* If stride[0] is 1 and stride[1] is 1, we are reading all of the
             * data so get rid of the loops.  If stride[0] is 1 and stride[1]
             * is not 0, we are reading whole columns, so get rid of inner loop
             * to speed up the code
             */
#if 0
            if ( (stride[0] == 1 && edge[0] == dims[0]) &&
                 (stride[1] == 1) ) {
                ReadCompressedDoubleData(mat,&z_copy,ptr,data_type,
                                         edge[0]*edge[1]);
            } else if ( stride[0] == 1 ) {
                for ( i = 0; i < edge[1]; i++ ) {
                    InflateSkipData(mat,&z_copy,data_type,start[0]);
                    ReadCompressedDoubleData(mat,&z_copy,ptr,data_type,edge[0]);
                    ptr += edge[0];
                    pos = dims[0]-(edge[0]-1)*stride[0]-1-start[0] + col_stride;
                    InflateSkipData(mat,&z_copy,data_type,pos);
                }
            } else {
#endif
            for ( i = 0; i < edge[1]; i++ ) {
                InflateSkipData(mat,&z_copy,data_type,start[0]);
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedDoubleData(mat,&z_copy,ptr++,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride[0]-1);
                }
                ReadCompressedDoubleData(mat,&z_copy,ptr++,data_type,1);
                pos = dims[0]-(edge[0]-1)*stride[0]-1-start[0] + col_stride;
                InflateSkipData(mat,&z_copy,data_type,pos);
            }
#if 0
            }
#endif
            break;
        }
        case MAT_C_SINGLE:
        {
            float *ptr;

            data_size = sizeof(float);
            ptr = data;
            row_stride = (stride[0]-1);
            col_stride = (stride[1]-1)*dims[0];
            InflateSkipData(mat,&z_copy,data_type,start[1]*dims[0]);
            for ( i = 0; i < edge[1]; i++ ) {
                InflateSkipData(mat,&z_copy,data_type,start[0]);
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedSingleData(mat,&z_copy,ptr++,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride[0]-1);
                }
                ReadCompressedSingleData(mat,&z_copy,ptr++,data_type,1);
                pos = dims[0]-(edge[0]-1)*stride[0]-1-start[0] + col_stride;
                InflateSkipData(mat,&z_copy,data_type,pos);
            }
            break;
        }
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
        {
            mat_int64_t *ptr;

            data_size = sizeof(mat_int64_t);
            ptr = data;
            row_stride = (stride[0]-1);
            col_stride = (stride[1]-1)*dims[0];
            InflateSkipData(mat,&z_copy,data_type,start[1]*dims[0]);
            for ( i = 0; i < edge[1]; i++ ) {
                InflateSkipData(mat,&z_copy,data_type,start[0]);
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedInt64Data(mat,&z_copy,ptr++,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride[0]-1);
                }
                ReadCompressedInt64Data(mat,&z_copy,ptr++,data_type,1);
                pos = dims[0]-(edge[0]-1)*stride[0]-1-start[0] + col_stride;
                InflateSkipData(mat,&z_copy,data_type,pos);
            }
            break;
        }
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
        {
            mat_uint64_t *ptr;

            data_size = sizeof(mat_uint64_t);
            ptr = data;
            row_stride = (stride[0]-1);
            col_stride = (stride[1]-1)*dims[0];
            InflateSkipData(mat,&z_copy,data_type,start[1]*dims[0]);
            for ( i = 0; i < edge[1]; i++ ) {
                InflateSkipData(mat,&z_copy,data_type,start[0]);
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedUInt64Data(mat,&z_copy,ptr++,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride[0]-1);
                }
                ReadCompressedUInt64Data(mat,&z_copy,ptr++,data_type,1);
                pos = dims[0]-(edge[0]-1)*stride[0]-1-start[0] + col_stride;
                InflateSkipData(mat,&z_copy,data_type,pos);
            }
            break;
        }
#endif /* HAVE_MAT_UINT64_T */
        case MAT_C_INT32:
        {
            mat_int32_t *ptr;

            data_size = sizeof(mat_int32_t);
            ptr = data;
            row_stride = (stride[0]-1);
            col_stride = (stride[1]-1)*dims[0];
            InflateSkipData(mat,&z_copy,data_type,start[1]*dims[0]);
            for ( i = 0; i < edge[1]; i++ ) {
                InflateSkipData(mat,&z_copy,data_type,start[0]);
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedInt32Data(mat,&z_copy,ptr++,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride[0]-1);
                }
                ReadCompressedInt32Data(mat,&z_copy,ptr++,data_type,1);
                pos = dims[0]-(edge[0]-1)*stride[0]-1-start[0] + col_stride;
                InflateSkipData(mat,&z_copy,data_type,pos);
            }
            break;
        }
        case MAT_C_UINT32:
        {
            mat_uint32_t *ptr;

            data_size = sizeof(mat_uint32_t);
            ptr = data;
            row_stride = (stride[0]-1);
            col_stride = (stride[1]-1)*dims[0];
            InflateSkipData(mat,&z_copy,data_type,start[1]*dims[0]);
            for ( i = 0; i < edge[1]; i++ ) {
                InflateSkipData(mat,&z_copy,data_type,start[0]);
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedUInt32Data(mat,&z_copy,ptr++,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride[0]-1);
                }
                ReadCompressedUInt32Data(mat,&z_copy,ptr++,data_type,1);
                pos = dims[0]-(edge[0]-1)*stride[0]-1-start[0] + col_stride;
                InflateSkipData(mat,&z_copy,data_type,pos);
            }
            break;
        }
        case MAT_C_INT16:
        {
            mat_int16_t *ptr;

            data_size = sizeof(mat_int16_t);
            ptr = data;
            row_stride = (stride[0]-1);
            col_stride = (stride[1]-1)*dims[0];
            InflateSkipData(mat,&z_copy,data_type,start[1]*dims[0]);
            for ( i = 0; i < edge[1]; i++ ) {
                InflateSkipData(mat,&z_copy,data_type,start[0]);
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedInt16Data(mat,&z_copy,ptr++,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride[0]-1);
                }
                ReadCompressedInt16Data(mat,&z_copy,ptr++,data_type,1);
                pos = dims[0]-(edge[0]-1)*stride[0]-1-start[0] + col_stride;
                InflateSkipData(mat,&z_copy,data_type,pos);
            }
            break;
        }
        case MAT_C_UINT16:
        {
            mat_uint16_t *ptr;

            data_size = sizeof(mat_uint16_t);
            ptr = data;
            row_stride = (stride[0]-1);
            col_stride = (stride[1]-1)*dims[0];
            InflateSkipData(mat,&z_copy,data_type,start[1]*dims[0]);
            for ( i = 0; i < edge[1]; i++ ) {
                InflateSkipData(mat,&z_copy,data_type,start[0]);
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedUInt16Data(mat,&z_copy,ptr++,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride[0]-1);
                }
                ReadCompressedUInt16Data(mat,&z_copy,ptr++,data_type,1);
                pos = dims[0]-(edge[0]-1)*stride[0]-1-start[0] + col_stride;
                InflateSkipData(mat,&z_copy,data_type,pos);
            }
            break;
        }
        case MAT_C_INT8:
        {
            mat_int8_t *ptr;

            data_size = sizeof(mat_int8_t);
            ptr = data;
            row_stride = (stride[0]-1);
            col_stride = (stride[1]-1)*dims[0];
            InflateSkipData(mat,&z_copy,data_type,start[1]*dims[0]);
            for ( i = 0; i < edge[1]; i++ ) {
                InflateSkipData(mat,&z_copy,data_type,start[0]);
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedInt8Data(mat,&z_copy,ptr++,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride[0]-1);
                }
                ReadCompressedInt8Data(mat,&z_copy,ptr++,data_type,1);
                pos = dims[0]-(edge[0]-1)*stride[0]-1-start[0] + col_stride;
                InflateSkipData(mat,&z_copy,data_type,pos);
            }
            break;
        }
        case MAT_C_UINT8:
        {
            mat_uint8_t *ptr;

            data_size = sizeof(mat_uint8_t);
            ptr = data;
            row_stride = (stride[0]-1);
            col_stride = (stride[1]-1)*dims[0];
            InflateSkipData(mat,&z_copy,data_type,start[1]*dims[0]);
            for ( i = 0; i < edge[1]; i++ ) {
                InflateSkipData(mat,&z_copy,data_type,start[0]);
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedUInt8Data(mat,&z_copy,ptr++,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride[0]-1);
                }
                ReadCompressedUInt8Data(mat,&z_copy,ptr++,data_type,1);
                pos = dims[0]-(edge[0]-1)*stride[0]-1-start[0] + col_stride;
                InflateSkipData(mat,&z_copy,data_type,pos);
            }
            break;
        }
        case MAT_C_CHAR:
        {
            char *ptr;

            data_size = 1;
            ptr = data;
            row_stride = (stride[0]-1);
            col_stride = (stride[1]-1)*dims[0];
            InflateSkipData(mat,&z_copy,data_type,start[1]*dims[0]);
            for ( i = 0; i < edge[1]; i++ ) {
                InflateSkipData(mat,&z_copy,data_type,start[0]);
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedCharData(mat,&z_copy,ptr++,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,stride[0]-1);
                }
                ReadCompressedCharData(mat,&z_copy,ptr++,data_type,1);
                pos = dims[0]-(edge[0]-1)*stride[0]-1-start[0] + col_stride;
                InflateSkipData(mat,&z_copy,data_type,pos);
            }
            break;
        }
        default:
            nBytes = 0;
    }
    inflateEnd(&z_copy);
    return nBytes;
}
#endif

/** @endcond */
