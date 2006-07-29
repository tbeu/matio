/** @file read_data.c
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
#include "matio_private.h"
#if defined(HAVE_ZLIB)
#   include <zlib.h>
#endif

/*
 * --------------------------------------------------------------------------
 *    Routines to read data of any type into arrays of a specific type
 * --------------------------------------------------------------------------
 */

int
ReadDoubleData(mat_t *mat,double *data,int data_type,int len)
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
                    (void)doubleSwap(data+i);
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
                    data[i] = floatSwap(&f);
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
                    data[i] = int32Swap(&i32);
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
                    data[i] = uint32Swap(&ui32);
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
                    data[i] = int16Swap(&i16);
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
                    data[i] = uint16Swap(&ui16);
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
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i8,data_size,1,mat->fp);
                    data[i] = i8;
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i8,data_size,1,mat->fp);
                    data[i] = i8;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui8,data_size,1,mat->fp);
                    data[i] = ui8;
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui8,data_size,1,mat->fp);
                    data[i] = ui8;
                }
            }
            break;
        }
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
/*
 * Converts data of any type to data in double precision format
 */
int
ReadCompressedDoubleData(mat_t *mat,z_stream *z,double *data,
    int data_type,int len)
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
                    (void)doubleSwap(data+i);
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
                        data[i] = int32Swap(buf.i32+i);
                } else {
                    int j;
                    len -= 256;
                    for ( i = 0; i < len; i+=256 ) {
                        InflateData(mat,z,buf.i32,256*data_size);
                        for ( j = 0; j < 256; j++ )
                            data[i+j] = int32Swap(buf.i32+j);
                    }
                    len = len-(i-256);
                    InflateData(mat,z,buf.i32,len*data_size);
                    for ( j = 0; j < len; j++ )
                        data[i+j] = int32Swap(buf.i32+j);
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
                        data[i] = uint32Swap(buf.ui32+i);
                } else {
                    int j;
                    len -= 256;
                    for ( i = 0; i < len; i+=256 ) {
                        InflateData(mat,z,buf.ui32,256*data_size);
                        for ( j = 0; j < 256; j++ )
                            data[i+j] = uint32Swap(buf.ui32+j);
                    }
                    len = len-(i-256);
                    InflateData(mat,z,buf.ui32,len*data_size);
                    for ( j = 0; j < len; j++ )
                        data[i+j] = uint32Swap(buf.ui32+j);
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
                        data[i] = int16Swap(buf.i16+i);
                } else {
                    int j;
                    len -= 512;
                    for ( i = 0; i < len; i+=512 ) {
                        InflateData(mat,z,buf.i16,512*data_size);
                        for ( j = 0; j < 512; j++ )
                            data[i+j] = int16Swap(buf.i16+j);
                    }
                    len = len-(i-512);
                    InflateData(mat,z,buf.i16,len*data_size);
                    for ( j = 0; j < len; j++ )
                        data[i+j] = int16Swap(buf.i16+j);
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
                        data[i] = uint16Swap(buf.ui16+i);
                } else {
                    int j;
                    len -= 512;
                    for ( i = 0; i < len; i+=512 ) {
                        InflateData(mat,z,buf.ui16,512*data_size);
                        for ( j = 0; j < 512; j++ )
                            data[i+j] = uint16Swap(buf.ui16+j);
                    }
                    len = len-(i-512);
                    InflateData(mat,z,buf.ui16,len*data_size);
                    for ( j = 0; j < len; j++ )
                        data[i+j] = uint16Swap(buf.ui16+j);
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
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

int
ReadSingleData(mat_t *mat,float *data,int data_type,int len)
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
                    data[i] = doubleSwap(&d);
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
                    data[i] = floatSwap(&f);
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
                    data[i] = int32Swap(&i32);
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
                    data[i] = uint32Swap(&ui32);
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
                    data[i] = int16Swap(&i16);
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
                    data[i] = uint16Swap(&ui16);
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
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i8,data_size,1,mat->fp);
                    data[i] = i8;
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i8,data_size,1,mat->fp);
                    data[i] = i8;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui8,data_size,1,mat->fp);
                    data[i] = ui8;
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui8,data_size,1,mat->fp);
                    data[i] = ui8;
                }
            }
            break;
        }
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
int
ReadCompressedSingleData(mat_t *mat,z_stream *z,float *data,
    int data_type,int len)
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
                    data[i] = doubleSwap(&d);
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
                    data[i] = floatSwap(&f);
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
                    data[i] = int32Swap(&i32);
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
                    data[i] = uint32Swap(&ui32);
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
                    data[i] = int16Swap(&i16);
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
                    data[i] = uint16Swap(&ui16);
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
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

int
ReadInt32Data(mat_t *mat,mat_int32_t *data,int data_type,int len)
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
                    data[i] = doubleSwap(&d);
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
                    data[i] = floatSwap(&f);
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
                    data[i] = int32Swap(&i32);
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
                    data[i] = uint32Swap(&ui32);
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
                    data[i] = int16Swap(&i16);
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
                    data[i] = uint16Swap(&ui16);
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
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i8,data_size,1,mat->fp);
                    data[i] = i8;
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i8,data_size,1,mat->fp);
                    data[i] = i8;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui8,data_size,1,mat->fp);
                    data[i] = ui8;
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui8,data_size,1,mat->fp);
                    data[i] = ui8;
                }
            }
            break;
        }
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
int
ReadCompressedInt32Data(mat_t *mat,z_stream *z,mat_int32_t *data,
    int data_type,int len)
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
                    data[i] = doubleSwap(&d);
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
                    data[i] = floatSwap(&f);
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
                    data[i] = int32Swap(&i32);
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
                    data[i] = uint32Swap(&ui32);
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
                    data[i] = int16Swap(&i16);
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
                    data[i] = uint16Swap(&ui16);
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
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

int
ReadUInt32Data(mat_t *mat,mat_uint32_t *data,int data_type,int len)
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
                    data[i] = doubleSwap(&d);
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
                    data[i] = floatSwap(&f);
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
                    data[i] = int32Swap(&i32);
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
                    data[i] = uint32Swap(&ui32);
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
                    data[i] = int16Swap(&i16);
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
                    data[i] = uint16Swap(&ui16);
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
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i8,data_size,1,mat->fp);
                    data[i] = i8;
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i8,data_size,1,mat->fp);
                    data[i] = i8;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui8,data_size,1,mat->fp);
                    data[i] = ui8;
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui8,data_size,1,mat->fp);
                    data[i] = ui8;
                }
            }
            break;
        }
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
int
ReadCompressedUInt32Data(mat_t *mat,z_stream *z,mat_uint32_t *data,
    int data_type,int len)
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
                    data[i] = doubleSwap(&d);
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
                    data[i] = floatSwap(&f);
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
                    data[i] = int32Swap(&i32);
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
                    data[i] = uint32Swap(&ui32);
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
                    data[i] = int16Swap(&i16);
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
                    data[i] = uint16Swap(&ui16);
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
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

int
ReadInt16Data(mat_t *mat,mat_int16_t *data,int data_type,int len)
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
                    data[i] = doubleSwap(&d);
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
                    data[i] = floatSwap(&f);
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
                    data[i] = int32Swap(&i32);
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
                    data[i] = uint32Swap(&ui32);
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
                    data[i] = int16Swap(&i16);
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
                    data[i] = uint16Swap(&ui16);
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
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i8,data_size,1,mat->fp);
                    data[i] = i8;
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i8,data_size,1,mat->fp);
                    data[i] = i8;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui8,data_size,1,mat->fp);
                    data[i] = ui8;
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui8,data_size,1,mat->fp);
                    data[i] = ui8;
                }
            }
            break;
        }
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
int
ReadCompressedInt16Data(mat_t *mat,z_stream *z,mat_int16_t *data,
    int data_type,int len)
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
                    data[i] = doubleSwap(&d);
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
                    data[i] = floatSwap(&f);
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
                    data[i] = int32Swap(&i32);
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
                    data[i] = uint32Swap(&ui32);
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
                    data[i] = int16Swap(&i16);
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
                    data[i] = uint16Swap(&ui16);
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
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

int
ReadUInt16Data(mat_t *mat,mat_uint16_t *data,int data_type,int len)
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
                    data[i] = doubleSwap(&d);
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
                    data[i] = floatSwap(&f);
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
                    data[i] = int32Swap(&i32);
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
                    data[i] = uint32Swap(&ui32);
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
                    data[i] = int16Swap(&i16);
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
                    data[i] = uint16Swap(&ui16);
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
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i8,data_size,1,mat->fp);
                    data[i] = i8;
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i8,data_size,1,mat->fp);
                    data[i] = i8;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui8,data_size,1,mat->fp);
                    data[i] = ui8;
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui8,data_size,1,mat->fp);
                    data[i] = ui8;
                }
            }
            break;
        }
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
int
ReadCompressedUInt16Data(mat_t *mat,z_stream *z,mat_uint16_t *data,
    int data_type,int len)
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
                    data[i] = doubleSwap(&d);
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
                    data[i] = floatSwap(&f);
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
                    data[i] = int32Swap(&i32);
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
                    data[i] = uint32Swap(&ui32);
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
                    data[i] = int16Swap(&i16);
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
                    data[i] = uint16Swap(&ui16);
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
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

int
ReadInt8Data(mat_t *mat,mat_int8_t *data,int data_type,int len)
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
                    data[i] = doubleSwap(&d);
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
                    data[i] = floatSwap(&f);
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
                    data[i] = int32Swap(&i32);
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
                    data[i] = uint32Swap(&ui32);
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
                    data[i] = int16Swap(&i16);
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
                    data[i] = uint16Swap(&ui16);
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
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i8,data_size,1,mat->fp);
                    data[i] = i8;
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i8,data_size,1,mat->fp);
                    data[i] = i8;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui8,data_size,1,mat->fp);
                    data[i] = ui8;
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui8,data_size,1,mat->fp);
                    data[i] = ui8;
                }
            }
            break;
        }
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
int
ReadCompressedInt8Data(mat_t *mat,z_stream *z,mat_int8_t *data,
    int data_type,int len)
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
                    data[i] = doubleSwap(&d);
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
                    data[i] = floatSwap(&f);
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
                    data[i] = int32Swap(&i32);
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
                    data[i] = uint32Swap(&ui32);
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
                    data[i] = int16Swap(&i16);
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
                    data[i] = uint16Swap(&ui16);
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
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

int
ReadUInt8Data(mat_t *mat,mat_uint8_t *data,int data_type,int len)
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
                    data[i] = doubleSwap(&d);
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
                    data[i] = floatSwap(&f);
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
                    data[i] = int32Swap(&i32);
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
                    data[i] = uint32Swap(&ui32);
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
                    data[i] = int16Swap(&i16);
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
                    data[i] = uint16Swap(&ui16);
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
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i8,data_size,1,mat->fp);
                    data[i] = i8;
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&i8,data_size,1,mat->fp);
                    data[i] = i8;
                }
            }
            break;
        }
        case MAT_T_UINT8:
        {
            mat_uint8_t ui8;

            data_size = sizeof(mat_uint8_t);
            if ( mat->byteswap ) {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui8,data_size,1,mat->fp);
                    data[i] = ui8;
                }
            } else {
                for ( i = 0; i < len; i++ ) {
                    bytesread += fread(&ui8,data_size,1,mat->fp);
                    data[i] = ui8;
                }
            }
            break;
        }
    }
    bytesread *= data_size;
    return bytesread;
}

#if defined(HAVE_ZLIB)
int
ReadCompressedUInt8Data(mat_t *mat,z_stream *z,mat_uint8_t *data,
    int data_type,int len)
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
                    data[i] = doubleSwap(&d);
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
                    data[i] = floatSwap(&f);
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
                    data[i] = int32Swap(&i32);
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
                    data[i] = uint32Swap(&ui32);
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
                    data[i] = int16Swap(&i16);
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
                    data[i] = uint16Swap(&ui16);
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
    }
    nBytes = len*data_size;
    return nBytes;
}
#endif

#if defined(HAVE_ZLIB)
int
ReadCompressedCharData(mat_t *mat,z_stream *z,char *data,int data_type,int len)
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
                    data[i] = uint16Swap(&i16);
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
ReadCharData(mat_t *mat,char *data,int data_type,int len)
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
                    data[i] = uint16Swap(&i16);
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

int
ReadDataSlabN(mat_t *mat,void *data,int class_type,int data_type,int rank,
    int *dims,int *start,int *stride,int *edge)
{
    int nBytes = 0, i, j, N, I = 0;
    int inc[10] = {0,}, cnt[10] = {0,}, dimp[10] = {0,};

    if ( (mat   == NULL) || (data   == NULL) || (mat->fp == NULL) ||
         (start == NULL) || (stride == NULL) || (edge    == NULL) ) {
        return 1;
    } else if ( rank > 10 ) {
        return 1; 
    }

    switch ( class_type ) {
        case MAT_C_DOUBLE:
        {
            inc[0] = stride[0]-1;
            N = edge[0];
            if ( start[0] > 0 )
                fseek(mat->fp,Mat_SizeOf(data_type)*start[0],SEEK_CUR);
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                if ( start[i] > 0 )
                    fseek(mat->fp,Mat_SizeOf(data_type)*start[i]*dimp[i-1],SEEK_CUR);
                Mat_Message("inc[%d] = %d",i,inc[i]);
            }
            for ( i = 0; i < N; i+=edge[0] ) {
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadDoubleData(mat,(double*)data+i+j,data_type,1);
                    fseek(mat->fp,Mat_SizeOf(data_type)*(stride[0]-1),SEEK_CUR);
                    I += stride[0];
                }
                I += dims[0]-edge[0]*stride[0]-start[0];
                fseek(mat->fp,Mat_SizeOf(data_type)*
                      (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                for ( j = 1; j < rank-1; j++ ) {
                    cnt[j]++;
                    if ( (cnt[j] % edge[j]) == 0 ) {
                        cnt[j] = 0;
                        if ( (I % dimp[j]) != 0 ) {
                            fseek(mat->fp,Mat_SizeOf(data_type)*
                                  (dimp[j]-(I % dimp[j])),SEEK_CUR);
                            I += dimp[j]-(I % dimp[j]);
                        }
                    } else {
                        I += inc[j];
                        fseek(mat->fp,Mat_SizeOf(data_type)*inc[j],SEEK_CUR);
                        break;
                    }
                }
            }
            break;
        }
        case MAT_C_SINGLE:
        {
            inc[0] = stride[0]-1;
            N = edge[0];
            if ( start[0] > 0 )
                fseek(mat->fp,Mat_SizeOf(data_type)*start[0],SEEK_CUR);
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                if ( start[i] > 0 )
                    fseek(mat->fp,Mat_SizeOf(data_type)*start[i]*dimp[i-1],SEEK_CUR);
                Mat_Message("inc[%d] = %d",i,inc[i]);
            }
            for ( i = 0; i < N; i+=edge[0] ) {
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadSingleData(mat,(float*)data+i+j,data_type,1);
                    fseek(mat->fp,Mat_SizeOf(data_type)*(stride[0]-1),SEEK_CUR);
                    I += stride[0];
                }
                I += dims[0]-edge[0]*stride[0]-start[0];
                fseek(mat->fp,Mat_SizeOf(data_type)*
                      (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                for ( j = 1; j < rank-1; j++ ) {
                    cnt[j]++;
                    if ( (cnt[j] % edge[j]) == 0 ) {
                        cnt[j] = 0;
                        if ( (I % dimp[j]) != 0 ) {
                            fseek(mat->fp,Mat_SizeOf(data_type)*
                                  (dimp[j]-(I % dimp[j])),SEEK_CUR);
                            I += dimp[j]-(I % dimp[j]);
                        }
                    } else {
                        I += inc[j];
                        fseek(mat->fp,Mat_SizeOf(data_type)*inc[j],SEEK_CUR);
                        break;
                    }
                }
            }
            break;
        }
        case MAT_C_INT32:
        {
            inc[0] = stride[0]-1;
            N = edge[0];
            if ( start[0] > 0 )
                fseek(mat->fp,Mat_SizeOf(data_type)*start[0],SEEK_CUR);
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                if ( start[i] > 0 )
                    fseek(mat->fp,Mat_SizeOf(data_type)*start[i]*dimp[i-1],SEEK_CUR);
                Mat_Message("inc[%d] = %d",i,inc[i]);
            }
            for ( i = 0; i < N; i+=edge[0] ) {
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadInt32Data(mat,(mat_int32_t*)data+i+j,data_type,1);
                    fseek(mat->fp,Mat_SizeOf(data_type)*(stride[0]-1),SEEK_CUR);
                    I += stride[0];
                }
                I += dims[0]-edge[0]*stride[0]-start[0];
                fseek(mat->fp,Mat_SizeOf(data_type)*
                      (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                for ( j = 1; j < rank-1; j++ ) {
                    cnt[j]++;
                    if ( (cnt[j] % edge[j]) == 0 ) {
                        cnt[j] = 0;
                        if ( (I % dimp[j]) != 0 ) {
                            fseek(mat->fp,Mat_SizeOf(data_type)*
                                  (dimp[j]-(I % dimp[j])),SEEK_CUR);
                            I += dimp[j]-(I % dimp[j]);
                        }
                    } else {
                        I += inc[j];
                        fseek(mat->fp,Mat_SizeOf(data_type)*inc[j],SEEK_CUR);
                        break;
                    }
                }
            }
            break;
        }
        case MAT_C_UINT32:
        {
            inc[0] = stride[0]-1;
            N = edge[0];
            if ( start[0] > 0 )
                fseek(mat->fp,Mat_SizeOf(data_type)*start[0],SEEK_CUR);
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                if ( start[i] > 0 )
                    fseek(mat->fp,Mat_SizeOf(data_type)*start[i]*dimp[i-1],SEEK_CUR);
                Mat_Message("inc[%d] = %d",i,inc[i]);
            }
            for ( i = 0; i < N; i+=edge[0] ) {
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadUInt32Data(mat,(mat_uint32_t*)data+i+j,data_type,1);
                    fseek(mat->fp,Mat_SizeOf(data_type)*(stride[0]-1),SEEK_CUR);
                    I += stride[0];
                }
                I += dims[0]-edge[0]*stride[0]-start[0];
                fseek(mat->fp,Mat_SizeOf(data_type)*
                      (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                for ( j = 1; j < rank-1; j++ ) {
                    cnt[j]++;
                    if ( (cnt[j] % edge[j]) == 0 ) {
                        cnt[j] = 0;
                        if ( (I % dimp[j]) != 0 ) {
                            fseek(mat->fp,Mat_SizeOf(data_type)*
                                  (dimp[j]-(I % dimp[j])),SEEK_CUR);
                            I += dimp[j]-(I % dimp[j]);
                        }
                    } else {
                        I += inc[j];
                        fseek(mat->fp,Mat_SizeOf(data_type)*inc[j],SEEK_CUR);
                        break;
                    }
                }
            }
            break;
        }
        case MAT_C_INT16:
        {
            inc[0] = stride[0]-1;
            N = edge[0];
            if ( start[0] > 0 )
                fseek(mat->fp,Mat_SizeOf(data_type)*start[0],SEEK_CUR);
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                if ( start[i] > 0 )
                    fseek(mat->fp,start[i]*dimp[i-1]*Mat_SizeOf(data_type),SEEK_CUR);
                Mat_Message("inc[%d] = %d",i,inc[i]);
            }
            for ( i = 0; i < N; i+=edge[0] ) {
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadInt16Data(mat,(mat_int16_t*)data+i+j,data_type,1);
                    fseek(mat->fp,Mat_SizeOf(data_type)*(stride[0]-1),SEEK_CUR);
                    I += stride[0];
                }
                I += dims[0]-edge[0]*stride[0]-start[0];
                fseek(mat->fp,Mat_SizeOf(data_type)*
                      (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                for ( j = 1; j < rank-1; j++ ) {
                    cnt[j]++;
                    if ( (cnt[j] % edge[j]) == 0 ) {
                        cnt[j] = 0;
                        if ( (I % dimp[j]) != 0 ) {
                            fseek(mat->fp,Mat_SizeOf(data_type)*
                                  (dimp[j]-(I % dimp[j])),SEEK_CUR);
                            I += dimp[j]-(I % dimp[j]);
                        }
                    } else {
                        I += inc[j];
                        fseek(mat->fp,Mat_SizeOf(data_type)*inc[j],SEEK_CUR);
                        break;
                    }
                }
            }
            break;
        }
        case MAT_C_UINT16:
        {
            inc[0] = stride[0]-1;
            N = edge[0];
            if ( start[0] > 0 )
                fseek(mat->fp,Mat_SizeOf(data_type)*start[0],SEEK_CUR);
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                if ( start[i] > 0 )
                    fseek(mat->fp,start[i]*dimp[i-1]*Mat_SizeOf(data_type),SEEK_CUR);
                Mat_Message("inc[%d] = %d",i,inc[i]);
            }
            for ( i = 0; i < N; i+=edge[0] ) {
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadUInt16Data(mat,(mat_uint16_t*)data+i+j,data_type,1);
                    fseek(mat->fp,Mat_SizeOf(data_type)*(stride[0]-1),SEEK_CUR);
                    I += stride[0];
                }
                I += dims[0]-edge[0]*stride[0]-start[0];
                fseek(mat->fp,Mat_SizeOf(data_type)*
                      (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                for ( j = 1; j < rank-1; j++ ) {
                    cnt[j]++;
                    if ( (cnt[j] % edge[j]) == 0 ) {
                        cnt[j] = 0;
                        if ( (I % dimp[j]) != 0 ) {
                            fseek(mat->fp,Mat_SizeOf(data_type)*
                                  (dimp[j]-(I % dimp[j])),SEEK_CUR);
                            I += dimp[j]-(I % dimp[j]);
                        }
                    } else {
                        I += inc[j];
                        fseek(mat->fp,Mat_SizeOf(data_type)*inc[j],SEEK_CUR);
                        break;
                    }
                }
            }
            break;
        }
        case MAT_C_INT8:
        {
            inc[0] = stride[0]-1;
            N = edge[0];
            if ( start[0] > 0 )
                fseek(mat->fp,Mat_SizeOf(data_type)*start[0],SEEK_CUR);
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                if ( start[i] > 0 )
                    fseek(mat->fp,start[i]*dimp[i-1]*Mat_SizeOf(data_type),SEEK_CUR);
                Mat_Message("inc[%d] = %d",i,inc[i]);
            }
            for ( i = 0; i < N; i+=edge[0] ) {
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadInt8Data(mat,(mat_int8_t*)data+i+j,data_type,1);
                    fseek(mat->fp,Mat_SizeOf(data_type)*(stride[0]-1),SEEK_CUR);
                    I += stride[0];
                }
                I += dims[0]-edge[0]*stride[0]-start[0];
                fseek(mat->fp,Mat_SizeOf(data_type)*
                      (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                for ( j = 1; j < rank-1; j++ ) {
                    cnt[j]++;
                    if ( (cnt[j] % edge[j]) == 0 ) {
                        cnt[j] = 0;
                        if ( (I % dimp[j]) != 0 ) {
                            fseek(mat->fp,Mat_SizeOf(data_type)*
                                  (dimp[j]-(I % dimp[j])),SEEK_CUR);
                            I += dimp[j]-(I % dimp[j]);
                        }
                    } else {
                        I += inc[j];
                        fseek(mat->fp,Mat_SizeOf(data_type)*inc[j],SEEK_CUR);
                        break;
                    }
                }
            }
            break;
        }
        case MAT_C_UINT8:
        {
            inc[0] = stride[0]-1;
            dimp[0] = dims[0];
            N = edge[0];
            if ( start[0] > 0 )
                fseek(mat->fp,Mat_SizeOf(data_type)*start[0],SEEK_CUR);
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                if ( start[i] > 0 )
                    fseek(mat->fp,start[i]*dimp[i-1]*Mat_SizeOf(data_type),SEEK_CUR);
                Mat_Message("inc[%d] = %d",i,inc[i]);
            }
            for ( i = 0; i < N; i+=edge[0] ) {
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadUInt8Data(mat,(mat_uint8_t*)data+i+j,data_type,1);
                    fseek(mat->fp,Mat_SizeOf(data_type)*(stride[0]-1),SEEK_CUR);
                    I += stride[0];
                }
                I += dims[0]-edge[0]*stride[0]-start[0];
                fseek(mat->fp,Mat_SizeOf(data_type)*
                      (dims[0]-edge[0]*stride[0]-start[0]),SEEK_CUR);
                for ( j = 1; j < rank-1; j++ ) {
                    cnt[j]++;
                    if ( (cnt[j] % edge[j]) == 0 ) {
                        cnt[j] = 0;
                        if ( (I % dimp[j]) != 0 ) {
                            fseek(mat->fp,Mat_SizeOf(data_type)*
                                  (dimp[j]-(I % dimp[j])),SEEK_CUR);
                            I += dimp[j]-(I % dimp[j]);
                        }
                    } else {
                        I += inc[j];
                        fseek(mat->fp,Mat_SizeOf(data_type)*inc[j],SEEK_CUR);
                        break;
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
int
ReadCompressedDataSlabN(mat_t *mat,z_stream *z,void *data,int class_type,
    int data_type,int rank,int *dims,int *start,int *stride,int *edge)
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

            ptr = (double *)data;
            inc[0] = stride[0]-1;
            dimp[0] = dims[0];
            N = edge[0];
            if ( start[0] > 0 )
                InflateSkipData(mat,&z_copy,data_type,start[0]);
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                if ( start[i] > 0 )
                    InflateSkipData(mat,&z_copy,data_type,start[i]*dimp[i-1]);
            }
            for ( i = 0; i < N; i+=edge[0] ) {
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedDoubleData(mat,&z_copy,ptr+i+j,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                    I += stride[0];
                }
                ReadCompressedDoubleData(mat,&z_copy,ptr+i+j,data_type,1);
                I += stride[0];
                for ( j = 1; j < rank-1; j++ ) {
                    cnt[j]++;
                    if ( (cnt[j] % edge[j]) == 0 ) {
                        cnt[j] = 0;
                        if ( (I % dimp[j]) != 0 ) {
                            InflateSkipData(mat,&z_copy,data_type,
                                  dimp[j]-(I % dimp[j]));
                            I += dimp[j]-(I % dimp[j]);
                        }
                    } else {
                        I += dims[0]-edge[0]*stride[0]-start[0];
                        InflateSkipData(mat,&z_copy,data_type,
                              dims[0]-edge[0]*stride[0]-start[0]);
                        I += inc[j];
                        InflateSkipData(mat,&z_copy,data_type,inc[j]);
                        break;
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
            N = edge[0];
            if ( start[0] > 0 )
                InflateSkipData(mat,&z_copy,data_type,start[0]);
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                if ( start[i] > 0 )
                    InflateSkipData(mat,&z_copy,data_type,start[i]*dimp[i-1]);
            }
            for ( i = 0; i < N; i+=edge[0] ) {
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedSingleData(mat,&z_copy,ptr+i+j,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                    I += stride[0];
                }
                ReadCompressedSingleData(mat,&z_copy,ptr+i+j,data_type,1);
                I += stride[0];
                for ( j = 1; j < rank-1; j++ ) {
                    cnt[j]++;
                    if ( (cnt[j] % edge[j]) == 0 ) {
                        cnt[j] = 0;
                        if ( (I % dimp[j]) != 0 ) {
                            InflateSkipData(mat,&z_copy,data_type,
                                  dimp[j]-(I % dimp[j]));
                            I += dimp[j]-(I % dimp[j]);
                        }
                    } else {
                        I += dims[0]-edge[0]*stride[0]-start[0];
                        InflateSkipData(mat,&z_copy,data_type,
                              dims[0]-edge[0]*stride[0]-start[0]);
                        I += inc[j];
                        InflateSkipData(mat,&z_copy,data_type,inc[j]);
                        break;
                    }
                }
            }
            break;
        }
        case MAT_C_INT32:
        {
            mat_int32_t *ptr;

            ptr     = data;
            inc[0]  = stride[0]-1;
            dimp[0] = dims[0];
            N = edge[0];
            if ( start[0] > 0 )
                InflateSkipData(mat,&z_copy,data_type,start[0]);
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                if ( start[i] > 0 )
                    InflateSkipData(mat,&z_copy,data_type,start[i]*dimp[i-1]);
            }
            for ( i = 0; i < N; i+=edge[0] ) {
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedInt32Data(mat,&z_copy,ptr+i+j,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                    I += stride[0];
                }
                ReadCompressedInt32Data(mat,&z_copy,ptr+i+j,data_type,1);
                I += stride[0];
                for ( j = 1; j < rank-1; j++ ) {
                    cnt[j]++;
                    if ( (cnt[j] % edge[j]) == 0 ) {
                        cnt[j] = 0;
                        if ( (I % dimp[j]) != 0 ) {
                            InflateSkipData(mat,&z_copy,data_type,
                                  dimp[j]-(I % dimp[j]));
                            I += dimp[j]-(I % dimp[j]);
                        }
                    } else {
                        I += dims[0]-edge[0]*stride[0]-start[0];
                        InflateSkipData(mat,&z_copy,data_type,
                              dims[0]-edge[0]*stride[0]-start[0]);
                        I += inc[j];
                        InflateSkipData(mat,&z_copy,data_type,inc[j]);
                        break;
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
            N = edge[0];
            if ( start[0] > 0 )
                InflateSkipData(mat,&z_copy,data_type,start[0]);
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                if ( start[i] > 0 )
                    InflateSkipData(mat,&z_copy,data_type,start[i]*dimp[i-1]);
            }
            for ( i = 0; i < N; i+=edge[0] ) {
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedUInt32Data(mat,&z_copy,ptr+i+j,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                    I += stride[0];
                }
                ReadCompressedUInt32Data(mat,&z_copy,ptr+i+j,data_type,1);
                I += stride[0];
                for ( j = 1; j < rank-1; j++ ) {
                    cnt[j]++;
                    if ( (cnt[j] % edge[j]) == 0 ) {
                        cnt[j] = 0;
                        if ( (I % dimp[j]) != 0 ) {
                            InflateSkipData(mat,&z_copy,data_type,
                                  dimp[j]-(I % dimp[j]));
                            I += dimp[j]-(I % dimp[j]);
                        }
                    } else {
                        I += dims[0]-edge[0]*stride[0]-start[0];
                        InflateSkipData(mat,&z_copy,data_type,
                              dims[0]-edge[0]*stride[0]-start[0]);
                        I += inc[j];
                        InflateSkipData(mat,&z_copy,data_type,inc[j]);
                        break;
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
            N = edge[0];
            if ( start[0] > 0 )
                InflateSkipData(mat,&z_copy,data_type,start[0]);
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                if ( start[i] > 0 )
                    InflateSkipData(mat,&z_copy,data_type,start[i]*dimp[i-1]);
            }
            for ( i = 0; i < N; i+=edge[0] ) {
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedInt16Data(mat,&z_copy,ptr+i+j,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                    I += stride[0];
                }
                ReadCompressedInt16Data(mat,&z_copy,ptr+i+j,data_type,1);
                I += stride[0];
                for ( j = 1; j < rank-1; j++ ) {
                    cnt[j]++;
                    if ( (cnt[j] % edge[j]) == 0 ) {
                        cnt[j] = 0;
                        if ( (I % dimp[j]) != 0 ) {
                            InflateSkipData(mat,&z_copy,data_type,
                                  dimp[j]-(I % dimp[j]));
                            I += dimp[j]-(I % dimp[j]);
                        }
                    } else {
                        I += dims[0]-edge[0]*stride[0]-start[0];
                        InflateSkipData(mat,&z_copy,data_type,
                              dims[0]-edge[0]*stride[0]-start[0]);
                        I += inc[j];
                        InflateSkipData(mat,&z_copy,data_type,inc[j]);
                        break;
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
            N = edge[0];
            if ( start[0] > 0 )
                InflateSkipData(mat,&z_copy,data_type,start[0]);
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                if ( start[i] > 0 )
                    InflateSkipData(mat,&z_copy,data_type,start[i]*dimp[i-1]);
            }
            for ( i = 0; i < N; i+=edge[0] ) {
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedUInt16Data(mat,&z_copy,ptr+i+j,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                    I += stride[0];
                }
                ReadCompressedUInt16Data(mat,&z_copy,ptr+i+j,data_type,1);
                I += stride[0];
                for ( j = 1; j < rank-1; j++ ) {
                    cnt[j]++;
                    if ( (cnt[j] % edge[j]) == 0 ) {
                        cnt[j] = 0;
                        if ( (I % dimp[j]) != 0 ) {
                            InflateSkipData(mat,&z_copy,data_type,
                                  dimp[j]-(I % dimp[j]));
                            I += dimp[j]-(I % dimp[j]);
                        }
                    } else {
                        I += dims[0]-edge[0]*stride[0]-start[0];
                        InflateSkipData(mat,&z_copy,data_type,
                              dims[0]-edge[0]*stride[0]-start[0]);
                        I += inc[j];
                        InflateSkipData(mat,&z_copy,data_type,inc[j]);
                        break;
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
            N = edge[0];
            if ( start[0] > 0 )
                InflateSkipData(mat,&z_copy,data_type,start[0]);
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                if ( start[i] > 0 )
                    InflateSkipData(mat,&z_copy,data_type,start[i]*dimp[i-1]);
            }
            for ( i = 0; i < N; i+=edge[0] ) {
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedInt8Data(mat,&z_copy,ptr+i+j,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                    I += stride[0];
                }
                ReadCompressedInt8Data(mat,&z_copy,ptr+i+j,data_type,1);
                I += stride[0];
                for ( j = 1; j < rank-1; j++ ) {
                    cnt[j]++;
                    if ( (cnt[j] % edge[j]) == 0 ) {
                        cnt[j] = 0;
                        if ( (I % dimp[j]) != 0 ) {
                            InflateSkipData(mat,&z_copy,data_type,
                                  dimp[j]-(I % dimp[j]));
                            I += dimp[j]-(I % dimp[j]);
                        }
                    } else {
                        I += dims[0]-edge[0]*stride[0]-start[0];
                        InflateSkipData(mat,&z_copy,data_type,
                              dims[0]-edge[0]*stride[0]-start[0]);
                        I += inc[j];
                        InflateSkipData(mat,&z_copy,data_type,inc[j]);
                        break;
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
            N = edge[0];
            if ( start[0] > 0 )
                InflateSkipData(mat,&z_copy,data_type,start[0]);
            for ( i = 1; i < rank; i++ ) {
                inc[i]  = stride[i]-1;
                dimp[i] = dims[i-1];
                for ( j = i ; j--; ) {
                    inc[i]  *= dims[j];
                    dimp[i] *= dims[j+1];
                }
                N *= edge[i];
                if ( start[i] > 0 )
                    InflateSkipData(mat,&z_copy,data_type,start[i]*dimp[i-1]);
            }
            for ( i = 0; i < N; i+=edge[0] ) {
                for ( j = 0; j < edge[0]-1; j++ ) {
                    ReadCompressedUInt8Data(mat,&z_copy,ptr+i+j,data_type,1);
                    InflateSkipData(mat,&z_copy,data_type,(stride[0]-1));
                    I += stride[0];
                }
                ReadCompressedUInt8Data(mat,&z_copy,ptr+i+j,data_type,1);
                I += stride[0];
                for ( j = 1; j < rank-1; j++ ) {
                    cnt[j]++;
                    if ( (cnt[j] % edge[j]) == 0 ) {
                        cnt[j] = 0;
                        if ( (I % dimp[j]) != 0 ) {
                            InflateSkipData(mat,&z_copy,data_type,
                                  dimp[j]-(I % dimp[j]));
                            I += dimp[j]-(I % dimp[j]);
                        }
                    } else {
                        I += dims[0]-edge[0]*stride[0]-start[0];
                        InflateSkipData(mat,&z_copy,data_type,
                              dims[0]-edge[0]*stride[0]-start[0]);
                        I += inc[j];
                        InflateSkipData(mat,&z_copy,data_type,inc[j]);
                        break;
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

int
ReadDataSlab2(mat_t *mat,void *data,int class_type,int data_type,
    int *dims,int *start,int *stride,int *edge)
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
            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadDoubleData(mat,ptr++,data_type,1);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
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
            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadSingleData(mat,ptr++,data_type,1);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
            }
            break;
        }
        case MAT_C_INT32:
        {
            mat_int32_t *ptr;

            ptr = (mat_int32_t *)data;
            row_stride = (stride[0]-1)*data_size;
            col_stride = stride[1]*dims[0]*data_size;
            pos = ftell(mat->fp);
            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadInt32Data(mat,ptr++,data_type,1);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
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
            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadUInt32Data(mat,ptr++,data_type,1);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
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
            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadInt16Data(mat,ptr++,data_type,1);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
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
            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadUInt16Data(mat,ptr++,data_type,1);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
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
            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadInt8Data(mat,ptr++,data_type,1);
                    fseek(mat->fp,row_stride,SEEK_CUR);
                }
                pos = pos+col_stride-ftell(mat->fp);
                fseek(mat->fp,pos,SEEK_CUR);
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
            fseek(mat->fp,start[1]*dims[0]*data_size,SEEK_CUR);
            for ( i = 0; i < edge[1]; i++ ) {
                pos = ftell(mat->fp);
                fseek(mat->fp,start[0]*data_size,SEEK_CUR);
                for ( j = 0; j < edge[0]; j++ ) {
                    ReadUInt8Data(mat,ptr++,data_type,1);
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

#if defined(HAVE_ZLIB)
int
ReadCompressedDataSlab2(mat_t *mat,z_stream *z,void *data,int class_type,
    int data_type,int *dims,int *start,int *stride,int *edge)
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
