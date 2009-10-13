/** @file endian.c
 * @brief Functions to handle endian specifics
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

/** @brief swap the bytes @c a and @c b
 * @ingroup mat_internal
 */
#define swap(a,b)   a^=b;b^=a;a^=b

#ifdef HAVE_MAT_INT64_T
/** @brief swap the bytes of a 64-bit signed integer
 * @ingroup mat_internal
 * @param a pointer to integer to swap
 * @return the swapped integer
 */
mat_int64_t
Mat_int64Swap( mat_int64_t *a )
{

    union {
        mat_int8_t    i1[8];
        mat_int64_t   i8;
    } tmp;

    tmp.i8 = *a;

    swap( tmp.i1[0], tmp.i1[7] );
    swap( tmp.i1[1], tmp.i1[6] );
    swap( tmp.i1[2], tmp.i1[5] );
    swap( tmp.i1[3], tmp.i1[4] );

    *a = tmp.i8;

    return *a;

}
#endif /* HAVE_MAT_INT64_T */

#ifdef HAVE_MAT_UINT64_T
/** @brief swap the bytes of a 64-bit unsigned integer
 * @ingroup mat_internal
 * @param a pointer to integer to swap
 * @return the swapped integer
 */
mat_uint64_t
Mat_uint64Swap( mat_uint64_t *a )
{

    union {
        mat_uint8_t    i1[8]; 
        mat_uint64_t   i8;
    } tmp;

    tmp.i8 = *a;

    swap( tmp.i1[0], tmp.i1[7] );
    swap( tmp.i1[1], tmp.i1[6] );
    swap( tmp.i1[2], tmp.i1[5] );
    swap( tmp.i1[3], tmp.i1[4] );

    *a = tmp.i8;

    return *a;

}
#endif /* HAVE_MAT_UINT64_T */

/** @brief swap the bytes of a 32-bit signed integer
 * @ingroup mat_internal
 * @param a pointer to integer to swap
 * @return the swapped integer
 */
mat_int32_t
Mat_int32Swap( mat_int32_t *a )
{

    union {
        mat_int8_t    i1[4]; 
        mat_int32_t   i4;
    } tmp;

    tmp.i4 = *a;

    swap( tmp.i1[0], tmp.i1[3] );
    swap( tmp.i1[1], tmp.i1[2] );

    *a = tmp.i4;

    return *a;

}

/** @brief swap the bytes of a 32-bit unsigned integer
 * @ingroup mat_internal
 * @param a pointer to integer to swap
 * @return the swapped integer
 */
mat_uint32_t
Mat_uint32Swap( mat_uint32_t *a )
{

    union {
        mat_uint8_t    i1[4]; 
        mat_uint32_t   i4;
    } tmp;

    tmp.i4 = *a;

    swap( tmp.i1[0], tmp.i1[3] );
    swap( tmp.i1[1], tmp.i1[2] );

    *a = tmp.i4;

    return *a;

}

/** @brief swap the bytes of a 16-bit signed integer
 * @ingroup mat_internal
 * @param a pointer to integer to swap
 * @return the swapped integer
 */
mat_int16_t
Mat_int16Swap( mat_int16_t *a ) 
{

    union {
        mat_int8_t   i1[2];
        mat_int16_t  i2;
    } tmp;

    tmp.i2 = *a;

    swap( tmp.i1[0], tmp.i1[1] );

    *a = tmp.i2;
    return *a;

}

/** @brief swap the bytes of a 16-bit unsigned integer
 * @ingroup mat_internal
 * @param a pointer to integer to swap
 * @return the swapped integer
 */
mat_uint16_t
Mat_uint16Swap( mat_uint16_t *a ) 
{

    union {
        mat_uint8_t   i1[2];
        mat_uint16_t  i2;
    } tmp;

    tmp.i2 = *a;

    swap( tmp.i1[0], tmp.i1[1] );

    *a = tmp.i2;
    return *a;

}

/** @brief swap the bytes of a 4 byte single-precision float
 * @ingroup mat_internal
 * @param a pointer to integer to swap
 * @return the swapped integer
 */
float
Mat_floatSwap( float *a )
{

    union {
        char  i1[4];
        float r4;
    } tmp;

    tmp.r4 = *a;

    swap( tmp.i1[0], tmp.i1[3] );
    swap( tmp.i1[1], tmp.i1[2] );

    *a = tmp.r4;
    return *a;

}

/** @brief swap the bytes of a 4 or 8 byte double-precision float
 * @ingroup mat_internal
 * @param a pointer to integer to swap
 * @return the swapped integer
 */
double
Mat_doubleSwap( double *a )
{
#ifndef SIZEOF_DOUBLE
#define SIZEOF_DOUBLE 8
#endif

    union {
        char   a[SIZEOF_DOUBLE];
        double b;
    } tmp;

    tmp.b = *a;

#if SIZEOF_DOUBLE == 4
    swap( tmp.a[0], tmp.a[3] );
    swap( tmp.a[1], tmp.a[2] );
#elif SIZEOF_DOUBLE == 8
    swap( tmp.a[0], tmp.a[7] );
    swap( tmp.a[1], tmp.a[6] );
    swap( tmp.a[2], tmp.a[5] );
    swap( tmp.a[3], tmp.a[4] );
#elif SIZEOF_DOUBLE == 16
    swap( tmp.a[0], tmp.a[15] );
    swap( tmp.a[1], tmp.a[14] );
    swap( tmp.a[2], tmp.a[13] );
    swap( tmp.a[3], tmp.a[12] );
    swap( tmp.a[4], tmp.a[11] );
    swap( tmp.a[5], tmp.a[10] );
    swap( tmp.a[6], tmp.a[9] );
    swap( tmp.a[7], tmp.a[8] );
#endif
    *a = tmp.b;
    return *a;

}
