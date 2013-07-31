/*
 * Copyright (C) 2010-2013   Christopher C. Hulbert
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
#ifndef MATIO_PUBCONF_H
#define MATIO_PUBCONF_H 1

/* Matio major version number */
#define MATIO_MAJOR_VERSION 1

/* Matio minor version number */
#define MATIO_MINOR_VERSION 5

/* Matio release level number */
#define MATIO_RELEASE_LEVEL 2

/* Matio version number */
#define MATIO_VERSION 152

/* Default file format */
#define MAT_FT_DEFAULT MAT_FT_MAT5

/* Define to 1 if you have the <stdint.h> header file. */
#undef MATIO_HAVE_STDINT_H

/* Define to 1 if you have the <inttypes.h> header file. */
#undef MATIO_HAVE_INTTYPES_H

/* int16 type */
#define _mat_int16_t short

/* int32 type */
#define _mat_int32_t int

/* int64 type */
#define _mat_int64_t long long

/* int8 type */
#define _mat_int8_t signed char

/* int16 type */
#define _mat_uint16_t unsigned short

/* int32 type */
#define _mat_uint32_t unsigned

/* int64 type */
#define _mat_uint64_t unsigned long long

/* int8 type */
#define _mat_uint8_t unsigned char

#if MATIO_HAVE_INTTYPES_H
#   include <inttypes.h>
#endif

#if MATIO_HAVE_STDINT_H
#   include <stdint.h>
#endif

#ifdef _mat_int64_t
    typedef _mat_int64_t mat_int64_t;
#endif
#ifdef _mat_uint64_t
    typedef _mat_uint64_t mat_uint64_t;
#endif
#ifdef _mat_int32_t
    typedef _mat_int32_t mat_int32_t;
#endif
#ifdef _mat_uint32_t
    typedef _mat_uint32_t mat_uint32_t;
#endif
#ifdef _mat_int16_t
    typedef _mat_int16_t mat_int16_t;
#endif
#ifdef _mat_uint16_t
    typedef _mat_uint16_t mat_uint16_t;
#endif
#ifdef _mat_int8_t
    typedef _mat_int8_t mat_int8_t;
#endif
#ifdef _mat_uint8_t
    typedef _mat_uint8_t mat_uint8_t;
#endif

#endif /* MATIO_PUBCONF_H */
