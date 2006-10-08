/** @file matio.h
 * LIBMATIO Header
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

#ifndef MATIO_H
#define MATIO_H

#include <stdlib.h>
#include <stdio.h>
#include "matioConfig.h"
#if defined(HAVE_INTTYPES_H)
#   include <inttypes.h>
#endif
#if defined(HAVE_STDINT_H)
#   include <stdint.h>
#endif

#include <stdarg.h>
#if defined(HAVE_ZLIB)
#   include <zlib.h>
#endif

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
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

/** @defgroup MAT Matlab MAT File I/O Library */
/** @defgroup mat_internal Internal Functions */

/** @brief MAT file access types
 *
 * @ingroup MAT
 * MAT file access types
 */
enum mat_acc {
    MAT_ACC_RDONLY = 1,  /**< @brief Read only file access                */
    MAT_ACC_RDWR   = 2   /**< @brief Read/Write file access               */
};

/** @brief MAT file versions
 *
 * @ingroup MAT
 * MAT file versions
 */
enum mat_ft {
    MAT_FT_MAT5  = 1,        /**< @brief Matlab level-5 file                  */
    MAT_FT_MAT4  = 1 << 16   /**< @brief Version 4 file                       */
};


/** @brief Matlab data types
 *
 * @ingroup MAT
 * Matlab data types
 */
enum matio_types {
    MAT_T_UNKNOWN    =  0,    /**< @brief UNKOWN data type                    */
    MAT_T_INT8       =  1,    /**< @brief 8-bit signed integer data type      */
    MAT_T_UINT8      =  2,    /**< @brief 8-bit unsigned integer data type    */
    MAT_T_INT16      =  3,    /**< @brief 16-bit signed integer data type     */
    MAT_T_UINT16     =  4,    /**< @brief 16-bit unsigned integer data type   */
    MAT_T_INT32      =  5,    /**< @brief 32-bit signed integer data type     */
    MAT_T_UINT32     =  6,    /**< @brief 32-bit unsigned integer data type   */
    MAT_T_SINGLE     =  7,    /**< @brief IEEE 754 single precision data type */
    MAT_T_DOUBLE     =  9,    /**< @brief IEEE 754 double precision data type */
    MAT_T_INT64      = 12,    /**< @brief 64-bit signed integer data type     */
    MAT_T_UINT64     = 13,    /**< @brief 64-bit unsigned integer data type   */
    MAT_T_MATRIX     = 14,    /**< @brief matrix data type                    */
    MAT_T_COMPRESSED = 15,    /**< @brief compressed data type                */
    MAT_T_UTF8       = 16,    /**< @brief 8-bit unicode text data type        */
    MAT_T_UTF16      = 17,    /**< @brief 16-bit unicode text data type       */
    MAT_T_UTF32      = 18,    /**< @brief 32-bit unicode text data type       */

    MAT_T_STRING     = 20,    /**< @brief String data type                    */
    MAT_T_CELL       = 21,    /**< @brief Cell array data type                */
    MAT_T_STRUCT     = 22,    /**< @brief Structure data type                 */
    MAT_T_ARRAY      = 23,    /**< @brief Array data type                     */
    MAT_T_FUNCTION   = 24     /**< @brief Function data type                  */
};

/** @brief Matlab variable classes
 *
 * @ingroup MAT
 * Matlab variable classes
 */
enum matio_classes {
    MAT_C_CELL     =  1, /**< @brief Matlab cell array class               */
    MAT_C_STRUCT   =  2, /**< @brief Matlab structure class                */
    MAT_C_OBJECT   =  3, /**< @brief Matlab object class                   */
    MAT_C_CHAR     =  4, /**< @brief Matlab character array class          */
    MAT_C_SPARSE   =  5, /**< @brief Matlab sparse array class             */
    MAT_C_DOUBLE   =  6, /**< @brief Matlab double-precision class         */
    MAT_C_SINGLE   =  7, /**< @brief Matlab single-precision class         */
    MAT_C_INT8     =  8, /**< @brief Matlab signed 8-bit integer class     */
    MAT_C_UINT8    =  9, /**< @brief Matlab unsigned 8-bit integer class   */
    MAT_C_INT16    = 10, /**< @brief Matlab signed 16-bit integer class    */
    MAT_C_UINT16   = 11, /**< @brief Matlab unsigned 16-bit integer class  */
    MAT_C_INT32    = 12, /**< @brief Matlab signed 32-bit integer class    */
    MAT_C_UINT32   = 13, /**< @brief Matlab unsigned 32-bit integer class  */
    MAT_C_INT64    = 14, /**< @brief Matlab unsigned 32-bit integer class  */
    MAT_C_UINT64   = 15, /**< @brief Matlab unsigned 32-bit integer class  */
    MAT_C_FUNCTION = 16 /**< @brief Matlab unsigned 32-bit integer class  */
};

/** @brief Matlab array flags
 *
 * @ingroup MAT
 * Matlab array flags
 */
enum matio_flags {
    MAT_F_COMPLEX = 0x0800,    /**< @brief Complex bit flag */
    MAT_F_GLOBAL  = 0x0400,    /**< @brief Global bit flag */
    MAT_F_LOGICAL = 0x0200,    /**< @brief Logical bit flag */
    MAT_F_CLASS_T = 0x00ff     /**< @brief Class-Type bits flag */
};

#define MEM_CONSERVE 1       /**< @brief Conserve memory */

/** @brief Matlab compression options
 *
 * @ingroup MAT
 * Matlab compression options
 */
enum matio_compression {
    COMPRESSION_NONE = 0,   /**< @brief No compression */
    COMPRESSION_ZLIB = 1    /**< @brief zlib compression */
};

/** @brief matio lookup type
 *
 * @ingroup MAT
 * matio lookup type
 */
enum {
    BY_NAME  = 1, /**< Lookup by name */
    BY_INDEX = 2  /**< Lookup by index */
};

/** @brief Complex data type using split storage
 *
 * Complex data type using split real/imaginary pointers
 * @ingroup MAT
 */
struct ComplexSplit {
    void *Re; /**< Pointer to the real part */
    void *Im; /**< Pointer to the imaginary part */
};

/** @brief Matlab MAT File information
 *
 * Contains information about a Matlab MAT file
 * @ingroup MAT
 */
typedef struct mat_t {
    FILE *fp;               /**< Pointer to the MAT file */
    char *header;           /**< MAT File header string */
    char *subsys_offset;    /**< offset */
    char *filename;         /**< Name of the file that fp points to */
    int   version;          /**< MAT File version */
    int   byteswap;         /**< 1 if byte swapping is required, 0 else */
    int   mode;             /**< Access mode */
    long  bof;              /**< Beginning of file not including header */
} mat_t;

/** @brief Matlab variable information
 *
 * Contains information about a Matlab variable
 * @ingroup MAT
 */
typedef struct matvar_t {
    int   nbytes;       /**< Number of bytes for the MAT variable */
    int   rank;         /**< Rank (Number of dimensions) of the data */
    int   data_type;    /**< Data type(MAT_T_*) */
    int   data_size;    /**< Bytes / element for the data */
    int   class_type;   /**< Class type in Matlab(mxDOUBLE_CLASS, etc) */
    int   isComplex;    /**< non-zero if the data is complex, 0 if real */
    int   isGlobal;     /**< non-zero if the variable is global */
    int   isLogical;    /**< non-zero if the variable is logical */
    int  *dims;         /**< Array of lengths for each dimension */
    char *name;         /**< Name of the variable */
    void *data;         /**< Pointer to the data */
    int   mem_conserve; /**< 1 if Memory was conserved with data */
    int   compression;  /**< Compression (0=>None,1=>ZLIB) */
    long  fpos;         /**< Offset from the beginning of the MAT file to the variable */
    long  datapos;      /**< Offset from the beginning of the MAT file to the data */
    mat_t    *fp;       /**< Pointer to the MAT file structure (mat_t) */
#if defined(HAVE_ZLIB)
    z_stream *z;        /**< zlib compression state */
#endif
} matvar_t;

/** @brief sparse data information
 *
 * Contains information and data for a sparse matrix
 * @ingroup MAT
 */
typedef struct sparse_t {
    int nzmax;               /**< Maximum number of non-zero elements */
    int *ir;                 /**< Array of size nzmax where ir[k] is the row of
                               *  data[k].  0 <= k <= nzmax
                               */
    int nir;                 /**< number of elements in ir */
    int *jc;                 /**< Array size N+1 (N is number of columsn) with
                               *  jc[k] being the index into ir/data of the
                               *  first non-zero element for row k.
                               */
    int   njc;               /**< Number of elements in jc */
    int   ndata;             /**< Number of complex/real data values */
    void *data;              /**< Array of data elements */
} sparse_t;

/*     io.c         */
EXTERN char  *strdup_vprintf(const char *format, va_list ap);
EXTERN char  *strdup_printf(const char *format, ...);
EXTERN int    Mat_SetVerbose( int verb, int s );
EXTERN int    Mat_SetDebug( int d );
EXTERN void   Mat_Critical( const char *format, ... );
EXTERN void   Mat_Error( const char *format, ... );
EXTERN void   Mat_Help( const char *helpstr[] );
EXTERN int    Mat_LogInit( char *progname );
EXTERN int    Mat_LogClose(void);
EXTERN int    Mat_LogInitFunc(char *prog_name,
                    void (*log_func)(int log_level, char *message) );
EXTERN int    Mat_Message( const char *format, ... );
EXTERN int    Mat_DebugMessage( int level, const char *format, ... );
EXTERN int    Mat_VerbMessage( int level, const char *format, ... );
EXTERN void   Mat_Warning( const char *format, ... );
EXTERN size_t Mat_SizeOf(int data_type);
EXTERN size_t Mat_SizeOfClass(int class_type);

/*   MAT File functions   */
EXTERN mat_t  *Mat_Create(const char *matname,const char *hdr_str);
EXTERN int     Mat_Close(mat_t *mat);
EXTERN mat_t  *Mat_Open(const char *matname,int mode);
EXTERN int     Mat_Rewind(mat_t *mat);

/* MAT variable functions */
EXTERN matvar_t  *Mat_VarCalloc(void);
EXTERN matvar_t  *Mat_VarCreate(const char *name,int class_type,int data_type,
                      int rank,int *dims,void *data, int opt);
EXTERN int        Mat_VarDelete(mat_t *mat, char *name);
EXTERN matvar_t  *Mat_VarDuplicate(const matvar_t *in, int opt);
EXTERN void       Mat_VarFree(matvar_t *matvar);
EXTERN matvar_t  *Mat_VarGetCell(matvar_t *matvar,int index);
EXTERN matvar_t **Mat_VarGetCells(matvar_t *matvar,int *start,int *stride,
                      int *edge);
EXTERN matvar_t **Mat_VarGetCellsLinear(matvar_t *matvar,int start,int stride,
                      int edge);
EXTERN size_t     Mat_VarGetSize(matvar_t *matvar);
EXTERN int        Mat_VarGetNumberOfFields(matvar_t *matvar);
EXTERN int        Mat_VarAddStructField(matvar_t *matvar,matvar_t **fields);
EXTERN matvar_t  *Mat_VarGetStructField(matvar_t *matvar,void *name_or_index,
                      int opt,int index);
EXTERN matvar_t  *Mat_VarGetStructs(matvar_t *matvar,int *start,int *stride,
                      int *edge,int copy_fields);
EXTERN matvar_t  *Mat_VarGetStructsLinear(matvar_t *matvar,int start,int stride,
                      int edge,int copy_fields);
EXTERN void       Mat_VarPrint( matvar_t *matvar, int printdata );
EXTERN matvar_t  *Mat_VarRead(mat_t *mat, char *name );
EXTERN int        Mat_VarReadData(mat_t *mat,matvar_t *matvar,void *data,
                      int *start,int *stride,int *edge);
EXTERN int        Mat_VarReadDataAll(mat_t *mat,matvar_t *matvar);
EXTERN int        Mat_VarReadDataLinear(mat_t *mat,matvar_t *matvar,void *data,
                      int start,int stride,int edge);
EXTERN matvar_t  *Mat_VarReadInfo( mat_t *mat, char *name );
EXTERN matvar_t  *Mat_VarReadNext( mat_t *mat );
EXTERN matvar_t  *Mat_VarReadNextInfo( mat_t *mat );
EXTERN int        Mat_VarWrite(mat_t *mat,matvar_t *matvar,int compress );
EXTERN int        Mat_VarWriteInfo(mat_t *mat,matvar_t *matvar);
EXTERN int        Mat_VarWriteData(mat_t *mat,matvar_t *matvar,void *data,
                      int *start,int *stride,int *edge);

/* Other functions */
EXTERN int       Mat_CalcSingleSubscript(int rank,int *dims,int *subs);
EXTERN int      *Mat_CalcSubscripts(int rank,int *dims,int index);

#endif
