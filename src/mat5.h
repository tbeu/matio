/*
 * Copyright (C) 2008   Christopher C. Hulbert
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
#ifndef MAT5_H
#define MAT5_H

#include "matio.h"

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

static size_t GetStructFieldBufSize(matvar_t *matvar);
static size_t GetCellArrayFieldBufSize(matvar_t *matvar);
static size_t GetMatrixMaxBufSize(matvar_t *matvar);
static int WriteEmptyCharData(mat_t *mat, int N, int data_type);
static int WriteEmptyData(mat_t *mat,int N,int data_type);
static int ReadNextCell( mat_t *mat, matvar_t *matvar );
static int ReadNextStructField( mat_t *mat, matvar_t *matvar );
static int ReadNextFunctionHandle(mat_t *mat, matvar_t *matvar);
static int WriteCellArrayFieldInfo(mat_t *mat,matvar_t *matvar);
static int WriteCellArrayField(mat_t *mat,matvar_t *matvar );
static int WriteStructField(mat_t *mat,matvar_t *matvar);
#if defined(HAVE_ZLIB)
static size_t WriteCompressedCharData(mat_t *mat,z_stream *z,void *data,int N,int data_type);
static int WriteCompressedEmptyData(mat_t *mat,z_stream *z,int N,int data_type);
static size_t WriteCompressedData(mat_t *mat,z_stream *z,void *data,int N,int data_type);
static size_t WriteCompressedCellArrayField(mat_t *mat,matvar_t *matvar,z_stream *z);
static size_t WriteCompressedStructField(mat_t *mat,matvar_t *matvar,z_stream *z);
#endif

/*   mat5.c    */
EXTERN mat_t *Mat_Create5(const char *matname,const char *hdr_str);

void      Mat_VarPrint5( matvar_t *matvar, int printdata );
matvar_t *Mat_VarReadNextInfo5( mat_t *mat );
void      Read5(mat_t *mat, matvar_t *matvar);
int       ReadData5(mat_t *mat,matvar_t *matvar,void *data, 
              int *start,int *stride,int *edge);
int       Mat_VarWrite5(mat_t *mat,matvar_t *matvar,int compress);
int       WriteCharDataSlab2(mat_t *mat,void *data,int data_type,int *dims,
              int *start,int *stride,int *edge);
int       WriteData(mat_t *mat,void *data,int N,int data_type);
int       WriteDataSlab2(mat_t *mat,void *data,int data_type,int *dims,
              int *start,int *stride,int *edge);
void      WriteInfo5(mat_t *mat, matvar_t *matvar);

#endif
