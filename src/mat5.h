/*
 * Copyright (C) 2005   Christopher C. Hulbert
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#ifndef MAT5_H
#define MAT5_H

#include "matio.h"

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

EXTERN int WriteCellArrayField( mat_t *mat, matvar_t *matvar,
                                int compress );
#if defined(HAVE_ZLIB)
EXTERN size_t WriteCompressedStructField(mat_t *mat,matvar_t *matvar,z_stream *z);
#endif
EXTERN int WriteStructField(mat_t *mat,matvar_t *matvar);
EXTERN int ReadNextStructField( mat_t *mat, matvar_t *matvar );
EXTERN int ReadNextCell( mat_t *mat, matvar_t *matvar );
EXTERN int WriteEmptyCharData(mat_t *mat, int N, int data_type);

/*   mat5.c    */
void      Mat_VarPrint5( matvar_t *matvar, int printdata );
matvar_t *Mat_VarReadNextInfo5( mat_t *mat );
void      Read5(mat_t *mat, matvar_t *matvar);
int       ReadData5(mat_t *mat,matvar_t *matvar,void *data, 
              int *start,int *stride,int *edge);
int       Write5(mat_t *mat,matvar_t *matvar,int compress);
int       WriteCharDataSlab2(mat_t *mat,void *data,int data_type,int *dims,
              int *start,int *stride,int *edge);
#if defined(HAVE_ZLIB)
size_t    WriteCompressedData(mat_t *mat,z_stream *z,void *data,int N,
              int data_type);
size_t    WriteCompressedCharData(mat_t *mat,z_stream *z,void *data,int N,
              int data_type);
#endif
int       WriteData(mat_t *mat,void *data,int N,int data_type);
int       WriteDataSlab2(mat_t *mat,void *data,int data_type,int *dims,
              int *start,int *stride,int *edge);
void      WriteInfo5(mat_t *mat, matvar_t *matvar);

#endif
