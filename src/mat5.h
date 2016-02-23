/*
 * Copyright (C) 2008-2016   Christopher C. Hulbert
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
#ifndef MAT5_H
#define MAT5_H

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

static size_t GetStructFieldBufSize(matvar_t *matvar);
static size_t GetCellArrayFieldBufSize(matvar_t *matvar);
static size_t GetMatrixMaxBufSize(matvar_t *matvar);
static size_t GetEmptyMatrixMaxBufSize(const char *name,int rank);
static size_t WriteCharData(mat_t *mat, void *data, int N,enum matio_types data_type);
static size_t WriteEmptyCharData(mat_t *mat, int N, enum matio_types data_type);
static size_t WriteEmptyData(mat_t *mat,int N,enum matio_types data_type);
static size_t ReadNextCell( mat_t *mat, matvar_t *matvar );
static size_t ReadNextStructField( mat_t *mat, matvar_t *matvar );
static size_t ReadNextFunctionHandle(mat_t *mat, matvar_t *matvar);
static int WriteCellArrayFieldInfo(mat_t *mat,matvar_t *matvar);
static int WriteCellArrayField(mat_t *mat,matvar_t *matvar );
static int WriteStructField(mat_t *mat,matvar_t *matvar);
static size_t Mat_WriteEmptyVariable5(mat_t *mat,const char *name,int rank,
                  size_t *dims);
#if defined(HAVE_ZLIB)
static size_t WriteCompressedCharData(mat_t *mat,z_streamp z,void *data,int N,
                  enum matio_types data_type);
static size_t WriteCompressedEmptyData(mat_t *mat,z_streamp z,int N,
                  enum matio_types data_type);
static size_t WriteCompressedData(mat_t *mat,z_streamp z,void *data,int N,
                  enum matio_types data_type);
static size_t WriteCompressedCellArrayField(mat_t *mat,matvar_t *matvar,
                  z_streamp z);
static size_t WriteCompressedStructField(mat_t *mat,matvar_t *matvar,
                  z_streamp z);
static size_t Mat_WriteCompressedEmptyVariable5(mat_t *mat,const char *name,
                  int rank,size_t *dims,z_streamp z);
#endif

/*   mat5.c    */
EXTERN mat_t *Mat_Create5(const char *matname,const char *hdr_str);

matvar_t *Mat_VarReadNextInfo5( mat_t *mat );
void      Read5(mat_t *mat, matvar_t *matvar);
int       ReadData5(mat_t *mat,matvar_t *matvar,void *data,
              int *start,int *stride,int *edge);
int       Mat_VarReadDataLinear5(mat_t *mat,matvar_t *matvar,void *data,
              int start,int stride,int edge);
int       Mat_VarWrite5(mat_t *mat,matvar_t *matvar,int compress);
int       WriteCharDataSlab2(mat_t *mat,void *data,enum matio_types data_type,
              size_t *dims,int *start,int *stride,int *edge);
int       WriteData(mat_t *mat,void *data,int N,enum matio_types data_type);
int       WriteDataSlab2(mat_t *mat,void *data,enum matio_types data_type,
              size_t *dims,int *start,int *stride,int *edge);
void      WriteInfo5(mat_t *mat, matvar_t *matvar);

#endif
