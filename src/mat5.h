/*
 * Copyright (C) 2008-2017   Christopher C. Hulbert
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

EXTERN mat_t    *Mat_Create5(const char *matname,const char *hdr_str);

EXTERN matvar_t *Mat_VarReadNextInfo5( mat_t *mat );
EXTERN void      Read5(mat_t *mat, matvar_t *matvar);
EXTERN int       ReadData5(mat_t *mat,matvar_t *matvar,void *data,
                     int *start,int *stride,int *edge);
EXTERN int       Mat_VarReadDataLinear5(mat_t *mat,matvar_t *matvar,void *data,
                     int start,int stride,int edge);
EXTERN int       Mat_VarWrite5(mat_t *mat,matvar_t *matvar,int compress);
EXTERN int       WriteCharDataSlab2(mat_t *mat,void *data,enum matio_types data_type,
                     size_t *dims,int *start,int *stride,int *edge);
EXTERN int       WriteData(mat_t *mat,void *data,int N,enum matio_types data_type);
EXTERN int       WriteDataSlab2(mat_t *mat,void *data,enum matio_types data_type,
                     size_t *dims,int *start,int *stride,int *edge);
EXTERN void      WriteInfo5(mat_t *mat, matvar_t *matvar);

#endif
