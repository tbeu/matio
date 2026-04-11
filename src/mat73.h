/*
 * Copyright (c) 2015-2026, The matio contributors
 * Copyright (c) 2005-2014, Christopher C. Hulbert
 * All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#ifndef MAT73_H
#define MAT73_H

#ifdef __cplusplus
#define EXTERN extern "C"
#else
#define EXTERN extern
#endif

EXTERN mat_t *Mat_Create73(const char *matname, const char *hdr_str);
EXTERN int Mat_Close73(mat_t *mat);
EXTERN int Mat_VarRead73(mat_t *mat, matvar_t *matvar);
EXTERN int Mat_VarReadData73(mat_t *mat, matvar_t *matvar, void *data, const int *start,
                             const int *stride, const int *edge);
EXTERN int Mat_VarReadDataLinear73(mat_t *mat, matvar_t *matvar, void *data, int start, int stride,
                                   int edge);
EXTERN matvar_t *Mat_VarReadNextInfo73(mat_t *mat, mat_iter_pred_t pred, const void *user_data);
EXTERN int Mat_VarWrite73(mat_t *mat, matvar_t *matvar, int compress);
EXTERN int Mat_VarWriteAppend73(mat_t *mat, matvar_t *matvar, int compress, int dim);
EXTERN int Mat_CalcDir73(mat_t *mat, size_t *n);

#endif
