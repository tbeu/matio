/*
 * Copyright (c) 2015-2026, The matio contributors
 * Copyright (c) 2008-2014, Christopher C. Hulbert
 * All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#ifndef MAT5_H
#define MAT5_H

#ifdef __cplusplus
#define EXTERN extern "C"
#else
#define EXTERN extern
#endif

EXTERN mat_t *Mat_Create5(const char *matname, const char *hdr_str);

EXTERN matvar_t *Mat_VarReadNextInfo5(mat_t *mat);
EXTERN int Mat_VarRead5(mat_t *mat, matvar_t *matvar);
EXTERN int Mat_VarReadData5(mat_t *mat, matvar_t *matvar, void *data, const int *start,
                            const int *stride, const int *edge);
EXTERN int Mat_VarReadDataLinear5(mat_t *mat, matvar_t *matvar, void *data, int start, int stride,
                                  int edge);
EXTERN int Mat_VarWrite5(mat_t *mat, matvar_t *matvar, int compress);

#endif
