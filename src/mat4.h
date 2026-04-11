/*
 * Copyright (c) 2015-2026, The matio contributors
 * Copyright (c) 2008-2014, Christopher C. Hulbert
 * All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#ifndef MAT4_H
#define MAT4_H

#ifdef __cplusplus
#define EXTERN extern "C"
#else
#define EXTERN extern
#endif

EXTERN mat_t *Mat_Create4(const char *matname);

EXTERN int Mat_VarWrite4(const mat_t *mat, const matvar_t *matvar);
EXTERN int Mat_VarRead4(mat_t *mat, matvar_t *matvar);
EXTERN int Mat_VarReadData4(mat_t *mat, const matvar_t *matvar, void *data, const int *start,
                            const int *stride, const int *edge);
EXTERN int Mat_VarReadDataLinear4(mat_t *mat, matvar_t *matvar, void *data, int start, int stride,
                                  int edge);
EXTERN matvar_t *Mat_VarReadNextInfo4(mat_t *mat);

#endif
