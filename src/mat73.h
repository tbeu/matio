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
#ifndef MAT73_H
#define MAT73_H

#include "hdf5.h"
#include "matio.h"

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

EXTERN mat_t    *Mat_Create73(const char *matname,const char *hdr_str);

EXTERN void      Mat_VarPrint73(matvar_t *matvar,int printdata);
EXTERN void      Mat_VarRead73(mat_t *mat,matvar_t *matvar);
EXTERN matvar_t *Mat_VarReadNextInfo73(mat_t *mat);
EXTERN int       Mat_VarWrite73(mat_t *mat,matvar_t *matvar,int compress);

#endif
