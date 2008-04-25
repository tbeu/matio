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
#ifndef MAT4_H
#define MAT4_H

#include "matio.h"

void Read4(mat_t *mat, matvar_t *matvar);
int  ReadData4(mat_t *mat,matvar_t *matvar,void *data,
         int *start,int *stride,int *edge);

void Mat_VarPrint4( matvar_t *matvar, int printdata );
matvar_t *Mat_VarReadNextInfo4(mat_t *mat);

#endif
