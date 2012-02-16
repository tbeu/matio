/*
 * Copyright (C) 2012   Christopher C. Hulbert
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
#include <stdlib.h>
#include <string.h>
#include "matio_private.h"

/** @brief Adds a field to a structure
 *
 * Adds the given field to the structure. fields should be an array of matvar_t
 * pointers of the same size as the structure (i.e. 1 field per structure
 * element).
 * @ingroup MAT
 * @param matvar Pointer to the Structure MAT variable
 * @param fields Array of fields to be added
 * @retval 0 on success
 */
int
Mat_VarAddStructField(matvar_t *matvar,matvar_t **fields)
{
    int       i, f, nfields, nmemb, cnt = 0;
    matvar_t **new_data,**old_data;

    if ( matvar == NULL || fields == NULL )
        return -1;
    nmemb = 1;
    for ( i = 0; i < matvar->rank; i++ )
        nmemb *= matvar->dims[i];

    nfields = matvar->nbytes / (nmemb*sizeof(matvar_t *));

    new_data = malloc((nfields+1)*nmemb*sizeof(matvar_t *));
    if ( new_data == NULL )
        return -1;

    old_data = matvar->data;
    for ( i = 0; i < nmemb; i++ ) {
        for ( f = 0; f < nfields; f++ )
            new_data[cnt++] = old_data[i*nfields+f];
        new_data[cnt++] = fields[i];
    }

    free(matvar->data);
    matvar->data = new_data;
    matvar->nbytes = (nfields+1)*nmemb*sizeof(matvar_t *);

    return 0;
}

/** @brief Returns the number of fields in a structure variable
 *
 * Returns the number of fields in the given structure.
 * @ingroup MAT
 * @param matvar Structure matlab variable
 * @returns Number of fields, or a negative number on error
 */
int
Mat_VarGetNumberOfFields(matvar_t *matvar)
{
    int i, nfields, nmemb = 1;
    if ( matvar == NULL || matvar->class_type != MAT_C_STRUCT   ||
         matvar->data_size == 0 ) {
        nfields = -1;
    } else {
        for ( i = 0; i < matvar->rank; i++ )
            nmemb *= matvar->dims[i];
        if ( nmemb > 0 )
            nfields = matvar->nbytes / (nmemb*matvar->data_size);
        else
            nfields = matvar->nbytes / (matvar->data_size);
    }
    return nfields;
}

/** @brief Finds a field of a structure by the field's index
 *
 * Returns a pointer to the structure field at the given 0-relative index.
 * @ingroup MAT
 * @param matvar Pointer to the Structure MAT variable
 * @param field_index 0-relative index of the field.
 * @param index linear index of the structure array
 * @return Pointer to the structure field on success, NULL on error
 */
matvar_t *
Mat_VarGetStructFieldByIndex(matvar_t *matvar,size_t field_index,size_t index)
{
    int       i, err = 0, nfields, nmemb;
    matvar_t *field = NULL;

    if ( matvar == NULL || matvar->class_type != MAT_C_STRUCT   ||
         matvar->data_size == 0 )
        return field;

    nmemb = 1;
    for ( i = 0; i < matvar->rank; i++ )
        nmemb *= matvar->dims[i];

    if ( nmemb > 0 )
        nfields = matvar->nbytes / (nmemb*matvar->data_size);
    else
        nfields = matvar->nbytes / (matvar->data_size);

    if ( nmemb > 0 && index >= nmemb ) {
        Mat_Critical("Mat_VarGetStructField: structure index out of bounds");
    } else if ( nfields > 0 ) {
        if ( field_index > nfields ) {
            Mat_Critical("Mat_VarGetStructField: field index out of bounds");
        } else {
            field = *((matvar_t **)matvar->data+index*nfields+field_index);
        }
    }

    return field;
}

/** @brief Finds a field of a structure by the field's name
 *
 * Returns a pointer to the structure field at the given 0-relative index.
 * @ingroup MAT
 * @param matvar Pointer to the Structure MAT variable
 * @param name Name of the structure field
 * @param index linear index of the structure array
 * @return Pointer to the structure field on success, NULL on error
 */
matvar_t *
Mat_VarGetStructFieldByName(matvar_t *matvar,const char *field_name,
    size_t index)
{
    int       i, err = 0, nfields, nmemb;
    matvar_t *field = NULL;

    if ( matvar == NULL || matvar->class_type != MAT_C_STRUCT   ||
         matvar->data_size == 0 )
        return field;


    nmemb = 1;
    for ( i = 0; i < matvar->rank; i++ )
        nmemb *= matvar->dims[i];

    if ( nmemb > 0 )
        nfields = matvar->nbytes / (nmemb*matvar->data_size);
    else
        nfields = matvar->nbytes / (matvar->data_size);

    if ( index < 0 || (nmemb > 0 && index >= nmemb ) ) {
        Mat_Critical("Mat_VarGetStructField: structure index out of bounds");
    } else if ( nfields > 0 ) {
        matvar_t **fields = (matvar_t **)matvar->data+index*nfields;
        for ( i = 0; i < nfields; i++ ) {
            field = fields[i];
            if ( NULL != field && NULL != field->name &&
                 !strcmp(field->name,field_name) )
                break;
            else
                field = NULL;
        }
    }

    return field;
}

/** @brief Finds a field of a structure
 *
 * Returns a pointer to the structure field at the given 0-relative index.
 * @ingroup MAT
 * @param matvar Pointer to the Structure MAT variable
 * @param name_or_index Name of the field, or the 1-relative index of the field
 * If the index is used, it should be the address of an integer variable whose
 * value is the index number.
 * @param opt MAT_BY_NAME if the name_or_index is the name or MAT_BY_INDEX if
 *            the index was passed.
 * @param index linear index of the structure to find the field of
 * @return Pointer to the Structure Field on success, NULL on error
 */
matvar_t *
Mat_VarGetStructField(matvar_t *matvar,void *name_or_index,int opt,int index)
{
    int       i, err = 0, nfields, nmemb;
    matvar_t *field = NULL;

    nmemb = 1;
    for ( i = 0; i < matvar->rank; i++ )
        nmemb *= matvar->dims[i];

    if ( nmemb > 0 )
        nfields = matvar->nbytes / (nmemb*matvar->data_size);
    else
        nfields = matvar->nbytes / (matvar->data_size);

    if ( index < 0 || (nmemb > 0 && index >= nmemb ))
        err = 1;
    else if ( nfields < 1 )
        err = 1;

    if ( !err && (opt == MAT_BY_INDEX) ) {
        size_t field_index = *(int *)name_or_index;
        if ( field_index > 0 )
            field = Mat_VarGetStructFieldByIndex(matvar,field_index-1,index);
    } else if ( !err && (opt == MAT_BY_NAME) ) {
        field = Mat_VarGetStructFieldByName(matvar,name_or_index,index);
    }

    return field;
}

/** @brief Indexes a structure
 *
 * Finds structures of a structure array given a start, stride, and edge for
 * each dimension.  The structures are placed in a new structure array.  If
 * copy_fields is non-zero, the indexed structures are copied and should be
 * freed, but if copy_fields is zero, the indexed structures are pointers to
 * the original, but should still be freed since the mem_conserve flag is set
 * so that the structures are not freed.
 * MAT File version must be 5.
 * @ingroup MAT
 * @param matvar Structure matlab variable
 * @param start vector of length rank with 0-relative starting coordinates for
 *              each diemnsion.
 * @param stride vector of length rank with strides for each diemnsion.
 * @param edge vector of length rank with the number of elements to read in
 *              each diemnsion.
 * @param copy_fields 1 to copy the fields, 0 to just set pointers to them.
 *        If 0 is used, the fields should not be freed themselves.
 * @returns A new structure with fields indexed from matvar.
 */
matvar_t *
Mat_VarGetStructs(matvar_t *matvar,int *start,int *stride,int *edge,
    int copy_fields)
{
    int i, j, N, I = 0;
    int inc[10] = {0,}, cnt[10] = {0,}, dimp[10] = {0,};
    int nfields, field;
    matvar_t **fields, *struct_slab;

    if ( (matvar == NULL) || (start == NULL) || (stride == NULL) ||
         (edge == NULL) ) {
        return NULL;
    } else if ( matvar->rank > 10 ) {
        return NULL;
    } else if ( matvar->class_type != MAT_C_STRUCT ) {
        return NULL;
    }

    struct_slab = Mat_VarDuplicate(matvar,0);
    if ( !copy_fields )
        struct_slab->mem_conserve = 1;

    nfields = matvar->nbytes / matvar->data_size;
    for ( i = 0; i < matvar->rank; i++ )
        nfields = nfields / matvar->dims[i];

    inc[0] = stride[0]-1;
    dimp[0] = matvar->dims[0];
    N = edge[0];
    I = start[0]*nfields;
    for ( i = 1; i < matvar->rank; i++ ) {
        inc[i]  = stride[i]-1;
        dimp[i] = matvar->dims[i-1];
        for ( j = i ; j--; ) {
            inc[i]  *= matvar->dims[j]*nfields;
            dimp[i] *= matvar->dims[j+1];
        }
        N *= edge[i];
        if ( start[i] > 0 )
            I += start[i]*dimp[i-1]*nfields;
    }
    struct_slab->nbytes    = N*nfields*sizeof(matvar_t *);
    struct_slab->data = malloc(struct_slab->nbytes);
    if ( struct_slab->data == NULL ) {
        Mat_VarFree(struct_slab);
        return NULL;
    }
    fields = struct_slab->data;
    for ( i = 0; i < N; i+=edge[0] ) {
        for ( j = 0; j < edge[0]; j++ ) {
            for ( field = 0; field < nfields; field++ ) {
                if ( copy_fields )
                    fields[(i+j)*nfields+field] =
                         Mat_VarDuplicate(*((matvar_t **)matvar->data + I),1);
                else
                    fields[(i+j)*nfields+field] =
                        *((matvar_t **)matvar->data + I);
                I++;
            }
            I += stride[0]*nfields;
        }
        for ( j = 1; j < matvar->rank-1; j++ ) {
            cnt[j]++;
            if ( (cnt[j] % edge[j]) == 0 ) {
                cnt[j] = 0;
                if ( (I % dimp[j]) != 0 ) {
                    I += dimp[j]-(I % dimp[j]);
                }
            } else {
                I += matvar->dims[0]-edge[0]*stride[0]-start[0];
                I += inc[j];
                break;
            }
        }
    }
    return struct_slab;
}

/** @brief Indexes a structure
 *
 * Finds structures of a structure array given a single (linear)start, stride,
 * and edge.  The structures are placed in a new structure array.  If
 * copy_fields is non-zero, the indexed structures are copied and should be
 * freed, but if copy_fields is zero, the indexed structures are pointers to
 * the original, but should still be freed since the mem_conserve flag is set
 * so that the structures are not freed.
 * MAT File version must be 5.
 * @ingroup MAT
 * @param matvar Structure matlab variable
 * @param start starting index
 * @param stride stride
 * @param edge Number of structures to get
 * @param copy_fields 1 to copy the fields, 0 to just set pointers to them.
 *        If 0 is used, the fields should not be freed themselves.
 * @returns A new structure with fields indexed from matvar
 */
matvar_t *
Mat_VarGetStructsLinear(matvar_t *matvar,int start,int stride,int edge,
    int copy_fields)
{
    int i, I = 0, field, nfields;
    matvar_t *struct_slab, **fields;

    /* FIXME: Check allocations */
    if ( matvar == NULL || matvar->rank > 10 ) {
       struct_slab = NULL;
    } else {

        struct_slab = Mat_VarDuplicate(matvar,0);
        if ( !copy_fields )
            struct_slab->mem_conserve = 1;

        nfields = matvar->nbytes / matvar->data_size;
        for ( i = 0; i < matvar->rank; i++ )
            nfields = nfields / matvar->dims[i];

        struct_slab->nbytes = edge*nfields*sizeof(matvar_t *);
        struct_slab->data = malloc(struct_slab->nbytes);
        fields = struct_slab->data;
        for ( i = 0; i < edge; i++ ) {
            if ( copy_fields ) {
                for ( field = 0; field < nfields; field++ ) {
                    fields[i*nfields+field] =
                        Mat_VarDuplicate(*((matvar_t **)matvar->data+I),1);
                    I++;
                }
            } else {
                for ( field = 0; field < nfields; field++ ) {
                    fields[i+field] = *((matvar_t **)matvar->data + I);
                    I++;
                }
            }
            I += stride;
        }
    }
    return struct_slab;
}
