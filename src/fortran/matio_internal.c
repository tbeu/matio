/*
 * Copyright (c) 2015-2022, The matio contributors
 * Copyright (c) 2005-2014, Christopher C. Hulbert
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#if defined(HAVE_STRINGS_H)
#include <strings.h>
#endif
#include "matio.h"
#include "matio_private.h"

#define fmat_loginit_c \
            FC_FUNC_(fmat_loginit_c,FMAT_LOGINIT_C)
#define fmat_create_c \
            FC_FUNC_(fmat_create_c,FMAT_CREATE_C)
#define fmat_open_c \
            FC_FUNC_(fmat_open_c,FMAT_OPEN_C)
#define fmat_close_c \
            FC_FUNC_(fmat_close_c,FMAT_CLOSE_C)
#define fmat_varcreate_c \
            FC_FUNC_(fmat_varcreate_c,FMAT_VARCREATE_C)
#define fmat_varfree_c \
            FC_FUNC_(fmat_varfree_c,FMAT_VARFREE_C)
#define fmat_varprint_c \
            FC_FUNC_(fmat_varprint_c,FMAT_VARPRINT_C)
#define fmat_varread_c \
            FC_FUNC_(fmat_varread_c,FMAT_VARREAD_C)
#define fmat_varreaddata_c \
            FC_FUNC_(fmat_varreaddata_c,FMAT_VARREADDATA_C)
#define fmat_varreadinfo_c \
            FC_FUNC_(fmat_varreadinfo_c,FMAT_VARREADINFO_C)
#define fmat_varreadnextinfo_c \
            FC_FUNC_(fmat_varreadnextinfo_c,FMAT_VARREADNEXTINFO_C)
#define fmat_varwriteinfo_c \
            FC_FUNC_(fmat_varwriteinfo_c,FMAT_VARWRITEINFO_C)
#define fmat_varwritedata_c \
            FC_FUNC_(fmat_varwritedata_c,FMAT_VARWRITEDATA_C)
#define fmat_varwrite_c \
            FC_FUNC_(fmat_varwrite_c,FMAT_VARWRITE_C)
#define fmat_getinfo \
            FC_FUNC_(fmat_getinfo,FMAT_GETINFO)
#define fmat_vargetnumberoffields_c \
            FC_FUNC_(fmat_vargetnumberoffields_c,FMAT_VARGETNUMBEROFFIELDS_C)
#define fmat_vargetstructfield_byname_c \
            FC_FUNC_(fmat_vargetstructfield_byname_c,FMAT_VARGETSTRUCTFIELD_BYNAME_C)

struct fmat_t {
    char   header[128];
    mat_t *mat_t_c_ptr;
} fmat_t;

struct fmatvar_t {
    int       nbytes;
    int       rank;
    int       data_type;
    int       data_size;
    int       class_type;
    int       isComplex;
    int       isGlobal;
    int       isLogical;
    size_t    dims[7];
    char      name[64];
    matvar_t *matvar_t_c_ptr;
} fmatvar_t;

void
fmat_loginit_c(char *prog_name, int len)
{
    char *name;

    name = malloc(len+1);
    strncpy(name,prog_name,len);
    name[len] = '\0';
    Mat_LogInit(name);
    free(name);
    return;
}

void fstr2cstr(char *fstr,int len)
{
    char *ptr;
    ptr = fstr+len-1;
    while ( *ptr == ' ' ) {
        *ptr = '\0';
        ptr--;
    }
}

char *fstrdup(char *fstr,int len)
{
    char *cstr;
    cstr = malloc(len+1);
    strncpy(cstr,fstr,len);
    cstr[len] = '\0';
    fstr2cstr(cstr,len);
    return cstr;
}

int
fmat_open_c(char *filename, int *mode, struct fmat_t *mat, int len)
{
    char *fname;
    int err = 0;

    fname = fstrdup(filename,len);

    if ( NULL == (mat->mat_t_c_ptr = Mat_Open(fname,*mode)) ) {
        Mat_Critical("Error opening file %s", fname);
        err = 1;
    } else {
        if (mat->mat_t_c_ptr->version & MAT_FT_MAT4) {
            /* V-4 matlab files don't have a header */
            strncpy(mat->header, "INVALID - V4 FORMAT",
                    sizeof(mat->header) - 1);
        } else {
            strncpy(mat->header,mat->mat_t_c_ptr->header,
                    strlen(mat->mat_t_c_ptr->header));
        }
        mat->header[sizeof(mat->header) - 1] = '\0';
    }
    free(fname);

    return err;
}

int
fmat_create_c(char *filename,enum mat_ft mat_file_ver,struct fmat_t *mat,
    char *header,int len,int hdrlen)
{
    char *fname,*hdr_str = NULL;
    int err = 0;

    fname = fstrdup(filename,len);

    if ( header != NULL )
        hdr_str = fstrdup(header,hdrlen);

    if (NULL == (mat->mat_t_c_ptr=Mat_CreateVer(fname,hdr_str,mat_file_ver))) {
        Mat_Critical("Error opening file %s", fname);
        err = 1;
    } else {
        strncpy(mat->header,mat->mat_t_c_ptr->header,
                strlen(mat->mat_t_c_ptr->header));
    }
    free(fname);

    return err;
}

int
fmat_close_c(struct fmat_t *mat)
{
    if ( mat->mat_t_c_ptr != NULL )
        Mat_Close(mat->mat_t_c_ptr);
    else
        return 1;
    return 0;
}

void
fmat_varprint_c(struct fmatvar_t *matvar)
{
    Mat_VarPrint(matvar->matvar_t_c_ptr,1);
    return;
}

int
fmat_varreadinfo_c(struct fmat_t *mat,char *name,struct fmatvar_t *matvar,int len)
{
    char *varname;
    int   err = 0;

    varname = fstrdup(name,len);
    if ( varname != NULL ) {

        matvar->matvar_t_c_ptr = Mat_VarReadInfo(mat->mat_t_c_ptr,varname);
        free(varname);
        if ( matvar->matvar_t_c_ptr != NULL ) {
            matvar->nbytes     = matvar->matvar_t_c_ptr->nbytes;
            matvar->rank       = matvar->matvar_t_c_ptr->rank;
            matvar->data_type  = matvar->matvar_t_c_ptr->data_type;
            matvar->data_size  = matvar->matvar_t_c_ptr->data_size;
            matvar->class_type = matvar->matvar_t_c_ptr->class_type;
            matvar->isComplex  = matvar->matvar_t_c_ptr->isComplex;
            matvar->isGlobal   = matvar->matvar_t_c_ptr->isGlobal;
            matvar->isLogical  = matvar->matvar_t_c_ptr->isLogical;
            /* FIXME: Check that matvar->rank <= 7 */
            memcpy(matvar->dims,matvar->matvar_t_c_ptr->dims,
                   matvar->rank*sizeof(matvar->dims));
            memset(matvar->name,0,sizeof(matvar->name));
            /* FIXME: Check that strlen(matvar->matvar_t_c_ptr->name) <= 64 */
            strncpy(matvar->name,matvar->matvar_t_c_ptr->name,
                    strlen(matvar->matvar_t_c_ptr->name));
        } else {
            err = 1;
        }
    } else {
        err = 1;
    }

    return err;
}

int
fmat_varreadnextinfo_c(struct fmat_t *mat,struct fmatvar_t *matvar)
{
    int   err = 0;

    matvar->matvar_t_c_ptr = Mat_VarReadNextInfo(mat->mat_t_c_ptr);
    if ( matvar->matvar_t_c_ptr != NULL ) {
        matvar->nbytes     = matvar->matvar_t_c_ptr->nbytes;
        matvar->rank       = matvar->matvar_t_c_ptr->rank;
        matvar->data_type  = matvar->matvar_t_c_ptr->data_type;
        matvar->data_size  = matvar->matvar_t_c_ptr->data_size;
        matvar->class_type = matvar->matvar_t_c_ptr->class_type;
        matvar->isComplex  = matvar->matvar_t_c_ptr->isComplex;
        matvar->isGlobal   = matvar->matvar_t_c_ptr->isGlobal;
        matvar->isLogical  = matvar->matvar_t_c_ptr->isLogical;
        /* FIXME: Check that matvar->rank <= 7 */
        memcpy(matvar->dims,matvar->matvar_t_c_ptr->dims,
               matvar->rank*sizeof(matvar->dims));
        memset(matvar->name,0,sizeof(matvar->name));
        /* FIXME: Check that strlen(matvar->matvar_t_c_ptr->name) <= 64 */
        strncpy(matvar->name,matvar->matvar_t_c_ptr->name,
                strlen(matvar->matvar_t_c_ptr->name));
    } else {
        err = 1;
    }
    return err;
}

int
fmat_varread_c(struct fmat_t *mat,char *name,struct fmatvar_t *matvar,int len)
{
    char *varname;
    int   err;

    varname = malloc(len+1);
    if ( varname != NULL ) {
        strncpy(varname,name,len);
        varname[len] = '\0';

        matvar->matvar_t_c_ptr = Mat_VarRead(mat->mat_t_c_ptr,varname);
        if ( matvar->matvar_t_c_ptr == NULL )
            err = 1;
    } else {
        err = 1;
    }

    return err;
}

int
fmat_varreaddata_c(struct fmat_t *mat,struct fmatvar_t *matvar,char *data,int *start,int *stride,int *edge)
{
    int *start_c, *stride_c, *edge_c, err, i;

    if ( (start == NULL) || (stride == NULL) || (edge == NULL) ) {
        start_c  = malloc(matvar->rank*sizeof(int));
        stride_c = malloc(matvar->rank*sizeof(int));
        edge_c   = malloc(matvar->rank*sizeof(int));
        if ( (start_c == NULL) || (stride_c == NULL) || (edge_c == NULL) ) {
            err = 1;
        } else {
            for ( i = 0; i < matvar->rank; i++ ) {
                start_c[i]  = 0;
                stride_c[i] = 1;
                edge_c[i]   = matvar->dims[i];
            }
            err = Mat_VarReadData(mat->mat_t_c_ptr,matvar->matvar_t_c_ptr,(data),start_c,
                                  stride_c,edge_c);
            free(start);
            free(stride);
            free(edge);
        }
    } else {
        err = Mat_VarReadData(mat->mat_t_c_ptr,matvar->matvar_t_c_ptr,(data),start,stride,
                              edge);
    }

    return err;
}

int
fmat_varwriteinfo_c(struct fmat_t *mat,struct fmatvar_t *matvar)
{
    int err = 0;
    if ( (mat == NULL) || (matvar == NULL) )
        err =  1;
    else if ( (matvar->matvar_t_c_ptr->name == NULL) ||
              (matvar->matvar_t_c_ptr->dims == NULL) )
        err = 2;
    else {
        err = Mat_VarWriteInfo(mat->mat_t_c_ptr,matvar->matvar_t_c_ptr);
    }

    return err;
}

int
fmat_varwritedata_c(struct fmat_t *mat,struct fmatvar_t *matvar,void *data,int *start,
    int *stride,int *edge)
{
    int *start_c, *stride_c, *edge_c, err = 0, i;

    if ( matvar == NULL || matvar->matvar_t_c_ptr == NULL || data == NULL ) {
       err = 1;
    } else if ( (start == NULL) || (stride == NULL) || (edge == NULL) ) {
        start_c  = malloc(matvar->matvar_t_c_ptr->rank*sizeof(int));
        stride_c = malloc(matvar->matvar_t_c_ptr->rank*sizeof(int));
        edge_c   = malloc(matvar->matvar_t_c_ptr->rank*sizeof(int));
        if ( (start_c == NULL) || (stride_c == NULL) || (edge_c == NULL) ) {
            err = 1;
        } else {
            for ( i = 0; i < matvar->matvar_t_c_ptr->rank; i++ ) {
                start_c[i]  = 0;
                stride_c[i] = 1;
                edge_c[i]   = matvar->matvar_t_c_ptr->dims[i];
            }
            err = Mat_VarWriteData(mat->mat_t_c_ptr,matvar->matvar_t_c_ptr,
                                   data,start_c,stride_c,edge_c);
            free(start_c);
            free(stride_c);
            free(edge_c);
        }
    } else {
        err = Mat_VarWriteData(mat->mat_t_c_ptr,matvar->matvar_t_c_ptr,data,
                               start,stride,edge);
    }

    return err;
}

int
fmat_varwrite_c(struct fmat_t *mat,struct fmatvar_t *matvar,void *data,
    int *compress)
{
    int err = 0;
    if ( (mat == NULL) || (matvar == NULL) || (data == NULL) ||
         (compress == NULL) )
        err =  1;
    else if ( (matvar->matvar_t_c_ptr->name == NULL) ||
              (matvar->matvar_t_c_ptr->dims == NULL) )
        err = 2;
    else {
        matvar->matvar_t_c_ptr->data = data;
        err = Mat_VarWrite(mat->mat_t_c_ptr,matvar->matvar_t_c_ptr,*compress);
        matvar->matvar_t_c_ptr->data = NULL;
    }

    return err;
}

int
fmat_varcreate_c(int *rank,size_t *dims,char *name,int *class_type,
    int *data_type, struct fmatvar_t *matvar,int len)
{
    char *varname;
    int err = 0, flags = 0;

    varname = malloc(len+1);
    if ( varname != NULL ) {
        strncpy(varname,name,len);
        varname[len] = '\0';
        if ( matvar->isComplex )
            flags |= MAT_F_COMPLEX;
        if ( matvar->isGlobal )
            flags |= MAT_F_GLOBAL;
        if ( matvar->isLogical )
            flags |= MAT_F_LOGICAL;
        matvar->matvar_t_c_ptr = Mat_VarCreate(varname,*class_type,*data_type,
                                               *rank,dims,NULL,flags);
        free(varname);
        if ( matvar->matvar_t_c_ptr == NULL ) {
            Mat_Critical("Mat_VarCreate returned NULL");
            err = 1;
        }
    }
    return err;
}

int
fmat_varfree_c(struct fmatvar_t *matvar)
{
    if ( matvar != NULL ) {
        Mat_VarFree(matvar->matvar_t_c_ptr);
#if 0
        matvar->matvar_t_c_ptr->data = NULL;
        matvar->matvar_t_c_ptr->dims = NULL;
        matvar->matvar_t_c_ptr->name = NULL;
        matvar->matvar_t_c_ptr->z    = NULL;
#endif
        return 0;
    } else {
        return 1;
    }
}

int
fmat_vargetnumberoffields_c(struct fmatvar_t *matvar)
{
    int nfields;
    if ( matvar == NULL || matvar->matvar_t_c_ptr == NULL )
        nfields = -1;
    else
        nfields = Mat_VarGetNumberOfFields(matvar->matvar_t_c_ptr);
    return nfields;
}

int
fmat_vargetstructfield_byname_c(struct fmatvar_t *matvar,char *name,int *index,
    struct fmatvar_t *field,int name_len)
{
    char *varname;
    int   err = 0;

    if ( matvar == NULL || field == NULL || name == NULL )
        return 1;

    varname = fstrdup(name,name_len);

    field->matvar_t_c_ptr = Mat_VarGetStructField(matvar->matvar_t_c_ptr,
        varname,MAT_BY_NAME,*index);
    free(varname);
    if ( field->matvar_t_c_ptr != NULL ) {
        field->nbytes     = field->matvar_t_c_ptr->nbytes;
        field->rank       = field->matvar_t_c_ptr->rank;
        field->data_type  = field->matvar_t_c_ptr->data_type;
        field->data_size  = field->matvar_t_c_ptr->data_size;
        field->class_type = field->matvar_t_c_ptr->class_type;
        field->isComplex  = field->matvar_t_c_ptr->isComplex;
        field->isGlobal   = field->matvar_t_c_ptr->isGlobal;
        field->isLogical  = field->matvar_t_c_ptr->isLogical;
        /* FIXME: Check that matvar->rank <= 7 */
        memcpy(field->dims,field->matvar_t_c_ptr->dims,
               field->rank*sizeof(matvar->dims));
        memset(matvar->name,0,sizeof(matvar->name));
        /* FIXME: Check that strlen(matvar->matvar_t_c_ptr->name) <= 64 */
        strncpy(field->name,field->matvar_t_c_ptr->name,
                strlen(field->matvar_t_c_ptr->name));
    } else {
        err = 1;
    }

    return err;
}

#if 0
void fmat_getinfo(fmatvar_t *matvar, int *rank, int *dims)
{
    *rank = matvar->rank;
    memcpy(dims,matvar->dims,matvar->rank*sizeof(int));
}
#endif
