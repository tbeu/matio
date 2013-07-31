/** @file mat.c
 * Matlab MAT version 5 file functions
 * @ingroup MAT
 */
/*
 * Copyright (C) 2005-2011   Christopher C. Hulbert
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

/* FIXME: Implement Unicode support */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
#if defined(_WIN64) || defined(_WIN32)
#   include <io.h>
#   define mktemp _mktemp
#endif
#ifdef _MSC_VER
#   define SIZE_T_FMTSTR "Iu"
#else
#   define SIZE_T_FMTSTR "zu"
#endif
#include "matio_private.h"
#include "mat5.h"
#include "mat4.h"
#if defined(MAT73) && MAT73
#   include "mat73.h"
#endif

static void
ReadData(mat_t *mat, matvar_t *matvar)
{
    if ( mat == NULL || matvar == NULL || mat->fp == NULL )
        return;
    else if ( mat->version == MAT_FT_MAT5 )
        Read5(mat,matvar);
#if defined(MAT73) && MAT73
    else if ( mat->version == MAT_FT_MAT73 )
        Mat_VarRead73(mat,matvar);
#endif
    else if ( mat->version == MAT_FT_MAT4 )
        Read4(mat,matvar);
    return;
}

static void
Mat_PrintNumber(enum matio_types type, void *data)
{
    switch ( type ) {
        case MAT_T_DOUBLE:
            printf("%g",*(double*)data);
            break;
        case MAT_T_SINGLE:
            printf("%g",*(float*)data);
            break;
#ifdef HAVE_MAT_INT64_T
        case MAT_T_INT64:
            printf("%lld",*(mat_int64_t*)data);
            break;
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_T_UINT64:
            printf("%llu",*(mat_uint64_t*)data);
            break;
#endif
        case MAT_T_INT32:
            printf("%d",*(mat_int32_t*)data);
            break;
        case MAT_T_UINT32:
            printf("%u",*(mat_uint32_t*)data);
            break;
        case MAT_T_INT16:
            printf("%hd",*(mat_int16_t*)data);
            break;
        case MAT_T_UINT16:
            printf("%hu",*(mat_uint16_t*)data);
            break;
        case MAT_T_INT8:
            printf("%hhd",*(mat_int8_t*)data);
            break;
        case MAT_T_UINT8:
            printf("%hhu",*(mat_uint8_t*)data);
            break;
        default:
            break;
    }
}

/*
 *====================================================================
 *                 Public Functions
 *====================================================================
 */

/** @brief Get the version of the library
 *
 * Gets the version number of the library
 * @param major Pointer to store the library major version number
 * @param major Pointer to store the library major version number
 * @param major Pointer to store the library major version number
 */
void
Mat_GetLibraryVersion(int *major,int *minor,int *release)
{
    if ( NULL != major )
        *major = MATIO_MAJOR_VERSION;
    if ( NULL != minor )
        *minor = MATIO_MINOR_VERSION;
    if ( NULL != release )
        *release = MATIO_RELEASE_LEVEL;
}

/** @brief Creates a new Matlab MAT file
 *
 * Tries to create a new Matlab MAT file with the given name and optional
 * header string.  If no header string is given, the default string
 * is used containing the software, version, and date in it.  If a header
 * string is given, at most the first 116 characters is written to the file.
 * The given header string need not be the full 116 characters, but MUST be
 * NULL terminated.
 * @ingroup MAT
 * @param matname Name of MAT file to create
 * @param hdr_str Optional header string, NULL to use default
 * @param mat_file_ver MAT file version to create
 * @return A pointer to the MAT file or NULL if it failed.  This is not a
 * simple FILE * and should not be used as one.
 */
mat_t *
Mat_CreateVer(const char *matname,const char *hdr_str,enum mat_ft mat_file_ver)
{
    mat_t *mat = NULL;

    switch ( mat_file_ver ) {
        case MAT_FT_MAT4:
            break;
        case MAT_FT_MAT5:
            mat = Mat_Create5(matname,hdr_str);
            break;
        case MAT_FT_MAT73:
#if defined(MAT73) && MAT73
            mat = Mat_Create73(matname,hdr_str);
#endif
            break;
    }

    return mat;
}

/** @brief Opens an existing Matlab MAT file
 *
 * Tries to open a Matlab MAT file with the given name
 * @ingroup MAT
 * @param matname Name of MAT file to open
 * @param mode File access mode (MAT_ACC_RDONLY,MAT_ACC_RDWR,etc).
 * @return A pointer to the MAT file or NULL if it failed.  This is not a
 * simple FILE * and should not be used as one.
 */
mat_t *
Mat_Open(const char *matname,int mode)
{
    FILE *fp = NULL;
    mat_int16_t tmp, tmp2;
    mat_t *mat = NULL;
    size_t bytesread = 0;

    if ( (mode & 0x00000001) == MAT_ACC_RDONLY ) {
        fp = fopen( matname, "rb" );
        if ( !fp )
            return NULL;
    } else if ( (mode & 0x00000001) == MAT_ACC_RDWR ) {
        fp = fopen( matname, "r+b" );
        if ( !fp ) {
            mat = Mat_CreateVer(matname,NULL,mode&0xfffffffe);
            return mat;
        }
    } else {
        mat = Mat_CreateVer(matname,NULL,mode&0xfffffffe);
        return mat;
    }

    mat = malloc(sizeof(*mat));
    if ( NULL == mat ) {
        Mat_Critical("Couldn't allocate memory for the MAT file");
        fclose(fp);
        return NULL;
    }

    mat->fp = fp;
    mat->header        = calloc(128,1);
    mat->subsys_offset = calloc(8,1);
    mat->filename      = NULL;
    mat->byteswap      = 0;
    mat->version       = 0;

    bytesread += fread(mat->header,1,116,fp);
    mat->header[116] = '\0';
    bytesread += fread(mat->subsys_offset,1,8,fp);
    bytesread += 2*fread(&tmp2,2,1,fp);
    bytesread += fread(&tmp,1,2,fp);

    if ( 128 == bytesread ) {
        /* v5 and v7.3 files have at least 128 byte header */
        mat->byteswap = -1;
        if (tmp == 0x4d49)
            mat->byteswap = 0;
        else if (tmp == 0x494d) {
            mat->byteswap = 1;
            Mat_int16Swap(&tmp2);
        }

        mat->version = (int)tmp2;
        if ( (mat->version == 0x0100 || mat->version == 0x0200) &&
             -1 != mat->byteswap ) {
            mat->bof = ftell(mat->fp);
            mat->next_index    = 0;
        } else {
            mat->version = 0;
        }
    }

    if ( 0 == mat->version ) {
        /* Maybe a V4 MAT file */
        matvar_t *var;
        if ( NULL != mat->header )
            free(mat->header);
        if ( NULL != mat->subsys_offset )
            free(mat->subsys_offset);

        mat->header        = NULL;
        mat->subsys_offset = NULL;
        mat->fp            = fp;
        mat->version       = MAT_FT_MAT4;
        mat->byteswap      = 0;
        mat->mode          = mode;
        mat->bof           = 0;
        mat->next_index    = 0;

        Mat_Rewind(mat);
        var = Mat_VarReadNextInfo4(mat);
        if ( NULL == var ) {
            /* Does not seem to be a valid V4 file */
            Mat_Critical("%s does not seem to be a valid MAT file",matname);
            Mat_Close(mat);
            mat = NULL;
        } else {
            Mat_VarFree(var);
            Mat_Rewind(mat);
        }
    }

    if ( NULL == mat )
        return mat;

    mat->filename = strdup_printf("%s",matname);
    mat->mode = mode;

    if ( mat->version == 0x0200 ) {
        fclose(mat->fp);
#if defined(MAT73) && MAT73

        mat->fp = malloc(sizeof(hid_t));

        if ( (mode & 0x00ff) == MAT_ACC_RDONLY )
            *(hid_t*)mat->fp=H5Fopen(mat->filename,H5F_ACC_RDONLY,H5P_DEFAULT);
        else if ( (mode & 0x00ff) == MAT_ACC_RDWR )
            *(hid_t*)mat->fp=H5Fopen(mat->filename,H5F_ACC_RDWR,H5P_DEFAULT);

        if ( -1 < *(hid_t*)mat->fp ) {
            hsize_t num_objs;
            H5Gget_num_objs(*(hid_t*)mat->fp,&num_objs);
            mat->num_datasets = num_objs;
            mat->refs_id      = -1;
        }
#else
        mat->fp = NULL;
        Mat_Close(mat);
        mat = NULL;
        Mat_Critical("No HDF5 support which is required to read the v7.3 "
                     "MAT file \"%s\"",matname);
#endif
    }

    return mat;
}

/** @brief Closes an open Matlab MAT file
 *
 * Closes the given Matlab MAT file and frees any memory with it.
 * @ingroup MAT
 * @param mat Pointer to the MAT file
 * @retval 0
 */
int
Mat_Close( mat_t *mat )
{
    if ( NULL != mat ) {
#if defined(MAT73) && MAT73
        if ( mat->version == 0x0200 ) {
            if ( mat->refs_id > -1 )
                H5Gclose(mat->refs_id);
            H5Fclose(*(hid_t*)mat->fp);
            free(mat->fp);
            mat->fp = NULL;
        }
#endif
        if ( mat->fp )
            fclose(mat->fp);
        if ( mat->header )
            free(mat->header);
        if ( mat->subsys_offset )
            free(mat->subsys_offset);
        if ( mat->filename )
            free(mat->filename);
        free(mat);
    }
    return 0;
}

/** @brief Gets the filename for the given MAT file
 *
 * Gets the filename for the given MAT file
 * @ingroup MAT
 * @param mat Pointer to the MAT file
 * @return MAT filename
 */
const char *
Mat_GetFilename(mat_t *matfp)
{
    const char *filename = NULL;
    if ( NULL != matfp )
        filename = matfp->filename;
    return filename;
}

/** @brief Gets the version of the given MAT file
 *
 * Gets the version of the given MAT file
 * @ingroup MAT
 * @param mat Pointer to the MAT file
 * @return MAT file version
 */
enum mat_ft
Mat_GetVersion(mat_t *matfp)
{
    enum mat_ft file_type = 0;
    if ( NULL != matfp )
        file_type = matfp->version;
    return file_type;
}

/** @brief Rewinds a Matlab MAT file to the first variable
 *
 * Rewinds a Matlab MAT file to the first variable
 * @ingroup MAT
 * @param mat Pointer to the MAT file
 * @retval 0 on success
 */
int
Mat_Rewind( mat_t *mat )
{
    switch ( mat->version ) {
        case MAT_FT_MAT73:
            mat->next_index = 0;
            break;
        case MAT_FT_MAT5:
            fseek(mat->fp,128L,SEEK_SET);
            break;
        case MAT_FT_MAT4:
            fseek(mat->fp,0L,SEEK_SET);
            break;
        default:
            return -1;
    }
    return 0;
}

/** @brief Returns the size of a Matlab Class
 *
 * Returns the size (in bytes) of the matlab class class_type
 * @ingroup MAT
 * @param class_type Matlab class type (MAT_C_*)
 * @returns Size of the class
 */
size_t
Mat_SizeOfClass(int class_type)
{
    switch (class_type) {
        case MAT_C_DOUBLE:
            return sizeof(double);
        case MAT_C_SINGLE:
            return sizeof(float);
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
            return sizeof(mat_int64_t);
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
            return sizeof(mat_uint64_t);
#endif
        case MAT_C_INT32:
            return sizeof(mat_int32_t);
        case MAT_C_UINT32:
            return sizeof(mat_uint32_t);
        case MAT_C_INT16:
            return sizeof(mat_int16_t);
        case MAT_C_UINT16:
            return sizeof(mat_uint16_t);
        case MAT_C_INT8:
            return sizeof(mat_int8_t);
        case MAT_C_UINT8:
            return sizeof(mat_uint8_t);
        case MAT_C_CHAR:
            return sizeof(mat_int16_t);
        default:
            return 0;
    }
}

/*
 *===================================================================
 *    MAT Variable Functions
 *===================================================================
 */

/** @brief Allocates memory for a new matvar_t and initializes all the fields
 *
 * @ingroup MAT
 * @return A newly allocated matvar_t
 */
matvar_t *
Mat_VarCalloc(void)
{
    matvar_t *matvar;

    matvar = malloc(sizeof(*matvar));

    if ( NULL != matvar ) {
        matvar->nbytes       = 0;
        matvar->rank         = 0;
        matvar->data_type    = MAT_T_UNKNOWN;
        matvar->data_size    = 0;
        matvar->class_type   = MAT_C_EMPTY;
        matvar->isComplex    = 0;
        matvar->isGlobal     = 0;
        matvar->isLogical    = 0;
        matvar->dims         = NULL;
        matvar->name         = NULL;
        matvar->data         = NULL;
        matvar->mem_conserve = 0;
        matvar->compression  = 0;
        matvar->internal     = malloc(sizeof(*matvar->internal));
        if ( NULL == matvar->internal ) {
            free(matvar);
            matvar = NULL;
        } else {
            matvar->internal->hdf5_name = NULL;
            matvar->internal->hdf5_ref  =  0;
            matvar->internal->id        = -1;
            matvar->internal->fp = NULL;
            matvar->internal->fpos         = 0;
            matvar->internal->datapos      = 0;
            matvar->internal->fieldnames   = NULL;
            matvar->internal->num_fields   = 0;
#if defined(HAVE_ZLIB)
            matvar->internal->z         = NULL;
#endif
        }
    }

    return matvar;
}

/** @brief Creates a MAT Variable with the given name and (optionally) data
 *
 * Creates a MAT variable that can be written to a Matlab MAT file with the
 * given name, data type, dimensions and data.  Rank should always be 2 or more.
 * i.e. Scalar values would have rank=2 and dims[2] = {1,1}.  Data type is
 * one of the MAT_T types.  MAT adds MAT_T_STRUCT and MAT_T_CELL to create
 * Structures and Cell Arrays respectively.  For MAT_T_STRUCT, data should be a
 * NULL terminated array of matvar_t * variables (i.e. for a 3x2 structure with
 * 10 fields, there should be 61 matvar_t * variables where the last one is
 * NULL).  For cell arrays, the NULL termination isn't necessary.  So to create
 * a cell array of size 3x2, data would be the address of an array of 6
 * matvar_t * variables.
 *
 * EXAMPLE:
 *   To create a struct of size 3x2 with 3 fields:
 * @code
 *     int rank=2, dims[2] = {3,2}, nfields = 3;
 *     matvar_t **vars;
 *
 *     vars = malloc((3*2*nfields+1)*sizeof(matvar_t *));
 *     vars[0]             = Mat_VarCreate(...);
 *        :
 *     vars[3*2*nfields-1] = Mat_VarCreate(...);
 *     vars[3*2*nfields]   = NULL;
 * @endcode
 *
 * EXAMPLE:
 *   To create a cell array of size 3x2:
 * @code
 *     int rank=2, dims[2] = {3,2};
 *     matvar_t **vars;
 *
 *     vars = malloc(3*2*sizeof(matvar_t *));
 *     vars[0]             = Mat_VarCreate(...);
 *        :
 *     vars[5] = Mat_VarCreate(...);
 * @endcode
 *
 * @ingroup MAT
 * @param name Name of the variable to create
 * @param class_type class type of the variable in Matlab(one of the mx Classes)
 * @param data_type data type of the variable (one of the MAT_T_ Types)
 * @param rank Rank of the variable
 * @param dims array of dimensions of the variable of size rank
 * @param data pointer to the data
 * @param opt 0, or bitwise or of the following options:
 * - MAT_F_DONT_COPY_DATA to just use the pointer to the data and not copy the
 *       data itself. Note that the pointer should not be freed until you are
 *       done with the mat variable.  The Mat_VarFree function will NOT free
 *       data that was created with MAT_F_DONT_COPY_DATA, so free it yourself.
 * - MAT_F_COMPLEX to specify that the data is complex.  The data variable
 *       should be a pointer to a mat_complex_split_t type.
 * - MAT_F_GLOBAL to assign the variable as a global variable
 * - MAT_F_LOGICAL to specify that it is a logical variable
 * @return A MAT variable that can be written to a file or otherwise used
 */
matvar_t *
Mat_VarCreate(const char *name,enum matio_classes class_type,
    enum matio_types data_type,int rank,size_t *dims,void *data,int opt)
{
    size_t i, nmemb = 1, nfields = 0, data_size;
    matvar_t *matvar = NULL;

    if (dims == NULL) return NULL;

    matvar = Mat_VarCalloc();
    if ( NULL == matvar )
        return NULL;

    matvar->compression = MAT_COMPRESSION_NONE;
    matvar->isComplex   = opt & MAT_F_COMPLEX;
    matvar->isGlobal    = opt & MAT_F_GLOBAL;
    matvar->isLogical   = opt & MAT_F_LOGICAL;
    if ( name )
        matvar->name = strdup_printf("%s",name);
    matvar->rank = rank;
    matvar->dims = malloc(matvar->rank*sizeof(*matvar->dims));
    for ( i = 0; i < matvar->rank; i++ ) {
        matvar->dims[i] = dims[i];
        nmemb *= dims[i];
    }
    matvar->class_type = class_type;
    matvar->data_type  = data_type;
    switch ( data_type ) {
        case MAT_T_INT8:
            data_size = 1;
            break;
        case MAT_T_UINT8:
            data_size = 1;
            break;
        case MAT_T_INT16:
            data_size = 2;
            break;
        case MAT_T_UINT16:
            data_size = 2;
            break;
        case MAT_T_INT64:
            data_size = 8;
            break;
        case MAT_T_UINT64:
            data_size = 8;
            break;
        case MAT_T_INT32:
            data_size = 4;
            break;
        case MAT_T_UINT32:
            data_size = 4;
            break;
        case MAT_T_SINGLE:
            data_size = sizeof(float);
            break;
        case MAT_T_DOUBLE:
            data_size = sizeof(double);
            break;
        case MAT_T_UTF8:
            data_size = 1;
            break;
        case MAT_T_UTF16:
            data_size = 2;
            break;
        case MAT_T_UTF32:
            data_size = 4;
            break;
        case MAT_T_CELL:
            data_size = sizeof(matvar_t **);
            break;
        case MAT_T_STRUCT:
        {
            matvar_t **fields;

            data_size = sizeof(matvar_t **);
            if ( data != NULL ) {
                fields = data;
                nfields = 0;
                while ( fields[nfields] != NULL )
                    nfields++;
                if ( nmemb )
                    nfields = nfields / nmemb;
                matvar->internal->num_fields = nfields;
                if ( nfields ) {
                    matvar->internal->fieldnames =
                        calloc(nfields,sizeof(*matvar->internal->fieldnames));
                    for ( i = 0; i < nfields; i++ )
                        matvar->internal->fieldnames[i] = strdup(fields[i]->name);
                    nmemb *= nfields;
                }
            }
            break;
        }
        default:
            Mat_Error("Unrecognized data_type");
            Mat_VarFree(matvar);
            return NULL;
    }
    if ( matvar->class_type == MAT_C_SPARSE ) {
        matvar->data_size = sizeof(mat_sparse_t);
        matvar->nbytes    = matvar->data_size;
    } else {
        matvar->data_size = data_size;
        matvar->nbytes = nmemb*matvar->data_size;
    }
    if ( data == NULL ) {
        if ( MAT_C_CELL == matvar->class_type && nmemb > 0 )
            matvar->data = calloc(nmemb,sizeof(matvar_t*));
        else
            matvar->data = NULL;
    } else if ( opt & MAT_F_DONT_COPY_DATA ) {
        matvar->data         = data;
        matvar->mem_conserve = 1;
    } else if ( MAT_C_SPARSE == matvar->class_type ) {
        mat_sparse_t *sparse_data, *sparse_data_in;

        sparse_data_in = data;
        sparse_data    = malloc(sizeof(mat_sparse_t));
        if ( NULL != sparse_data ) {
            sparse_data->nzmax = sparse_data_in->nzmax;
            sparse_data->nir   = sparse_data_in->nir;
            sparse_data->njc   = sparse_data_in->njc;
            sparse_data->ndata = sparse_data_in->ndata;
            sparse_data->ir = malloc(sparse_data->nir*sizeof(*sparse_data->ir));
            if ( NULL != sparse_data->ir )
                memcpy(sparse_data->ir,sparse_data_in->ir,
                       sparse_data->nir*sizeof(*sparse_data->ir));
            sparse_data->jc = malloc(sparse_data->njc*sizeof(*sparse_data->jc));
            if ( NULL != sparse_data->jc )
                memcpy(sparse_data->jc,sparse_data_in->jc,
                       sparse_data->njc*sizeof(*sparse_data->jc));
            if ( matvar->isComplex ) {
                sparse_data->data = malloc(sizeof(mat_complex_split_t));
                if ( NULL != sparse_data->data ) {
                    mat_complex_split_t *complex_data,*complex_data_in;
                    complex_data     = sparse_data->data;
                    complex_data_in  = sparse_data_in->data;
                    complex_data->Re = malloc(sparse_data->ndata*data_size);
                    complex_data->Im = malloc(sparse_data->ndata*data_size);
                    if ( NULL != complex_data->Re )
                        memcpy(complex_data->Re,complex_data_in->Re,
                               sparse_data->ndata*data_size);
                    if ( NULL != complex_data->Im )
                        memcpy(complex_data->Im,complex_data_in->Im,
                               sparse_data->ndata*data_size);
                }
            } else {
                sparse_data->data = malloc(sparse_data->ndata*data_size);
                if ( NULL != sparse_data->data )
                    memcpy(sparse_data->data,sparse_data_in->data,
                           sparse_data->ndata*data_size);
            }
        }
        matvar->data = sparse_data;
    } else {
        if ( matvar->isComplex ) {
            matvar->data   = malloc(sizeof(mat_complex_split_t));
            if ( NULL != matvar->data && matvar->nbytes > 0 ) {
                mat_complex_split_t *complex_data    = matvar->data;
                mat_complex_split_t *complex_data_in = data;

                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL != complex_data->Re )
                    memcpy(complex_data->Re,complex_data_in->Re,matvar->nbytes);
                if ( NULL != complex_data->Im )
                    memcpy(complex_data->Im,complex_data_in->Im,matvar->nbytes);
            }
        } else if ( matvar->nbytes > 0 ) {
            matvar->data   = malloc(matvar->nbytes);
            if ( NULL != matvar->data )
                memcpy(matvar->data,data,matvar->nbytes);
        }
        matvar->mem_conserve = 0;
    }

    return matvar;
}

/** @brief Deletes a variable from a file
 *
 * @ingroup MAT
 * @param mat Pointer to the mat_t file structure
 * @param name Name of the variable to delete
 * @returns 0 on success
 */
int
Mat_VarDelete(mat_t *mat, const char *name)
{
    int   err = 1;
    enum mat_ft mat_file_ver = MAT_FT_DEFAULT;
    char *tmp_name, *new_name, *temp;
    mat_t *tmp;
    matvar_t *matvar;

    if ( NULL == mat || NULL == name )
        return err;

    switch ( mat->version ) {
        case 0x0200:
            mat_file_ver = MAT_FT_MAT73;
            break;
        case 0x0100:
            mat_file_ver = MAT_FT_MAT5;
            break;
        case 0x0010:
            mat_file_ver = MAT_FT_MAT4;
            break;
    }

    temp     = strdup_printf("XXXXXX");
    tmp_name = mktemp(temp);
    tmp      = Mat_CreateVer(tmp_name,mat->header,mat_file_ver);
    if ( tmp != NULL ) {
        while ( NULL != (matvar = Mat_VarReadNext(mat)) ) {
            if ( strcmp(matvar->name,name) )
                Mat_VarWrite(tmp,matvar,0);
            else
                err = 0;
            Mat_VarFree(matvar);
        }
        /* FIXME: Memory leak */
        new_name = strdup_printf("%s",mat->filename);
        fclose(mat->fp);

        if ( (err = remove(new_name)) == -1 ) {
            Mat_Critical("remove of %s failed",new_name);
        } else if ( !Mat_Close(tmp) && (err=rename(tmp_name,new_name))==-1) {
            Mat_Critical("rename failed oldname=%s,newname=%s",tmp_name,
                new_name);
        } else {
            tmp = Mat_Open(new_name,mat->mode);
            if ( NULL != tmp )
                memcpy(mat,tmp,sizeof(mat_t));
        }
        free(tmp);
        free(new_name);
    }
    free(temp);
    return err;
}

/** @brief Duplicates a matvar_t structure
 *
 * Provides a clean function for duplicating a matvar_t structure.
 * @ingroup MAT
 * @param in pointer to the matvar_t structure to be duplicated
 * @param opt 0 does a shallow duplicate and only assigns the data pointer to
 *            the duplicated array.  1 will do a deep duplicate and actually
 *            duplicate the contents of the data.  Warning: If you do a shallow
 *            copy and free both structures, the data will be freed twice and
 *            memory will be corrupted.  This may be fixed in a later release.
 * @returns Pointer to the duplicated matvar_t structure.
 */
matvar_t *
Mat_VarDuplicate(const matvar_t *in, int opt)
{
    matvar_t *out;
    int i;

    out = Mat_VarCalloc();
    if ( out == NULL )
        return NULL;

    out->nbytes       = in->nbytes;
    out->rank         = in->rank;
    out->data_type    = in->data_type;
    out->data_size    = in->data_size;
    out->class_type   = in->class_type;
    out->isComplex    = in->isComplex;
    out->isGlobal     = in->isGlobal;
    out->isLogical    = in->isLogical;
    out->mem_conserve = in->mem_conserve;
    out->compression  = in->compression;

    out->name = NULL;
    out->dims = NULL;
    out->data = NULL;

    if ( NULL != in->internal->hdf5_name )
        out->internal->hdf5_name = strdup(in->internal->hdf5_name);

    out->internal->hdf5_ref = in->internal->hdf5_ref;
    out->internal->id       = in->internal->id;
    out->internal->fpos     = in->internal->fpos;
    out->internal->datapos  = in->internal->datapos;
#if defined(HAVE_ZLIB)
    out->internal->z        = NULL;
#endif
    out->internal->num_fields = in->internal->num_fields;
    if ( NULL != in->internal->fieldnames && in->internal->num_fields > 0 ) {
        out->internal->fieldnames = calloc(in->internal->num_fields,
                                           sizeof(*in->internal->fieldnames));
        for ( i = 0; i < in->internal->num_fields; i++ ) {
            if ( NULL != in->internal->fieldnames[i] )
                out->internal->fieldnames[i] =
                    strdup(in->internal->fieldnames[i]);
        }
    }

    if (in->name != NULL && (NULL != (out->name = malloc(strlen(in->name)+1))))
        memcpy(out->name,in->name,strlen(in->name)+1);

    out->dims = malloc(in->rank*sizeof(*out->dims));
    if ( out->dims != NULL )
        memcpy(out->dims,in->dims,in->rank*sizeof(*out->dims));
#if defined(HAVE_ZLIB)
    if ( (in->internal->z != NULL) && (NULL != (out->internal->z = malloc(sizeof(z_stream)))) )
        inflateCopy(out->internal->z,in->internal->z);
#endif

    if ( !opt ) {
        out->data = in->data;
    } else if ( (in->data != NULL) && (in->class_type == MAT_C_STRUCT) ) {
        matvar_t **infields, **outfields;
        int nfields = 0;

        out->data = malloc(in->nbytes);
        if ( out->data != NULL && in->data_size > 0 ) {
            nfields   = in->nbytes / in->data_size;
            infields  = (matvar_t **)in->data;
            outfields = (matvar_t **)out->data;
            for ( i = 0; i < nfields; i++ ) {
                outfields[i] = Mat_VarDuplicate(infields[i],opt);
            }
        }
    } else if ( (in->data != NULL) && (in->class_type == MAT_C_CELL) ) {
        matvar_t **incells, **outcells;
        int ncells = 0;

        out->data = malloc(in->nbytes);
        if ( out->data != NULL && in->data_size > 0 ) {
            ncells   = in->nbytes / in->data_size;
            incells  = (matvar_t **)in->data;
            outcells = (matvar_t **)out->data;
            for ( i = 0; i < ncells; i++ ) {
                outcells[i] = Mat_VarDuplicate(incells[i],opt);
            }
        }
    } else if ( in->data != NULL ) {
        if ( out->isComplex ) {
            out->data = malloc(sizeof(mat_complex_split_t));
            if ( out->data != NULL ) {
                mat_complex_split_t *out_data = out->data;
                mat_complex_split_t *in_data  = in->data;
                out_data->Re = malloc(out->nbytes);
                if ( NULL != out_data->Re )
                    memcpy(out_data->Re,in_data->Re,out->nbytes);
                out_data->Im = malloc(out->nbytes);
                if ( NULL != out_data->Im )
                    memcpy(out_data->Im,in_data->Im,out->nbytes);
            }
        } else {
            out->data = malloc(in->nbytes);
            if ( out->data != NULL )
                memcpy(out->data,in->data,in->nbytes);
        }
    }
    return out;
}

/** @brief Frees all the allocated memory associated with the structure
 *
 * Frees memory used by a MAT variable.  Frees the data associated with a
 * MAT variable if it's non-NULL and MAT_F_DONT_COPY_DATA was not used.
 * @ingroup MAT
 * @param matvar Pointer to the matvar_t structure
 */
void
Mat_VarFree(matvar_t *matvar)
{
    size_t nmemb = 0, i;
    if ( !matvar )
        return;
    if ( matvar->dims ) {
        nmemb = 1;
        for ( i = 0; i < matvar->rank; i++ )
            nmemb *= matvar->dims[i];
        free(matvar->dims);
    }
    if ( matvar->name )
        free(matvar->name);
    if ( matvar->data != NULL) {
        switch (matvar->class_type ) {
            case MAT_C_STRUCT:
                if ( !matvar->mem_conserve && NULL != matvar->data ) {
                    matvar_t **fields = matvar->data;
                    int nfields = matvar->internal->num_fields;
                    for ( i = 0; i < nmemb*nfields; i++ )
                        Mat_VarFree(fields[i]);

                    free(matvar->data);
                    break;
                }
            case MAT_C_CELL:
                if ( !matvar->mem_conserve && NULL != matvar->data ) {
                    matvar_t **cells = matvar->data;
                    for ( i = 0; i < nmemb; i++ )
                        Mat_VarFree(cells[i]);

                    free(matvar->data);
                }
                break;
            case MAT_C_SPARSE:
                if ( !matvar->mem_conserve ) {
                    mat_sparse_t *sparse;
                    sparse = matvar->data;
                    if ( sparse->ir != NULL )
                        free(sparse->ir);
                    if ( sparse->jc != NULL )
                        free(sparse->jc);
                    if ( matvar->isComplex && NULL != sparse->data ) {
                        mat_complex_split_t *complex_data = sparse->data;
                        free(complex_data->Re);
                        free(complex_data->Im);
                        free(complex_data);
                    } else if ( sparse->data != NULL ) {
                        free(sparse->data);
                    }
                    free(sparse);
                }
                break;
            case MAT_C_DOUBLE:
            case MAT_C_SINGLE:
            case MAT_C_INT64:
            case MAT_C_UINT64:
            case MAT_C_INT32:
            case MAT_C_UINT32:
            case MAT_C_INT16:
            case MAT_C_UINT16:
            case MAT_C_INT8:
            case MAT_C_UINT8:
            case MAT_C_CHAR:
                if ( !matvar->mem_conserve && NULL != matvar->data ) {
                    if ( matvar->isComplex ) {
                        mat_complex_split_t *complex_data = matvar->data;
                        free(complex_data->Re);
                        free(complex_data->Im);
                        free(complex_data);
                    } else {
                        free(matvar->data);
                    }
                }
                break;
            case MAT_C_EMPTY:
            case MAT_C_OBJECT:
            case MAT_C_FUNCTION:
                break;
        }
    }

    if ( NULL != matvar->internal ) {
#if defined(HAVE_ZLIB)
        if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
            inflateEnd(matvar->internal->z);
            free(matvar->internal->z);
        }
#endif
#if defined(MAT73) && MAT73
        if ( -1 < matvar->internal->id ) {
            switch ( H5Iget_type(matvar->internal->id) ) {
                case H5I_GROUP:
                    H5Gclose(matvar->internal->id);
                    matvar->internal->id = -1;
                    break;
                case H5I_DATASET:
                    H5Dclose(matvar->internal->id);
                    matvar->internal->id = -1;
                    break;
                default:
                    break;
            }
        }
        if ( 0 < matvar->internal->hdf5_ref ) {
            switch ( H5Iget_type(matvar->internal->id) ) {
                case H5I_GROUP:
                    H5Gclose(matvar->internal->id);
                    matvar->internal->hdf5_ref = -1;
                    break;
                case H5I_DATASET:
                    H5Dclose(matvar->internal->id);
                    matvar->internal->hdf5_ref = -1;
                    break;
                default:
                    break;
            }
        }
        if ( NULL != matvar->internal->hdf5_name ) {
            free(matvar->internal->hdf5_name);
            matvar->internal->hdf5_name = NULL;
        }
#endif
        if ( NULL != matvar->internal->fieldnames &&
             matvar->internal->num_fields > 0 ) {
            size_t i;
            for ( i = 0; i < matvar->internal->num_fields; i++ ) {
                if ( NULL != matvar->internal->fieldnames[i] )
                    free(matvar->internal->fieldnames[i]);
            }
            free(matvar->internal->fieldnames);
        }
        free(matvar->internal);
        matvar->internal = NULL;
    }
    /* FIXME: Why does this cause a SEGV? */
#if 0
    memset(matvar,0,sizeof(matvar_t));
#endif
    free(matvar);
}

void
Mat_VarFree2(matvar_t *matvar)
{
    if ( !matvar )
        return;
    if ( matvar->dims )
        free(matvar->dims);
    if ( matvar->name )
        free(matvar->name);
    if ( (matvar->data != NULL) && (matvar->class_type == MAT_C_STRUCT ||
          matvar->class_type == MAT_C_CELL) && matvar->data_size > 0 ) {
        int i;
        matvar_t **fields = (matvar_t **)matvar->data;
        int nfields = matvar->nbytes / matvar->data_size;
        for ( i = 0; i < nfields; i++ )
            Mat_VarFree(fields[i]);
        free(matvar->data);
    } else if ( (matvar->data != NULL) && (!matvar->mem_conserve) &&
                (matvar->class_type == MAT_C_SPARSE) ) {
        mat_sparse_t *sparse;
        sparse = matvar->data;
        if ( sparse->ir != NULL )
            free(sparse->ir);
        if ( sparse->jc != NULL )
            free(sparse->jc);
        if ( sparse->data != NULL )
            free(sparse->data);
        free(sparse);
    } else {
        if ( matvar->data && !matvar->mem_conserve )
            free(matvar->data);
    }
#if defined(HAVE_ZLIB)
    if ( matvar->compression == MAT_COMPRESSION_ZLIB )
        inflateEnd(matvar->internal->z);
#endif
    /* FIXME: Why does this cause a SEGV? */
#if 0
    memset(matvar,0,sizeof(matvar_t));
#endif
}

/** @brief Calculate a single subscript from a set of subscript values
 *
 * Calculates a single linear subscript (0-relative) given a 1-relative
 * subscript for each dimension.  The calculation uses the formula below where
 * index is the linear index, s is an array of length RANK where each element
 * is the subscript for the correspondind dimension, D is an array whose
 * elements are the dimensions of the variable.
 * \f[
 *   index = \sum\limits_{k=0}^{RANK-1} [(s_k - 1) \prod\limits_{l=0}^{k} D_l ]
 * \f]
 * @ingroup MAT
 * @param rank Rank of the variable
 * @param dims dimensions of the variable
 * @param subs Dimension subscripts
 * @return Single (linear) subscript
 */
int
Mat_CalcSingleSubscript(int rank,int *dims,int *subs)
{
    int index = 0, i, j, k, err = 0;

    for ( i = 0; i < rank; i++ ) {
        k = subs[i];
        if ( k > dims[i] ) {
            err = 1;
            Mat_Critical("Mat_CalcSingleSubscript: index out of bounds");
            break;
        } else if ( k < 1 ) {
            err = 1;
            break;
        }
        k--;
        for ( j = i; j--; )
            k *= dims[j];
        index += k;
    }
    if ( err )
        index = -1;

    return index;
}


/** @brief Calculate a set of subscript values from a single(linear) subscript
 *
 * Calculates 1-relative subscripts for each dimension given a 0-relative
 * linear index.  Subscripts are calculated as follows where s is the array
 * of dimension subscripts, D is the array of dimensions, and index is the
 * linear index.
 * \f[
 *   s_k = \lfloor\frac{1}{L} \prod\limits_{l = 0}^{k} D_l\rfloor + 1
 * \f]
 * \f[
 *   L = index - \sum\limits_{l = k}^{RANK - 1} s_k \prod\limits_{m = 0}^{k} D_m
 * \f]
 * @ingroup MAT
 * @param rank Rank of the variable
 * @param dims dimensions of the variable
 * @param index linear index
 * @return Array of dimension subscripts
 */
int *
Mat_CalcSubscripts(int rank,int *dims,int index)
{
    int i, j, k, *subs;
    double l;

    subs = malloc(rank*sizeof(int));
    l = index;
    for ( i = rank; i--; ) {
        k = 1;
        for ( j = i; j--; )
            k *= dims[j];
        subs[i] = floor(l / (double)k);
        l -= subs[i]*k;
        subs[i]++;
    }

    return subs;
}

/** @brief Calculates the size of a matlab variable in bytes
 *
 * @ingroup MAT
 * @param matvar matlab variable
 * @returns size of the variable in bytes
 */
size_t
Mat_VarGetSize(matvar_t *matvar)
{
    int nmemb, i;
    size_t bytes = 0;

    if ( matvar->class_type == MAT_C_STRUCT ) {
        int nfields;
        matvar_t **fields;
        /* This is really nmemb*nfields, but we'll get a
         * more accurate count of the bytes by loopoing over all of them
         */
        nfields = matvar->internal->num_fields;
        fields  = matvar->data;
        for ( i = 0; i < nfields; i++ )
            bytes += Mat_VarGetSize(fields[i]);
    } else if ( matvar->class_type == MAT_C_CELL ) {
        int ncells;
        matvar_t **cells;

        ncells = matvar->nbytes / matvar->data_size;
        cells  = matvar->data;
        for ( i = 0; i < ncells; i++ )
            bytes += Mat_VarGetSize(cells[i]);
    } else {
        nmemb = 1;
        for ( i = 0; i < matvar->rank; i++ )
            nmemb *= matvar->dims[i];
        bytes += nmemb*Mat_SizeOfClass(matvar->class_type);
    }
    return bytes;
}

/** @brief Prints the variable information
 *
 * Prints to stdout the values of the @ref matvar_t structure
 * @ingroup MAT
 * @param matvar Pointer to the matvar_t structure
 * @param printdata set to 1 if the Variables data should be printed, else 0
 */
void
Mat_VarPrint( matvar_t *matvar, int printdata )
{
    size_t nmemb;
    int i, j;
    const char *class_type_desc[16] = {"Undefined","Cell Array","Structure",
       "Object","Character Array","Sparse Array","Double Precision Array",
       "Single Precision Array", "8-bit, signed integer array",
       "8-bit, unsigned integer array","16-bit, signed integer array",
       "16-bit, unsigned integer array","32-bit, signed integer array",
       "32-bit, unsigned integer array","64-bit, signed integer array",
       "64-bit, unsigned integer array"};
    const char *data_type_desc[23] = {"Unknown","8-bit, signed integer",
       "8-bit, unsigned integer","16-bit, signed integer",
       "16-bit, unsigned integer","32-bit, signed integer",
       "32-bit, unsigned integer","IEEE 754 single-precision","RESERVED",
       "IEEE 754 double-precision","RESERVED","RESERVED",
       "64-bit, signed integer","64-bit, unsigned integer", "Matlab Array",
       "Compressed Data","Unicode UTF-8 Encoded Character Data",
       "Unicode UTF-16 Encoded Character Data",
       "Unicode UTF-32 Encoded Character Data","","String","Cell Array",
       "Structure"};

    if ( matvar == NULL )
        return;
    if ( matvar->name )
        printf("      Name: %s\n", matvar->name);
    printf("      Rank: %d\n", matvar->rank);
    if ( matvar->rank == 0 )
        return;
    printf("Dimensions: %" SIZE_T_FMTSTR,matvar->dims[0]);
    nmemb = matvar->dims[0];
    for ( i = 1; i < matvar->rank; i++ ) {
        printf(" x %" SIZE_T_FMTSTR,matvar->dims[i]);
        nmemb *= matvar->dims[i];
    }
    printf("\n");
    printf("Class Type: %s",class_type_desc[matvar->class_type]);
    if ( matvar->isComplex )
        printf(" (complex)");
    else if ( matvar->isLogical )
        printf(" (logical)");
    printf("\n");
    if ( matvar->data_type )
        printf(" Data Type: %s\n", data_type_desc[matvar->data_type]);

    if ( MAT_C_STRUCT == matvar->class_type ) {
        matvar_t **fields = (matvar_t **)matvar->data;
        int nfields = matvar->internal->num_fields;
        if ( nmemb*nfields > 0 ) {
            printf("Fields[%" SIZE_T_FMTSTR "] {\n", nfields*nmemb);
            for ( i = 0; i < nfields*nmemb; i++ ) {
                if ( NULL == fields[i] ) {
                    printf("      Name: %s\n      Rank: %d\n",
                           matvar->internal->fieldnames[i%nfields],0);
                } else {
                    Mat_VarPrint(fields[i],printdata);
                }
            }
            printf("}\n");
        } else {
            printf("Fields[%d] {\n", nfields);
            for ( i = 0; i < nfields; i++ )
                printf("      Name: %s\n      Rank: %d\n",
                       matvar->internal->fieldnames[i],0);
            printf("}\n");
        }
        return;
    } else if ( matvar->data == NULL || matvar->data_size < 1 ) {
        return;
    } else if ( MAT_C_CELL == matvar->class_type ) {
        matvar_t **cells = (matvar_t **)matvar->data;
        int ncells = matvar->nbytes / matvar->data_size;
        printf("{\n");
        for ( i = 0; i < ncells; i++ )
            Mat_VarPrint(cells[i],printdata);
        printf("}\n");
        return;
    } else if ( !printdata ) {
        return;
    }

    printf("{\n");

    if ( matvar->rank > 2 ) {
        printf("I can't print more than 2 dimensions\n");
    } else if ( matvar->rank == 1 && matvar->dims[0] > 15 ) {
        printf("I won't print more than 15 elements in a vector\n");
    } else if ( matvar->rank==2 ) {
        switch( matvar->class_type ) {
            case MAT_C_DOUBLE:
            case MAT_C_SINGLE:
#ifdef HAVE_MAT_INT64_T
            case MAT_C_INT64:
#endif
#ifdef HAVE_MAT_UINT64_T
            case MAT_C_UINT64:
#endif
            case MAT_C_INT32:
            case MAT_C_UINT32:
            case MAT_C_INT16:
            case MAT_C_UINT16:
            case MAT_C_INT8:
            case MAT_C_UINT8:
            {
                size_t stride = Mat_SizeOf(matvar->data_type);
                if ( matvar->isComplex ) {
                    mat_complex_split_t *complex_data = matvar->data;
                    char *rp = complex_data->Re;
                    char *ip = complex_data->Im;
                   for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ ) {
                            size_t idx = matvar->dims[0]*j+i;
                            Mat_PrintNumber(matvar->data_type,rp+idx*stride);
                            printf(" + ");
                            Mat_PrintNumber(matvar->data_type,ip+idx*stride);
                            printf("i ");
                        }
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
               } else {
                   char *data = matvar->data;
                   for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ ) {
                            size_t idx = matvar->dims[0]*j+i;
                            Mat_PrintNumber(matvar->data_type,
                                            data+idx*stride);
                            printf(" ");
                        }
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                }
                break;
            }
            case MAT_C_CHAR:
            {
                char *data = matvar->data;
                if ( !printdata )
                    break;
                for ( i = 0; i < matvar->dims[0]; i++ ) {
                    for ( j = 0; j < matvar->dims[1]; j++ )
                        printf("%c",data[j*matvar->dims[0]+i]);
                    printf("\n");
                }
                break;
            }
            case MAT_C_SPARSE:
            {
                mat_sparse_t *sparse;
                size_t stride = Mat_SizeOf(matvar->data_type);
#if !defined(EXTENDED_SPARSE)
                if ( MAT_T_DOUBLE != matvar->data_type )
                    break;
#endif
                sparse = matvar->data;
                if ( matvar->isComplex ) {
                    mat_complex_split_t *complex_data = sparse->data;
                    char *re,*im;
                    re = complex_data->Re;
                    im = complex_data->Im;
                    for ( i = 0; i < sparse->njc-1; i++ ) {
                        for (j = sparse->jc[i];
                             j<sparse->jc[i+1] && j<sparse->ndata;j++ ) {
                            printf("    (%d,%d)  ",sparse->ir[j]+1,i+1);
                            Mat_PrintNumber(matvar->data_type,re+j*stride);
                            printf(" + ");
                            Mat_PrintNumber(matvar->data_type,im+j*stride);
                            printf("i\n");
                        }
                    }
                } else {
                    char *data;
                    data = sparse->data;
                    for ( i = 0; i < sparse->njc-1; i++ ) {
                        for (j = sparse->jc[i];
                             j<sparse->jc[i+1] && j<sparse->ndata;j++ ){
                            printf("    (%d,%d)  ",sparse->ir[j]+1,i+1);
                            Mat_PrintNumber(matvar->data_type,data+j*stride);
                            printf("\n");
                        }
                    }
                }
                break;
            } /* case MAT_C_SPARSE: */
            default:
                break;
        } /* switch( matvar->class_type ) */
    }

    printf("}\n");

    return;
}

/** @brief Reads MAT variable data from a file
 *
 * Reads data from a MAT variable.  The variable must have been read by
 * Mat_VarReadInfo.
 * @ingroup MAT
 * @param mat MAT file to read data from
 * @param matvar MAT variable information
 * @param data pointer to store data in (must be pre-allocated)
 * @param start array of starting indeces
 * @param stride stride of data
 * @param edge array specifying the number to read in each direction
 * @retval 0 on success
 */
int
Mat_VarReadData(mat_t *mat,matvar_t *matvar,void *data,
      int *start,int *stride,int *edge)
{
    int err = 0;

    switch ( matvar->class_type ) {
        case MAT_C_DOUBLE:
        case MAT_C_SINGLE:
        case MAT_C_INT64:
        case MAT_C_UINT64:
        case MAT_C_INT32:
        case MAT_C_UINT32:
        case MAT_C_INT16:
        case MAT_C_UINT16:
        case MAT_C_INT8:
        case MAT_C_UINT8:
            break;
        default:
            return -1;
    }

    switch ( mat->version ) {
        case MAT_FT_MAT73:
#if defined(MAT73) && MAT73
            err = Mat_VarReadData73(mat,matvar,data,start,stride,edge);
#else
            err = 1;
#endif
            break;
        case MAT_FT_MAT5:
            err = ReadData5(mat,matvar,data,start,stride,edge);
            break;
        case MAT_FT_MAT4:
            err = ReadData4(mat,matvar,data,start,stride,edge);
            break;
    }

    return err;
}

/** @brief Reads all the data for a matlab variable
 *
 * Allocates memory for an reads the data for a given matlab variable.
 * @ingroup MAT
 * @param mat Matlab MAT file structure pointer
 * @param matvar Variable whose data is to be read
 * @returns non-zero on error
 */
int
Mat_VarReadDataAll(mat_t *mat,matvar_t *matvar)
{
    int err = 0;

    if ( (mat == NULL) || (matvar == NULL) )
        err = 1;
    else
        ReadData(mat,matvar);

    return err;
}

/** @brief Reads a subset of a MAT variable using a 1-D indexing
 *
 * Reads data from a MAT variable using a linear (1-D) indexing mode. The
 * variable must have been read by Mat_VarReadInfo.
 * @ingroup MAT
 * @param mat MAT file to read data from
 * @param matvar MAT variable information
 * @param data pointer to store data in (must be pre-allocated)
 * @param start starting index
 * @param stride stride of data
 * @param edge number of elements to read
 * @retval 0 on success
 */
int
Mat_VarReadDataLinear(mat_t *mat,matvar_t *matvar,void *data,int start,
    int stride,int edge)
{
    int err = 0;

    switch ( matvar->class_type ) {
        case MAT_C_DOUBLE:
        case MAT_C_SINGLE:
        case MAT_C_INT64:
        case MAT_C_UINT64:
        case MAT_C_INT32:
        case MAT_C_UINT32:
        case MAT_C_INT16:
        case MAT_C_UINT16:
        case MAT_C_INT8:
        case MAT_C_UINT8:
            break;
        default:
            return -1;
    }

    switch ( mat->version ) {
        case MAT_FT_MAT73:
#if defined(MAT73) && MAT73
            err = Mat_VarReadDataLinear73(mat,matvar,data,start,stride,edge);
#else
            err = 1;
#endif
            break;
        case MAT_FT_MAT5:
            err = Mat_VarReadDataLinear5(mat,matvar,data,start,stride,edge);
            break;
        case MAT_FT_MAT4:
            err = Mat_VarReadDataLinear4(mat,matvar,data,start,stride,edge);
            break;
    }

    return err;
}

/** @brief Reads the information of the next variable in a MAT file
 *
 * Reads the next variable's information (class,flags-complex/global/logical,
 * rank,dimensions, name, etc) from the Matlab MAT file.  After reading, the MAT
 * file is positioned past the current variable.
 * @ingroup MAT
 * @param mat Pointer to the MAT file
 * @return Pointer to the @ref matvar_t structure containing the MAT
 * variable information
 */
matvar_t *
Mat_VarReadNextInfo( mat_t *mat )
{
    matvar_t *matvar = NULL;
    if( mat == NULL )
        return NULL;

    switch ( mat->version ) {
        case MAT_FT_MAT5:
            matvar = Mat_VarReadNextInfo5(mat);
            break;
        case MAT_FT_MAT73:
#if defined(MAT73) && MAT73
            matvar = Mat_VarReadNextInfo73(mat);
#endif
            break;
        case MAT_FT_MAT4:
            matvar = Mat_VarReadNextInfo4(mat);
            break;
    }

    return matvar;
}

/** @brief Reads the information of a variable with the given name from a MAT file
 *
 * Reads the named variable (or the next variable if name is NULL) information
 * (class,flags-complex/global/logical,rank,dimensions,and name) from the
 * Matlab MAT file
 * @ingroup MAT
 * @param mat Pointer to the MAT file
 * @param name Name of the variable to read
 * @return Pointer to the @ref matvar_t structure containing the MAT
 * variable information
 */
matvar_t *
Mat_VarReadInfo( mat_t *mat, const char *name )
{

    long  fpos;
    matvar_t *matvar = NULL;

    if ( (mat == NULL) || (name == NULL) )
        return NULL;

    if ( mat->version == MAT_FT_MAT73 ) {
        do {
            matvar = Mat_VarReadNextInfo(mat);
            if ( matvar != NULL ) {
                if ( !matvar->name ) {
                    Mat_VarFree(matvar);
                    matvar = NULL;
                } else if ( strcmp(matvar->name,name) ) {
                    Mat_VarFree(matvar);
                    matvar = NULL;
                }
            } else {
                Mat_Critical("An error occurred in reading the MAT file");
                break;
            }
        } while ( NULL == matvar && mat->next_index < mat->num_datasets);
    } else {
        fpos = ftell(mat->fp);
        fseek(mat->fp,mat->bof,SEEK_SET);
        do {
            matvar = Mat_VarReadNextInfo(mat);
            if ( matvar != NULL ) {
                if ( !matvar->name ) {
                    Mat_VarFree(matvar);
                    matvar = NULL;
                } else if ( strcmp(matvar->name,name) ) {
                    Mat_VarFree(matvar);
                    matvar = NULL;
                }
            } else {
                Mat_Critical("An error occurred in reading the MAT file");
                break;
            }
        } while ( !matvar && !feof(((FILE *)mat->fp)) );

        fseek(mat->fp,fpos,SEEK_SET);
    }
    return matvar;
}

/** @brief Reads the variable with the given name from a MAT file
 *
 * Reads the next variable in the Matlab MAT file
 * @ingroup MAT
 * @param mat Pointer to the MAT file
 * @param name Name of the variable to read
 * @return Pointer to the @ref matvar_t structure containing the MAT
 * variable information
 */
matvar_t *
Mat_VarRead( mat_t *mat, const char *name )
{
    long  fpos = 0;
    matvar_t *matvar = NULL;;

    if ( (mat == NULL) || (name == NULL) )
        return NULL;

    if ( MAT_FT_MAT73 != mat->version )
        fpos = ftell(mat->fp);

    matvar = Mat_VarReadInfo(mat,name);
    if ( matvar )
        ReadData(mat,matvar);

    if ( MAT_FT_MAT73 != mat->version )
        fseek(mat->fp,fpos,SEEK_SET);
    return matvar;
}

/** @brief Reads the next variable in a MAT file
 *
 * Reads the next variable in the Matlab MAT file
 * @ingroup MAT
 * @param mat Pointer to the MAT file
 * @return Pointer to the @ref matvar_t structure containing the MAT
 * variable information
 */
matvar_t *
Mat_VarReadNext( mat_t *mat )
{
    long fpos = 0;
    matvar_t *matvar = NULL;

    if ( mat->version != MAT_FT_MAT73 ) {
        if ( feof(((FILE *)mat->fp)) )
            return NULL;
        /* Read position so we can reset the file position if an error occurs */
        fpos = ftell(mat->fp);
    }
    matvar = Mat_VarReadNextInfo(mat);
    if ( matvar )
        ReadData(mat,matvar);
    else if (mat->version != MAT_FT_MAT73 )
        fseek(mat->fp,fpos,SEEK_SET);
    return matvar;
}

/** @brief Writes the given MAT variable to a MAT file
 *
 * Writes the MAT variable information stored in matvar to the given MAT file.
 * The variable will be written to the end of the file.
 * @ingroup MAT
 * @param mat MAT file to write to
 * @param matvar MAT variable information to write
 * @retval 0 on success
 */
int
Mat_VarWriteInfo(mat_t *mat, matvar_t *matvar )
{
    if ( mat == NULL || matvar == NULL || mat->fp == NULL )
        return -1;
    else if ( mat->version != MAT_FT_MAT4 )
        WriteInfo5(mat,matvar);
#if 0
    else if ( mat->version == MAT_FT_MAT4 )
        WriteInfo4(mat,matvar);
#endif

    return 0;
}

/** @brief Writes the given data to the MAT variable
 *
 * Writes data to a MAT variable.  The variable must have previously been
 * written with Mat_VarWriteInfo.
 * @ingroup MAT
 * @param mat MAT file to write to
 * @param matvar MAT variable information to write
 * @param data pointer to the data to write
 * @param start array of starting indeces
 * @param stride stride of data
 * @param edge array specifying the number to read in each direction
 * @retval 0 on success
 */
int
Mat_VarWriteData(mat_t *mat,matvar_t *matvar,void *data,
      int *start,int *stride,int *edge)
{
    int err = 0, k, N = 1;

    fseek(mat->fp,matvar->internal->datapos+8,SEEK_SET);

    if ( mat == NULL || matvar == NULL || data == NULL ) {
        err = -1;
    } else if ( start == NULL && stride == NULL && edge == NULL ) {
        for ( k = 0; k < matvar->rank; k++ )
            N *= matvar->dims[k];
        if ( matvar->compression == MAT_COMPRESSION_NONE )
            WriteData(mat,data,N,matvar->data_type);
#if 0
        else if ( matvar->compression == MAT_COMPRESSION_ZLIB ) {
            WriteCompressedData(mat,matvar->internal->z,data,N,matvar->data_type);
            (void)deflateEnd(matvar->internal->z);
            free(matvar->internal->z);
            matvar->internal->z = NULL;
        }
#endif
    } else if ( start == NULL || stride == NULL || edge == NULL ) {
        err = 1;
    } else if ( matvar->rank == 2 ) {
        if ( stride[0]*(edge[0]-1)+start[0]+1 > matvar->dims[0] ) {
            err = 1;
        } else if ( stride[1]*(edge[1]-1)+start[1]+1 > matvar->dims[1] ) {
            err = 1;
        } else {
            switch ( matvar->class_type ) {
                case MAT_C_DOUBLE:
                case MAT_C_SINGLE:
                case MAT_C_INT64:
                case MAT_C_UINT64:
                case MAT_C_INT32:
                case MAT_C_UINT32:
                case MAT_C_INT16:
                case MAT_C_UINT16:
                case MAT_C_INT8:
                case MAT_C_UINT8:
                    WriteDataSlab2(mat,data,matvar->data_type,matvar->dims,
                                   start,stride,edge);
                    break;
                case MAT_C_CHAR:
                    WriteCharDataSlab2(mat,data,matvar->data_type,matvar->dims,
                                   start,stride,edge);
                    break;
                default:
                    break;
            }
        }
    }

    return err;
}

/** @brief Writes the given MAT variable to a MAT file
 *
 * Writes the MAT variable information stored in matvar to the given MAT file.
 * The variable will be written to the end of the file.
 * @ingroup MAT
 * @param mat MAT file to write to
 * @param matvar MAT variable information to write
 * @param compress Whether or not to compress the data
 *        (Only valid for version 5 MAT files and variables with numeric data)
 * @retval 0 on success
 */
int
Mat_VarWrite(mat_t *mat,matvar_t *matvar,enum matio_compression compress)
{
    if ( mat == NULL || matvar == NULL )
        return -1;
    else if ( mat->version == MAT_FT_MAT5 )
        Mat_VarWrite5(mat,matvar,compress);
#if defined(MAT73) && MAT73
    else if ( mat->version == MAT_FT_MAT73 )
        Mat_VarWrite73(mat,matvar,compress);
#endif

    return 0;
}
