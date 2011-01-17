/** @file mat.c
 * Matlab MAT version 5 file functions
 * @ingroup MAT
 */
/*
 * Copyright (C) 2005-2010   Christopher C. Hulbert
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

/* FIXME: Implement Unicode support */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <time.h>
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
            printf("%lu",*(mat_int64_t*)data);
            break;
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_T_UINT64:
            printf("%lu",*(mat_uint64_t*)data);
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
    }
}

/*
 *====================================================================
 *                 Public Functions
 *====================================================================
 */

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
    mat_t *mat;

    if ( MAT_FT_MAT5 == mat_file_ver )
        mat = Mat_Create5(matname,hdr_str);
    else if ( MAT_FT_MAT73 == mat_file_ver )
#if defined(MAT73) && MAT73
        mat = Mat_Create73(matname,hdr_str);
#else
        mat = NULL;
#endif
    else
        mat = Mat_Create5(matname,hdr_str);

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
    int     err;
    mat_t *mat = NULL;

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
    if ( mode & MAT_FT_MAT4 ) {
        mat->header        = NULL;
        mat->subsys_offset = NULL;
        mat->version       = MAT_FT_MAT4;
        mat->byteswap      = 0;
        mat->mode          = mode;
        mat->filename      = strdup_printf("%s",matname);
        mat->bof           = ftell(mat->fp);
        mat->next_index    = 0;
    } else {
        mat->header        = malloc(128);
        mat->subsys_offset = malloc(8);
        mat->filename      = NULL;

        err = fread(mat->header,1,116,fp);
        mat->header[116] = '\0';
        err = fread(mat->subsys_offset,1,8,fp);
        err = fread(&tmp2,2,1,fp);
        fread (&tmp,1,2,fp);
        mat->bof = ftell(mat->fp);
        mat->next_index    = 0;

        mat->byteswap = -1;
        if (tmp == 0x4d49)
            mat->byteswap = 0;
        else if (tmp == 0x494d) {
            mat->byteswap = 1;
            Mat_int16Swap(&tmp2);
        }
        mat->version = (int)tmp2;

        if ( mat->byteswap < 0 ) {
            Mat_Critical("%s does not seem to be a valid MAT file",matname);
            Mat_Close(mat);
            mat=NULL;
        } else if (mat->version != 0x0100 && mat->version != 0x0200) {
            Mat_Critical("%s is not a recognized MAT file version", matname);
            Mat_Close(mat);
            mat=NULL;
        } else {
            mat->filename = strdup_printf("%s",matname);
            mat->mode = mode;
        }
    }

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
        }
#else
        mat->fp = NULL;
        Mat_Close(mat);
        mat = NULL;
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
    if ( mat->version != MAT_FT_MAT4 )
        fseek(mat->fp,128L,SEEK_SET);
    else
        fseek(mat->fp,0L,SEEK_SET);
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
 * - MEM_CONSERVE to just use the pointer to the data and not copy the data
 *       itself.  Note that the pointer should not be freed until you are done
 *       with the mat variable.  The Mat_VarFree function will NOT free
 *       data that was created with MEM_CONSERVE, so free it yourself.
 * - MAT_F_COMPLEX to specify that the data is complex.  The data variable
 *       should be a pointer to a struct ComplexSplit type.
 * - MAT_F_GLOBAL to assign the variable as a global variable
 * - MAT_F_LOGICAL to specify that it is a logical variable
 * @return A MAT variable that can be written to a file or otherwise used
 */
matvar_t *
Mat_VarCreate(const char *name,enum matio_classes class_type,
    enum matio_types data_type,int rank,size_t *dims,void *data,int opt)
{
    int i, nmemb = 1, nfields = 0;
    matvar_t *matvar = NULL;
    size_t data_size;

    if (dims == NULL) return NULL;

    matvar = Mat_VarCalloc();
    if ( NULL == matvar )
        return NULL;

    matvar->compression = COMPRESSION_NONE;
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
            nmemb = 0;
            if ( data != NULL ) {
                fields = data;
                nfields = 0;
                while ( fields[nfields] != NULL )
                    nfields++;
                nmemb = nfields; /* nfields is really nmemb*nfields */
            }
            break;
        }
        default:
            Mat_Error("Unrecognized data_type");
            Mat_VarFree(matvar);
            return NULL;
    }
    if ( matvar->class_type == MAT_C_SPARSE ) {
        matvar->data_size = sizeof(sparse_t);
        matvar->nbytes    = matvar->data_size;
    } else {
        matvar->data_size = data_size;
        matvar->nbytes = nmemb*matvar->data_size;
    }
    if ( data == NULL ) {
        matvar->data = NULL;
    } else if ( opt & MEM_CONSERVE ) {
        matvar->data         = data;
        matvar->mem_conserve = 1;
    } else if ( MAT_C_SPARSE == matvar->class_type ) {
        sparse_t *sparse_data, *sparse_data_in;

        sparse_data_in = data;
        sparse_data    = malloc(sizeof(sparse_t));
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
                sparse_data->data = malloc(sizeof(struct ComplexSplit));
                if ( NULL != sparse_data->data ) {
                    struct ComplexSplit *complex_data    = sparse_data->data,
                                        *complex_data_in = sparse_data_in->data;
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
            matvar->data   = malloc(sizeof(struct ComplexSplit));
            if ( NULL != matvar->data ) {
                struct ComplexSplit *complex_data    = matvar->data;
                struct ComplexSplit *complex_data_in = data;

                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                if ( NULL != complex_data->Re )
                    memcpy(complex_data->Re,complex_data_in->Re,matvar->nbytes);
                if ( NULL != complex_data->Im )
                    memcpy(complex_data->Im,complex_data_in->Im,matvar->nbytes);
            }
        } else {
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
    enum mat_ft mat_file_ver;
    char *tmp_name, *new_name, *temp;
    mat_t *tmp;
    matvar_t *matvar;

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
            out->data = malloc(sizeof(struct ComplexSplit));
            if ( out->data != NULL ) {
                struct ComplexSplit *out_data = out->data;
                struct ComplexSplit *in_data  = in->data;
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
 * MAT variable if it's non-NULL and MEM_CONSERVE was not used.
 * @ingroup MAT
 * @param matvar Pointer to the matvar_t structure
 */
void
Mat_VarFree(matvar_t *matvar)
{
    if ( !matvar )
        return;
    if ( matvar->dims )
        free(matvar->dims);
    if ( matvar->name )
        free(matvar->name);
    if ( matvar->data != NULL) {
        switch (matvar->class_type ) {
            case MAT_C_STRUCT:
            case MAT_C_CELL:
                if ( matvar->data_size > 0 ) {
                    int i;
                    matvar_t **fields = matvar->data;
                    int nfields = matvar->nbytes / matvar->data_size;
                    for ( i = 0; i < nfields; i++ )
                        Mat_VarFree(fields[i]);
                    free(matvar->data);
                }
                break;
            case MAT_C_SPARSE:
                if ( !matvar->mem_conserve ) {
                    sparse_t *sparse;
                    sparse = matvar->data;
                    if ( sparse->ir != NULL )
                        free(sparse->ir);
                    if ( sparse->jc != NULL )
                        free(sparse->jc);
                    if ( matvar->isComplex && NULL != sparse->data ) {
                        struct ComplexSplit *complex_data = sparse->data;
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
                        struct ComplexSplit *complex_data = matvar->data;
                        free(complex_data->Re);
                        free(complex_data->Im);
                        free(complex_data);
                    } else {
                        free(matvar->data);
                    }
                }
                break;
        }
    }
#if defined(HAVE_ZLIB)
    if ( matvar->compression == COMPRESSION_ZLIB ) {
        inflateEnd(matvar->internal->z);
        free(matvar->internal->z);
    }
#endif
#if defined(MAT73) && MAT73
    if ( NULL != matvar->internal ) {
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
        free(matvar->internal);
        matvar->internal = NULL;
    }
#endif
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
        sparse_t *sparse;
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
    if ( matvar->compression == COMPRESSION_ZLIB )
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

/** @brief Returns a pointer to the Cell array at a specific index
 *
 * Returns a pointer to the Cell Array Field at the given 1-relative index.
 * MAT file must be a version 5 matlab file.
 * @ingroup MAT
 * @param matvar Pointer to the Cell Array MAT variable
 * @param index linear index of cell to return
 * @return Pointer to the Cell Array Field on success, NULL on error
 */
matvar_t *
Mat_VarGetCell(matvar_t *matvar,int index)
{
    int       nmemb = 1, i;
    matvar_t *cell = NULL;

    if ( matvar == NULL )
        return NULL;

    for ( i = 0; i < matvar->rank; i++ )
        nmemb *= matvar->dims[i];

    if ( index < nmemb )
        cell = *((matvar_t **)matvar->data + index);

    return cell;
}

/** @brief Indexes a cell array
 *
 * Finds cells of a cell array given a start, stride, and edge for each.
 * dimension.  The cells are placed in a pointer array.  The cells should not
 * be freed, but the array of pointers should be.  If copies are needed,
 * use Mat_VarDuplicate on each cell.
 * MAT File version must be 5.
 * @ingroup MAT
 * @param matvar Cell Array matlab variable
 * @param start vector of length rank with 0-relative starting coordinates for
 *              each diemnsion.
 * @param stride vector of length rank with strides for each diemnsion.
 * @param edge vector of length rank with the number of elements to read in
 *              each diemnsion.
 * @returns an array of pointers to the cells
 */
matvar_t **
Mat_VarGetCells(matvar_t *matvar,int *start,
    int *stride,int *edge)
{
    int i, j, N, I = 0;
    int inc[10] = {0,}, cnt[10] = {0,}, dimp[10] = {0,};
    matvar_t **cells;

    if ( (matvar == NULL) || (start == NULL) || (stride == NULL) ||  
         (edge == NULL) ) {
        return NULL;
    } else if ( matvar->rank > 10 ) {
        return NULL;
    }

    inc[0] = stride[0]-1;
    dimp[0] = matvar->dims[0];
    N = edge[0];
    I = start[0];
    for ( i = 1; i < matvar->rank; i++ ) {
        inc[i]  = stride[i]-1;
        dimp[i] = matvar->dims[i-1];
        for ( j = i ; j--; ) {
            inc[i]  *= matvar->dims[j];
            dimp[i] *= matvar->dims[j+1];
        }
        N *= edge[i];
        if ( start[i] > 0 )
            I += start[i]*dimp[i-1];
    }
    cells = malloc(N*sizeof(matvar_t *));
    for ( i = 0; i < N; i+=edge[0] ) {
        for ( j = 0; j < edge[0]; j++ ) {
            cells[i+j] = *((matvar_t **)matvar->data + I);
            I += stride[0];
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
    return cells;
}

/** @brief Indexes a cell array
 *
 * Finds cells of a cell array given a linear indexed start, stride, and edge.
 * The cells are placed in a pointer array.  The cells themself should not
 * be freed as they are part of the original cell array, but the pointer array
 * should be.  If copies are needed, use Mat_VarDuplicate on each of the cells.
 * MAT file version must be 5.
 * @ingroup MAT
 * @param matvar Cell Array matlab variable
 * @param start starting index
 * @param stride stride
 * @param edge Number of cells to get
 * @returns an array of pointers to the cells
 */
matvar_t **
Mat_VarGetCellsLinear(matvar_t *matvar,int start,int stride,int edge)
{
    int i, I = 0;
    matvar_t **cells;

    if ( matvar == NULL || matvar->rank > 10 ) {
        cells = NULL;
    } else {
        cells = malloc(edge*sizeof(matvar_t *));
        for ( i = 0; i < edge; i++ ) {
            cells[i] = *((matvar_t **)matvar->data + I);
            I += stride;
        }
    }
    return cells;
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
        nfields = matvar->nbytes / matvar->data_size;
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
 * MAT file version must be 5.
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
        nfields = matvar->nbytes / (nmemb*matvar->data_size);
    }
    return nfields;
}

/** @brief Finds a field of a structure
 *
 * Returns a pointer to the structure field at the given 0-relative index. MAT
 * file version must be 5.
 * @ingroup MAT
 * @param matvar Pointer to the Structure MAT variable
 * @param name_or_index Name of the field, or the 1-relative index of the field.
 * If the index is used, it should be the address of an integer variable whose
 * value is the index number.
 * @param opt BY_NAME if the name_or_index is the name or BY_INDEX if the index
 * was passed.
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

    nfields = matvar->nbytes / (nmemb*sizeof(matvar_t *));

    if ( index >= nmemb || index < 0)
        err = 1;

    if ( !err && (opt == BY_INDEX) ) {
        int field_index;

        field_index = *(int *)name_or_index;

        if ( field_index > nfields || field_index < 1 )
            Mat_Critical("Mat_VarGetStructField: field index out of bounds");
        else
            field = *((matvar_t **)matvar->data+index*nfields+field_index - 1);
    } else if ( !err && (opt == BY_NAME) ) {
        char *field_name;

        field_name = (char *)name_or_index;

        for ( i = 0; i < nfields; i++ ) {
            field = *((matvar_t **)matvar->data+index*nfields+i);
            if ( !strcmp(field->name,field_name) )
                break;
            else
                field = NULL;
        }
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
    printf("Dimensions: %d",matvar->dims[0]);
    for ( i = 1; i < matvar->rank; i++ )
        printf(" x %d",matvar->dims[i]);
    printf("\n");
    printf("Class Type: %s",class_type_desc[matvar->class_type]);
    if ( matvar->isComplex )
        printf(" (complex)");
    printf("\n");
    if ( matvar->data_type )
        printf(" Data Type: %s\n", data_type_desc[matvar->data_type]);

    if ( matvar->data == NULL || matvar->data_size < 1 ) {
        return;
    } else if ( MAT_C_STRUCT == matvar->class_type ) {
        matvar_t **fields = (matvar_t **)matvar->data;
        int nfields = matvar->nbytes / matvar->data_size;
        printf("Fields[%d] {\n", nfields);
        for ( i = 0; i < nfields; i++ )
            Mat_VarPrint(fields[i],printdata);
        printf("}\n");
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
                    struct ComplexSplit *complex_data = matvar->data;
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
                if ( !printdata )
                    break;
                if ( matvar->dims[0] == 1 ) {
                    printf("%s\n",(char *)matvar->data);
                } else {
                    char *data = matvar->data;
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        j = 0;
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%c",data[j*matvar->dims[0]+i]);
                        printf("\n");
                    }
                }
                break;
            }
            case MAT_C_SPARSE:
            {
                sparse_t *sparse;
                size_t stride = Mat_SizeOf(matvar->data_type);
#if !defined(EXTENDED_SPARSE)
                if ( MAT_T_DOUBLE != matvar->data_type )
                    break;
#endif
                sparse = matvar->data;
                if ( matvar->isComplex ) {
                    struct ComplexSplit *complex_data = sparse->data;
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

    if ( mat->version != MAT_FT_MAT4 )
        err = ReadData5(mat,matvar,data,start,stride,edge);
    else
        err = ReadData4(mat,matvar,data,start,stride,edge);
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

/** @brief Reads MAT variable data from a file
 *
 * Reads data from a MAT variable using a linear indexingmode. The variable
 * must have been read by Mat_VarReadInfo.
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
    int err = 0, nmemb = 1, i, real_bytes = 0;
    mat_int32_t tag[2];
#if defined(HAVE_ZLIB)
    z_stream z;
#endif

    if ( mat->version == MAT_FT_MAT4 )
        return -1;
    fseek(mat->fp,matvar->internal->datapos,SEEK_SET);
    if ( matvar->compression == COMPRESSION_NONE ) {
        fread(tag,4,2,mat->fp);
        if ( mat->byteswap ) {
            Mat_int32Swap(tag);
            Mat_int32Swap(tag+1);
        }
        matvar->data_type = tag[0] & 0x000000ff;
        if ( tag[0] & 0xffff0000 ) { /* Data is packed in the tag */
            fseek(mat->fp,-4,SEEK_CUR);
            real_bytes = 4+(tag[0] >> 16);
        } else {
            real_bytes = 8+tag[1];
        }
#if defined(HAVE_ZLIB)
    } else if ( matvar->compression == COMPRESSION_ZLIB ) {
        matvar->internal->z->avail_in = 0;
        err = inflateCopy(&z,matvar->internal->z);
        InflateDataType(mat,&z,tag);
        if ( mat->byteswap ) {
            Mat_int32Swap(tag);
            Mat_int32Swap(tag+1);
        }
        matvar->data_type = tag[0] & 0x000000ff;
        if ( !(tag[0] & 0xffff0000) ) {/* Data is NOT packed in the tag */
            /* We're cheating, but InflateDataType just inflates 4 bytes */
            InflateDataType(mat,&z,tag+1);
            if ( mat->byteswap ) {
                Mat_int32Swap(tag+1);
            }
            real_bytes = 8+tag[1];
        } else {
            real_bytes = 4+(tag[0] >> 16);
        }
#endif
    }
    if ( real_bytes % 8 )
        real_bytes += (8-(real_bytes % 8));

    for ( i = 0; i < matvar->rank; i++ )
        nmemb *= matvar->dims[i];

    if ( stride*(edge-1)+start+1 > nmemb ) {
        err = 1;
    } else if ( matvar->compression == COMPRESSION_NONE ) {
        if ( matvar->isComplex ) {
            struct ComplexSplit *complex_data = data;

            ReadDataSlab1(mat,complex_data->Re,matvar->class_type,
                matvar->data_type,start,stride,edge);
            fseek(mat->fp,matvar->internal->datapos+real_bytes,SEEK_SET);
            fread(tag,4,2,mat->fp);
            if ( mat->byteswap ) {
                Mat_int32Swap(tag);
                Mat_int32Swap(tag+1);
            }
            matvar->data_type = tag[0] & 0x000000ff;
            if ( tag[0] & 0xffff0000 ) { /* Data is packed in the tag */
                fseek(mat->fp,-4,SEEK_CUR);
            }
            ReadDataSlab1(mat,complex_data->Im,matvar->class_type,
                          matvar->data_type,start,stride,edge);
        } else {
            ReadDataSlab1(mat,data,matvar->class_type,
                matvar->data_type,start,stride,edge);
        }
#if defined(HAVE_ZLIB)
    } else if ( matvar->compression == COMPRESSION_ZLIB ) {
        if ( matvar->isComplex ) {
            struct ComplexSplit *complex_data = data;
            
            ReadCompressedDataSlab1(mat,&z,complex_data->Re,
                matvar->class_type,matvar->data_type,start,stride,edge);
            
            fseek(mat->fp,matvar->internal->datapos,SEEK_SET);
            
            /* Reset zlib knowledge to before reading real tag */
            inflateEnd(&z);
            err = inflateCopy(&z,matvar->internal->z);
            InflateSkip(mat,&z,real_bytes);
            z.avail_in = 0;
            InflateDataType(mat,&z,tag);
            if ( mat->byteswap ) {
                Mat_int32Swap(tag);
            }
            matvar->data_type = tag[0] & 0x000000ff;
            if ( !(tag[0] & 0xffff0000) ) {/*Data is NOT packed in the tag*/
                InflateSkip(mat,&z,4);
            }
            ReadCompressedDataSlab1(mat,&z,complex_data->Im,
                matvar->class_type,matvar->data_type,start,stride,edge);
            inflateEnd(&z);
        } else {
            ReadCompressedDataSlab1(mat,&z,data,matvar->class_type,
                matvar->data_type,start,stride,edge);
            inflateEnd(&z);
        }
#endif
    }

    switch(matvar->class_type) {
        case MAT_C_DOUBLE:
            matvar->data_type = MAT_T_DOUBLE;
            matvar->data_size = sizeof(double);
            break;
        case MAT_C_SINGLE:
            matvar->data_type = MAT_T_SINGLE;
            matvar->data_size = sizeof(float);
            break;
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
            matvar->data_type = MAT_T_INT64;
            matvar->data_size = sizeof(mat_int64_t);
            break;
#endif /* HAVE_MAT_INT64_T */
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
            matvar->data_type = MAT_T_UINT64; 
            matvar->data_size = sizeof(mat_uint64_t);
            break;
#endif /* HAVE_MAT_UINT64_T */
        case MAT_C_INT32:
            matvar->data_type = MAT_T_INT32;
            matvar->data_size = sizeof(mat_int32_t);
            break;
        case MAT_C_UINT32:
            matvar->data_type = MAT_T_UINT32;
            matvar->data_size = sizeof(mat_uint32_t);
            break;
        case MAT_C_INT16:
            matvar->data_type = MAT_T_INT16;
            matvar->data_size = sizeof(mat_int16_t);
            break;
        case MAT_C_UINT16:
            matvar->data_type = MAT_T_UINT16;
            matvar->data_size = sizeof(mat_uint16_t);
            break;
        case MAT_C_INT8:
            matvar->data_type = MAT_T_INT8;
            matvar->data_size = sizeof(mat_int8_t);
            break;
        case MAT_C_UINT8:
            matvar->data_type = MAT_T_UINT8;
            matvar->data_size = sizeof(mat_uint8_t);
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
    if( mat == NULL )
        return NULL;
    else if ( mat->version == MAT_FT_MAT5 )
        return Mat_VarReadNextInfo5(mat);
#if defined(MAT73) && MAT73
    else if ( mat->version == MAT_FT_MAT73 )
        return Mat_VarReadNextInfo73(mat);
#endif
    else
        return Mat_VarReadNextInfo4(mat);

    return NULL;
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
        } while ( !matvar && !feof(mat->fp) );

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
    long  fpos;
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
    long fpos;
    matvar_t *matvar = NULL;

    if ( mat->version != MAT_FT_MAT73 ) {
        if ( feof(mat->fp) )
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
        if ( matvar->compression == COMPRESSION_NONE )
            WriteData(mat,data,N,matvar->data_type);
#if 0
        else if ( matvar->compression == COMPRESSION_ZLIB ) {
            WriteCompressedData(mat,matvar->internal->z,data,N,matvar->data_type);
            (void)deflateEnd(matvar->internal->z);
            free(matvar->internal->z);
            matvar->internal->z = NULL;
        }
#endif
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
Mat_VarWrite(mat_t *mat,matvar_t *matvar,int compress)
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
