/** @file mat4.c
 * Matlab MAT version 4 file functions
 * @ingroup MAT
 */
/*
 * Copyright (C) 2005-2016   Christopher C. Hulbert
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
#include <stdio.h>
#include <math.h>
#include <limits.h>
#if defined(__GLIBC__)
#include <endian.h>
#endif
#include "matio_private.h"
#include "mat4.h"

/** @if mat_devman
 * @brief Creates a new Matlab MAT version 4 file
 *
 * Tries to create a new Matlab MAT file with the given name.
 * @ingroup MAT
 * @param matname Name of MAT file to create
 * @return A pointer to the MAT file or NULL if it failed.  This is not a
 * simple FILE * and should not be used as one.
 * @endif
 */
mat_t *
Mat_Create4(const char* matname)
{
    FILE *fp = NULL;
    mat_t *mat = NULL;
    int byteswap;

#if defined(__GLIBC__)
#if (__BYTE_ORDER == __LITTLE_ENDIAN)
    byteswap = 0;
#elif (__BYTE_ORDER == __BIG_ENDIAN)
    byteswap = 1;
#else
    return NULL;
#endif
#elif defined(_BIG_ENDIAN) && !defined(_LITTLE_ENDIAN)
    byteswap = 1;
#elif defined(_LITTLE_ENDIAN) && !defined(_BIG_ENDIAN)
    byteswap = 0;
#elif defined(__sparc) || defined(__sparc__) || defined(_POWER) || defined(__powerpc__) || \
      defined(__ppc__) || defined(__hpux) || defined(_MIPSEB) || defined(_POWER) || defined(__s390__)
    byteswap = 1;
#elif defined(__i386__) || defined(__alpha__) || defined(__ia64) || defined(__ia64__) || \
      defined(_M_IX86) || defined(_M_IA64) || defined(_M_ALPHA) || defined(__amd64) || \
      defined(__amd64__) || defined(_M_AMD64) || defined(__x86_64) || defined(__x86_64__) || \
      defined(_M_X64) || defined(__bfin__)
    byteswap = 0;
#else
    return NULL;
#endif

    fp = fopen(matname,"wb");
    if ( !fp )
        return NULL;

    TRY {
        mat = NEW(mat_t);
    } CATCH(mat==NULL) {
        fclose(fp);
        END(Mat_Critical("Couldn't allocate memory for the MAT file"),NULL);
    }

    mat->header        = NULL;
    mat->subsys_offset = NULL;
    mat->fp            = fp;
    mat->version       = MAT_FT_MAT4;
    mat->byteswap      = byteswap;
    mat->bof           = 0;
    mat->next_index    = 0;
    mat->refs_id       = -1;
    mat->filename      = STRDUP(matname);
    mat->mode          = 0;

    Mat_Rewind(mat);

    return mat;
}

/** @if mat_devman
 * @brief Writes a matlab variable to a version 4 matlab file
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @retval 0 on success
 * @endif
 */
int
Mat_VarWrite4(mat_t *mat,matvar_t *matvar)
{
    typedef struct {
        mat_int32_t type;
        mat_int32_t mrows;
        mat_int32_t ncols;
        mat_int32_t imagf;
        mat_int32_t namelen;
    } Fmatrix;

    mat_int32_t nmemb = 1, i;
    Fmatrix x;

    if ( NULL == mat || NULL == matvar || NULL == matvar->name || matvar->rank != 2 )
        return -1;

    /* FIXME: SEEK_END is not Guaranteed by the C standard */
    (void)fseek((FILE*)mat->fp,0,SEEK_END);         /* Always write at end of file */

    switch ( matvar->data_type ) {
        case MAT_T_DOUBLE:
            x.type = 0;
            break;
        case MAT_T_SINGLE:
            x.type = 10;
            break;
        case MAT_T_INT32:
            x.type = 20;
            break;
        case MAT_T_INT16:
            x.type = 30;
            break;
        case MAT_T_UINT16:
            x.type = 40;
            break;
        case MAT_T_UINT8:
            x.type = 50;
            break;
        default:
            return 2;
    }

    if ( mat->byteswap )
        x.type += 1000;

    x.namelen = (mat_int32_t)strlen(matvar->name) + 1;

    switch ( matvar->class_type ) {
        case MAT_C_CHAR:
            x.type++;
            /* Fall through */
        case MAT_C_DOUBLE:
        case MAT_C_SINGLE:
        case MAT_C_INT32:
        case MAT_C_INT16:
        case MAT_C_UINT16:
        case MAT_C_UINT8:
            for ( i = 0; i < matvar->rank; i++ ) {
                mat_int32_t dim;
                dim = (mat_int32_t)matvar->dims[i];
                nmemb *= dim;
            }

            x.mrows = (mat_int32_t)matvar->dims[0];
            x.ncols = (mat_int32_t)matvar->dims[1];
            x.imagf = matvar->isComplex ? 1 : 0;
            fwrite(&x, sizeof(Fmatrix), 1, (FILE*)mat->fp);
            fwrite(matvar->name, sizeof(char), x.namelen, (FILE*)mat->fp);
            if (matvar->isComplex) {
                mat_complex_split_t *complex_data;

                complex_data = (mat_complex_split_t*)matvar->data;
                fwrite(complex_data->Re, matvar->data_size, nmemb, (FILE*)mat->fp);
                fwrite(complex_data->Im, matvar->data_size, nmemb, (FILE*)mat->fp);
            }
            else {
                fwrite(matvar->data, matvar->data_size, nmemb, (FILE*)mat->fp);
            }
            break;
        case MAT_C_SPARSE:
        {
            mat_sparse_t* sparse;
            double tmp;
            int i, j;
            size_t stride = Mat_SizeOf(matvar->data_type);
#if !defined(EXTENDED_SPARSE)
            if ( MAT_T_DOUBLE != matvar->data_type )
                break;
#endif

            sparse = (mat_sparse_t*)matvar->data;
            x.type += 2;
            x.mrows = sparse->njc > 0 ? sparse->jc[sparse->njc - 1] + 1 : 1;
            x.ncols = matvar->isComplex ? 4 : 3;
            x.imagf = 0;

            fwrite(&x, sizeof(Fmatrix), 1, (FILE*)mat->fp);
            fwrite(matvar->name, sizeof(char), x.namelen, (FILE*)mat->fp);

            for ( i = 0; i < sparse->njc - 1; i++ ) {
                for ( j = sparse->jc[i];
                      j < sparse->jc[i + 1] && j < sparse->ndata; j++ ) {
                    tmp = sparse->ir[j] + 1;
                    fwrite(&tmp, sizeof(double), 1, (FILE*)mat->fp);
                }
            }
            tmp = matvar->dims[0];
            fwrite(&tmp, sizeof(double), 1, (FILE*)mat->fp);
            for ( i = 0; i < sparse->njc - 1; i++ ) {
                for ( j = sparse->jc[i];
                      j < sparse->jc[i + 1] && j < sparse->ndata; j++ ) {
                    tmp = i + 1;
                    fwrite(&tmp, sizeof(double), 1, (FILE*)mat->fp);
                }
            }
            tmp = matvar->dims[1];
            fwrite(&tmp, sizeof(double), 1, (FILE*)mat->fp);
            tmp = 0.;
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data;
                char* re, *im;

                complex_data = (mat_complex_split_t*)sparse->data;
                re = (char*)complex_data->Re;
                im = (char*)complex_data->Im;
                for ( i = 0; i < sparse->njc - 1; i++ ) {
                    for ( j = sparse->jc[i];
                          j < sparse->jc[i + 1] && j < sparse->ndata; j++ ) {
                        fwrite(re + j*stride, stride, 1, (FILE*)mat->fp);
                    }
                }
                fwrite(&tmp, stride, 1, (FILE*)mat->fp);
                for ( i = 0; i < sparse->njc - 1; i++ ) {
                    for ( j = sparse->jc[i];
                          j < sparse->jc[i + 1] && j < sparse->ndata; j++ ) {
                        fwrite(im + j*stride, stride, 1, (FILE*)mat->fp);
                    }
                }
            } else {
                char *data = (char*)sparse->data;
                for ( i = 0; i < sparse->njc - 1; i++ ) {
                    for ( j = sparse->jc[i];
                          j < sparse->jc[i + 1] && j < sparse->ndata; j++ ) {
                        fwrite(data + j*stride, stride, 1, (FILE*)mat->fp);
                    }
                }
            }
            fwrite(&tmp, stride, 1, (FILE*)mat->fp);
            break;
        }
        default:
            break;
    }

    return 0;
}

/** @if mat_devman
 * @brief Reads the data of a version 4 MAT file variable
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar MAT variable pointer to read the data
 * @endif
 */
void
Read4(mat_t *mat,matvar_t *matvar)
{
    unsigned int N;

    (void)fseek((FILE*)mat->fp,matvar->internal->datapos,SEEK_SET);

    N = matvar->dims[0]*matvar->dims[1];
    switch ( matvar->class_type ) {
        case MAT_C_DOUBLE:
            matvar->data_size = sizeof(double);
            matvar->nbytes    = N*matvar->data_size;
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data;

                TRY {
                    complex_data = NEW(mat_complex_split_t);
                } CATCH(complex_data==NULL) {
                    V_END(Mat_Critical("Memory allocation failure"));
                }

                TRY {
                    complex_data->Re = NEW_ARRAY(char,matvar->nbytes);
                    complex_data->Im = NEW_ARRAY(char,matvar->nbytes);
                    matvar->data     = complex_data;
                } CATCH(complex_data->Re == NULL || complex_data->Im == NULL) {
                    V_END(Mat_Critical("Memory allocation failure"));
                }

                ReadDoubleData(mat, (double*)complex_data->Re, matvar->data_type, N);
                ReadDoubleData(mat, (double*)complex_data->Im, matvar->data_type, N);

            } else {
                TRY {
                    matvar->data = NEW_ARRAY(char,matvar->nbytes);
                } CATCH(matvar->data == NULL ) {
                    V_END(Mat_Critical("Memory allocation failure"));
                }

                ReadDoubleData(mat, (double*)matvar->data, matvar->data_type, N);
            }
            /* Update data type to match format of matvar->data */
            matvar->data_type = MAT_T_DOUBLE;
            break;
        case MAT_C_CHAR:
            matvar->data_size = 1;
            matvar->nbytes = N;

            TRY {
                matvar->data = NEW_ARRAY(char,matvar->nbytes);
            } CATCH (matvar->data==NULL) {
                V_END(Mat_Critical("Memory allocation failure"));
            }

            ReadUInt8Data(mat,(mat_uint8_t*)matvar->data,matvar->data_type,N);
            matvar->data_type = MAT_T_UINT8;
            break;
        case MAT_C_SPARSE: {

            double tmp;
            int i,j;
            mat_sparse_t* sparse;
            mat_int32_t *jc;
            long fpos;

            matvar->data_size = sizeof(mat_sparse_t);

            TRY {
                matvar->data = NEW_ARRAY(char,matvar->data_size);
            } CATCH (matvar->data==NULL) {
                V_END(Mat_Critical("Memory allocation failure"));
            }

            enum matio_types data_type = MAT_T_DOUBLE;

            /* matvar->dims[1] either is 3 for real or 4 for complex sparse */
            matvar->isComplex = matvar->dims[1] == 4 ? 1 : 0;
            sparse = (mat_sparse_t*)matvar->data;
            sparse->nir = matvar->dims[0] - 1;
            sparse->nzmax = sparse->nir;

            TRY {
                sparse->ir = NEW_ARRAY(mat_int32_t,sparse->nir);

            } CATCH(sparse->ir==NULL) {
                DELETE_BUFFER(matvar->data);
                matvar->data = NULL;
                V_END(Mat_Critical("Memory allocation failure"));
            }

            ReadInt32Data(mat, sparse->ir, data_type, sparse->nir);
            for ( i = 0; i < sparse->nir; i++ )
                sparse->ir[i] = sparse->ir[i] - 1;

            ReadDoubleData(mat, &tmp, data_type, 1);
            matvar->dims[0] = tmp;

            TRY {
                fpos = ftell((FILE*)mat->fp);
            } CATCH(fpos==-1L) {
                DELETE_ARRAY(sparse->ir);
                DELETE_BUFFER(matvar->data);
                matvar->data = NULL;
                V_END(Mat_Critical("Couldn't determine file position"));
            }

            TRY {
                (void)fseek((FILE*)mat->fp,sparse->nir*Mat_SizeOf(data_type),
                    SEEK_CUR);
                ReadDoubleData(mat, &tmp, data_type, 1);
            } CATCH ( tmp > INT_MAX-1 || tmp < 0 ) {
                DELETE_ARRAY(sparse->ir);
                DELETE_BUFFER(matvar->data);
                matvar->data = NULL;
                V_END(Mat_Critical("Invalid column dimension for sparse matrix"));
            }
            matvar->dims[1] = tmp < 0 ? 0 : ( tmp > INT_MAX-1 ? INT_MAX-1 : (size_t)tmp );

            TRY {
                (void)fseek((FILE*)mat->fp,fpos,SEEK_SET);
            } CATCH ( matvar->dims[1] > INT_MAX-1 ) {
                DELETE_ARRAY(sparse->ir);
                DELETE_BUFFER(matvar->data);
                matvar->data = NULL;
                V_END(Mat_Critical("Invalid column dimension for sparse matrix"));
            }

            TRY {
                sparse->njc = (int)matvar->dims[1] + 1;
                sparse->jc = NEW_ARRAY(mat_int32_t,sparse->njc);
            } CATCH(sparse->jc == NULL) {
                DELETE_ARRAY(sparse->ir);
                DELETE_BUFFER(matvar->data);
                matvar->data = NULL;
                V_END(Mat_Critical("Memory allocation failure"));
            }

            TRY {
                jc = NEW_ARRAY(mat_int32_t,sparse->nir);
            } CATCH( jc == NULL ) {
                DELETE_ARRAY(sparse->jc);
                DELETE_ARRAY(sparse->ir);
                DELETE_BUFFER(matvar->data);
                matvar->data = NULL;
                V_END(Mat_Critical("Memory allocation failure"));
            }

            j = 0;
            sparse->jc[0] = 0;
            ReadInt32Data(mat, jc, data_type, sparse->nir);
            for ( i = 1; i < sparse->njc-1; i++ ) {
                while ( j < sparse->nir && jc[j] <= i )
                    j++;
                sparse->jc[i] = j;
            }
            DELETE_ARRAY(jc);
            /* terminating nnz */
            sparse->jc[sparse->njc-1] = sparse->nir;

            ReadDoubleData(mat, &tmp, data_type, 1);
            sparse->ndata = sparse->nir;
            data_type = matvar->data_type;
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data;

                TRY {
                    complex_data = NEW(mat_complex_split_t);
                } CATCH( complex_data == NULL ) {
                    DELETE_ARRAY(sparse->jc);
                    DELETE_ARRAY(sparse->ir);
                    DELETE_BUFFER(matvar->data);
                    matvar->data = NULL;
                    V_END(Mat_Critical("Memory allocation failure"));
                }

                TRY {
                    complex_data->Re = NEW_ARRAY(char,sparse->ndata*Mat_SizeOf(data_type));
                    complex_data->Im = NEW_ARRAY(char,sparse->ndata*Mat_SizeOf(data_type));
                    sparse->data     = complex_data;
                } CATCH( complex_data->Re == NULL || complex_data->Im == NULL ) {
                    DELETE(complex_data);
                    DELETE_ARRAY(sparse->jc);
                    DELETE_ARRAY(sparse->ir);
                    DELETE_BUFFER(matvar->data);
                    matvar->data = NULL;
                    V_END(Mat_Critical("Memory allocation failure"));
                }
#if defined(EXTENDED_SPARSE)
                switch ( data_type ) {
                    case MAT_T_DOUBLE:
                        ReadDoubleData(mat, (double*)complex_data->Re,
                            data_type, sparse->ndata);
                        ReadDoubleData(mat, &tmp, data_type, 1);
                        ReadDoubleData(mat, (double*)complex_data->Im,
                            data_type, sparse->ndata);
                        ReadDoubleData(mat, &tmp, data_type, 1);
                        break;
                    case MAT_T_SINGLE:
                    {
                        float tmp2;
                        ReadSingleData(mat, (float*)complex_data->Re,
                            data_type, sparse->ndata);
                        ReadSingleData(mat, &tmp2, data_type, 1);
                        ReadSingleData(mat, (float*)complex_data->Im,
                            data_type, sparse->ndata);
                        ReadSingleData(mat, &tmp2, data_type, 1);
                        break;
                    }
                    case MAT_T_INT32:
                    {
                        mat_int32_t tmp2;
                        ReadInt32Data(mat, (mat_int32_t*)complex_data->Re,
                            data_type, sparse->ndata);
                        ReadInt32Data(mat, &tmp2, data_type, 1);
                        ReadInt32Data(mat, (mat_int32_t*)complex_data->Im,
                            data_type, sparse->ndata);
                        ReadInt32Data(mat, &tmp2, data_type, 1);
                        break;
                    }
                    case MAT_T_INT16:
                    {
                        mat_int16_t tmp2;
                        ReadInt16Data(mat, (mat_int16_t*)complex_data->Re,
                            data_type, sparse->ndata);
                        ReadInt16Data(mat, &tmp2, data_type, 1);
                        ReadInt16Data(mat, (mat_int16_t*)complex_data->Im,
                            data_type, sparse->ndata);
                        ReadInt16Data(mat, &tmp2, data_type, 1);
                        break;
                    }
                    case MAT_T_UINT16:
                    {
                        mat_uint16_t tmp2;
                        ReadUInt16Data(mat, (mat_uint16_t*)complex_data->Re,
                            data_type, sparse->ndata);
                        ReadUInt16Data(mat, &tmp2, data_type, 1);
                        ReadUInt16Data(mat, (mat_uint16_t*)complex_data->Im,
                            data_type, sparse->ndata);
                        ReadUInt16Data(mat, &tmp2, data_type, 1);
                        break;
                    }
                    case MAT_T_UINT8:
                    {
                        mat_uint8_t tmp2;
                        ReadUInt8Data(mat, (mat_uint8_t*)complex_data->Re,
                            data_type, sparse->ndata);
                        ReadUInt8Data(mat, &tmp2, data_type, 1);
                        ReadUInt8Data(mat, (mat_uint8_t*)complex_data->Im,
                            data_type, sparse->ndata);
                        ReadUInt8Data(mat, &tmp2, data_type, 1);
                        break;
                    }
                    default:
                        DELETE_BUFFER(complex_data->Re);
                        DELETE_BUFFER(complex_data->Im);
                        DELETE(complex_data);
                        DELETE_ARRAY(sparse->jc);
                        DELETE_ARRAY(sparse->ir);
                        DELETE_BUFFER(matvar->data);
                        matvar->data = NULL;
                        V_END(Mat_Critical("Read4: %d is not a supported data type for ",
                              "extended sparse", data_type));
                }
#else
                ReadDoubleData(mat, (double*)complex_data->Re,
                    data_type, sparse->ndata);
                ReadDoubleData(mat, &tmp, data_type, 1);
                ReadDoubleData(mat, (double*)complex_data->Im,
                    data_type, sparse->ndata);
                ReadDoubleData(mat, &tmp, data_type, 1);
#endif
            } else {
                TRY {
                    sparse->data = NEW_ARRAY(char,sparse->ndata*Mat_SizeOf(data_type));
                } CATCH( sparse->data == NULL ) {
                    DELETE_ARRAY(sparse->jc);
                    DELETE_ARRAY(sparse->ir);
                    DELETE_BUFFER(matvar->data);
                    matvar->data = NULL;
                    V_END(Mat_Critical("Memory allocation failure"));
                }
#if defined(EXTENDED_SPARSE)
                switch ( data_type ) {
                    case MAT_T_DOUBLE:
                        ReadDoubleData(mat, (double*)sparse->data,
                            data_type, sparse->ndata);
                        ReadDoubleData(mat, &tmp, data_type, 1);
                        break;
                    case MAT_T_SINGLE:
                    {
                        float tmp2;
                        ReadSingleData(mat, (float*)sparse->data,
                            data_type, sparse->ndata);
                        ReadSingleData(mat, &tmp2, data_type, 1);
                        break;
                    }
                    case MAT_T_INT32:
                    {
                        mat_int32_t tmp2;
                        ReadInt32Data(mat, (mat_int32_t*)sparse->data,
                            data_type, sparse->ndata);
                        ReadInt32Data(mat, &tmp2, data_type, 1);
                        break;
                    }
                    case MAT_T_INT16:
                    {
                        mat_int16_t tmp2;
                        ReadInt16Data(mat, (mat_int16_t*)sparse->data,
                            data_type, sparse->ndata);
                        ReadInt16Data(mat, &tmp2, data_type, 1);
                        break;
                    }
                    case MAT_T_UINT16:
                    {
                        mat_uint16_t tmp2;
                        ReadUInt16Data(mat, (mat_uint16_t*)sparse->data,
                            data_type, sparse->ndata);
                        ReadUInt16Data(mat, &tmp2, data_type, 1);
                        break;
                    }
                    case MAT_T_UINT8:
                    {
                        mat_uint8_t tmp2;
                        ReadUInt8Data(mat, (mat_uint8_t*)sparse->data,
                            data_type, sparse->ndata);
                        ReadUInt8Data(mat, &tmp2, data_type, 1);
                        break;
                    }
                    default:
                        DELETE_BUFFER(sparse->data);
                        DELETE_ARRAY(sparse->jc);
                        DELETE_ARRAY(sparse->ir);
                        DELETE_BUFFER(matvar->data);
                        matvar->data = NULL;
                        V_END(Mat_Critical("Read4: %d is not a supported data type for ",
                                 "extended sparse", data_type));
                }
#else
                ReadDoubleData(mat, (double*)sparse->data, data_type, sparse->ndata);
                ReadDoubleData(mat, &tmp, data_type, 1);
#endif
            }
            break;
        }
        default:
            V_END(Mat_Critical("MAT V4 data type error"));
    }

    return;
}

/** @if mat_devman
 * @brief Reads a slab of data from a version 4 MAT file for the @c matvar variable
 *
 * @ingroup mat_internal
 * @param mat Version 4 MAT file pointer
 * @param matvar pointer to the mat variable
 * @param data pointer to store the read data in (must be of size
 *             edge[0]*...edge[rank-1]*Mat_SizeOfClass(matvar->class_type))
 * @param start index to start reading data in each dimension
 * @param stride write data every @c stride elements in each dimension
 * @param edge number of elements to read in each dimension
 * @retval 0 on success
 * @endif
 */
int
ReadData4(mat_t *mat,matvar_t *matvar,void *data,
      int *start,int *stride,int *edge)
{
    int err = 0;
    enum matio_classes class_type = MAT_C_EMPTY;

    (void)fseek((FILE*)mat->fp,matvar->internal->datapos,SEEK_SET);

    switch( matvar->data_type ) {
        case MAT_T_DOUBLE:
            class_type = MAT_C_DOUBLE;
            break;
        case MAT_T_SINGLE:
            class_type = MAT_C_SINGLE;
            break;
        case MAT_T_INT32:
            class_type = MAT_C_INT32;
            break;
        case MAT_T_INT16:
            class_type = MAT_C_INT16;
            break;
        case MAT_T_UINT16:
            class_type = MAT_C_UINT16;
            break;
        case MAT_T_UINT8:
            class_type = MAT_C_UINT8;
            break;
        default:
            return 1;
    }

    if ( matvar->rank == 2 ) {
        if ( stride[0]*(edge[0]-1)+start[0]+1 > matvar->dims[0] )
            err = 1;
        else if ( stride[1]*(edge[1]-1)+start[1]+1 > matvar->dims[1] )
            err = 1;
        if ( matvar->isComplex ) {
            mat_complex_split_t *cdata = (mat_complex_split_t*)data;
            long nbytes = edge[0]*edge[1]*Mat_SizeOf(matvar->data_type);

            ReadDataSlab2(mat,cdata->Re,class_type,matvar->data_type,
                    matvar->dims,start,stride,edge);
            (void)fseek((FILE*)mat->fp,matvar->internal->datapos+nbytes,SEEK_SET);
            ReadDataSlab2(mat,cdata->Im,class_type,
                matvar->data_type,matvar->dims,start,stride,edge);
        } else {
            ReadDataSlab2(mat,data,class_type,matvar->data_type,
                    matvar->dims,start,stride,edge);
        }
    } else if ( matvar->isComplex ) {
        int i;
        mat_complex_split_t *cdata = (mat_complex_split_t*)data;
        long nbytes = Mat_SizeOf(matvar->data_type);

        for ( i = 0; i < matvar->rank; i++ )
            nbytes *= edge[i];

        ReadDataSlabN(mat,cdata->Re,class_type,matvar->data_type,
            matvar->rank,matvar->dims,start,stride,edge);
        (void)fseek((FILE*)mat->fp,matvar->internal->datapos+nbytes,SEEK_SET);
        ReadDataSlabN(mat,cdata->Im,class_type,matvar->data_type,
            matvar->rank,matvar->dims,start,stride,edge);
    } else {
        ReadDataSlabN(mat,data,class_type,matvar->data_type,
            matvar->rank,matvar->dims,start,stride,edge);
    }
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
Mat_VarReadDataLinear4(mat_t *mat,matvar_t *matvar,void *data,int start,
                       int stride,int edge)
{
    size_t i, nmemb = 1;
    int err = 0;

    (void)fseek((FILE*)mat->fp,matvar->internal->datapos,SEEK_SET);

    matvar->data_size = Mat_SizeOf(matvar->data_type);

    for ( i = 0; i < matvar->rank; i++ )
        nmemb *= matvar->dims[i];

    if ( stride*(edge-1)+start+1 > nmemb ) {
        return 1;
    }
    if ( matvar->isComplex ) {
            mat_complex_split_t *complex_data = (mat_complex_split_t*)data;
            long nbytes = nmemb*matvar->data_size;

            ReadDataSlab1(mat,complex_data->Re,matvar->class_type,
                          matvar->data_type,start,stride,edge);
            (void)fseek((FILE*)mat->fp,matvar->internal->datapos+nbytes,SEEK_SET);
            ReadDataSlab1(mat,complex_data->Im,matvar->class_type,
                          matvar->data_type,start,stride,edge);
    } else {
        ReadDataSlab1(mat,data,matvar->class_type,matvar->data_type,start,
                      stride,edge);
    }

    return err;
}

/** @if mat_devman
 * @brief Reads the header information for the next MAT variable in a version 4 MAT file
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @return pointer to the MAT variable or NULL
 * @endif
 */
matvar_t *
Mat_VarReadNextInfo4(mat_t *mat)
{
    int       tmp,M,O,data_type,class_type;
    long      nBytes;
    size_t    err;
    matvar_t *matvar = NULL;
    union {
        mat_uint32_t u;
        mat_uint8_t  c[4];
    } endian;

    if ( mat == NULL || mat->fp == NULL )
        END(Mat_Critical("Bad mat descriptor"),NULL);
    else if ( NULL == (matvar = Mat_VarCalloc()) )
        END(Mat_Critical("Memory allocation failure"),NULL);

    TRY {
        matvar->internal->fp   = mat;
        matvar->internal->fpos = ftell((FILE*)mat->fp);
    } CATCH ( matvar->internal->fpos == -1L ) {
        Mat_VarFree(matvar);
        END(Mat_Critical("Couldn't determine file position"),NULL);
    }

    err = fread(&tmp,sizeof(int),1,(FILE*)mat->fp);
    if ( !err ) {
        Mat_VarFree(matvar);
        return NULL;
    }

    endian.u = 0x01020304;

    /* See if MOPT may need byteswapping */
    if ( tmp < 0 || tmp > 4052 ) {
        if ( Mat_int32Swap(&tmp) > 4052 ) {
            Mat_VarFree(matvar);
            END(Mat_Critical("Bad data descriptor"),NULL);
        }
    }

    M = floor(tmp / 1000.0);
    tmp -= M*1000;
    O = floor(tmp / 100.0);
    tmp -= O*100;
    data_type = floor(tmp / 10.0);
    tmp -= data_type*10;
    class_type = floor(tmp / 1.0);

    switch ( M ) {
        case 0:
            /* IEEE little endian */
            mat->byteswap = (endian.c[0] != 4);
            break;
        case 1:
            /* IEEE big endian */
            mat->byteswap = (endian.c[0] != 1);
            break;
        default:
            /* VAX, Cray, or bogus */
            Mat_VarFree(matvar);
            END(Mat_Critical("Wrong/Unsupported floating point format"),NULL);
    }
    /* O must be zero */
    if ( 0 != O ) {
        Mat_VarFree(matvar);
        END(Mat_Critical("Bad data descriptor"),NULL);
    }
    /* Convert the V4 data type */
    switch ( data_type ) {
        case 0:
            matvar->data_type = MAT_T_DOUBLE;
            break;
        case 1:
            matvar->data_type = MAT_T_SINGLE;
            break;
        case 2:
            matvar->data_type = MAT_T_INT32;
            break;
        case 3:
            matvar->data_type = MAT_T_INT16;
            break;
        case 4:
            matvar->data_type = MAT_T_UINT16;
            break;
        case 5:
            matvar->data_type = MAT_T_UINT8;
            break;
        default:
            Mat_VarFree(matvar);
            END(Mat_Critical("Wrong data type"),NULL);
    }
    switch ( class_type ) {
        case 0:
            matvar->class_type = MAT_C_DOUBLE;
            break;
        case 1:
            matvar->class_type = MAT_C_CHAR;
            break;
        case 2:
            matvar->class_type = MAT_C_SPARSE;
            break;
        default:
            Mat_VarFree(matvar);
            END(Mat_Critical("Wrong class type"),NULL);
    }
    matvar->rank = 2;
    TRY {
        matvar->dims = NEW_ARRAY(size_t,2);
    } CATCH ( NULL == matvar->dims ) {
        Mat_VarFree(matvar);
        END(Mat_Critical("Memory allocation failure"),NULL);
    }

    TRY {
        err = fread(&tmp,sizeof(int),1,(FILE*)mat->fp);
        if ( mat->byteswap )
            Mat_int32Swap(&tmp);
        matvar->dims[0] = tmp;
    } CATCH ( !err ) {
        Mat_VarFree(matvar);
        END(Mat_Critical("Couldn't read data from file"),NULL);
    }

    TRY {
        err = fread(&tmp,sizeof(int),1,(FILE*)mat->fp);
        if ( mat->byteswap )
            Mat_int32Swap(&tmp);
        matvar->dims[1] = tmp;
    } CATCH( !err ) {
        Mat_VarFree(matvar);
        END(Mat_Critical("Couldn't read data from file"),NULL);
    }

    TRY {
        err = fread(&(matvar->isComplex),sizeof(int),1,(FILE*)mat->fp);
        //  No byte swap here. This is stange. TODO: POSSIBLE BUG.
    } CATCH( !err ) {
        Mat_VarFree(matvar);
        END(Mat_Critical("Couldn't read data from file"),NULL);
    }

    TRY {
        err = fread(&tmp,sizeof(int),1,(FILE*)mat->fp);
        if ( mat->byteswap )
            Mat_int32Swap(&tmp);
    } CATCH( !err ) {
        Mat_VarFree(matvar);
        END(Mat_Critical("Couldn't read data from file"),NULL);
    }
    /* Check that the length of the variable name is at least 1 */
    if ( tmp < 1 ) {
        Mat_VarFree(matvar);
        END(Mat_Critical("Length of variable name must be at least 1"),NULL);
    }

    TRY {
        matvar->name = NEW_ARRAY(char,tmp);
    } CATCH ( NULL == matvar->name ) {
        Mat_VarFree(matvar);
        END(Mat_Critical("Memory allocation failure"),NULL);
    }

    TRY {
        err = fread(matvar->name,1,tmp,(FILE*)mat->fp);
    } CATCH( !err ) {
        Mat_VarFree(matvar);
        END(Mat_Critical("Couldn't read data from file"),NULL);
    }

    matvar->internal->datapos = ftell((FILE*)mat->fp);
    if ( matvar->internal->datapos == -1L ) {
        Mat_VarFree(matvar);
        END(Mat_Critical("Couldn't determine file position"),NULL);
    }
    nBytes = matvar->dims[0]*matvar->dims[1]*Mat_SizeOf(matvar->data_type);
    if ( matvar->isComplex )
        nBytes *= 2;
    (void)fseek((FILE*)mat->fp,nBytes,SEEK_CUR);
    //  No check here, TODO: POSSIBLE BUG.

    return matvar;
}
