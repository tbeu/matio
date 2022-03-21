/** @file mat4.c
 * Matlab MAT version 4 file functions
 * @ingroup MAT
 */
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

#include "matio_private.h"
#include "mat4.h"
#if defined(__GLIBC__)
#include <endian.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <limits.h>
#if defined(_MSC_VER) || defined(__MINGW32__)
#define strdup _strdup
#endif

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
Mat_Create4(const char *matname)
{
    FILE *fp = NULL;
    mat_t *mat = NULL;

#if defined(_WIN32) && defined(_MSC_VER)
    wchar_t *wname = utf82u(matname);
    if ( NULL != wname ) {
        fp = _wfopen(wname, L"w+b");
        free(wname);
    }
#else
    fp = fopen(matname, "w+b");
#endif
    if ( !fp )
        return NULL;

    mat = (mat_t *)malloc(sizeof(*mat));
    if ( NULL == mat ) {
        fclose(fp);
        Mat_Critical("Couldn't allocate memory for the MAT file");
        return NULL;
    }

    mat->fp = fp;
    mat->header = NULL;
    mat->subsys_offset = NULL;
    mat->filename = strdup(matname);
    mat->version = MAT_FT_MAT4;
    mat->byteswap = 0;
    mat->mode = 0;
    mat->bof = 0;
    mat->next_index = 0;
    mat->num_datasets = 0;
#if defined(MAT73) && MAT73
    mat->refs_id = -1;
#endif
    mat->dir = NULL;

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
Mat_VarWrite4(mat_t *mat, matvar_t *matvar)
{
    typedef struct
    {
        mat_int32_t type;
        mat_int32_t mrows;
        mat_int32_t ncols;
        mat_int32_t imagf;
        mat_int32_t namelen;
    } Fmatrix;

    mat_uint32_t i;
    Fmatrix x;

    if ( NULL == mat || NULL == matvar )
        return MATIO_E_BAD_ARGUMENT;
    if ( NULL == matvar->name || matvar->rank != 2 )
        return MATIO_E_OUTPUT_BAD_DATA;

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
            return MATIO_E_OUTPUT_BAD_DATA;
    }

#if defined(__GLIBC__)
#if ( __BYTE_ORDER == __LITTLE_ENDIAN )
#elif (__BYTE_ORDER == __BIG_ENDIAN)
    x.type += 1000;
#else
    return MATIO_E_OPERATION_NOT_SUPPORTED;
#endif
#elif defined(_BIG_ENDIAN) && !defined(_LITTLE_ENDIAN)
    x.type += 1000;
#elif defined(_LITTLE_ENDIAN) && !defined(_BIG_ENDIAN)
#elif defined(__sparc) || defined(__sparc__) || defined(_POWER) || defined(__powerpc__) || \
    defined(__ppc__) || defined(__hpux) || defined(_MIPSEB) || defined(_POWER) ||          \
    defined(__s390__)
    x.type += 1000;
#elif defined(__i386__) || defined(__alpha__) || defined(__ia64) || defined(__ia64__) ||   \
    defined(_M_IX86) || defined(_M_IA64) || defined(_M_ALPHA) || defined(__amd64) ||       \
    defined(__amd64__) || defined(_M_AMD64) || defined(__x86_64) || defined(__x86_64__) || \
    defined(_M_X64) || defined(__bfin__) || defined(__loongarch64)
#else
    return MATIO_E_OPERATION_NOT_SUPPORTED;
#endif

    x.namelen = (mat_int32_t)strlen(matvar->name) + 1;

    /* FIXME: SEEK_END is not Guaranteed by the C standard */
    (void)fseeko((FILE *)mat->fp, 0, SEEK_END); /* Always write at end of file */

    switch ( matvar->class_type ) {
        case MAT_C_CHAR:
            x.type++;
            /* Fall through */
        case MAT_C_DOUBLE:
        case MAT_C_SINGLE:
        case MAT_C_INT32:
        case MAT_C_INT16:
        case MAT_C_UINT16:
        case MAT_C_UINT8: {
            size_t nelems = 1;
            int err = Mat_MulDims(matvar, &nelems);
            if ( err ) {
                Mat_Critical("Integer multiplication overflow");
                return err;
            }

            x.mrows = (mat_int32_t)matvar->dims[0];
            x.ncols = (mat_int32_t)matvar->dims[1];
            x.imagf = matvar->isComplex ? 1 : 0;
            fwrite(&x, sizeof(Fmatrix), 1, (FILE *)mat->fp);
            fwrite(matvar->name, sizeof(char), x.namelen, (FILE *)mat->fp);
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data;

                complex_data = (mat_complex_split_t *)matvar->data;
                fwrite(complex_data->Re, matvar->data_size, nelems, (FILE *)mat->fp);
                fwrite(complex_data->Im, matvar->data_size, nelems, (FILE *)mat->fp);
            } else {
                fwrite(matvar->data, matvar->data_size, nelems, (FILE *)mat->fp);
            }
            break;
        }
        case MAT_C_SPARSE: {
            mat_sparse_t *sparse;
            double tmp;
            mat_uint32_t j;
            size_t stride = Mat_SizeOf(matvar->data_type);
#if !defined(EXTENDED_SPARSE)
            if ( MAT_T_DOUBLE != matvar->data_type )
                break;
#endif

            sparse = (mat_sparse_t *)matvar->data;
            x.type += 2;
            x.mrows = sparse->njc > 0 ? sparse->jc[sparse->njc - 1] + 1 : 1;
            x.ncols = matvar->isComplex ? 4 : 3;
            x.imagf = 0;

            fwrite(&x, sizeof(Fmatrix), 1, (FILE *)mat->fp);
            fwrite(matvar->name, sizeof(char), x.namelen, (FILE *)mat->fp);

            for ( i = 0; i < sparse->njc - 1; i++ ) {
                for ( j = sparse->jc[i]; j < sparse->jc[i + 1] && j < sparse->ndata; j++ ) {
                    tmp = sparse->ir[j] + 1;
                    fwrite(&tmp, sizeof(double), 1, (FILE *)mat->fp);
                }
            }
            tmp = (double)matvar->dims[0];
            fwrite(&tmp, sizeof(double), 1, (FILE *)mat->fp);
            for ( i = 0; i < sparse->njc - 1; i++ ) {
                for ( j = sparse->jc[i]; j < sparse->jc[i + 1] && j < sparse->ndata; j++ ) {
                    tmp = i + 1;
                    fwrite(&tmp, sizeof(double), 1, (FILE *)mat->fp);
                }
            }
            tmp = (double)matvar->dims[1];
            fwrite(&tmp, sizeof(double), 1, (FILE *)mat->fp);
            tmp = 0.;
            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data;
                char *re, *im;

                complex_data = (mat_complex_split_t *)sparse->data;
                re = (char *)complex_data->Re;
                im = (char *)complex_data->Im;
                for ( i = 0; i < sparse->njc - 1; i++ ) {
                    for ( j = sparse->jc[i]; j < sparse->jc[i + 1] && j < sparse->ndata; j++ ) {
                        fwrite(re + j * stride, stride, 1, (FILE *)mat->fp);
                    }
                }
                fwrite(&tmp, stride, 1, (FILE *)mat->fp);
                for ( i = 0; i < sparse->njc - 1; i++ ) {
                    for ( j = sparse->jc[i]; j < sparse->jc[i + 1] && j < sparse->ndata; j++ ) {
                        fwrite(im + j * stride, stride, 1, (FILE *)mat->fp);
                    }
                }
            } else {
                char *data = (char *)sparse->data;
                for ( i = 0; i < sparse->njc - 1; i++ ) {
                    for ( j = sparse->jc[i]; j < sparse->jc[i + 1] && j < sparse->ndata; j++ ) {
                        fwrite(data + j * stride, stride, 1, (FILE *)mat->fp);
                    }
                }
            }
            fwrite(&tmp, stride, 1, (FILE *)mat->fp);
            break;
        }
        default:
            break;
    }

    return MATIO_E_NO_ERROR;
}

/** @if mat_devman
 * @brief Reads the data of a version 4 MAT file variable
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar MAT variable pointer to read the data
 * @retval 0 on success
 * @endif
 */
int
Mat_VarRead4(mat_t *mat, matvar_t *matvar)
{
    int err;
    size_t nelems = 1;

    err = Mat_MulDims(matvar, &nelems);
    if ( err ) {
        Mat_Critical("Integer multiplication overflow");
        return err;
    }

    (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);

    switch ( matvar->class_type ) {
        case MAT_C_DOUBLE:
            matvar->data_size = sizeof(double);
            err = Mul(&matvar->nbytes, nelems, matvar->data_size);
            if ( err ) {
                Mat_Critical("Integer multiplication overflow");
                return err;
            }

            if ( matvar->isComplex ) {
                mat_complex_split_t *complex_data = ComplexMalloc(matvar->nbytes);
                if ( NULL != complex_data ) {
                    size_t readcount;
                    readcount =
                        ReadDoubleData(mat, (double *)complex_data->Re, matvar->data_type, nelems);
                    err = readcount != nelems;
                    readcount =
                        ReadDoubleData(mat, (double *)complex_data->Im, matvar->data_type, nelems);
                    err |= readcount != nelems;
                    if ( 0 == err ) {
                        matvar->data = complex_data;
                    } else {
                        ComplexFree(complex_data);
                        return MATIO_E_FILE_FORMAT_VIOLATION;
                    }
                } else {
                    Mat_Critical("Couldn't allocate memory for the complex data");
                    return MATIO_E_OUT_OF_MEMORY;
                }
            } else {
                matvar->data = malloc(matvar->nbytes);
                if ( NULL != matvar->data ) {
                    const size_t readcount =
                        ReadDoubleData(mat, (double *)matvar->data, matvar->data_type, nelems);
                    if ( readcount != nelems ) {
                        free(matvar->data);
                        matvar->data = NULL;
                        return MATIO_E_FILE_FORMAT_VIOLATION;
                    }
                } else {
                    Mat_Critical("Couldn't allocate memory for the data");
                    return MATIO_E_OUT_OF_MEMORY;
                }
            }
            /* Update data type to match format of matvar->data */
            matvar->data_type = MAT_T_DOUBLE;
            break;
        case MAT_C_CHAR:
            matvar->data_size = 1;
            matvar->nbytes = nelems;
            matvar->data = malloc(matvar->nbytes);
            if ( NULL != matvar->data ) {
                const size_t readcount =
                    ReadUInt8Data(mat, (mat_uint8_t *)matvar->data, matvar->data_type, nelems);
                if ( readcount != nelems ) {
                    free(matvar->data);
                    matvar->data = NULL;
                    return MATIO_E_FILE_FORMAT_VIOLATION;
                }
            } else {
                Mat_Critical("Couldn't allocate memory for the data");
                return MATIO_E_OUT_OF_MEMORY;
            }
            matvar->data_type = MAT_T_UINT8;
            break;
        case MAT_C_SPARSE:
            matvar->data_size = sizeof(mat_sparse_t);
            matvar->data = calloc(1, matvar->data_size);
            if ( NULL != matvar->data ) {
                double tmp;
                mat_uint32_t i;
                mat_sparse_t *sparse;
                mat_off_t fpos;
                enum matio_types data_type = MAT_T_DOUBLE;
                size_t readcount;

                /* matvar->dims[1] either is 3 for real or 4 for complex sparse */
                matvar->isComplex = matvar->dims[1] == 4 ? 1 : 0;
                if ( matvar->dims[0] < 2 ) {
                    return MATIO_E_FILE_FORMAT_VIOLATION;
                }
                sparse = (mat_sparse_t *)matvar->data;
                sparse->nir = (mat_uint32_t)(matvar->dims[0] - 1);
                sparse->nzmax = sparse->nir;
                err = Mul(&readcount, sparse->nir, sizeof(mat_uint32_t));
                if ( err ) {
                    Mat_Critical("Integer multiplication overflow");
                    return err;
                }
                sparse->ir = (mat_uint32_t *)malloc(readcount);
                if ( sparse->ir != NULL ) {
                    readcount = ReadUInt32Data(mat, sparse->ir, data_type, sparse->nir);
                    if ( readcount != sparse->nir ) {
                        free(sparse->ir);
                        free(matvar->data);
                        matvar->data = NULL;
                        return MATIO_E_FILE_FORMAT_VIOLATION;
                    }
                    for ( i = 0; i < sparse->nir; i++ ) {
                        if ( 0 == sparse->ir[i] ) {
                            err = MATIO_E_FILE_FORMAT_VIOLATION;
                            break;
                        }
                        sparse->ir[i] = sparse->ir[i] - 1;
                    }
                    if ( err ) {
                        free(sparse->ir);
                        free(matvar->data);
                        matvar->data = NULL;
                        return err;
                    }
                } else {
                    free(matvar->data);
                    matvar->data = NULL;
                    Mat_Critical("Couldn't allocate memory for the sparse row array");
                    return MATIO_E_OUT_OF_MEMORY;
                }
                readcount = ReadDoubleData(mat, &tmp, data_type, 1);
                if ( readcount != 1 || tmp > UINT_MAX - 1 || tmp < 0 ) {
                    free(sparse->ir);
                    free(matvar->data);
                    matvar->data = NULL;
                    Mat_Critical("Invalid row dimension for sparse matrix");
                    return MATIO_E_FILE_FORMAT_VIOLATION;
                }
                matvar->dims[0] = (size_t)tmp;

                fpos = ftello((FILE *)mat->fp);
                if ( fpos == -1L ) {
                    free(sparse->ir);
                    free(matvar->data);
                    matvar->data = NULL;
                    Mat_Critical("Couldn't determine file position");
                    return MATIO_E_FILE_FORMAT_VIOLATION;
                }
                (void)fseeko((FILE *)mat->fp, sparse->nir * Mat_SizeOf(data_type), SEEK_CUR);
                readcount = ReadDoubleData(mat, &tmp, data_type, 1);
                if ( readcount != 1 || tmp > UINT_MAX - 1 || tmp < 0 ) {
                    free(sparse->ir);
                    free(matvar->data);
                    matvar->data = NULL;
                    Mat_Critical("Invalid column dimension for sparse matrix");
                    return MATIO_E_FILE_FORMAT_VIOLATION;
                }
                matvar->dims[1] = (size_t)tmp;
                (void)fseeko((FILE *)mat->fp, fpos, SEEK_SET);
                if ( matvar->dims[1] > UINT_MAX - 1 ) {
                    free(sparse->ir);
                    free(matvar->data);
                    matvar->data = NULL;
                    Mat_Critical("Invalid column dimension for sparse matrix");
                    return MATIO_E_FILE_FORMAT_VIOLATION;
                }
                sparse->njc = (mat_uint32_t)matvar->dims[1] + 1;
                err = Mul(&readcount, sparse->njc, sizeof(mat_uint32_t));
                if ( err ) {
                    Mat_Critical("Integer multiplication overflow");
                    return err;
                }
                sparse->jc = (mat_uint32_t *)malloc(readcount);
                if ( sparse->jc != NULL ) {
                    mat_uint32_t *jc;
                    err = Mul(&readcount, sparse->nir, sizeof(mat_uint32_t));
                    if ( err ) {
                        Mat_Critical("Integer multiplication overflow");
                        return err;
                    }
                    jc = (mat_uint32_t *)malloc(readcount);
                    if ( jc != NULL ) {
                        mat_uint32_t j = 0;
                        sparse->jc[0] = 0;
                        readcount = ReadUInt32Data(mat, jc, data_type, sparse->nir);
                        if ( readcount != sparse->nir ) {
                            free(jc);
                            free(sparse->jc);
                            free(sparse->ir);
                            free(matvar->data);
                            matvar->data = NULL;
                            return MATIO_E_FILE_FORMAT_VIOLATION;
                        }
                        for ( i = 1; i < sparse->njc - 1; i++ ) {
                            while ( j < sparse->nir && jc[j] <= i )
                                j++;
                            sparse->jc[i] = j;
                        }
                        free(jc);
                        /* terminating nnz */
                        sparse->jc[sparse->njc - 1] = sparse->nir;
                    } else {
                        free(sparse->jc);
                        free(sparse->ir);
                        free(matvar->data);
                        matvar->data = NULL;
                        Mat_Critical("Couldn't allocate memory for the sparse index array");
                        return MATIO_E_OUT_OF_MEMORY;
                    }
                } else {
                    free(sparse->ir);
                    free(matvar->data);
                    matvar->data = NULL;
                    Mat_Critical("Couldn't allocate memory for the sparse index array");
                    return MATIO_E_OUT_OF_MEMORY;
                }
                readcount = ReadDoubleData(mat, &tmp, data_type, 1);
                if ( readcount != 1 ) {
                    free(sparse->jc);
                    free(sparse->ir);
                    free(matvar->data);
                    matvar->data = NULL;
                    return MATIO_E_FILE_FORMAT_VIOLATION;
                }
                sparse->ndata = sparse->nir;
                data_type = matvar->data_type;
                if ( matvar->isComplex ) {
                    mat_complex_split_t *complex_data =
                        ComplexMalloc(sparse->ndata * Mat_SizeOf(data_type));
                    if ( NULL != complex_data ) {
                        sparse->data = complex_data;
#if defined(EXTENDED_SPARSE)
                        switch ( data_type ) {
                            case MAT_T_DOUBLE:
                                readcount = ReadDoubleData(mat, (double *)complex_data->Re,
                                                           data_type, sparse->ndata);
                                err = readcount != sparse->ndata;
                                readcount = ReadDoubleData(mat, &tmp, data_type, 1);
                                err |= readcount != 1;
                                readcount = ReadDoubleData(mat, (double *)complex_data->Im,
                                                           data_type, sparse->ndata);
                                err |= readcount != sparse->ndata;
                                readcount = ReadDoubleData(mat, &tmp, data_type, 1);
                                err |= readcount != 1;
                                break;
                            case MAT_T_SINGLE: {
                                float tmp2;
                                readcount = ReadSingleData(mat, (float *)complex_data->Re,
                                                           data_type, sparse->ndata);
                                err = readcount != sparse->ndata;
                                readcount = ReadSingleData(mat, &tmp2, data_type, 1);
                                err |= readcount != 1;
                                readcount = ReadSingleData(mat, (float *)complex_data->Im,
                                                           data_type, sparse->ndata);
                                err |= readcount != sparse->ndata;
                                readcount = ReadSingleData(mat, &tmp2, data_type, 1);
                                err |= readcount != 1;
                                break;
                            }
                            case MAT_T_INT32: {
                                mat_int32_t tmp2;
                                readcount = ReadInt32Data(mat, (mat_int32_t *)complex_data->Re,
                                                          data_type, sparse->ndata);
                                err = readcount != sparse->ndata;
                                readcount = ReadInt32Data(mat, &tmp2, data_type, 1);
                                err |= readcount != 1;
                                readcount = ReadInt32Data(mat, (mat_int32_t *)complex_data->Im,
                                                          data_type, sparse->ndata);
                                err |= readcount != sparse->ndata;
                                readcount = ReadInt32Data(mat, &tmp2, data_type, 1);
                                err |= readcount != 1;
                                break;
                            }
                            case MAT_T_INT16: {
                                mat_int16_t tmp2;
                                readcount = ReadInt16Data(mat, (mat_int16_t *)complex_data->Re,
                                                          data_type, sparse->ndata);
                                err = readcount != sparse->ndata;
                                readcount = ReadInt16Data(mat, &tmp2, data_type, 1);
                                err |= readcount != 1;
                                readcount = ReadInt16Data(mat, (mat_int16_t *)complex_data->Im,
                                                          data_type, sparse->ndata);
                                err |= readcount != sparse->ndata;
                                readcount = ReadInt16Data(mat, &tmp2, data_type, 1);
                                err |= readcount != 1;
                                break;
                            }
                            case MAT_T_UINT16: {
                                mat_uint16_t tmp2;
                                readcount = ReadUInt16Data(mat, (mat_uint16_t *)complex_data->Re,
                                                           data_type, sparse->ndata);
                                err = readcount != sparse->ndata;
                                readcount = ReadUInt16Data(mat, &tmp2, data_type, 1);
                                err |= readcount != 1;
                                readcount = ReadUInt16Data(mat, (mat_uint16_t *)complex_data->Im,
                                                           data_type, sparse->ndata);
                                err |= readcount != sparse->ndata;
                                readcount = ReadUInt16Data(mat, &tmp2, data_type, 1);
                                err |= readcount != 1;
                                break;
                            }
                            case MAT_T_UINT8: {
                                mat_uint8_t tmp2;
                                readcount = ReadUInt8Data(mat, (mat_uint8_t *)complex_data->Re,
                                                          data_type, sparse->ndata);
                                err = readcount != sparse->ndata;
                                readcount = ReadUInt8Data(mat, &tmp2, data_type, 1);
                                err |= readcount != 1;
                                readcount = ReadUInt8Data(mat, (mat_uint8_t *)complex_data->Im,
                                                          data_type, sparse->ndata);
                                err |= readcount != sparse->ndata;
                                readcount = ReadUInt8Data(mat, &tmp2, data_type, 1);
                                err |= readcount != 1;
                                break;
                            }
                            default:
                                ComplexFree(complex_data);
                                free(sparse->jc);
                                free(sparse->ir);
                                free(matvar->data);
                                matvar->data = NULL;
                                Mat_Critical(
                                    "Mat_VarRead4: %d is not a supported data type for "
                                    "extended sparse",
                                    data_type);
                                return MATIO_E_FILE_FORMAT_VIOLATION;
                        }
#else
                        readcount = ReadDoubleData(mat, (double *)complex_data->Re, data_type,
                                                   sparse->ndata);
                        err = readcount != sparse->ndata;
                        readcount = ReadDoubleData(mat, &tmp, data_type, 1);
                        err |= readcount != 1;
                        readcount = ReadDoubleData(mat, (double *)complex_data->Im, data_type,
                                                   sparse->ndata);
                        err |= readcount != sparse->ndata;
                        readcount = ReadDoubleData(mat, &tmp, data_type, 1);
                        err |= readcount != 1;
#endif
                        if ( err ) {
                            ComplexFree(complex_data);
                            free(sparse->jc);
                            free(sparse->ir);
                            free(matvar->data);
                            matvar->data = NULL;
                            return MATIO_E_FILE_FORMAT_VIOLATION;
                        }
                    } else {
                        free(sparse->jc);
                        free(sparse->ir);
                        free(matvar->data);
                        matvar->data = NULL;
                        Mat_Critical("Couldn't allocate memory for the complex sparse data");
                        return MATIO_E_OUT_OF_MEMORY;
                    }
                } else {
                    sparse->data = malloc(sparse->ndata * Mat_SizeOf(data_type));
                    if ( sparse->data != NULL ) {
#if defined(EXTENDED_SPARSE)
                        switch ( data_type ) {
                            case MAT_T_DOUBLE:
                                readcount = ReadDoubleData(mat, (double *)sparse->data, data_type,
                                                           sparse->ndata);
                                err = readcount != sparse->ndata;
                                readcount = ReadDoubleData(mat, &tmp, data_type, 1);
                                err |= readcount != 1;
                                break;
                            case MAT_T_SINGLE: {
                                float tmp2;
                                readcount = ReadSingleData(mat, (float *)sparse->data, data_type,
                                                           sparse->ndata);
                                err = readcount != sparse->ndata;
                                readcount = ReadSingleData(mat, &tmp2, data_type, 1);
                                err |= readcount != 1;
                                break;
                            }
                            case MAT_T_INT32: {
                                mat_int32_t tmp2;
                                readcount = ReadInt32Data(mat, (mat_int32_t *)sparse->data,
                                                          data_type, sparse->ndata);
                                err = readcount != sparse->ndata;
                                readcount = ReadInt32Data(mat, &tmp2, data_type, 1);
                                err |= readcount != 1;
                                break;
                            }
                            case MAT_T_INT16: {
                                mat_int16_t tmp2;
                                readcount = ReadInt16Data(mat, (mat_int16_t *)sparse->data,
                                                          data_type, sparse->ndata);
                                err = readcount != sparse->ndata;
                                readcount = ReadInt16Data(mat, &tmp2, data_type, 1);
                                err |= readcount != 1;
                                break;
                            }
                            case MAT_T_UINT16: {
                                mat_uint16_t tmp2;
                                readcount = ReadUInt16Data(mat, (mat_uint16_t *)sparse->data,
                                                           data_type, sparse->ndata);
                                err = readcount != sparse->ndata;
                                readcount = ReadUInt16Data(mat, &tmp2, data_type, 1);
                                err |= readcount != 1;
                                break;
                            }
                            case MAT_T_UINT8: {
                                mat_uint8_t tmp2;
                                readcount = ReadUInt8Data(mat, (mat_uint8_t *)sparse->data,
                                                          data_type, sparse->ndata);
                                err = readcount != sparse->ndata;
                                readcount = ReadUInt8Data(mat, &tmp2, data_type, 1);
                                err |= readcount != 1;
                                break;
                            }
                            default:
                                free(sparse->data);
                                free(sparse->jc);
                                free(sparse->ir);
                                free(matvar->data);
                                matvar->data = NULL;
                                Mat_Critical(
                                    "Mat_VarRead4: %d is not a supported data type for "
                                    "extended sparse",
                                    data_type);
                                return MATIO_E_FILE_FORMAT_VIOLATION;
                        }
#else
                        readcount =
                            ReadDoubleData(mat, (double *)sparse->data, data_type, sparse->ndata);
                        err = readcount != sparse->ndata;
                        readcount = ReadDoubleData(mat, &tmp, data_type, 1);
                        err |= readcount != 1;
#endif
                        if ( err ) {
                            free(sparse->data);
                            free(sparse->jc);
                            free(sparse->ir);
                            free(matvar->data);
                            matvar->data = NULL;
                            return MATIO_E_FILE_FORMAT_VIOLATION;
                        }
                    } else {
                        free(sparse->jc);
                        free(sparse->ir);
                        free(matvar->data);
                        matvar->data = NULL;
                        Mat_Critical("Couldn't allocate memory for the sparse data");
                        return MATIO_E_OUT_OF_MEMORY;
                    }
                }
                break;
            } else {
                Mat_Critical("Couldn't allocate memory for the data");
                return MATIO_E_OUT_OF_MEMORY;
            }
        default:
            Mat_Critical("MAT V4 data type error");
            return MATIO_E_FILE_FORMAT_VIOLATION;
    }

    return MATIO_E_NO_ERROR;
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
Mat_VarReadData4(mat_t *mat, matvar_t *matvar, void *data, int *start, int *stride, int *edge)
{
    int err = MATIO_E_NO_ERROR;

    (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);

    switch ( matvar->data_type ) {
        case MAT_T_DOUBLE:
        case MAT_T_SINGLE:
        case MAT_T_INT32:
        case MAT_T_INT16:
        case MAT_T_UINT16:
        case MAT_T_UINT8:
            break;
        default:
            return MATIO_E_FILE_FORMAT_VIOLATION;
    }

    if ( matvar->rank == 2 ) {
        if ( (size_t)stride[0] * (edge[0] - 1) + start[0] + 1 > matvar->dims[0] )
            err = MATIO_E_BAD_ARGUMENT;
        else if ( (size_t)stride[1] * (edge[1] - 1) + start[1] + 1 > matvar->dims[1] )
            err = MATIO_E_BAD_ARGUMENT;
        if ( matvar->isComplex ) {
            mat_complex_split_t *cdata = (mat_complex_split_t *)data;
            size_t nbytes = Mat_SizeOf(matvar->data_type);
            err = Mat_MulDims(matvar, &nbytes);
            if ( err ) {
                Mat_Critical("Integer multiplication overflow");
                return err;
            }

            ReadDataSlab2(mat, cdata->Re, matvar->class_type, matvar->data_type, matvar->dims,
                          start, stride, edge);
            (void)fseeko((FILE *)mat->fp, matvar->internal->datapos + nbytes, SEEK_SET);
            ReadDataSlab2(mat, cdata->Im, matvar->class_type, matvar->data_type, matvar->dims,
                          start, stride, edge);
        } else {
            ReadDataSlab2(mat, data, matvar->class_type, matvar->data_type, matvar->dims, start,
                          stride, edge);
        }
    } else if ( matvar->isComplex ) {
        mat_complex_split_t *cdata = (mat_complex_split_t *)data;
        size_t nbytes = Mat_SizeOf(matvar->data_type);
        err = Mat_MulDims(matvar, &nbytes);
        if ( err ) {
            Mat_Critical("Integer multiplication overflow");
            return err;
        }

        ReadDataSlabN(mat, cdata->Re, matvar->class_type, matvar->data_type, matvar->rank,
                      matvar->dims, start, stride, edge);
        (void)fseeko((FILE *)mat->fp, matvar->internal->datapos + nbytes, SEEK_SET);
        ReadDataSlabN(mat, cdata->Im, matvar->class_type, matvar->data_type, matvar->rank,
                      matvar->dims, start, stride, edge);
    } else {
        ReadDataSlabN(mat, data, matvar->class_type, matvar->data_type, matvar->rank, matvar->dims,
                      start, stride, edge);
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
Mat_VarReadDataLinear4(mat_t *mat, matvar_t *matvar, void *data, int start, int stride, int edge)
{
    int err;
    size_t nelems = 1;

    err = Mat_MulDims(matvar, &nelems);
    if ( err ) {
        Mat_Critical("Integer multiplication overflow");
        return err;
    }

    (void)fseeko((FILE *)mat->fp, matvar->internal->datapos, SEEK_SET);

    matvar->data_size = (int)Mat_SizeOf(matvar->data_type);

    if ( (size_t)stride * (edge - 1) + start + 1 > nelems ) {
        return MATIO_E_BAD_ARGUMENT;
    }
    if ( matvar->isComplex ) {
        mat_complex_split_t *complex_data = (mat_complex_split_t *)data;
        err = Mul(&nelems, nelems, matvar->data_size);
        if ( err ) {
            Mat_Critical("Integer multiplication overflow");
            return err;
        }

        ReadDataSlab1(mat, complex_data->Re, matvar->class_type, matvar->data_type, start, stride,
                      edge);
        (void)fseeko((FILE *)mat->fp, matvar->internal->datapos + nelems, SEEK_SET);
        ReadDataSlab1(mat, complex_data->Im, matvar->class_type, matvar->data_type, start, stride,
                      edge);
    } else {
        ReadDataSlab1(mat, data, matvar->class_type, matvar->data_type, start, stride, edge);
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
    int M, O, data_type, class_type;
    mat_int32_t tmp;
    mat_off_t nBytes, fpos;
    matvar_t *matvar = NULL;
    union {
        mat_uint32_t u;
        mat_uint8_t c[4];
    } endian;

    if ( mat == NULL || mat->fp == NULL )
        return NULL;

    if ( IsEndOfFile((FILE *)mat->fp, &fpos) )
        return NULL;

    if ( fpos == -1L )
        return NULL;

    {
        size_t nbytes = 0;
        int err = Read(&tmp, sizeof(mat_int32_t), 1, (FILE *)mat->fp, &nbytes);
        if ( err || 0 == nbytes )
            return NULL;
    }

    endian.u = 0x01020304;

    /* See if MOPT may need byteswapping */
    if ( tmp < 0 || tmp > 4052 ) {
        if ( Mat_int32Swap(&tmp) > 4052 ) {
            return NULL;
        }
    }

    M = (int)floor(tmp / 1000.0);
    switch ( M ) {
        case 0:
            /* IEEE little endian */
            mat->byteswap = endian.c[0] != 4;
            break;
        case 1:
            /* IEEE big endian */
            mat->byteswap = endian.c[0] != 1;
            break;
        default:
            /* VAX, Cray, or bogus */
            return NULL;
    }

    tmp -= M * 1000;
    O = (int)floor(tmp / 100.0);
    /* O must be zero */
    if ( 0 != O ) {
        return NULL;
    }

    if ( NULL == (matvar = Mat_VarCalloc()) )
        return NULL;

    tmp -= O * 100;
    data_type = (int)floor(tmp / 10.0);
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
            return NULL;
    }

    tmp -= data_type * 10;
    class_type = (int)floor(tmp / 1.0);
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
            return NULL;
    }

    matvar->rank = 2;
    matvar->dims = (size_t *)calloc(2, sizeof(*matvar->dims));
    if ( NULL == matvar->dims ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    if ( 0 != Read(&tmp, sizeof(int), 1, (FILE *)mat->fp, NULL) ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    if ( mat->byteswap )
        Mat_int32Swap(&tmp);
    matvar->dims[0] = tmp;

    if ( 0 != Read(&tmp, sizeof(int), 1, (FILE *)mat->fp, NULL) ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    if ( mat->byteswap )
        Mat_int32Swap(&tmp);
    matvar->dims[1] = tmp;

    if ( 0 != Read(&(matvar->isComplex), sizeof(int), 1, (FILE *)mat->fp, NULL) ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    if ( matvar->isComplex && MAT_C_CHAR == matvar->class_type ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    if ( 0 != Read(&tmp, sizeof(int), 1, (FILE *)mat->fp, NULL) ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    if ( mat->byteswap )
        Mat_int32Swap(&tmp);
    /* Check that the length of the variable name is at least 1 */
    if ( tmp < 1 ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    matvar->name = (char *)malloc(tmp);
    if ( NULL == matvar->name ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    if ( 0 != Read(matvar->name, sizeof(char), tmp, (FILE *)mat->fp, NULL) ) {
        Mat_VarFree(matvar);
        return NULL;
    } else {
        matvar->name[tmp - 1] = '\0';
    }

    matvar->internal->datapos = ftello((FILE *)mat->fp);
    if ( matvar->internal->datapos == -1L ) {
        Mat_VarFree(matvar);
        Mat_Critical("Couldn't determine file position");
        return NULL;
    }
    {
        int err;
        size_t tmp2 = Mat_SizeOf(matvar->data_type);
        if ( matvar->isComplex )
            tmp2 *= 2;
        err = Mat_MulDims(matvar, &tmp2);
        if ( err ) {
            Mat_VarFree(matvar);
            Mat_Critical("Integer multiplication overflow");
            return NULL;
        }

        nBytes = (mat_off_t)tmp2;
    }
    (void)fseeko((FILE *)mat->fp, nBytes, SEEK_CUR);

    return matvar;
}
