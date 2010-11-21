/** @file mat4.c
 * Matlab MAT version 4 file functions
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
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "matio_private.h"
#include "mat4.h"

static const char *v4_class_type_desc[16] = {
       "Undefined","Cell Array","Structure",
       "Object","Character Array","Sparse Array","Double Precision Array",
       "Single Precision Array", "8-bit, signed Integer Array",
       "8-bit, Unsigned Integer Array","16-bit, signed Integer Array",
       "16-bit, unsigned Integer Array","32-bit, signed Integer Array",
       "32-bit, unsigned Integer Array","Matlab Array","Compressed Data"
};
static const char *v4_data_type_desc[23] = {
       "Unknown","8-bit, signed integer",
       "8-bit, unsigned integer","16-bit, signed integer",
       "16-bit, unsigned integer","32-bit, signed integer",
       "32-bit, unsigned integer","IEEE 754 single-precision","RESERVED",
       "IEEE 754 double-precision","RESERVED","RESERVED",
       "64-bit, signed integer","64-bit, unsigned integer", "Matlab Array",
       "Compressed Data","Unicode UTF-8 Encoded Character Data",
       "Unicode UTF-16 Encoded Character Data",
       "Unicode UTF-32 Encoded Character Data","","String","Cell Array",
       "Structure"
};

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
    if ( fseek(mat->fp,matvar->internal->datapos,SEEK_SET) )
        return;

    N = matvar->dims[0]*matvar->dims[1];
    switch ( matvar->data_type ) {
        case MAT_T_DOUBLE:
            matvar->data_size = sizeof(double);
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data;

                matvar->nbytes   = N*sizeof(double);
                complex_data     = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                matvar->data     = complex_data;
                if ( complex_data != NULL &&
                     complex_data->Re != NULL && complex_data->Im != NULL ) {
                    fread(complex_data->Re,N,sizeof(double),mat->fp);
                    fread(complex_data->Im,N,sizeof(double),mat->fp);
                }
            } else {
                matvar->nbytes = N*sizeof(double);
                matvar->data   = malloc(matvar->nbytes);
                if ( matvar->data != NULL )
                    fread(matvar->data,N,sizeof(double),mat->fp);
            }
            break;
        case MAT_T_SINGLE:
            matvar->data_size = sizeof(float);
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data;

                matvar->nbytes   = N*sizeof(float);
                complex_data     = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                matvar->data     = complex_data;
                if ( complex_data != NULL &&
                     complex_data->Re != NULL && complex_data->Im != NULL ) {
                    fread(complex_data->Re,N,sizeof(float),mat->fp);
                    fread(complex_data->Im,N,sizeof(float),mat->fp);
                }
            } else {
                matvar->nbytes = N*sizeof(float);
                matvar->data   = malloc(matvar->nbytes);
                if ( matvar->data != NULL )
                    fread(matvar->data,N,sizeof(float),mat->fp);
            }
            break;
        case MAT_T_INT32:
            matvar->data_size = sizeof(mat_int32_t);
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data;

                matvar->nbytes   = N*sizeof(mat_int32_t);
                complex_data     = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                matvar->data     = complex_data;
                if ( complex_data != NULL &&
                     complex_data->Re != NULL && complex_data->Im != NULL ) {
                    fread(complex_data->Re,N,sizeof(mat_int32_t),mat->fp);
                    fread(complex_data->Im,N,sizeof(mat_int32_t),mat->fp);
                }
            } else {
                matvar->nbytes = N*sizeof(mat_int32_t);
                matvar->data   = malloc(matvar->nbytes);
                if ( matvar->data != NULL )
                    fread(matvar->data,N,sizeof(mat_int32_t),mat->fp);
            }
            break;
        case MAT_T_INT16:
            matvar->data_size = sizeof(mat_int16_t);
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data;

                matvar->nbytes   = N*sizeof(mat_int16_t);
                complex_data     = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                matvar->data     = complex_data;
                if ( complex_data != NULL &&
                     complex_data->Re != NULL && complex_data->Im != NULL ) {
                    fread(complex_data->Re,N,sizeof(mat_int16_t),mat->fp);
                    fread(complex_data->Im,N,sizeof(mat_int16_t),mat->fp);
                }
            } else {
                matvar->nbytes = N*sizeof(mat_int16_t);
                matvar->data   = malloc(matvar->nbytes);
                if ( matvar->data != NULL )
                    fread(matvar->data,N,sizeof(mat_int16_t),mat->fp);
            }
            break;
        case MAT_T_UINT16:
            matvar->data_size = sizeof(mat_uint16_t);
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data;

                matvar->nbytes   = N*sizeof(mat_uint16_t);
                complex_data     = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                matvar->data     = complex_data;
                if ( complex_data != NULL &&
                     complex_data->Re != NULL && complex_data->Im != NULL ) {
                    fread(complex_data->Re,N,sizeof(mat_uint16_t),mat->fp);
                    fread(complex_data->Im,N,sizeof(mat_uint16_t),mat->fp);
                }
            } else {
                matvar->nbytes = N*sizeof(mat_uint16_t);
                matvar->data   = malloc(matvar->nbytes);
                if ( matvar->data != NULL )
                    fread(matvar->data,N,sizeof(mat_uint16_t),mat->fp);
            }
            break;
        case MAT_T_UINT8:
            matvar->data_size = sizeof(mat_uint8_t);
            if ( matvar->isComplex ) {
                struct ComplexSplit *complex_data;

                matvar->nbytes   = N*sizeof(mat_uint8_t);
                complex_data     = malloc(sizeof(*complex_data));
                complex_data->Re = malloc(matvar->nbytes);
                complex_data->Im = malloc(matvar->nbytes);
                matvar->data     = complex_data;
                if ( complex_data != NULL &&
                     complex_data->Re != NULL && complex_data->Im != NULL ) {
                    fread(complex_data->Re,N,sizeof(mat_uint8_t),mat->fp);
                    fread(complex_data->Im,N,sizeof(mat_uint8_t),mat->fp);
                }
            } else {
                matvar->nbytes = N*sizeof(mat_uint8_t);
                matvar->data   = malloc(matvar->nbytes);
                if ( matvar->data != NULL )
                    fread(matvar->data,N,sizeof(mat_uint8_t),mat->fp);
            }
            break;
        default:
            Mat_Critical("MAT V4 data type error");
            return;
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

    fseek(mat->fp,matvar->internal->datapos,SEEK_SET);

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
    }

    if ( matvar->rank == 2 ) {
        if ( stride[0]*(edge[0]-1)+start[0]+1 > matvar->dims[0] )
            err = 1;
        else if ( stride[1]*(edge[1]-1)+start[1]+1 > matvar->dims[1] )
            err = 1;
        if ( matvar->isComplex ) {
            struct ComplexSplit *cdata = data;
            long nbytes = edge[0]*edge[1]*Mat_SizeOf(matvar->data_type);

            ReadDataSlab2(mat,cdata->Re,class_type,matvar->data_type,
                    matvar->dims,start,stride,edge);
            fseek(mat->fp,matvar->internal->datapos+nbytes,SEEK_SET);
            ReadDataSlab2(mat,cdata->Im,class_type,
                matvar->data_type,matvar->dims,start,stride,edge);
        } else {
            ReadDataSlab2(mat,data,class_type,matvar->data_type,
                    matvar->dims,start,stride,edge);
        }
    } else {
        if ( matvar->isComplex ) {
            int i;
            struct ComplexSplit *cdata = data;
            long nbytes = Mat_SizeOf(matvar->data_type);

            for ( i = 0; i < matvar->rank; i++ )
                nbytes *= edge[i];

            ReadDataSlabN(mat,cdata->Re,class_type,matvar->data_type,
                matvar->rank,matvar->dims,start,stride,edge);
            fseek(mat->fp,matvar->internal->datapos+nbytes,SEEK_SET);
            ReadDataSlab2(mat,cdata->Im,class_type,
                matvar->data_type,matvar->dims,start,stride,edge);
        } else {
            ReadDataSlabN(mat,data,class_type,matvar->data_type,
                matvar->rank,matvar->dims,start,stride,edge);
        }
    }
    return err;
}

/** @if mat_devman
 * @brief Reads the header information for the next MAT variable in a version 4 MAT file
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @retuen pointer to the MAT variable or NULL
 * @endif
 */
matvar_t *
Mat_VarReadNextInfo4(mat_t *mat)
{       
    int       tmp,M,O,data_type,class_type;
    long      nBytes;
    size_t    err;
    matvar_t *matvar = NULL;

    if ( mat == NULL || mat->fp == NULL )
        return NULL;
    else if ( NULL == (matvar = Mat_VarCalloc()) )
        return NULL;

    matvar->internal->fp   = mat;
    matvar->internal->fpos = ftell(mat->fp);

    err = fread(&tmp,sizeof(int),1,mat->fp);
    if ( !err ) {
        free(matvar);
        return NULL;
    }
    M = floor(tmp / 1000.0);
    tmp -= M*1000;
    O = floor(tmp / 100.0);
    tmp -= O*100;
    data_type = floor(tmp / 10.0);
    tmp -= matvar->data_type*10;
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
            matvar->data_type = MAT_T_UNKNOWN;
            break;
    }
    class_type = floor(tmp);
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
    }
    matvar->rank = 2;
    /* FIXME: Check allocation */
    matvar->dims = malloc(2*sizeof(*matvar->dims));
    err = fread(&tmp,sizeof(int),1,mat->fp);
    matvar->dims[0] = tmp;
    if ( !err ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    err = fread(&tmp,sizeof(int),1,mat->fp);
    matvar->dims[1] = tmp;
    if ( !err ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    err = fread(&(matvar->isComplex),sizeof(int),1,mat->fp);
    if ( !err ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    err = fread(&tmp,sizeof(int),1,mat->fp);
    if ( !err ) {
        Mat_VarFree(matvar);
        return NULL;
    }
    /* FIXME: Check allocation */
    matvar->name = malloc(tmp);
    err = fread(matvar->name,1,tmp,mat->fp);
    if ( !err ) {
        Mat_VarFree(matvar);
        return NULL;
    }

    matvar->internal->datapos = ftell(mat->fp);
    nBytes = matvar->dims[0]*matvar->dims[1]*Mat_SizeOf(matvar->data_type);
    if ( matvar->isComplex )
        nBytes *= 2;
    fseek(mat->fp,nBytes,SEEK_CUR);
                
    return matvar;  
}

/** @if mat_devman
 * @brief Prints the variable from a MAT version 4 file
 *
 * @ingroup mat_internal
 * @param mat MAT file pointer
 * @param matvar pointer to the mat variable
 * @endif
 */
void
Mat_VarPrint4(matvar_t *matvar,int printdata)
{
    int i, j;

    if ( matvar == NULL )
        return;
    if ( matvar->name )
        Mat_Message("      Name: %s", matvar->name);
    Mat_Message("      Rank: %d", matvar->rank);
    if ( matvar->rank == 0 )
        return;
    if ( matvar->isComplex )
        Mat_Message("Class Type: %s (complex)",v4_class_type_desc[matvar->class_type]);
    else
        Mat_Message("Class Type: %s",v4_class_type_desc[matvar->class_type]);
    if ( matvar->data_type )
        Mat_Message(" Data Type: %s",v4_data_type_desc[matvar->data_type]);
    if ( matvar->data != NULL && matvar->data_size > 0 ) {
        switch( matvar->class_type ) {
            case MAT_C_DOUBLE:
                if ( !printdata )
                    break;
                if ( matvar->rank > 2 ) {
                    printf("I can't print more than 2 dimensions\n");
                } else if ( matvar->rank == 1 && matvar->dims[0] > 15 ) {
                    printf("I won't print more than 15 elements in a vector\n");
                } else if (matvar->rank == 2 &&
                           (matvar->dims[0] > 15 || matvar->dims[1] > 15) && 
                           matvar->isComplex ) {
                    int N;
                    struct ComplexSplit *cdata = matvar->data;
                    double *re = cdata->Re,*im = cdata->Im;
                    N = matvar->dims[0]*matvar->dims[1];
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%f + %fi ",re[matvar->dims[0]*j+i],
                                               im[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if (matvar->rank == 2 &&
                           (matvar->dims[0] > 15 || matvar->dims[1] > 15) ) {
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%f ", ((double*)matvar->data)[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if ( matvar->rank == 2 && matvar->isComplex) {
                    int N;
                    struct ComplexSplit *cdata = matvar->data;
                    double *re = cdata->Re,*im = cdata->Im;
                    N = matvar->dims[0]*matvar->dims[1];
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%f + %fi ",re[matvar->dims[0]*j+i],
                                               im[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                } else if ( matvar->rank == 2 ) {
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%f ", ((double*)matvar->data)[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                }
                break;
            case MAT_C_SINGLE:
                if ( !printdata )
                    break;
                if ( matvar->rank > 2 ) {
                    printf("I can't print more than 2 dimensions\n");
                } else if ( matvar->rank == 1 && matvar->dims[0] > 15 ) {
                    printf("I won't print more than 15 elements in a vector\n");
                } else if (matvar->rank == 2 &&
                           (matvar->dims[0] > 15 || matvar->dims[1] > 15) && 
                           matvar->isComplex ) {
                    int N;
                    struct ComplexSplit *cdata = matvar->data;
                    float *re = cdata->Re,*im = cdata->Im;
                    N = matvar->dims[0]*matvar->dims[1];
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%f + %fi",
                                re[matvar->dims[0]*j+i],
                                im[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if (matvar->rank == 2 &&
                           (matvar->dims[0] > 15 || matvar->dims[1] > 15) ) {
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%f ", ((float*)matvar->data)[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if ( matvar->rank == 2 && matvar->isComplex) {
                    int N;
                    struct ComplexSplit *cdata = matvar->data;
                    float *re = cdata->Re,*im = cdata->Im;
                    N = matvar->dims[0]*matvar->dims[1];
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%f + %fi ",re[matvar->dims[0]*j+i],
                                               im[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                } else if ( matvar->rank == 2 ) {
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%f ", ((float*)matvar->data)[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                }
                break;
            case MAT_C_INT32:
            {
                if ( !printdata )
                    break;
                if ( matvar->rank > 2 ) {
                    printf("I can't print more than 2 dimensions\n");
                } else if ( matvar->rank == 1 && matvar->dims[0] > 15 ) {
                    printf("I won't print more than 15 elements in a vector\n");
                } else if (matvar->rank == 2 &&
                           (matvar->dims[0] > 15 || matvar->dims[1] > 15) && 
                           matvar->isComplex ) {
                    int N;
                    struct ComplexSplit *cdata = matvar->data;
                    mat_int32_t *re = cdata->Re,*im = cdata->Im;
                    N = matvar->dims[0]*matvar->dims[1];
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%d + %di ",re[matvar->dims[0]*j+i],
                                               im[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if (matvar->rank == 2 &&
                           (matvar->dims[0] > 15 || matvar->dims[1] > 15) ) {
                    mat_int32_t *data = matvar->data;
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%d ",data[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if ( matvar->rank == 2 && matvar->isComplex) {
                    int N;
                    struct ComplexSplit *cdata = matvar->data;
                    mat_int32_t *re = cdata->Re,*im = cdata->Im;
                    N = matvar->dims[0]*matvar->dims[1];
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%d + %di ",re[matvar->dims[0]*j+i],
                                               im[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                } else if ( matvar->rank == 2 ) {
                    mat_int32_t *data = matvar->data;
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%d ", data[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                }
                break;
            }
            case MAT_C_INT16:
            {
                if ( !printdata )
                    break;
                if ( matvar->rank > 2 ) {
                    printf("I can't print more than 2 dimensions\n");
                } else if ( matvar->rank == 1 && matvar->dims[0] > 15 ) {
                    printf("I won't print more than 15 elements in a vector\n");
                } else if (matvar->rank == 2 &&
                           (matvar->dims[0] > 15 || matvar->dims[1] > 15) && 
                           matvar->isComplex ) {
                    int N;
                    struct ComplexSplit *cdata = matvar->data;
                    mat_int16_t *re = cdata->Re,*im = cdata->Im;
                    N = matvar->dims[0]*matvar->dims[1];
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%hd + %hdi ",re[matvar->dims[0]*j+i],
                                   im[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if (matvar->rank == 2 &&
                           (matvar->dims[0] > 15 || matvar->dims[1] > 15) ) {
                    mat_int16_t *data = matvar->data;
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%hd ",data[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if ( matvar->rank == 2 && matvar->isComplex) {
                    int N;
                    struct ComplexSplit *cdata = matvar->data;
                    mat_int16_t *re = cdata->Re,*im = cdata->Im;
                    N = matvar->dims[0]*matvar->dims[1];
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%hd + %hdi ",re[matvar->dims[0]*j+i],
                                im[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                } else if ( matvar->rank == 2 ) {
                    mat_int16_t *data = matvar->data;
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%hd ", data[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                }
                break;
            }
            case MAT_C_UINT16:
            {
                if ( !printdata )
                    break;
                if ( matvar->rank > 2 ) {
                    printf("I can't print more than 2 dimensions\n");
                } else if ( matvar->rank == 1 && matvar->dims[0] > 15 ) {
                    printf("I won't print more than 15 elements in a vector\n");
                } else if (matvar->rank == 2 &&
                           (matvar->dims[0] > 15 || matvar->dims[1] > 15) && 
                           matvar->isComplex ) {
                    int N;
                    struct ComplexSplit *cdata = matvar->data;
                    mat_uint16_t *re = cdata->Re,*im = cdata->Im;
                    N = matvar->dims[0]*matvar->dims[1];
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%hu + %hui ",re[matvar->dims[0]*j+i],
                                                 im[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if (matvar->rank == 2 &&
                           (matvar->dims[0] > 15 || matvar->dims[1] > 15) ) {
                    mat_int16_t *data = matvar->data;
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%hu ",data[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if ( matvar->rank == 2 && matvar->isComplex) {
                    int N;
                    struct ComplexSplit *cdata = matvar->data;
                    mat_uint16_t *re = cdata->Re,*im = cdata->Im;
                    N = matvar->dims[0]*matvar->dims[1];
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%hu + %hui ",re[matvar->dims[0]*j+i],
                                                 im[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                } else if ( matvar->rank == 2 ) {
                    mat_int16_t *data = matvar->data;
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%hu ", data[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                }
                break;
            }
            case MAT_C_UINT8:
            {
                if ( !printdata )
                    break;
                if ( matvar->rank > 2 ) {
                    printf("I can't print more than 2 dimensions\n");
                } else if ( matvar->rank == 1 && matvar->dims[0] > 15 ) {
                    printf("I won't print more than 15 elements in a vector\n");
                } else if (matvar->rank == 2 &&
                           (matvar->dims[0] > 15 || matvar->dims[1] > 15) && 
                           matvar->isComplex ) {
                    int N;
                    struct ComplexSplit *cdata = matvar->data;
                    mat_int8_t *re = cdata->Re,*im = cdata->Im;
                    N = matvar->dims[0]*matvar->dims[1];
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%hu + %hui ",re[matvar->dims[0]*j+i],
                                                 im[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if (matvar->rank == 2 &&
                           (matvar->dims[0] > 15 || matvar->dims[1] > 15) ) {
                    mat_int8_t *data = matvar->data;
                    for ( i = 0; i < matvar->dims[0] && i < 15; i++ ) {
                        for ( j = 0; j < matvar->dims[1] && j < 15; j++ )
                            printf("%hu ",(mat_uint16_t)data[matvar->dims[0]*j+i]);
                        if ( j < matvar->dims[1] )
                            printf("...");
                        printf("\n");
                    }
                    if ( i < matvar->dims[0] )
                        printf(".\n.\n.\n");
                } else if ( matvar->rank == 2 && matvar->isComplex) {
                    int N;
                    struct ComplexSplit *cdata = matvar->data;
                    mat_int8_t *re = cdata->Re,*im = cdata->Im;
                    N = matvar->dims[0]*matvar->dims[1];
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%hu + %hui ",re[matvar->dims[0]*j+i],
                                                 im[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                } else if ( matvar->rank == 2 ) {
                    mat_int8_t *data = matvar->data;
                    for ( i = 0; i < matvar->dims[0]; i++ ) {
                        for ( j = 0; j < matvar->dims[1]; j++ )
                            printf("%hu ",(mat_uint16_t)data[matvar->dims[0]*j+i]);
                        printf("\n");
                    }
                }
                break;
            }
        }
    }
    return;
}
