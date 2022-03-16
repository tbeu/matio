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

#include "matio.h"
#include <getopt.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#if defined(_MSC_VER) || defined(__MINGW32__)
#define SIZE_T_FMTSTR "Iu"
#else
#define SIZE_T_FMTSTR "zu"
#endif

static const char *optstring = "df:hvHV";
static struct option options[] = {{"data", no_argument, NULL, 'd'},
                                  {"format", required_argument, NULL, 'f'},
                                  {"human", no_argument, NULL, 'h'},
                                  {"verbose", optional_argument, NULL, 'v'},
                                  {"help", no_argument, NULL, 'H'},
                                  {"version", no_argument, NULL, 'V'},
                                  {NULL, 0, NULL, 0}};

static const char *helpstr[] = {"",
                                "Usage: matdump [OPTIONS] mat_file [var1 var2 ...]",
                                "",
                                "Dumps the variables of the MAT file using libmatio",
                                "",
                                "OPTIONS",
                                "-d,--data         Print data with header information",
                                "-f,--format whos  Turn on 'whos' display mode",
                                "-h,--human        Human readable sizes in 'whos' display mode",
                                "-v,--verbose      Turn on verbose messages",
                                "-H,--help         This output",
                                "-V,--version      version information",
                                "",
                                "mat_file          name of the MAT file to dump",
                                "var1 var2 ...     If specified, dumps only listed variables",
                                "",
                                "Report bugs to <t-beu@users.sourceforge.net>.",
                                NULL};

static const char *mxclass[18] = {
    "mxUNKNOWN_CLASS", "mxCELL_CLASS",     "mxSTRUCT_CLASS", "mxOBJECT_CLASS", "mxCHAR_CLASS",
    "mxSPARSE_CLASS",  "mxDOUBLE_CLASS",   "mxSINGLE_CLASS", "mxINT8_CLASS",   "mxUINT8_CLASS",
    "mxINT16_CLASS",   "mxUINT16_CLASS",   "mxINT32_CLASS",  "mxUINT32_CLASS", "mxINT64_CLASS",
    "mxUINT64_CLASS",  "mxFUNCTION_CLASS", "mxOPAQUE_CLASS"};
static int printdata = 0;
static int human_readable = 0;
static int print_whos_first = 1;

/* Print Functions */
static void print_whos(matvar_t *matvar);
static void print_default(matvar_t *matvar);

static void (*printfunc)(matvar_t *matvar) = NULL;

static char *
get_next_token(char *str)
{
    const char *tokens = "(){}.";
    char *next_tok, *tok;

    next_tok = NULL;
    while ( *tokens != '\0' ) {
        tok = strchr(str, *tokens);
        if ( tok != NULL ) {
            if ( NULL == next_tok )
                next_tok = tok;
            else if ( tok < next_tok )
                next_tok = tok;
        }
        tokens++;
    }
    if ( NULL == next_tok )
        next_tok = str;
    return next_tok;
}

static int
slab_get_rank(char *open, char *close)
{
    int rank = 0;
    char *ptr = open + 1;
    rank = 1;
    while ( ptr != close ) {
        if ( *ptr++ == ',' )
            rank++;
    }
    return rank;
}

static void
slab_get_select(char *open, char *close, int rank, int *start, int *stride, int *edge)
{
    char *ptr, *valptr;
    int nvals, dim, i;

    ptr = open;
    valptr = open + 1;
    dim = 0;
    nvals = 0;
    do {
        ptr++;
        if ( *ptr == ',' ) {
            if ( nvals == 2 ) {
                *ptr = '\0';
                if ( !strcmp(valptr, "end") ) {
                    edge[dim] = -1;
                } else {
                    i = (int)strtol(valptr, NULL, 10);
                    edge[dim] = i;
                }
            } else if ( nvals == 1 ) {
                *ptr = '\0';
                if ( !strcmp(valptr, "end") ) {
                    edge[dim] = -1;
                } else {
                    i = (int)strtol(valptr, NULL, 10);
                    edge[dim] = i;
                }
            } else if ( nvals == 0 ) {
                *ptr = '\0';
                if ( !strcmp(valptr, "end") ) {
                    start[dim] = -1;
                    edge[dim] = -1;
                } else {
                    i = (int)strtol(valptr, NULL, 10);
                    start[dim] = i - 1;
                    edge[dim] = i;
                }
            }
            dim++;
            valptr = ptr + 1;
            nvals = 0;
        } else if ( *ptr == ':' ) {
            *ptr = '\0';
            if ( !strcmp(valptr, "end") ) {
                if ( nvals == 0 )
                    start[dim] = -1;
                else if ( nvals == 1 )
                    edge[dim] = -1;
                else if ( nvals == 2 )
                    edge[dim] = -1;
                else
                    fprintf(stderr, "Too many inputs to dim %d", dim + 1);
            } else {
                i = (int)strtol(valptr, NULL, 10);
                if ( nvals == 0 )
                    start[dim] = i - 1;
                else if ( nvals == 1 )
                    stride[dim] = i;
                else if ( nvals == 2 )
                    edge[dim] = i;
                else
                    fprintf(stderr, "Too many inputs to dim %d", dim + 1);
            }
            nvals++;
            valptr = ptr + 1;
        } else if ( *ptr == ')' || *ptr == '}' ) {
            *ptr = '\0';
            if ( !strcmp(valptr, "end") ) {
                if ( nvals == 0 ) {
                    start[dim] = -1;
                    edge[dim] = -1;
                } else if ( nvals == 1 )
                    edge[dim] = -1;
                else if ( nvals == 2 )
                    edge[dim] = -1;
                else
                    fprintf(stderr, "Too many inputs to dim %d", dim + 1);
            } else {
                i = (int)strtol(valptr, NULL, 10);
                if ( nvals == 0 ) {
                    start[dim] = i - 1;
                    edge[dim] = i;
                } else if ( nvals == 1 )
                    edge[dim] = i;
                else if ( nvals == 2 )
                    edge[dim] = i;
                else
                    fprintf(stderr, "Too many inputs to dim %d", dim + 1);
            }
            nvals++;
            valptr = ptr + 1;
        }
    } while ( ptr != close );
}

static int
slab_select_valid(int rank, int *start, int *stride, int *edge, matvar_t *matvar)
{
    int valid = 1, i, nmemb = 1;

    if ( (matvar->rank != rank) && (rank != 1) ) {
        valid = 0;
    } else if ( rank == 1 ) {
        for ( i = 0; i < matvar->rank; i++ )
            nmemb *= matvar->dims[i];
        if ( *stride < 1 ) {
            /* Check stride is at least 1 */
            fprintf(stderr, "stride must be positive");
            valid = 0;
        } else if ( *edge > nmemb ) {
            /* edge can't be bigger than the size of the dimension */
            fprintf(stderr, "edge out of bound");
            valid = 0;
        } else if ( *start >= nmemb || (*start > *edge && *edge > 0) ) {
            /* Start can't be bigger than the size of the dimension and
             * can't be greater than the edge unless edge == -1 => end
             */
            fprintf(stderr, "start out of bound");
            valid = 0;
        } else if ( *edge == -1 && *start == -1 ) {
            /* If edge == start == -1, then a single end was used */
            *edge = 1;
            *start = nmemb - 1;
        } else if ( *edge == -1 && *stride == 1 ) {
            /* index of the form 1:end, 1:1:end, or : */
            *edge = nmemb;
            /* If ':' was specified, start[i] will be -1 */
            if ( *start < 0 )
                *start = 0;
        } else if ( *edge == -1 ) {
            /* index of the form 1:stride:end */
            *edge = nmemb;
            *edge = (int)floor((double)(*edge - *start - 1) / (double)*stride) + 1;
        } else if ( *edge > 0 ) {
            *edge = (int)floor((double)(*edge - *start - 1) / (double)*stride) + 1;
        }
        nmemb = *edge;
    } else {
        for ( i = 0; i < rank && valid; i++ ) {
            if ( stride[i] < 1 ) {
                /* Check stride is at least 1 */
                fprintf(stderr, "stride must be positive");
                valid = 0;
                break;
            } else if ( edge[i] == -1 && start[i] == -1 ) {
                /* If edge == start == -1, then a single end was used */
                edge[i] = 1;
                start[i] = matvar->dims[i] - 1;
            } else if ( edge[i] < 0 && stride[i] == 1 ) {
                /* index of the form 1:end, 1:1:end, or : */
                edge[i] = matvar->dims[i];
                /* If ':' was specified, start[i] will be -1 */
                if ( start[i] < 0 )
                    start[i] = 0;
            } else if ( edge[i] < 0 ) {
                /* index of the form 1:stride:end */
                edge[i] =
                    (int)floor((double)(matvar->dims[i] - start[i] - 1) / (double)stride[i]) + 1;
            } else if ( (size_t)edge[i] > matvar->dims[i] ) {
                /* edge can't be bigger than the size of the dimension */
                fprintf(stderr, "edge out of bound on dimension %d", i + 1);
                valid = 0;
                break;
            } else if ( (size_t)start[i] >= matvar->dims[i] ||
                        (start[i] > edge[i] && edge[i] > 0) ) {
                /* Start can't be bigger than the size of the dimension and
                 * can't be greater than the edge unless edge == -1 => end
                 */
                fprintf(stderr, "start out of bound on dimension %d", i + 1);
                valid = 0;
                break;
            } else if ( edge[i] == (start[i] + 1) ) {
                /* index of the form 3:3 */
                edge[i] = 1;
            } else if ( edge[i] > 0 ) {
                edge[i] = (int)floor((double)(edge[i] - start[i] - 1) / (double)stride[i]) + 1;
            }
            nmemb *= edge[i];
        }
    }
    if ( !valid )
        nmemb = 0;
    return nmemb;
}

static void
read_selected_data(mat_t *mat, matvar_t **_matvar, char *index_str)
{
    char *next_tok_pos, next_tok = 0;
    char *open = NULL, *close = NULL;
    int err = 1, i = 0, j, done = 0;
    matvar_t *matvar = *_matvar;

    next_tok_pos = get_next_token(index_str);
    next_tok = *next_tok_pos;

    while ( !done ) {
        /* Check If the user is selecting a subset of the dataset */
        if ( next_tok == '(' ) {
            int rank, *start, *stride, *edge, nmemb;

            open = next_tok_pos;
            close = strchr(open + 1, ')');

            /* Get the next token after this selection */
            next_tok_pos = get_next_token(close + 1);
            if ( next_tok_pos != (close + 1) ) {
                *next_tok_pos = '\0';
                next_tok = *next_tok_pos;
            } else {
                done = 1;
            }
            /* Make sure that the partial I/O is the last token */
            if ( !done ) {
                fprintf(stderr, "Partial I/O must be the last operation in the expression");
                break;
            }
            /* Get the rank of the dataset */
            rank = slab_get_rank(open, close);
            start = (int *)malloc(rank * sizeof(int));
            stride = (int *)malloc(rank * sizeof(int));
            edge = (int *)malloc(rank * sizeof(int));
            for ( j = 0; j < rank; j++ ) {
                start[j] = 0;
                stride[j] = 1;
                edge[j] = 1;
            }
            /* Get the start,stride,edge using matlab syntax */
            slab_get_select(open, close, rank, start, stride, edge);

            /* Check if the users selection is valid and if so read the data */
            if ( (nmemb = slab_select_valid(rank, start, stride, edge, matvar)) ) {
                matvar->data_size = Mat_SizeOfClass(matvar->class_type);
                matvar->nbytes = (size_t)nmemb * matvar->data_size;
                if ( matvar->isComplex ) {
                    mat_complex_split_t *z;
                    matvar->data = malloc(sizeof(*z));
                    z = (mat_complex_split_t *)matvar->data;
                    z->Re = malloc(matvar->nbytes);
                    z->Im = malloc(matvar->nbytes);
                } else {
                    matvar->data = malloc(matvar->nbytes);
                }
                if ( matvar->data == NULL ) {
                    fprintf(stderr, "Couldn't allocate memory for the data");
                    err = 1;
                } else if ( rank == 1 ) {
                    Mat_VarReadDataLinear(mat, matvar, matvar->data, *start, *stride, *edge);
                    if ( matvar->rank == 2 && matvar->dims[0] == 1 ) {
                        matvar->dims[1] = *edge;
                    } else if ( matvar->rank == 2 && matvar->dims[1] == 1 ) {
                        matvar->dims[0] = *edge;
                    } else {
                        matvar->rank = 2;
                        matvar->dims[0] = *edge;
                        matvar->dims[1] = 1;
                    }
                } else {
                    err = Mat_VarReadData(mat, matvar, matvar->data, start, stride, edge);
                    for ( i = 0; i < rank; i++ )
                        matvar->dims[i] = (size_t)edge[i];
                }
            }
            free(start);
            free(stride);
            free(edge);
        } else if ( next_tok == '.' ) {
            matvar_t *field;
            char *varname;

            varname = next_tok_pos + 1;
            if ( matvar->class_type == MAT_C_STRUCT ) {
                next_tok_pos = get_next_token(next_tok_pos + 1);
                if ( next_tok_pos != varname ) {
                    next_tok = *next_tok_pos;
                    *next_tok_pos = '\0';
                } else {
                    done = 1;
                }
                /* FIXME: Handle structures > 1x1 */
                field = Mat_VarGetStructFieldByName(matvar, varname, 0);
                if ( field == NULL ) {
                    fprintf(stderr, "field %s was not found in structure %s", varname,
                            matvar->name);
                    break;
                }
                field = Mat_VarDuplicate(field, 1);
                Mat_VarFree(matvar);
                matvar = field;
                if ( done == 1 ) {
                    err = Mat_VarReadDataAll(mat, matvar);
                }
            } else if ( matvar->class_type == MAT_C_CELL ) {
                int ncells;
                matvar_t *cell, **cells;

                ncells = matvar->nbytes / matvar->data_size;
                cells = (matvar_t **)matvar->data;
                next_tok_pos = get_next_token(next_tok_pos + 1);
                if ( next_tok_pos != varname ) {
                    next_tok = *next_tok_pos;
                    *next_tok_pos = '\0';
                } else {
                    done = 1;
                }
                for ( j = 0; j < ncells; j++ ) {
                    cell = Mat_VarGetCell(matvar, j);
                    if ( cell == NULL || cell->class_type != MAT_C_STRUCT ) {
                        fprintf(stderr, "cell index %d is not a structure", j);
                        break;
                    } else {
                        /* FIXME: Handle structures > 1x1 */
                        field = Mat_VarGetStructFieldByName(cell, varname, 0);
                        if ( field == NULL ) {
                            fprintf(stderr, "field %s was not found in structure %s", varname,
                                    matvar->name);
                            break;
                        }
                        field = Mat_VarDuplicate(field, 1);
                        Mat_VarFree(cell);
                        cells[j] = field;
                    }
                }
                if ( j != ncells )
                    break;
            } else {
                fprintf(stderr, "%s is not a structure", varname);
                break;
            }
        } else if ( next_tok == '{' ) {
            int rank, *start, *stride, *edge, nmemb;

            if ( matvar->class_type != MAT_C_CELL ) {
                fprintf(stderr, "Only Cell Arrays can index with {}");
                break;
            }
            open = next_tok_pos;
            close = strchr(open + 1, '}');

            /* Get the next token after this selection */
            next_tok_pos = get_next_token(close + 1);
            if ( *next_tok_pos != '\0' ) {
                next_tok = *next_tok_pos;
                *next_tok_pos = '\0';
            } else {
                done = 1;
            }
            /* Get the rank of the dataset */
            rank = slab_get_rank(open, close);
            start = (int *)malloc(rank * sizeof(int));
            stride = (int *)malloc(rank * sizeof(int));
            edge = (int *)malloc(rank * sizeof(int));
            for ( j = 0; j < rank; j++ ) {
                start[j] = 0;
                stride[j] = 1;
                edge[j] = 1;
            }
            /* Get the start,stride,edge using matlab syntax */
            slab_get_select(open, close, rank, start, stride, edge);
            /* Check if the users selection is valid and if so read the data */
            if ( (nmemb = slab_select_valid(rank, start, stride, edge, matvar)) ) {
                matvar_t **cells, *tmp;
                if ( rank == 1 ) {
                    cells = Mat_VarGetCellsLinear(matvar, *start, *stride, *edge);
                    if ( matvar->rank == 2 && matvar->dims[0] == 1 ) {
                        matvar->dims[1] = *edge;
                    } else if ( matvar->rank == 2 && matvar->dims[1] == 1 ) {
                        matvar->dims[0] = *edge;
                    } else {
                        matvar->rank = 1;
                        matvar->dims[0] = *edge;
                    }
                } else {
                    cells = Mat_VarGetCells(matvar, start, stride, edge);
                    for ( i = 0; i < rank; i++ )
                        matvar->dims[i] = (size_t)edge[i];
                }
                if ( cells == NULL ) {
                    fprintf(stderr, "Error getting the indexed cells");
                    err = 1;
                } else {
                    for ( j = 0; j < nmemb; j++ )
                        cells[j] = Mat_VarDuplicate(cells[j], 1);
                    tmp = Mat_VarCreate(matvar->name, MAT_C_CELL, MAT_T_CELL, matvar->rank,
                                        matvar->dims, cells, MAT_F_DONT_COPY_DATA);
                    Mat_VarFree(matvar);
                    matvar = tmp;
                }
            } else {
                fprintf(stderr, "Cell selection not valid");
                err = 1;
            }
            free(start);
            free(stride);
            free(edge);
            if ( err )
                break;
        }
    }
    *_matvar = matvar;
}

static void
print_whos(matvar_t *matvar)
{
    int i;
    size_t nbytes;
    char size[32] = {
        '\0',
    };

    if ( print_whos_first ) {
        printf("%-20s       %-10s     %-10s     %-18s\n\n", "Name", "Size", "Bytes", "Class");
        print_whos_first = 0;
    }
    printf("%-20s", matvar->name);
    if ( matvar->rank > 0 ) {
        int cnt = 0;
        printf("%8" SIZE_T_FMTSTR, matvar->dims[0]);
        for ( i = 1; i < matvar->rank; i++ ) {
            if ( ceil(log10((double)matvar->dims[i])) + 1 < 32 )
                cnt += sprintf(size + cnt, "x%" SIZE_T_FMTSTR, matvar->dims[i]);
        }
        printf("%-10s", size);
    } else {
        printf("                    ");
    }
    nbytes = Mat_VarGetSize(matvar);
    if ( human_readable ) {
        if ( nbytes > 1073741824L )
            printf(" %10.1fG", (double)nbytes / 1073741824.0);
        else if ( nbytes > 1048576 )
            printf(" %10.1fM", (double)nbytes / 1048576.0);
        else if ( nbytes > 1024 )
            printf(" %10.1fK", (double)nbytes / 1024.0);
        else
            printf(" %10" SIZE_T_FMTSTR "B", nbytes);
    } else {
        printf("  %10" SIZE_T_FMTSTR, nbytes);
    }
    printf("  %-18s\n", mxclass[matvar->class_type]);
}

static size_t indent = 0;

static void
default_printf_func(int log_level, char *message)
{
    size_t i;

    for ( i = 0; i < indent; i++ )
        printf("    ");
    printf("%s\n", message);
}

static void
print_default_number(enum matio_types type, void *data)
{
    switch ( type ) {
        case MAT_T_DOUBLE:
            printf("%g", *(double *)data);
            break;
        case MAT_T_SINGLE:
            printf("%g", *(float *)data);
            break;
#ifdef _mat_int64_t
        case MAT_T_INT64:
#if MATIO_HAVE_INTTYPES_H
            printf("%" PRIi64, *(mat_int64_t *)data);
#elif defined(_MSC_VER) && _MSC_VER >= 1200
            printf("%I64i", *(mat_int64_t *)data);
#else
            printf("%lld", (long long)(*(mat_int64_t *)data));
#endif
            break;
#endif
#ifdef _mat_uint64_t
        case MAT_T_UINT64:
#if MATIO_HAVE_INTTYPES_H
            printf("%" PRIu64, *(mat_uint64_t *)data);
#elif defined(_MSC_VER) && _MSC_VER >= 1200
            printf("%I64u", *(mat_uint64_t *)data);
#else
            printf("%llu", (unsigned long long)(*(mat_uint64_t *)data));
#endif
            break;
#endif
        case MAT_T_INT32:
            printf("%d", *(mat_int32_t *)data);
            break;
        case MAT_T_UINT32:
            printf("%u", *(mat_uint32_t *)data);
            break;
        case MAT_T_INT16:
            printf("%hd", *(mat_int16_t *)data);
            break;
        case MAT_T_UINT16:
            printf("%hu", *(mat_uint16_t *)data);
            break;
        case MAT_T_INT8:
            printf("%hhd", *(mat_int8_t *)data);
            break;
        case MAT_T_UINT8:
            printf("%hhu", *(mat_uint8_t *)data);
            break;
        default:
            break;
    }
}

static void
print_default_numeric_2d(matvar_t *matvar)
{
    size_t i, j, stride;

    if ( NULL == matvar->data )
        return;

    stride = Mat_SizeOf(matvar->data_type);
    if ( matvar->isComplex ) {
        mat_complex_split_t *complex_data = (mat_complex_split_t *)matvar->data;
        char *rp = (char *)complex_data->Re;
        char *ip = (char *)complex_data->Im;
        for ( i = 0; i < matvar->dims[0]; i++ ) {
            for ( j = 0; j < matvar->dims[1]; j++ ) {
                size_t idx = matvar->dims[0] * j + i;
                print_default_number(matvar->data_type, rp + idx * stride);
                printf(" + ");
                print_default_number(matvar->data_type, ip + idx * stride);
                printf("i ");
            }
            printf("\n");
        }
    } else {
        char *data = (char *)matvar->data;
        for ( i = 0; i < matvar->dims[0]; i++ ) {
            for ( j = 0; j < matvar->dims[1]; j++ ) {
                size_t idx = matvar->dims[0] * j + i;
                print_default_number(matvar->data_type, data + idx * stride);
                printf(" ");
            }
            printf("\n");
        }
    }
}

static void
print_default_numeric_3d(matvar_t *matvar)
{
    size_t i, j, k, l, stride;

    if ( NULL == matvar->data )
        return;

    stride = Mat_SizeOf(matvar->data_type);
    if ( matvar->isComplex ) {
        mat_complex_split_t *complex_data = (mat_complex_split_t *)matvar->data;
        char *rp = (char *)complex_data->Re;
        char *ip = (char *)complex_data->Im;
        for ( k = 0; k < matvar->dims[2]; k++ ) {
            Mat_Message("%s(:,:,%zu) = ", matvar->name, k);
            indent++;
            for ( i = 0; i < matvar->dims[0]; i++ ) {
                for ( l = 0; l < indent; l++ )
                    printf("    ");
                for ( j = 0; j < matvar->dims[1]; j++ ) {
                    size_t idx = matvar->dims[0] * matvar->dims[1] * k + matvar->dims[0] * j + i;
                    print_default_number(matvar->data_type, rp + idx * stride);
                    printf(" + ");
                    print_default_number(matvar->data_type, ip + idx * stride);
                    printf("i ");
                }
                printf("\n");
            }
            indent--;
            printf("\n");
        }
    } else {
        char *data = (char *)matvar->data;
        for ( k = 0; k < matvar->dims[2]; k++ ) {
            Mat_Message("%s(:,:,%zu) = ", matvar->name, k);
            indent++;
            for ( i = 0; i < matvar->dims[0]; i++ ) {
                for ( l = 0; l < indent; l++ )
                    printf("    ");
                for ( j = 0; j < matvar->dims[1]; j++ ) {
                    size_t idx = matvar->dims[0] * matvar->dims[1] * k + matvar->dims[0] * j + i;
                    print_default_number(matvar->data_type, data + idx * stride);
                    printf(" ");
                }
                printf("\n");
            }
            indent--;
            printf("\n");
        }
    }
}

static void
print_default(matvar_t *matvar)
{
    if ( NULL == matvar )
        return;

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
        case MAT_C_UINT8: {
            if ( matvar->rank == 2 )
                print_default_numeric_2d(matvar);
            else if ( matvar->rank == 3 )
                print_default_numeric_3d(matvar);
            break;
        }
        case MAT_C_CHAR:
        case MAT_C_SPARSE:
            Mat_VarPrint(matvar, printdata);
            break;
        case MAT_C_STRUCT: {
            int nfields;
            int i;
            size_t nmemb;

            if ( matvar->name )
                Mat_Message("      Name: %s", matvar->name);
            Mat_Message("      Rank: %d", matvar->rank);
            if ( matvar->rank == 0 )
                return;
            Mat_Message("Class Type: Structure");
            nfields = Mat_VarGetNumberOfFields(matvar);
            nmemb = matvar->dims[0];
            for ( i = 1; i < matvar->rank; i++ )
                nmemb *= matvar->dims[i];
            if ( nfields > 0 && nmemb < 1 ) {
                char *const *fieldnames = Mat_VarGetStructFieldnames(matvar);
                Mat_Message("Fields[%d] {", nfields);
                indent++;
                if ( NULL != fieldnames ) {
                    for ( i = 0; i < nfields; i++ )
                        Mat_Message("    Name: %s", fieldnames[i]);
                }
                indent--;
                Mat_Message("}");
            } else if ( nfields > 0 && nmemb > 0 ) {
                Mat_Message("Fields[%d] {", nfields);
                indent++;
                {
                    size_t j;
                    matvar_t **fields = (matvar_t **)matvar->data;
                    if ( NULL != fields )
                        for ( j = 0; j < nfields * nmemb; j++ )
                            print_default(fields[j]);
                }
                indent--;
                Mat_Message("}");
            }
            break;
        }
        case MAT_C_CELL: {
            size_t ncells;
            int i;

            if ( matvar->name )
                Mat_Message("      Name: %s", matvar->name);
            Mat_Message("      Rank: %d", matvar->rank);
            if ( matvar->rank == 0 )
                return;
            ncells = matvar->dims[0];
            for ( i = 1; i < matvar->rank; i++ )
                ncells *= matvar->dims[i];
            Mat_Message("Class Type: Cell Array");
            Mat_Message("{");
            indent++;
            {
                size_t j;
                matvar_t **cells = (matvar_t **)matvar->data;
                if ( NULL != cells )
                    for ( j = 0; j < ncells; j++ )
                        print_default(cells[j]);
            }
            indent--;
            Mat_Message("}");
            break;
        }
        default:
            Mat_Message("Empty");
    }
}

int
main(int argc, char *argv[])
{
    const char *prog_name = "matdump";
    int i, c, err = EXIT_SUCCESS;
    mat_t *mat;
    matvar_t *matvar;
    int version[3];

    Mat_GetLibraryVersion(version, version + 1, version + 2);
    if ( MATIO_MAJOR_VERSION != version[0] || MATIO_MINOR_VERSION != version[1] ||
         MATIO_RELEASE_LEVEL != version[2] ) {
        fprintf(stderr, "matio version in header does not match runtime version\n");
        return EXIT_FAILURE;
    }

    Mat_LogInitFunc(prog_name, default_printf_func);

    printfunc = print_default;

    while ( (c = getopt_long(argc, argv, optstring, options, NULL)) != EOF ) {
        switch ( c ) {
            case 'd':
                printdata = 1;
                Mat_VerbMessage(1, "Printing data\n");
                break;
            case 'f':
                if ( NULL != optarg ) {
                    if ( !strcmp(optarg, "whos") ) {
                        printfunc = print_whos;
                    } else {
                        Mat_Warning("%s is not a recognized output format. Using default\n",
                                    optarg);
                    }
                } else {
                    Mat_Warning("Missing output format. Using default\n");
                }
                break;
            case 'h':
                human_readable = 1;
                break;
            case 'v':
                Mat_SetVerbose(1, 0);
                break;
            case 'H':
                Mat_Help(helpstr);
                /* Note: Mat_Help() calls exit() */
            case 'V':
                printf(
                    "%s %s\nWritten by Christopher Hulbert\n\n"
                    "Copyright(C) 2015-2022, The matio contributors\n"
                    "Copyright(C) 2006-2014, Christopher C. Hulbert\n",
                    prog_name, MATIO_VERSION_STR);
                exit(EXIT_SUCCESS);
            default:
                printf("%c not a valid option\n", c);
                break;
        }
    }

    if ( (argc - optind) < 1 ) {
        Mat_Critical("Must specify at least one argument");
        return EXIT_FAILURE;
    }

    mat = Mat_Open(argv[optind], MAT_ACC_RDONLY);
    if ( NULL == mat ) {
        Mat_Critical("Error opening %s\n", argv[optind]);
        return EXIT_FAILURE;
    }

    optind++;

    if ( optind < argc ) {
        /* variables specified on the command line */
        for ( i = optind; i < argc; i++ ) {
            char *next_tok_pos, next_tok = 0;

            next_tok_pos = get_next_token(argv[i]);
            if ( next_tok_pos != argv[i] ) {
                next_tok = *next_tok_pos;
                *next_tok_pos = '\0';
            }

            matvar = Mat_VarReadInfo(mat, argv[i]);
            if ( matvar ) {
                if ( printdata ) {
                    if ( next_tok == '\0' ) {
                        /* No indexing tokens found, so read all of the data */
                        err = Mat_VarReadDataAll(mat, matvar);
                    } else {
                        *next_tok_pos = next_tok;
                        read_selected_data(mat, &matvar, next_tok_pos);
                    }
                }
                (*printfunc)(matvar);
                Mat_VarFree(matvar);
            } else {
                Mat_Warning("Couldn't find variable %s in the MAT file", argv[i]);
            }
        } /* for ( i = optind; i < argc; i++ ) */
    } else {
        /* print all variables */
        if ( printdata ) {
            while ( (matvar = Mat_VarReadNext(mat)) != NULL ) {
                (*printfunc)(matvar);
                Mat_VarFree(matvar);
            }
        } else {
            while ( (matvar = Mat_VarReadNextInfo(mat)) != NULL ) {
                (*printfunc)(matvar);
                Mat_VarFree(matvar);
            }
        }
    }

    Mat_Close(mat);

    Mat_LogClose();

    return err;
}
