/*
 * Copyright (C) 2005-2006   Christopher C. Hulbert
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
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <matio.h>
#if !defined(HAVE_STRCASECMP)
#   define strcasecmp(a,b) strcmp(a,b)
#endif

static const char *helpstr[] = {
    "",
    "Usage: test_mat [OPTIONS] test [TEST_OPTS]",
    "",
    "Runs various test on the Matlab I/O library libmatio",
    "",
    "OPTIONS",
    "--help         This output",
    "--help-tests   List of tests",
    "--help TEST    help information on test TEST",
    "--version      version information",
    "",
    "test        - name of the test to run",
    "TEST_OPTS   - If required, specify arguments to a test(See --help TEST)",
    "",
    NULL
};

static const char *helptestsstr[] = {
"   Version 5 MAT File tests",
"================================================================",
"copy                    - Copies one matlab file to another",
"write                   - Writes a matlab file",
"writecompressed         - Writes a compressed matlab file",
"readvar                 - Reads a specific variable from a file",
"write_struct            - Writes a structure",
"writecell               - Writes a Cell Array",
"getstructfield          - Tests Mat_VarGetStructField getting fields from a",
"                          structure",
"readvarinfo             - Reads a variables header information only",
"readslab                - Tests reading a part of a dataset",
"writeinf                - Tests writing inf (Infinity) values",
"writenan                - Tests writing NaN (Not A Number) values",
"writenull               - Tests writing empty variables",
"writeslab               - Tests writing a part of a dataset",
"writesparse             - Tests writing a sparse matrix",
"write_struct            - Test writing structures",
"write_compressed_struct - Test writing compressed structures",
"",
"",
"   Version 4 MAT File tests",
"================================================================",
"readvar4       - Reads a specific variable from a file",
"readslab4      - Tests reading a part of a dataset",
"",
"",
"   Other Tests",
"================================================================",
"ind2sub - Calculates a set of subscripts from a linear index",
"sub2ind - Calculates the linear index from subscript values",
"",
NULL
};

static const char *helptest_copy[] = {
    "TEST: copy",
    "",
    "Usage: test_mat copy FILE",
    "",
    "  Copies FILE to test_mat_copy.mat",
    "",
    NULL
};

static const char *helptest_write[] = {
    "TEST: write",
    "",
    "Usage: test_mat write",
    "",
    "Writes various datasets to test_mat_write.mat",
    "The output file should have 6 datasets as described below",
    "",
    "Dataset Name  Data Type   Rank   Dimensions   Data",
    "---------------------------------------------------------------",
    "    d         Double      2      5x10         reshape(1:50,5,10)",
    "    f         Single      2      5x10         single(reshape(1:50,5,10))",
    "  i32         Int 32      2      5x10         int32(reshape(1:50,5,10))",
    "  i16         Int 16      2      5x10         int16(reshape(1:50,5,10))",
    "   i8         Int  8      2      5x10         int8(reshape(1:50,5,10))",
    "  str         Char        2      1x14         'This is a string'",
    "",
    NULL
};

static const char *helptest_writecompressed[] = {
    "TEST: writecompressed",
    "",
    "Usage: test_mat writecompressed",
    "",
    "Writes various datasets to test_mat_write.mat using zlib compression.",
    "The output file should have 6 datasets as described below",
    "",
    "Dataset Name  Data Type   Rank   Dimensions   Data",
    "---------------------------------------------------------------",
    "    d         Double      2      5x10         reshape(1:50,5,10)",
    "    f         Single      2      5x10         single(reshape(1:50,5,10))",
    "  i32         Int 32      2      5x10         int32(reshape(1:50,5,10))",
    "  i16         Int 16      2      5x10         int16(reshape(1:50,5,10))",
    "   i8         Int  8      2      5x10         int8(reshape(1:50,5,10))",
    "",
    NULL
};

static const char *helptest_readvar[] = {
    "TEST: readvar",
    "",
    "Usage: test_mat readvar FILE variable_name",
    "",
    "Reads variable_name from FILE and prints out it's information and data"
    "If possible",
    "",
    NULL
};

static const char *helptest_write_struct[] = {
    "TEST: write_struct",
    "",
    "Usage: test_mat write_struct",
    "",
    "Writes a structure of size 4x1 with one field (data) of various types to",
    "file test_mat_write_struct.mat",
    "",
    "Index    Data Type   Rank   Dimensions   Data",
    "---------------------------------------------------------------",
    " 1,1     Double      2      5x10         reshape(1:50,5,10)",
    " 2,1     Single      2      5x10         single(reshape(1:50,5,10))",
    " 3,1     Int 32      2      5x10         int32(reshape(1:50,5,10))",
    " 4,1     Char        2      1x16         'This is a string'",
    " 5,1     Struct      2      4x1          structure(1:4,1)",
    "",
    NULL
};

static const char *helptest_write_compressed_struct[] = {
    "TEST: write_compressed_struct",
    "",
    "Usage: test_mat write_compressed_struct",
    "",
    "Writes a compressed structure of size 4x1 with one field (data) of",
    "various types to file test_mat_write_compressed_struct.mat",
    "",
    "Index    Data Type   Rank   Dimensions   Data",
    "---------------------------------------------------------------",
    " 1,1     Double      2      5x10         reshape(1:50,5,10)",
    " 2,1     Single      2      5x10         single(reshape(1:50,5,10))",
    " 3,1     Int 32      2      5x10         int32(reshape(1:50,5,10))",
    " 4,1     Char        2      1x16         'This is a string'",
    " 5,1     Struct      2      4x1          structure(1:4,1)",
    "",
    NULL
};

static const char *helptest_writecell[] = {
    "TEST: writecell",
    "",
    "Usage: test_mat writecell",
    "",
    "Writes a cell array of size 4x1 with various data types to",
    "file test_mat_writecell.mat",
    "",
    "Index    Data Type   Rank   Dimensions   Data",
    "---------------------------------------------------------------",
    " 1,1     Double      2      5x10         reshape(1:50,5,10)",
    " 2,1     Single      2      5x10         single(reshape(1:50,5,10))",
    " 3,1     Double      2      5x10         int32(reshape(1:50,5,10))",
    " 4,1     Struct      2      3x1          structure(1,1).data=cell{1},etc",
    "",
    NULL
};

static const char *helptest_getstructfield[] = {
    "TEST: getstructfield",
    "",
    "Usage: test_mat getstructfield FILE structure field",
    "",
    "  Tests the Mat_GetStructField function by reading fields from",
    "  a structure. FILE is the name of the input file containing a Matlab",
    "  structure named structure_name and either the field name or",
    "  1-relative field index. i.e. to read the data field of the structure",
    "  created by the write_struct test, use:",
    "    test_mat getstructfield test_mat_write_struct.mat structure data",
    "  OR",
    "    test_mat getstructfield test_mat_write_struct.mat structure 1",
    "",
    NULL
};

static const char *helptest_readvarinfo[] = {
    "TEST: readvarinfo",
    "",
    "Usage: test_mat readvarinfo FILE variable_name",
    "",
    "Reads information for variable_name from FILE and prints it out",
    "",
    NULL
};

static const char *helptest_readslab[] = {
    "TEST: readslab",
    "",
    "Usage: test_mat readslab FILE variable_name",
    "",
    "Reads the corner points of the variable variable_name from file FILE and",
    "prints them out.  variable_name should be a double-precision 2-D array",
    "",
    NULL
};

static const char *helptest_writeslab[] = {
    "TEST: writeslab",
    "",
    "Usage: test_mat writeslab",
    "",
    "Writes slabs of data to test_mat_writelslab.mat  Every other element",
    "in the file is written.  Three datasets are written of types double,",
    "single, and int32",
    "",
    NULL
};

static const char *helptest_writesparse[] = {
    "TEST: writesparse",
    "",
    "Usage: test_mat writesparse",
    "",
    "Writes a sparse matrix variable with name sparse_matrix to ",
    "test_mat_writesparse.mat.  When loaded into matlab, the data should be:",
    "",
    "    (1,1)        1",
    "    (5,1)        5",
    "    (2,2)        7",
    "    (3,2)        8",
    "    (4,2)        9",
    "    (1,3)       11",
    "    (5,3)       15",
    "    (2,4)       17",
    "    (3,4)       18",
    "    (4,4)       19",
    "    (1,5)       21",
    "    (5,5)       25",
    "    (2,6)       27",
    "    (3,6)       28",
    "    (4,6)       29",
    "    (1,7)       31",
    "    (5,7)       35",
    "    (2,8)       37",
    "    (3,8)       38",
    "    (4,8)       39",
    "    (1,9)       41",
    "    (5,9)       45",
    "    (2,10)      47",
    "    (3,10)      48",
    "    (4,10)      49",
    "",
    NULL
};

static const char *helptest_writenull[] = {
    "TEST: writenull",
    "",
    "Usage: test_mat writenull",
    "",
    "Writes to the file test_write_null.mat a real and complex empty numeric",
    "array, a structure with a real and complex empty numeric arrays, a",
    "structure with no fields, and a cell array with an empty cell.",
    "",
    NULL
};

static const char *helptest_writenan[] = {
    "TEST: writenan",
    "",
    "Usage: test_mat writenan",
    "",
    "Writes to the file test_writenan.mat a 5x5 double precision matrix",
    "with NaN's down the diagonal.",
    "",
    NULL
};

static const char *helptest_writeinf[] = {
    "TEST: writeinf",
    "",
    "Usage: test_mat writeinf",
    "",
    "Writes to the file test_writeinf.mat a 5x5 double precision matrix",
    "with Inf's down the diagonal.",
    "",
    NULL
};

static const char *helptest_sub2ind[] = {
    "TEST: sub2ind",
    "",
    "Usage: test_mat sub2ind",
    "",
    "  Calculates a linear (single) index from a set of subscript indeces.",
    "  The size of the array used is [256,256,124].  The 1-relative indeces",
    "  are (233,74,1).  Therefore, the calculated linear index should be"
    "  18921.",
    "",
    NULL
};

static const char *helptest_ind2sub[] = {
    "TEST: ind2sub",
    "",
    "Usage: test_mat ind2sub",
    "",
    "  Calculates a set of subscript indeces from a linear (single) index.",
    "  The size of the array used is [256,256,124].  The 1-relative linear",
    "  index used is 18921.  Therefore, the calculated subscripts should be"
    "  (233,74,1).",
    "",
    NULL
};

static void
help_test(const char *test)
{
    if ( !strcmp(test,"copy") )
        Mat_Help(helptest_copy);
    else if ( !strcmp(test,"write") )
        Mat_Help(helptest_write);
    else if ( !strcmp(test,"writecompressed") )
        Mat_Help(helptest_writecompressed);
    else if ( !strcmp(test,"readvar") )
        Mat_Help(helptest_readvar);
    else if ( !strcmp(test,"write_struct") )
        Mat_Help(helptest_write_struct);
    else if ( !strcmp(test,"write_compressed_struct") )
        Mat_Help(helptest_write_compressed_struct);
    else if ( !strcmp(test,"writecell") )
        Mat_Help(helptest_writecell);
    else if ( !strcmp(test,"readvarinfo") )
        Mat_Help(helptest_readvarinfo);
    else if ( !strcmp(test,"readslab") )
        Mat_Help(helptest_readslab);
    else if ( !strcmp(test,"writeslab") )
        Mat_Help(helptest_writeslab);
    else if ( !strcmp(test,"writesparse") )
        Mat_Help(helptest_writesparse);
    else if ( !strcmp(test,"writenull") )
        Mat_Help(helptest_writenull);
    else if ( !strcmp(test,"getstructfield") )
        Mat_Help(helptest_getstructfield);
    else if ( !strcmp(test,"ind2sub") )
        Mat_Help(helptest_ind2sub);
    else if ( !strcmp(test,"sub2ind") )
        Mat_Help(helptest_sub2ind);
    else if ( !strcmp(test,"writenan") )
        Mat_Help(helptest_writenan);
    else if ( !strcmp(test,"writeinf") )
        Mat_Help(helptest_writeinf);
}

static int
test_write( void )
{
    int dims[2] = {5,10}, err = 0, i;
    double    d[50];
    float     f[50];
    mat_int32_t i32[50];
    mat_int16_t i16[50];
    mat_int8_t   i8[50];
    char *str = "This is a string";
    mat_t *mat;
    matvar_t *matvar;

    for ( i = 0; i < 50; i++ ) {
          d[i] = i+1;
          f[i] = i+1;
        i32[i] = i+1;
        i16[i] = i+1;
         i8[i] = i+1;
    }

    mat = Mat_Open("test_mat_write.mat",MAT_ACC_RDWR);
    if ( mat ) {
        matvar = Mat_VarCreate("d",MAT_C_DOUBLE,MAT_T_DOUBLE,2,dims,d,0);
        Mat_VarWrite( mat, matvar, 0);
        Mat_VarFree(matvar);
        matvar = Mat_VarCreate("f",MAT_C_SINGLE,MAT_T_SINGLE,2,dims,f,0);
        Mat_VarWrite( mat, matvar, 0);
        Mat_VarFree(matvar);
        matvar = Mat_VarCreate("i32",MAT_C_INT32,MAT_T_INT32,2,dims,i32,0);
        Mat_VarWrite( mat, matvar, 0);
        Mat_VarFree(matvar);
        matvar = Mat_VarCreate("i16",MAT_C_INT16,MAT_T_INT16,2,dims,i16,0);
        Mat_VarWrite( mat, matvar, 0);
        Mat_VarFree(matvar);
        matvar = Mat_VarCreate("i8",MAT_C_INT8,MAT_T_INT8,2,dims,i8,0);
        Mat_VarWrite( mat, matvar, 0);
        Mat_VarFree(matvar);
        dims[0] = 1;
        dims[1] = strlen(str);
        matvar = Mat_VarCreate("str",MAT_C_CHAR,MAT_T_INT8,2,dims,str,0);
        Mat_VarWrite( mat, matvar, 0);
        Mat_VarFree(matvar);
        Mat_Close(mat);
    } else {
        err = 1;
    }

    return err;
}

static int
test_write_compressed( void )
{
    int dims[2] = {5,10}, err = 0, i;
    double    d[50];
    float     f[50];
    mat_int32_t i32[50];
    mat_int16_t i16[50];
    mat_int8_t   i8[50];
    char *str = "This is a string";
    mat_t *mat;
    matvar_t *matvar;

    for ( i = 0; i < 50; i++ ) {
          d[i] = i+1;
          f[i] = i+1;
        i32[i] = i+1;
        i16[i] = i+1;
         i8[i] = i+1;
    }

    mat = Mat_Open("test_mat_write_compressed.mat",MAT_ACC_RDWR);
    if ( mat ) {
        matvar = Mat_VarCreate("d",MAT_C_DOUBLE,MAT_T_DOUBLE,2,dims,d,0);
        matvar->compression = COMPRESSION_ZLIB;
        Mat_VarWrite( mat, matvar,COMPRESSION_ZLIB);
        Mat_VarFree(matvar);
        matvar = Mat_VarCreate("f",MAT_C_SINGLE,MAT_T_SINGLE,2,dims,f,0);
        Mat_VarWrite( mat, matvar,COMPRESSION_ZLIB);
        Mat_VarFree(matvar);
        matvar = Mat_VarCreate("i32",MAT_C_INT32,MAT_T_INT32,2,dims,i32,0);
        Mat_VarWrite( mat, matvar,COMPRESSION_ZLIB);
        Mat_VarFree(matvar);
        matvar = Mat_VarCreate("i16",MAT_C_INT16,MAT_T_INT16,2,dims,i16,0);
        Mat_VarWrite( mat, matvar,COMPRESSION_ZLIB);
        Mat_VarFree(matvar);
        matvar = Mat_VarCreate("i8",MAT_C_INT8,MAT_T_INT8,2,dims,i8,0);
        Mat_VarWrite( mat, matvar,COMPRESSION_ZLIB);
        Mat_VarFree(matvar);
        dims[0] = 1;
        dims[1] = strlen(str);
        matvar = Mat_VarCreate("str",MAT_C_CHAR,MAT_T_INT8,2,dims,str,0);
        Mat_VarWrite(mat,matvar,COMPRESSION_ZLIB);
        Mat_VarFree(matvar);
        Mat_Close(mat);
    } else {
        err = 1;
    }

    return err;
}

static int
test_write_complex_compressed( void )
{
    int dims[2] = {5,10}, err = 0, i;
    mat_uint8_t    real[50],imag[50];
    struct ComplexSplit c;
    mat_t *mat;
    matvar_t *matvar;

    for ( i = 0; i < 50; i++ ) {
          real[i] = i+1;
          imag[i] = 50-i;
    }

    c.Re = real;
    c.Im = imag;

    mat = Mat_Open("test_mat_write_complex_compressed.mat",MAT_ACC_RDWR);
    if ( mat ) {
        matvar = Mat_VarCreate("complex_data",MAT_C_DOUBLE,MAT_T_UINT8,2,
            dims,&c,MAT_F_COMPLEX | MEM_CONSERVE);
#if 0
        matvar->compression = COMPRESSION_ZLIB;
#endif
        Mat_VarWrite(mat,matvar,COMPRESSION_ZLIB);
        Mat_VarFree(matvar);
        Mat_Close(mat);
    } else {
        err = 1;
    }

    return err;
}

static int
test_readvar(const char *inputfile, const char *var)
{
    int err = 0;
    mat_t *mat;
    matvar_t *matvar;

    mat = Mat_Open(inputfile,MAT_ACC_RDONLY);
    if ( mat ) {
        matvar = Mat_VarRead(mat,(char*)var);
        if ( matvar == NULL ) {
            err = 1;
        } else {
            Mat_VarPrint( matvar, 1);
            Mat_VarFree(matvar);
        }
        Mat_Close(mat);
    } else {
        err = 1;
    }
    return err;
}

static int
test_readvar4(const char *inputfile, const char *var)
{
    int err = 0;
    mat_t *mat;
    matvar_t *matvar;

    mat = Mat_Open(inputfile,MAT_ACC_RDONLY | MAT_FT_MAT4);
    if ( mat ) {
        matvar = Mat_VarRead(mat,(char*)var);
        if ( matvar == NULL ) {
            err = 1;
        } else {
            Mat_VarPrint(matvar, 1);
            Mat_VarFree(matvar);
        }
        Mat_Close(mat);
    } else {
        err = 1;
    }
    return err;
}

static int
test_write_struct()
{
    int     dims[2] = {5,10};
    double  data[50]={0.0,};
    float  fdata[50]={0.0,};
    int    idata[50]={0.0,};
    char  *str = "This is a string";
    int    err = 0, i;
    mat_t     *mat;
    matvar_t **matvar, *struct_matvar, *substruct_matvar;
    
    for ( i = 0; i < 50; i++ ) {
         data[i] = i+1;
        fdata[i] = i+1;
        idata[i] = i+1;
    }

    mat = Mat_Create("test_mat_write_struct.mat",NULL);
    if ( mat ) {
        matvar = malloc(6*sizeof(matvar_t *));
        matvar[0] = Mat_VarCreate("data",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,data,MEM_CONSERVE);
        matvar[1] = Mat_VarCreate("data",MAT_C_SINGLE,MAT_T_SINGLE,2,
                       dims,fdata,MEM_CONSERVE);
        matvar[2] = Mat_VarCreate("data",MAT_C_INT32,MAT_T_INT32,2,
                       dims,idata,MEM_CONSERVE);
        dims[0]   = 1;
        dims[1]   = strlen(str);
        matvar[3] = Mat_VarCreate("data",MAT_C_CHAR,MAT_T_UINT8,2,
                       dims,str,MEM_CONSERVE);
        matvar[4] = NULL;
        dims[0] = 4;
        dims[1] = 1;
        substruct_matvar = Mat_VarCreate("data",MAT_C_STRUCT,MAT_T_STRUCT,
                            2,dims,matvar,0);
        matvar[4] = substruct_matvar;
        matvar[5] = NULL;

        dims[0] = 5;
        dims[1] = 1;
        struct_matvar = Mat_VarCreate("structure",MAT_C_STRUCT,MAT_T_STRUCT,2,
                            dims,matvar,0);
        Mat_VarWrite(mat,struct_matvar,0);
        free(matvar[0]);
        free(matvar[1]);
        free(matvar[2]);
        free(matvar[3]);
        free(matvar);
        free(struct_matvar);
        free(substruct_matvar);
        Mat_Close(mat);
    }
    return err;
}

static int
test_write_compressed_struct()
{
    int     dims[2] = {5,10};
    double  data[50]={0.0,};
    float  fdata[50]={0.0,};
    int    idata[50]={0.0,};
    char  *str = "This is a string";
    int    err = 0, i;
    mat_t     *mat;
    matvar_t **matvar, *struct_matvar, *substruct_matvar;
    
    for ( i = 0; i < 50; i++ ) {
         data[i] = i+1;
        fdata[i] = i+1;
        idata[i] = i+1;
    }

    mat = Mat_Create("test_mat_write_compressed_struct.mat",NULL);
    if ( mat ) {
        matvar = malloc(6*sizeof(matvar_t *));
        matvar[0] = Mat_VarCreate("data",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,data,MEM_CONSERVE);
        matvar[1] = Mat_VarCreate("data",MAT_C_SINGLE,MAT_T_SINGLE,2,
                       dims,fdata,MEM_CONSERVE);
        matvar[2] = Mat_VarCreate("data",MAT_C_INT32,MAT_T_INT32,2,
                       dims,idata,MEM_CONSERVE);
        dims[0]   = 1;
        dims[1]   = strlen(str);
        matvar[3] = Mat_VarCreate("data",MAT_C_CHAR,MAT_T_UINT8,2,
                       dims,str,MEM_CONSERVE);
        matvar[4] = NULL;
        dims[0] = 4;
        dims[1] = 1;
        substruct_matvar = Mat_VarCreate("data",MAT_C_STRUCT,MAT_T_STRUCT,
                            2,dims,matvar,0);
        dims[0] = 5;
        dims[1] = 10;
        matvar[0] = Mat_VarCreate("data",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,data,MEM_CONSERVE);
        matvar[1] = Mat_VarCreate("data",MAT_C_SINGLE,MAT_T_SINGLE,2,
                       dims,fdata,MEM_CONSERVE);
        matvar[2] = Mat_VarCreate("data",MAT_C_INT32,MAT_T_INT32,2,
                       dims,idata,MEM_CONSERVE);
        dims[0]   = 1;
        dims[1]   = strlen(str);
        matvar[3] = Mat_VarCreate("data",MAT_C_CHAR,MAT_T_UINT8,2,
                       dims,str,MEM_CONSERVE);
        matvar[4] = substruct_matvar;
        matvar[5] = NULL;

        dims[0] = 5;
        dims[1] = 1;
        struct_matvar = Mat_VarCreate("structure",MAT_C_STRUCT,MAT_T_STRUCT,2,
                            dims,matvar,0);
        Mat_VarWrite(mat,struct_matvar,COMPRESSION_ZLIB);
        free(matvar);
        Mat_VarFree(struct_matvar);
        Mat_Close(mat);
    }
    return err;
}

static int
test_write_cell()
{
    int     dims[2] = {5,10};
    double  data[50]={0.0,};
    float  fdata[50]={0.0,};
    int    idata[50]={0.0,};
    int    err = 0, i;
    mat_t     *mat;
    matvar_t **matvar, *struct_matvar, *substruct_matvar;
    
    for ( i = 0; i < 50; i++ ) {
         data[i] = i+1;
        fdata[i] = i+1;
        idata[i] = i+1;
    }

    mat = Mat_Create("test_mat_writecell.mat",NULL);
    if ( mat ) {
        matvar = malloc(5*sizeof(matvar_t *));
        matvar[0] = Mat_VarCreate("data",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,data,MEM_CONSERVE);
        matvar[1] = Mat_VarCreate("data",MAT_C_SINGLE,MAT_T_SINGLE,2,
                       dims,fdata,MEM_CONSERVE);
        matvar[2] = Mat_VarCreate("data",MAT_C_INT32,MAT_T_INT32,2,
                       dims,idata,MEM_CONSERVE);
        matvar[3] = NULL;
        dims[0] = 3;
        dims[1] = 1;
        substruct_matvar = Mat_VarCreate("structure",MAT_C_STRUCT,MAT_T_STRUCT,
                            2,dims,matvar,0);
        matvar[3] = substruct_matvar;
        dims[0] = 4;
        dims[1] = 1;
        struct_matvar = Mat_VarCreate("cell",MAT_C_CELL,MAT_T_CELL,2,
                            dims,matvar,0);
        Mat_VarWrite(mat,struct_matvar,0);
        free(matvar[0]);
        free(matvar[1]);
        free(matvar[2]);
        free(matvar);
        free(struct_matvar);
        free(substruct_matvar);
        Mat_Close(mat);
    }
    return err;
}

static int
test_write_null(void)
{
    int       err = 0;
    mat_t    *mat;
    matvar_t *struct_matvar,*cell_matvar;
    matvar_t *struct_fields[5] = {NULL,NULL,NULL,NULL,NULL};
    int       dims[3] = {0,1,10};

    mat = Mat_Create("test_write_null.mat",NULL);
    if ( mat != NULL ) {
        struct_fields[0] = Mat_VarCreate("d_null",MAT_C_DOUBLE,MAT_T_DOUBLE,3,
                            dims,NULL,0);
        Mat_VarWrite(mat,struct_fields[0],0);
        struct_fields[1] = Mat_VarCreate("cd_null",MAT_C_DOUBLE,MAT_T_DOUBLE,3,
                            dims,NULL,MAT_F_COMPLEX);
        Mat_VarWrite(mat,struct_fields[1],0);
        dims[0] = 1;
        struct_matvar = Mat_VarCreate("struct_null",MAT_C_STRUCT,MAT_T_STRUCT,2,
                            dims,NULL,0);
        Mat_VarWrite(mat,struct_matvar,0);
        Mat_VarFree(struct_matvar);
        struct_matvar = Mat_VarCreate("struct_null_fields",MAT_C_STRUCT,
                            MAT_T_STRUCT,2,dims,struct_fields,0);
        Mat_VarWrite(mat,struct_matvar,0);
        cell_matvar = Mat_VarCreate("cell_null_cells",MAT_C_CELL,MAT_T_CELL,2,
                            dims,struct_fields,MEM_CONSERVE);
        Mat_VarWrite(mat,cell_matvar,0);
        Mat_VarFree(struct_matvar);
        Mat_Close(mat);
    } else {
        err = 1;
    }

    return err;
}

static int
test_get_struct_field(const char *file,const char *structname,
    const char *fieldname)
{
    mat_t *mat;
    matvar_t *matvar, *field;
    int index = 1, err = 0;

    mat = Mat_Open(file,MAT_ACC_RDONLY);
    if ( mat ) {
        matvar = Mat_VarRead(mat,(char*)structname);
        if ( matvar ) {
            switch ( *fieldname ) {
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    index = atoi(fieldname);
                    field = Mat_VarGetStructField(matvar,&index,BY_INDEX,0);
                    err = (field == NULL) ? 1 : 0;
                    if ( !err )
                        Mat_VarPrint( field, 0);
                    break;
                default:
                    field = Mat_VarGetStructField(matvar,fieldname,BY_NAME,0);
                    err = (field == NULL) ? 1 : 0;
                    if ( !err )
                        Mat_VarPrint(field,0);
                    break;
            }
            Mat_VarFree(matvar);
        } else {
            err = 1;
        }
        Mat_Close(mat);
    } else {
        err = 1;
    }
    return err;
}

static int
test_readslab(const char *file, const char *var)
{
    int   start[2]={0,0},stride[2]={1,1},edge[2]={2,2}, err = 0;
    double ptr[4];
    mat_t  *mat;
   matvar_t *matvar;

    mat = Mat_Open(file,MAT_ACC_RDONLY);
    if ( mat ) {
        matvar = Mat_VarReadInfo(mat,(char *)var);
        if ( matvar != NULL ) {
            stride[0] = matvar->dims[0]-1;
            stride[1] = matvar->dims[1]-1;
            Mat_VarReadData(mat,matvar,ptr,start,stride,edge);
            printf("%f    %f\n%f    %f\n",ptr[0],ptr[1],ptr[2],ptr[3]);
            Mat_VarFree(matvar);
        } else {
            err = 1;
        }
        Mat_Close(mat);
    } else {
        err = 1;
    }
    return err;
}

static int
test_readslab4(const char *file, const char *var)
{
    int   start[2]={0,0},stride[2]={1,1},edge[2]={2,2}, err = 0;
    double ptr[4];
    mat_t  *mat;
   matvar_t *matvar;

    mat = Mat_Open((const char *)file,MAT_ACC_RDONLY | MAT_FT_MAT4);
    if ( mat ) {
        matvar = Mat_VarReadInfo(mat,(char*)var);
        if ( matvar != NULL ) {
            stride[0] = matvar->dims[0]-1;
            stride[1] = matvar->dims[1]-1;
            Mat_VarReadData(mat,matvar,ptr,start,stride,edge);
            printf("%f    %f\n%f    %f\n",ptr[0],ptr[1],ptr[2],ptr[3]);
            Mat_VarFree(matvar);
        } else {
            err = 1;
        }
        Mat_Close(mat);
    } else {
        err = 1;
    }
    return err;
}

static int
test_writeslab(void)
{
    int        err = 0, i;
    int        dims[2] = {6,10},start[2]={0,0},stride[2]={2,2},edge[2]={3,5};
    double     data[60]={0.0,};
    float     fdata[60]={0.0,};
    int       idata[60]={0.0,};
    mat_t    *mat;
    matvar_t *matvar, *matvar2, *matvar3;
    
    for ( i = 0; i < 60; i++ ) {
         data[i] = i+1;
        fdata[i] = i+1;
        idata[i] = i+1;
    }

    mat = Mat_Create("test_mat_writeslab.mat",NULL);
    if ( mat != NULL ) {
        matvar = Mat_VarCreate("d",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,NULL,0);
        matvar2 = Mat_VarCreate("f",MAT_C_SINGLE,MAT_T_SINGLE,2,
                       dims,NULL,0);
        matvar3 = Mat_VarCreate("i",MAT_C_INT32,MAT_T_INT32,2,
                       dims,NULL,0);
        Mat_VarWriteInfo(mat,matvar);
        Mat_VarWriteInfo(mat,matvar2);
        Mat_VarWriteInfo(mat,matvar3);
        Mat_VarWriteData(mat,matvar3,idata,start,stride,edge);
        Mat_VarWriteData(mat,matvar,data,start,stride,edge);
        Mat_VarWriteData(mat,matvar2,fdata,start,stride,edge);
        Mat_VarFree(matvar);
        Mat_VarFree(matvar2);
        Mat_VarFree(matvar3);
        Mat_Close(mat);
    } else {
        err = 1;
    }
    return err;
}

static int
test_writenan(void)
{
    int        err = 0, i;
    int        dims[2] = {5,5};
    double     data[25]={0.0,};
    mat_t    *mat;
    matvar_t *matvar;
    
    for ( i = 0; i < 25; i++ )
         data[i] = i+1;

    for ( i = 0; i < 25; i+= 6 )
        data[i] = 0.0/0.0;

    mat = Mat_Create("test_writenan.mat",NULL);
    if ( mat != NULL ) {
        matvar = Mat_VarCreate("d",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,data,MEM_CONSERVE);
        Mat_VarWrite(mat,matvar,0);
        Mat_VarFree(matvar);
        Mat_Close(mat);
    } else {
        err = 1;
    }
    return err;
}

static int
test_writeinf(void)
{
    int        err = 0, i;
    int        dims[2] = {5,5};
    double     data[25]={0.0,};
    mat_t    *mat;
    matvar_t *matvar;
    
    for ( i = 0; i < 25; i++ )
         data[i] = i+1;

    for ( i = 0; i < 25; i+= 6 )
        data[i] = 1.0/0.0;

    mat = Mat_Create("test_writeinf.mat",NULL);
    if ( mat != NULL ) {
        matvar = Mat_VarCreate("d",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,data,MEM_CONSERVE);
        Mat_VarWrite(mat,matvar,0);
        Mat_VarFree(matvar);
        Mat_Close(mat);
    } else {
        err = 1;
    }
    return err;
}

static int
test_writesparse( void )
{
    int dims[2] = {5,10}, err = 0, i;
    double    d[50] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,37,38,39,
                       41,45,47,48,49};
    mat_int32_t  ir[25] = {0,4,1,2,3,0,4,1,2,3,0,4,1,2,3,0,4,1,2,3,0,4,1,2,3};
    mat_int32_t  jc[11] = {0,2,5,7,10,12,15,17,20,22,25};
    mat_t *mat;
    matvar_t *matvar;
    sparse_t  sparse = {0,};

    sparse.nzmax = 25;
    sparse.nir   = 25;
    sparse.ir    = ir;
    sparse.njc   = 11;
    sparse.jc    = jc;
    sparse.ndata = 25;
    sparse.data  = d;
    mat = Mat_Open("test_mat_writesparse.mat",MAT_ACC_RDWR);
    if ( mat ) {
        matvar = Mat_VarCreate("sparse_matrix",MAT_C_SPARSE,
                       MAT_T_DOUBLE,2,dims,&sparse,MEM_CONSERVE);
        if ( matvar != NULL ) {
            Mat_VarWrite( mat, matvar, 0);
            Mat_VarFree(matvar);
        } else {
            Mat_Critical("test_writesparse: Couldn't create matlab variable");
            err = 1;
        }
        Mat_Close(mat);
    } else {
        err = 1;
    }

    return err;
}

static int
test_delete(char *file,char *name)
{
    int err = 0;
    mat_t *mat;

    mat = Mat_Open(file,MAT_ACC_RDWR);
    if ( mat != NULL ) {
        err = Mat_VarDelete(mat,name);
        Mat_Close(mat);
    } else {
        Mat_Critical("MAT file %s doesn't exist", file);
        err = 1;
    }
    return err;
}

int main (int argc, char *argv[])
{
    char *prog_name = "test_mat";
    int   i, k, err = 0, ntests = 0;
    mat_t *mat, *mat2;
    matvar_t *matvar, *matvar2, *matvar3;

    Mat_LogInit(prog_name);

    if ( argc < 2 ) {
        Mat_Error("Must specify a test, or --help");
    } else if  ( (argc == 2) && !strcmp(argv[1],"--help") ) {
        Mat_Help(helpstr);
    } else if  ( (argc == 2) && !strcmp(argv[1],"--help-tests") ) {
        Mat_Help(helptestsstr);
    } else if  ( (argc == 3) && !strcmp(argv[1],"--help") ) {
        help_test(argv[2]);
    } else if  ( (argc == 2) && !strcmp(argv[1],"--version") ) {
        printf("%s v%d.%d.%d (compiled %s, %s for %s)\n", prog_name,
               MATIO_MAJOR_VERSION, MATIO_MINOR_VERSION, MATIO_RELEASE_LEVEL,
               __DATE__, __TIME__, MATIO_PLATFORM );
        exit(EXIT_SUCCESS);
    }

    for ( k = 1; k < argc; ) {
        if ( !strcasecmp(argv[k],"copy") ) {
            k++;
            mat = Mat_Create("test_mat_copy.mat",NULL);
            mat2 = Mat_Open(argv[k++],MAT_ACC_RDONLY);
            if ( mat && mat2 ) {
                while ( NULL != (matvar = Mat_VarReadNext(mat2)) )
                    Mat_VarWrite( mat, matvar, 0);
                Mat_Close(mat);
                Mat_Close(mat2);
            }
            ntests++;
        } else if ( !strcasecmp(argv[k],"delete") ) {
            k++;
            err += test_delete(argv[k],argv[k+1]);
            k+= 2;
            ntests++;
        } else if ( !strcasecmp(argv[k],"write") ) {
            k++;
            err += test_write();
            ntests++;
        } else if ( !strcasecmp(argv[k],"writenull") ) {
            k++;
            err += test_write_null();
            ntests++;
        } else if ( !strcasecmp(argv[k],"writenan") ) {
            k++;
            err += test_writenan();
            ntests++;
        } else if ( !strcasecmp(argv[k],"writeinf") ) {
            k++;
            err += test_writeinf();
            ntests++;
        } else if ( !strcasecmp(argv[k],"writecompressed_complex") ) {
            k++;
            err += test_write_complex_compressed();
            ntests++;
        } else if ( !strcasecmp(argv[k],"writecompressed") ) {
            k++;
            err += test_write_compressed();
            ntests++;
        } else if ( !strcasecmp(argv[k],"readvar") ) {
            k++;
            if ( argc < 4 ) {
                Mat_Critical("Must specify the input file and variable respectively");
                err++;
            } else {
                err += test_readvar(argv[k],argv[k+1]);
                k+=2;
            }
            ntests++;
        } else if ( !strcasecmp(argv[k],"readvar4") ) {
            k++;
            if ( argc < 4 ) {
                Mat_Critical("Must specify the input file and variable respectively");
                err++;
            } else {
                err += test_readvar4(argv[k],argv[k+1]);
                k+=2;
            }
            ntests++;
        } else if ( !strcasecmp(argv[k],"write_struct") ) {
            k++;
            err += test_write_struct();
            ntests++;
        } else if ( !strcasecmp(argv[k],"write_compressed_struct") ) {
            k++;
            err += test_write_compressed_struct();
            ntests++;
        } else if ( !strcasecmp(argv[k],"writecell") ) {
            k++;
            err += test_write_cell();
            ntests++;
        } else if ( !strcasecmp(argv[k],"getstructfield") ) {
            k++;
            if ( argc-k < 3 ) {
                Mat_Critical("Must specify the input file, structure name, "
                             "and field name/index");
                err++;
            } else {
                err += test_get_struct_field(argv[k],argv[k+1],argv[k+2]);
                k += 3;
            }
            ntests++;
        } else if ( !strcasecmp(argv[k],"readvarinfo") ) {
            k++;
            mat = Mat_Open(argv[k++],MAT_ACC_RDONLY);
            if ( mat ) {
                matvar = Mat_VarReadInfo(mat,argv[k++]);
                if ( matvar ) {
                    Mat_VarPrint( matvar, 0);
                    Mat_VarFree(matvar);
                }
                Mat_Close(mat);
            } else {
                k++;
                err ++;
            }
            ntests++;
        } else if ( !strcasecmp(argv[k],"writeslab") ) {
           k++;
           err += test_writeslab();
            ntests++;
    #if 0
        } else if ( !strcasecmp(argv[1],"cellslab") ) {
            matvar_t *cellmatvar, **cellfields;
                cellfields = malloc(6*sizeof(matvar_t *));
                cellfields[0] = matvar;
                cellfields[1] = matvar2;
                cellfields[2] = matvar3;
                cellfields[3] = matvar;
                cellfields[4] = matvar2;
                cellfields[5] = matvar3;
                dims[0] = 3;
                dims[1] = 2;
                cellmatvar = Mat_VarCreate("c",MAT_C_CELL,MAT_T_CELL,2,
                               dims,cellfields,0);
                Mat_VarWriteInfo(mat,cellmatvar);
                Mat_VarPrint(Mat_VarGetCell(cellmatvar,1,1),0);
                cellmatvar->data = NULL;
                Mat_VarFree(cellmatvar);
            ntests++;
    #endif
        } else if ( !strcasecmp(argv[k],"readslab") ) {
            k++;
            test_readslab(argv[k],argv[k+1]);
            k+=2;
            ntests++;
        } else if ( !strcasecmp(argv[k],"readslab4") ) {
            k++;
            test_readslab4(argv[k],argv[k+1]);
            k+=2;
            ntests++;
        } else if ( !strcasecmp(argv[k],"slab3") ) {
            int   start[3]={1,1,1},stride[3]={1,1,1},edge[3]={1,1,1};
            int j, l;
            double ptr[150] = {0,};
    
            k++;
            mat = Mat_Open("test_slab_d3.mat",MAT_ACC_RDONLY);
            if ( mat ) {
                matvar = Mat_VarReadInfo(mat,"d3");
                Mat_VarReadData(mat,matvar,ptr,start,stride,edge);
                for ( i = 0; i < 3; i++ ) {
                   for ( j = 0; j < 5; j++ ) {
                      for ( l = 0; l < 10; l++ )
                          printf("%f ",*(ptr+50*i+5*l+j));
                      printf("\n");
                    }
                    printf("\n\n");
                }
                Mat_VarFree(matvar);
                Mat_Close(mat);
            }
            ntests++;
        } else if ( !strcasecmp(argv[k],"writesparse") ) {
            k++;
            err += test_writesparse();
            ntests++;
        } else if ( !strcasecmp(argv[k],"ind2sub") ) {
            int *subs, dims[3] = {256,256,124};

            subs = Mat_CalcSubscripts(3,dims,18921-1);
            Mat_Message("%d,%d,%d",subs[0],subs[1],subs[2]);
            free(subs);
            k++;
            ntests++;
        } else if ( !strcasecmp(argv[k],"sub2ind") ) {
            int  dims[3] = {256,256,124}, index[3] = {233,74,1};
            int  linear_index;

            linear_index = Mat_CalcSingleSubscript(3,dims,index);
            Mat_Message("%d",linear_index);
            k++;
            ntests++;
        } else {
            Mat_Critical("Unrecognized test %s", argv[k]);
            k++;
            break;
        }
    }

    printf("%d of %d tests completed successfully\n",ntests-err,ntests);
    
    return 0;
}
