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
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <getopt.h>
#include "matio_private.h"
#if !defined(HAVE_STRCASECMP)
#   define strcasecmp(a,b) strcmp(a,b)
#endif

static const char *optstring = "v:HLT:Vz";
static struct option options[] = {
    {"compress",    no_argument,NULL,'z'},
    {"mat-version", required_argument,NULL,'v'},
    {"help",        no_argument,NULL,'H'},
    {"help-test",   required_argument,NULL,'T'},
    {"list-tests",  no_argument,      NULL,'L'},
    {"version",     no_argument,      NULL,'V'},
    {NULL,0,NULL,0}
};

static enum mat_ft            mat_file_ver = MAT_FT_DEFAULT;
static enum matio_compression compression  = COMPRESSION_NONE;

static const char *helpstr[] = {
    "",
    "Usage: test_mat [OPTIONS] test [TEST_OPTS]",
    "",
    "Runs various test on the Matlab I/O library libmatio",
    "",
    "OPTIONS",
    "-H, --help           This output",
    "-L, --list-tests     List of tests",
    "-T, --help-test TEST help information on test TEST",
    "-v, --mat-version x  Set MAT file version to x (4, 5, 7.3)",
    "-V, --version        version information",
    "-z, --compress       Enable compression for MAT 5 files",
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
"readvar                 - Reads a specific variable from a file",
"getstructfield          - Tests Mat_VarGetStructField getting fields from a",
"                          structure",
"readvarinfo             - Reads a variables header information only",
"readslab                - Tests reading a part of a dataset",
"write                   - Writes a matlab file",
"writecompressed         - Writes a compressed matlab file",
"writesparse             - Tests writing a sparse matrix",
"write_compressed_sparse - Tests writing a compressed sparse matrix",
"write_struct            - Test writing structures",
"write_compressed_struct - Test writing compressed structures",
"writecell               - Writes a Cell Array",
"write_compressed_cell   - Writes a compressed Cell Array",
"writeinf                - Tests writing inf (Infinity) values",
"writenan                - Tests writing NaN (Not A Number) values",
"writenull               - Tests writing empty variables",
"writeslab               - Tests writing a part of a dataset",
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
#ifdef HAVE_MAT_INT64_T
    "  i64         int64       2      5x10         int64(reshape(1:50,5,10))",
#endif
#ifdef HAVE_MAT_INT64_T
    " ui64         uint64      2      5x10         uint64(reshape(1:50,5,10))",
#endif
    "  i32         int32       2      5x10         int32(reshape(1:50,5,10))",
    "  i16         int16       2      5x10         int16(reshape(1:50,5,10))",
    "   i8         int8        2      5x10         int8(reshape(1:50,5,10))",
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
#ifdef HAVE_MAT_INT64_T
    "  i64         int64       2      5x10         int64(reshape(1:50,5,10))",
#endif
#ifdef HAVE_MAT_INT64_T
    " ui64         uint64      2      5x10         uint64(reshape(1:50,5,10))",
#endif
    "  i32         Int 32      2      5x10         int32(reshape(1:50,5,10))",
    "  i16         Int 16      2      5x10         int16(reshape(1:50,5,10))",
    "   i8         Int  8      2      5x10         int8(reshape(1:50,5,10))",
    "  str         Char        2      1x14         'This is a string'",
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
    "Index Data Type   Rank   Dimensions   Data",
    "---------------------------------------------------------------",
    " 1,1  Double         2    5x10         reshape(1:50,5,10)",
    " 2,1  Single         2    5x10         single(reshape(1:50,5,10))",
    " 3,1  Int 32         2    5x10         int32(reshape(1:50,5,10))",
    " 4,1  Char           2    1x16         'This is a string'",
    " 5,1  Struct         2    4x1          structure(1:4,1)",
    " 6,1  Double Complex 2    5x10   reshape(1:25,5,5)+j*reshape(26:50,5,5)",
    " 7,1  Single Complex 2    5x10   single(reshape(1:25,5,5)+j*reshape(26:50,5,5))",
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
    " 1,1     double      2      5x10         reshape(1:50,5,10)",
    " 2,1     single      2      5x10         single(reshape(1:50,5,10))",
    " 3,1     int32       2      5x10         int32(reshape(1:50,5,10))",
    " 4,1     char        2      1x16         'This is a string'",
    " 5,1     cell        2      4x1          {structure(1:4).data}'",
    " 6,1     struct      2      5x1          structure(1:5,1)",
    "",
    NULL
};

static const char *helptest_writecell[] = {
    "TEST: writecell",
    "",
    "Usage: test_mat writecell",
    "",
    "Writes a cell array of size 5x1 with various data types to",
    "file test_mat_writecell.mat",
    "",
    "Index    Data Type   Rank   Dimensions   Data",
    "---------------------------------------------------------------",
    " 1,1     double      2      5x10         reshape(1:50,5,10)",
    " 2,1     single      2      5x10         single(reshape(1:50,5,10))",
    " 3,1     int32       2      5x10         int32(reshape(1:50,5,10))",
    " 4,1     struct      2      3x1          structure(1,1).data=cell{1},etc",
    " 5,1     cell        2      4x1          cell{5}={cell{1:4}}.'",
    "",
    NULL
};

static const char *helptest_write_compressed_cell[] = {
    "TEST: write_compressed_cell",
    "",
    "Usage: test_mat write_compressed_cell",
    "",
    "Writes a cell array of size 5x1 with various data types to",
    "file test_mat_write_compressed_cell.mat",
    "",
    "Index    Data Type   Rank   Dimensions   Data",
    "---------------------------------------------------------------",
    " 1,1     double      2      5x10         reshape(1:50,5,10)",
    " 2,1     single      2      5x10         single(reshape(1:50,5,10))",
    " 3,1     int32       2      5x10         int32(reshape(1:50,5,10))",
    " 4,1     struct      2      3x1          structure(1,1).data=cell{1},etc",
    " 5,1     cell        2      4x1          cell{5}={cell{1:4}}.'",
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

static const char *helptest_write_compressed_sparse[] = {
    "TEST: write_compressed_sparse",
    "",
    "Usage: test_mat write_compressed_sparse",
    "",
    "Writes a compressed sparse matrix variable with name sparse_matrix to ",
    "test_mat_write_compressedsparse.mat.",
    "",
    "When loaded into matlab, the data should be:",
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

static const char *helptest_readvar4[] = {
    "TEST: readvar4",
    "",
    "Usage: test_mat readvar4 FILE variable_name",
    "",
    "Reads variable_name from the Matlab v4 MAT file FILE and prints out it's",
    "information and data if possible to the screen.",
    "",
    NULL
};

static const char *helptest_readvarinfo4[] = {
    "TEST: readvarinfo4",
    "",
    "Usage: test_mat readvarinfo4 FILE variable_name",
    "",
    "Reads header information for variable_name from the Matlab v4 MAT file"
    "FILE and prints it out to the screen.",
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
    else if ( !strcmp(test,"readvar") )
        Mat_Help(helptest_readvar);
    else if ( !strcmp(test,"readvarinfo") )
        Mat_Help(helptest_readvarinfo);
    else if ( !strcmp(test,"readslab") )
        Mat_Help(helptest_readslab);
    else if ( !strcmp(test,"write") )
        Mat_Help(helptest_write);
    else if ( !strcmp(test,"writecompressed") )
        Mat_Help(helptest_writecompressed);
    else if ( !strcmp(test,"writesparse") )
        Mat_Help(helptest_writesparse);
    else if ( !strcmp(test,"write_compressed_sparse") )
        Mat_Help(helptest_writesparse);
    else if ( !strcmp(test,"write_struct") )
        Mat_Help(helptest_write_struct);
    else if ( !strcmp(test,"write_compressed_struct") )
        Mat_Help(helptest_write_compressed_struct);
    else if ( !strcmp(test,"writecell") )
        Mat_Help(helptest_writecell);
    else if ( !strcmp(test,"write_compressed_cell") )
        Mat_Help(helptest_write_compressed_cell);
    else if ( !strcmp(test,"writeinf") )
        Mat_Help(helptest_writeinf);
    else if ( !strcmp(test,"writenan") )
        Mat_Help(helptest_writenan);
    else if ( !strcmp(test,"writenull") )
        Mat_Help(helptest_writenull);
    else if ( !strcmp(test,"writeslab") )
        Mat_Help(helptest_writeslab);
    else if ( !strcmp(test,"getstructfield") )
        Mat_Help(helptest_getstructfield);
    else if ( !strcmp(test,"readvar4") )
        Mat_Help(helptest_readvar4);
    else if ( !strcmp(test,"readvarinfo4") )
        Mat_Help(helptest_readvarinfo4);
    else if ( !strcmp(test,"ind2sub") )
        Mat_Help(helptest_ind2sub);
    else if ( !strcmp(test,"sub2ind") )
        Mat_Help(helptest_sub2ind);
    else
        exit(EXIT_FAILURE);
}

static int
test_write( void )
{
    size_t dims[2] = {5,10};
    int    err = 0, i;
    double    d[50];
    float     f[50];
    mat_int32_t i32[50];
    mat_int16_t i16[50];
    mat_int8_t   i8[50];
#ifdef HAVE_MAT_INT64_T
    mat_int64_t i64[50];
#endif
#ifdef HAVE_MAT_UINT64_T
    mat_uint64_t ui64[50];
#endif
    struct ComplexSplit z = {NULL,NULL},s = {NULL,NULL};
    char *str = "This is a string";
    mat_t *mat;
    matvar_t *matvar;

    for ( i = 0; i < 50; i++ ) {
          d[i] = i+1;
          f[i] = i+1;
        i32[i] = i+1;
        i16[i] = i+1;
         i8[i] = i+1;
#ifdef HAVE_MAT_INT64_T
        i64[i] = i+1;
#endif
#ifdef HAVE_MAT_UINT64_T
        ui64[i] = i+1;
#endif
    }

    z.Re = d;
    z.Im = d+25;
    s.Re = f;
    s.Im = f+25;

    mat = Mat_CreateVer("test_mat_write.mat",NULL,mat_file_ver);
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
#ifdef HAVE_MAT_INT64_T
        dims[0] = 5;
        dims[1] = 10;
        matvar = Mat_VarCreate("i64",MAT_C_INT64,MAT_T_INT64,2,dims,i64,0);
        Mat_VarWrite(mat,matvar,0);
        Mat_VarFree(matvar);
#endif
#ifdef HAVE_MAT_UINT64_T
        dims[0] = 5;
        dims[1] = 10;
        matvar = Mat_VarCreate("ui64",MAT_C_UINT64,MAT_T_UINT64,2,dims,ui64,0);+        Mat_VarWrite(mat,matvar,0);
        Mat_VarFree(matvar);
#endif
        dims[0] = 5;
        dims[1] = 5;
        matvar = Mat_VarCreate("z",MAT_C_DOUBLE,MAT_T_DOUBLE,2,dims,&z,
                               MAT_F_COMPLEX);
        Mat_VarWrite(mat,matvar,0);
        Mat_VarFree(matvar);
        matvar = Mat_VarCreate("s",MAT_C_SINGLE,MAT_T_SINGLE,2,dims,&s,
                               MAT_F_COMPLEX);
        Mat_VarWrite(mat,matvar,0);
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
    size_t dims[2] = {5,10};
    int    err = 0, i;
    double    d[50];
    float     f[50];
    mat_int32_t i32[50];
    mat_int16_t i16[50];
    mat_int8_t   i8[50];
#ifdef HAVE_MAT_INT64_T
    mat_int64_t i64[50];
#endif
#ifdef HAVE_MAT_UINT64_T
    mat_uint64_t ui64[50];
#endif
    char *str = "This is a string";
    mat_t *mat;
    matvar_t *matvar;

    for ( i = 0; i < 50; i++ ) {
          d[i] = i+1;
          f[i] = i+1;
        i32[i] = i+1;
        i16[i] = i+1;
         i8[i] = i+1;
#ifdef HAVE_MAT_INT64_T
        i64[i] = i+1;
#endif
#ifdef HAVE_MAT_UINT64_T
       ui64[i] = i+1;
#endif
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
#ifdef HAVE_MAT_INT64_T
        dims[0] = 5;
        dims[1] = 10;
        matvar = Mat_VarCreate("i64",MAT_C_INT64,MAT_T_INT64,2,dims,i64,0);
        Mat_VarWrite(mat,matvar,COMPRESSION_ZLIB);
        Mat_VarFree(matvar);
#endif
#ifdef HAVE_MAT_UINT64_T
        dims[0] = 5;
        dims[1] = 10;
        matvar = Mat_VarCreate("ui64",MAT_C_UINT64,MAT_T_UINT64,2,dims,ui64,0);+        Mat_VarWrite(mat,matvar,COMPRESSION_ZLIB);
        Mat_VarFree(matvar);
#endif
        Mat_Close(mat);
    } else {
        err = 1;
    }

    return err;
}

static int
test_write_complex_compressed( void )
{
    size_t dims[2] = {5,10};
    int    err = 0, i;
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
    size_t  dims[2] = {5,10};
    double  data[50]={0.0,};
    float  fdata[50]={0.0,};
    int    idata[50]={0.0,};
    char  *str = "This is a string";
    struct ComplexSplit z = {NULL,NULL},s = {NULL,NULL};
    int    err = 0, i;
    mat_t     *mat;
    matvar_t **matvar, *struct_matvar, *substruct_matvar;
    
    for ( i = 0; i < 50; i++ ) {
         data[i] = i+1;
        fdata[i] = i+1;
        idata[i] = i+1;
    }

    z.Re = data;
    z.Im = data+25;
    s.Re = fdata;
    s.Im = fdata+25;

    mat = Mat_CreateVer("test_mat_write_struct.mat",NULL,mat_file_ver);
    if ( mat ) {
        matvar = malloc(8*sizeof(*matvar));
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
        dims[0] = 5;
        dims[1] = 5;
        matvar[5] = Mat_VarCreate("data",MAT_C_DOUBLE,MAT_T_DOUBLE,2,dims,&z,
                               MAT_F_COMPLEX);
        matvar[6] = Mat_VarCreate("data",MAT_C_SINGLE,MAT_T_SINGLE,2,dims,&s,
                               MAT_F_COMPLEX);
        matvar[7] = NULL;

        dims[0] = 7;
        dims[1] = 1;
        struct_matvar = Mat_VarCreate("structure",MAT_C_STRUCT,MAT_T_STRUCT,2,
                            dims,matvar,0);
        Mat_VarWrite(mat,struct_matvar,0);
        free(matvar[0]);
        free(matvar[1]);
        free(matvar[2]);
        free(matvar[3]);
        free(matvar[5]);
        free(matvar[6]);
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
    size_t  dims[2] = {5,10};
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

    mat = Mat_CreateVer("test_mat_write_compressed_struct.mat",NULL,mat_file_ver);
    if ( mat ) {
        matvar = malloc(7*sizeof(matvar_t *));
        /*--------------------------------------------------------------*/
        /*  Create some variables for the cell array                    */
        /*--------------------------------------------------------------*/
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
        dims[0] = 4;
        dims[1] = 1;
        matvar[4] = Mat_VarCreate("data",MAT_C_CELL,MAT_T_CELL,2,dims,
                                    matvar,0);
        /*--------------------------------------------------------------*/

        /*--------------------------------------------------------------*/
        /*  Create some variables for the structure                     */
        /*--------------------------------------------------------------*/
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
        matvar[5] = NULL;
        dims[0] = 5;
        dims[1] = 1;
        substruct_matvar = Mat_VarCreate("data",MAT_C_STRUCT,MAT_T_STRUCT,
                            2,dims,matvar,0);
        /*--------------------------------------------------------------*/

        /*--------------------------------------------------------------*/
        /*  Create some variables for a cell array                      */
        /*--------------------------------------------------------------*/
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
        dims[0] = 4;
        dims[1] = 1;
        matvar[4] = Mat_VarCreate("data",MAT_C_CELL,MAT_T_CELL,2,dims,
                                    matvar,0);
        /*--------------------------------------------------------------*/

        /*--------------------------------------------------------------*/
        /*  Create some variables for the main structure                */
        /*--------------------------------------------------------------*/
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
        matvar[5] = substruct_matvar;
        matvar[6] = NULL;

        dims[0] = 6;
        dims[1] = 1;
        struct_matvar = Mat_VarCreate("structure",MAT_C_STRUCT,MAT_T_STRUCT,2,
                            dims,matvar,0);
        /*--------------------------------------------------------------*/
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
    size_t  dims[2] = {5,10};
    double  data[50]={0.0,};
    float  fdata[50]={0.0,};
    int    idata[50]={0.0,};
    int    err = 0, i;
    mat_t     *mat;
    matvar_t **matvar, *cell_matvar, *substruct_matvar;
    
    for ( i = 0; i < 50; i++ ) {
         data[i] = i+1;
        fdata[i] = i+1;
        idata[i] = i+1;
    }

    mat = Mat_CreateVer("test_mat_writecell.mat",NULL,mat_file_ver);
    if ( mat ) {
        matvar = malloc(5*sizeof(matvar_t *));
        matvar[0] = Mat_VarCreate("data",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,data,MEM_CONSERVE);
        matvar[1] = Mat_VarCreate("data",MAT_C_SINGLE,MAT_T_SINGLE,2,
                       dims,fdata,MEM_CONSERVE);
        matvar[2] = Mat_VarCreate("data",MAT_C_INT32,MAT_T_INT32,2,
                       dims,idata,MEM_CONSERVE);
        matvar[3] = NULL;
        dims[0] = 3; dims[1] = 1;
        substruct_matvar = Mat_VarCreate("structure",MAT_C_STRUCT,MAT_T_STRUCT,
                            2,dims,matvar,0);

        dims[0] = 5; dims[1] = 10;
        matvar[0] = Mat_VarCreate("data",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,data,MEM_CONSERVE);
        matvar[1] = Mat_VarCreate("data",MAT_C_SINGLE,MAT_T_SINGLE,2,
                       dims,fdata,MEM_CONSERVE);
        matvar[2] = Mat_VarCreate("data",MAT_C_INT32,MAT_T_INT32,2,
                       dims,idata,MEM_CONSERVE);
        matvar[3] = substruct_matvar;
        dims[0] = 4; dims[1] = 1;
        cell_matvar = Mat_VarCreate("cell",MAT_C_CELL,MAT_T_CELL,2,
                            dims,matvar,0);

        dims[0] = 5; dims[1] = 10;
        matvar[0] = Mat_VarCreate("data",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,data,MEM_CONSERVE);
        matvar[1] = Mat_VarCreate("data",MAT_C_SINGLE,MAT_T_SINGLE,2,
                       dims,fdata,MEM_CONSERVE);
        matvar[2] = Mat_VarCreate("data",MAT_C_INT32,MAT_T_INT32,2,
                       dims,idata,MEM_CONSERVE);
        matvar[3] = NULL;
        dims[0] = 3; dims[1] = 1;
        substruct_matvar = Mat_VarCreate("structure",MAT_C_STRUCT,MAT_T_STRUCT,
                            2,dims,matvar,0);

        dims[0] = 5; dims[1] = 10;
        matvar[0] = Mat_VarCreate("data",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,data,MEM_CONSERVE);
        matvar[1] = Mat_VarCreate("data",MAT_C_SINGLE,MAT_T_SINGLE,2,
                       dims,fdata,MEM_CONSERVE);
        matvar[2] = Mat_VarCreate("data",MAT_C_INT32,MAT_T_INT32,2,
                       dims,idata,MEM_CONSERVE);
        matvar[3] = substruct_matvar;
        matvar[4] = cell_matvar;
        dims[0] = 5; dims[1] = 1;
        cell_matvar = Mat_VarCreate("cell",MAT_C_CELL,MAT_T_CELL,2,
                            dims,matvar,0);

        Mat_VarWrite(mat,cell_matvar,COMPRESSION_NONE);
        free(matvar);
        Mat_VarFree(cell_matvar);
        Mat_Close(mat);
    }
    return err;
}

static int
test_write_compressed_cell()
{
    size_t  dims[2] = {5,10};
    double  data[50]={0.0,};
    float  fdata[50]={0.0,};
    int    idata[50]={0.0,};
    int    err = 0, i;
    mat_t     *mat;
    matvar_t **matvar, *cell_matvar, *substruct_matvar;
    
    for ( i = 0; i < 50; i++ ) {
         data[i] = i+1;
        fdata[i] = i+1;
        idata[i] = i+1;
    }

    mat = Mat_CreateVer("test_mat_write_compressed_cell.mat",NULL,mat_file_ver);
    if ( mat ) {
        matvar = malloc(5*sizeof(*matvar));
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

        matvar[0] = Mat_VarCreate("data",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,data,MEM_CONSERVE);
        matvar[1] = Mat_VarCreate("data",MAT_C_SINGLE,MAT_T_SINGLE,2,
                       dims,fdata,MEM_CONSERVE);
        matvar[2] = Mat_VarCreate("data",MAT_C_INT32,MAT_T_INT32,2,
                       dims,idata,MEM_CONSERVE);
        matvar[3] = substruct_matvar;
        dims[0] = 4;
        dims[1] = 1;
        cell_matvar = Mat_VarCreate("cell",MAT_C_CELL,MAT_T_CELL,2,
                            dims,matvar,0);

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

        matvar[0] = Mat_VarCreate("data",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,data,MEM_CONSERVE);
        matvar[1] = Mat_VarCreate("data",MAT_C_SINGLE,MAT_T_SINGLE,2,
                       dims,fdata,MEM_CONSERVE);
        matvar[2] = Mat_VarCreate("data",MAT_C_INT32,MAT_T_INT32,2,
                       dims,idata,MEM_CONSERVE);
        matvar[3] = substruct_matvar;
        matvar[4] = cell_matvar;
        dims[0] = 5;
        dims[1] = 1;
        cell_matvar = Mat_VarCreate("cell",MAT_C_CELL,MAT_T_CELL,2,
                            dims,matvar,0);

        Mat_VarWrite(mat,cell_matvar,COMPRESSION_ZLIB);

        free(matvar);
        Mat_VarFree(cell_matvar);
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
    size_t    dims[3] = {0,1,10};

    mat = Mat_CreateVer("test_write_null.mat",NULL,mat_file_ver);
    if ( mat != NULL ) {
        struct_fields[0] = Mat_VarCreate("d_null",MAT_C_DOUBLE,MAT_T_DOUBLE,3,
                            dims,NULL,0);
        Mat_VarWrite(mat,struct_fields[0],compression);
        struct_fields[1] = Mat_VarCreate("cd_null",MAT_C_DOUBLE,MAT_T_DOUBLE,3,
                            dims,NULL,MAT_F_COMPLEX);
        Mat_VarWrite(mat,struct_fields[1],compression);
        struct_fields[2] = Mat_VarCreate("char_null",MAT_C_CHAR,MAT_T_UINT8,2,
                            dims,NULL,0);
        Mat_VarWrite(mat,struct_fields[2],compression);
        struct_matvar = Mat_VarCreate("struct_null",MAT_C_STRUCT,MAT_T_STRUCT,2,
                            dims,NULL,0);
        Mat_VarWrite(mat,struct_matvar,compression);
        Mat_VarFree(struct_matvar);
        struct_matvar = Mat_VarCreate("struct_empty_with_fields",MAT_C_STRUCT,
                            MAT_T_STRUCT,3,dims,struct_fields,MEM_CONSERVE);
        Mat_VarWrite(mat,struct_matvar,compression);
        /* Reset data to NULL so the fields are not free'd */
        struct_matvar->data = NULL;
        Mat_VarFree(struct_matvar);
        dims[0] = 1;
        struct_matvar = Mat_VarCreate("struct_null_fields",MAT_C_STRUCT,
                            MAT_T_STRUCT,2,dims,struct_fields,MEM_CONSERVE);
        Mat_VarWrite(mat,struct_matvar,compression);
        /* Reset data to NULL so the fields are not free'd */
        struct_matvar->data = NULL;
        Mat_VarFree(struct_matvar);
        dims[0] = 0;
        cell_matvar = Mat_VarCreate("cell_null",MAT_C_CELL,MAT_T_CELL,2,
                            dims,NULL,MEM_CONSERVE);
        Mat_VarWrite(mat,cell_matvar,compression);
        Mat_VarFree(cell_matvar);

        dims[0] = 3;
        cell_matvar = Mat_VarCreate("cell_null_cells",MAT_C_CELL,MAT_T_CELL,2,
                            dims,struct_fields,MEM_CONSERVE);
        Mat_VarWrite(mat,cell_matvar,compression);
        /* Reset data to NULL so the cells are not free'd */
        cell_matvar->data = NULL;
        Mat_VarFree(cell_matvar);

        Mat_VarFree(struct_fields[0]);
        Mat_VarFree(struct_fields[1]);
        Mat_VarFree(struct_fields[2]);
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
    size_t     dims[2] = {6,10};
    int        start[2]={0,0},stride[2]={2,2},edge[2]={3,5};
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

    mat = Mat_CreateVer("test_mat_writeslab.mat",NULL,mat_file_ver);
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
    size_t     dims[2] = {5,5};
    double     data[25]={0.0,};
    double     zero = 0.0;
    mat_t    *mat;
    matvar_t *matvar;
    
    for ( i = 0; i < 25; i++ )
         data[i] = i+1;

    for ( i = 0; i < 25; i+= 6 )
        data[i] = 0.0/zero;

    mat = Mat_CreateVer("test_writenan.mat",NULL,mat_file_ver);
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
    size_t     dims[2] = {5,5};
    double     data[25]={0.0,};
    double     zero = 0.0;
    mat_t    *mat;
    matvar_t *matvar;
    
    for ( i = 0; i < 25; i++ )
         data[i] = i+1;

    for ( i = 0; i < 25; i+= 6 )
        data[i] = 1.0/zero;

    mat = Mat_CreateVer("test_writeinf.mat",NULL,mat_file_ver);
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
    int    err = 0, i;
    size_t dims[2] = {5,10};
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
test_write_compressed_sparse( void )
{
    int    err = 0, i;
    size_t dims[2] = {5,10};
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
    mat = Mat_Open("test_mat_write_compressed_sparse.mat",MAT_ACC_RDWR);
    if ( mat ) {
        matvar = Mat_VarCreate("sparse_matrix",MAT_C_SPARSE,
                       MAT_T_DOUBLE,2,dims,&sparse,MEM_CONSERVE);
        if ( matvar != NULL ) {
            Mat_VarWrite(mat,matvar,COMPRESSION_ZLIB);
            Mat_VarFree(matvar);
        } else {
            Mat_Critical("test_write_compressed_sparse: Couldn't create "
                         "matlab variable");
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
    int   c,i, k, err = 0, ntests = 0;
    mat_t *mat, *mat2;
    matvar_t *matvar, *matvar2, *matvar3;

    Mat_LogInit(prog_name);

    while ((c = getopt_long(argc,argv,optstring,options,NULL)) != EOF) {
        switch (c) {
            case 'v':
                if ( !strcmp(optarg,"5") ) {
                    mat_file_ver = MAT_FT_MAT5;
                } else if ( !strcmp(optarg,"7.3") ) {
                    mat_file_ver = MAT_FT_MAT73;
                } else {
                    fprintf(stderr,"Unrecognized MAT file version %s",argv[2]);
                    exit(EXIT_FAILURE);
                }
                break;
            case 'H':
                Mat_Help(helpstr);
                exit(EXIT_SUCCESS);
            case 'L':
                Mat_Help(helptestsstr);
                exit(EXIT_SUCCESS);
            case 'T':
                help_test(optarg);
                exit(EXIT_SUCCESS);
            case 'V':
                printf("%s %d.%d.%d\n"
                       "Written by Christopher Hulbert\n\n"
                       "Copyright(C) 2006-2008 Christopher C. Hulbert\n",
                       prog_name,MATIO_MAJOR_VERSION,MATIO_MINOR_VERSION,
                       MATIO_RELEASE_LEVEL);
                exit(EXIT_SUCCESS);
            case 'z':
                compression = COMPRESSION_ZLIB;
                break;
            case '?':
                exit(EXIT_FAILURE);
            default:
                printf("%c not a valid option\n", c);
                break;
        }
    }

    for ( k = optind; k < argc; ) {
        if ( !strcasecmp(argv[k],"copy") ) {
            k++;
            mat = Mat_CreateVer("test_mat_copy.mat",NULL,mat_file_ver);
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
        } else if ( !strcasecmp(argv[k],"write_compressed_cell") ) {
            k++;
            err += test_write_compressed_cell();
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
        } else if ( !strcasecmp(argv[k],"write_compressed_sparse") ) {
            k++;
            err += test_write_compressed_sparse();
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
