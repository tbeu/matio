/*
 * Copyright (C) 2005-2011   Christopher C. Hulbert
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

static const char *optstring = "c:o:v:HLT:Vz";
static struct option options[] = {
    {"class",       required_argument,NULL,'c'},
    {"output",      required_argument,NULL,'o'},
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
    "-c, --class c         Set variable class to 'c'",
    "-H, --help            This output",
    "-L, --list-tests      List of tests",
    "-o, --output filename Set the name of the output MAT file",
    "-T, --help-test TEST  help information on test TEST",
    "-v, --mat-version x   Set MAT file version to x (4, 5, 7.3)",
    "-V, --version         version information",
    "-z, --compress        Enable compression for MAT 5 files",
    "",
    "test        - name of the test to run",
    "TEST_OPTS   - If required, specify arguments to a test(See --help TEST)",
    "",
    "The classes recognized by the -c option are:",
    "  * double - Double precision floating point",
    "  * single - Single precision floating point",
    "  * int64  - 64-bit signed integer",
    "  * uint64 - 64-bit unsigned integer",
    "  * int32  - 32-bit signed integer",
    "  * uint32 - 32-bit unsigned integer",
    "  * int16  - 16-bit signed integer",
    "  * uint16 - 16-bit unsigned integer",
    "  * int8   - 8-bit signed integer",
    "  * uint8  - 8-bit unsigned integer",
    "",
    NULL
};

static const char *helptestsstr[] = {
"write_2d_numeric         - Write a real 2D numeric array to a matlab file.",
"                           The class of the numeric array is set by the -c",
"                           option or double if not set.",
"write_complex_2d_numeric - Write a complex 2D numeric array to a matlab file.",
"                           The class of the numeric array is set by the -c",
"                           option or double if not set.",
"write_sparse             - Write a real 2D sparse array to a matlab file.",
"                           The class of the numeric array is set by the -c",
"                           option or double if not set.",
"write_complex_sparse     - Write a complex 2D sparse array to a matlab file.",
"                           The class of the numeric array is set by the -c",
"                           option or double if not set.",
"write_empty_2d_numeric   - Write an empty 2D numeric array to a matlab file.",
"                           The class of the numeric array is set by the -c",
"                           option or double if not set.",
"",
"    Structure Variable Tests",
"================================================================",
"write_struct_2d_numeric         - Write a structure with real 2D numeric",
"                                  array to a matlab file. The class of the",
"                                  numeric array is set by the -c option or",
"                                  double if not set.",
"write_struct_complex_2d_numeric - Write a structure with complex 2D numeric",
"                                  array to a matlab file. The class of the",
"                                  numeric array is set by the -c option or",
"                                  double if not set.",
"write_empty_struct              - Write empty structure and structure with",
"                                  empty fields",
"",
"    Cell Array Variable Tests",
"================================================================",
"write_cell_2d_numeric         - Write a structure with real 2D numeric",
"                                array to a matlab file. The class of the",
"                                numeric array is set by the -c option or",
"                                double if not set.",
"write_cell_complex_2d_numeric - Write a structure with complex 2D numeric",
"                                array to a matlab file. The class of the",
"                                numeric array is set by the -c option or",
"                                double if not set.",
"write_empty_cell              - Write empty structure and structure with",
"                                empty fields",
"",
"    Character Variable Tests",
"================================================================",
"write_char               - Write a 2D character array.",
"",
"   Version 5 MAT File tests",
"================================================================",
"copy                    - Copies one matlab file to another",
"readvar                 - Reads a specific variable from a file",
"getstructfield          - Tests Mat_VarGetStructField getting fields from a",
"                          structure",
"readvarinfo             - Reads a variables header information only",
"readslab                - Tests reading a part of a dataset",
"writeinf                - Tests writing inf (Infinity) values",
"writenan                - Tests writing NaN (Not A Number) values",
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

static const char *helptest_write_2d_numeric[] = {
    "TEST: write_2d_numeric",
    "",
    "Usage: test_mat write_2d_numeric",
    "",
    "Writes a variable named a to a MAT file. The variable is a 2d real",
    "numeric array of dimensions 5x10 containing the numbers from 1 to 50.",
    "the class of the variable is double, or set by the -c option. The",
    "MAT file is the default file version, or set by the -v option. If the",
    "MAT file is version 5, compression can be enabled using the -z option",
    "if built with zlib library",
    "",
    "MATLAB code to generate expected data",
    "",
    "    classtype = 'double';",
    "    a = cast(reshape(1:50,5,10),classtype);",
    "",
    NULL
};

static const char *helptest_write_complex_2d_numeric[] = {
    "TEST: write_complex_2d_numeric",
    "",
    "Usage: test_mat write_complex_2d_numeric",
    "",
    "Writes a variable named a to a MAT file. The variable is a 2d complex",
    "numeric array of dimensions 5x10 containing the numbers from 1 to 50 in",
    "the real part, and the numbers 51:100 in the imaginary part. The class",
    "of the variable is double, or set by the -c option. The MAT file is the ",
    "default file version, or set by the -v option. If the MAT file is ",
    "version 5, compression can be enabled using the -z option if built with",
    "zlib library.",
    "",
    "MATLAB code to generate expected data",
    "",
    "    classtype = 'double';",
    "    a = cast(reshape((1:50) + j*(51:100),5,10),classtype);",
    "",
    NULL
};

static const char *helptest_write_sparse[] = {
    "TEST: write_sparse",
    "",
    "Usage: test_mat write_sparse",
    "",
    "Writes a variable named sparse_matrix to a MAT file. The variable is a 2d",
    "real sparse array of dimensions 5x10. The class of the variable is",
    "double. The MAT file is the default file version, or set by the -v",
    "option. If the MAT file is version 5, compression can be enabled using",
    "the -z option if built with zlib library.",
    "",
    "MATLAB code to generate expected data",
    "",
    "    sparse_matrix = zeros(5,10);",
    "    sparse_matrix(1:4:end,1:2:end) = 1;",
    "    sparse_matrix(2:4,2:2:end) = 1;",
    "    sparse_matrix = sparse_matrix.*reshape(1:50,5,10);",
    "    sparse_matrix = sparse(sparse_matrix);",
    "",
    NULL
};

static const char *helptest_write_complex_sparse[] = {
    "TEST: write_complex_sparse",
    "",
    "Usage: test_mat write_complex_sparse",
    "",
    "Writes a variable named sparse_matrix to a MAT file. The variable is a 2d",
    "complex sparse array of dimensions 5x10. The class of the variable is",
    "double. The MAT file is the default file version, or set by the -v",
    "option. If the MAT file is version 5, compression can be enabled using",
    "the -z option if built with zlib library.",
    "",
    "MATLAB code to generate expected data",
    "",
    "    sparse_matrix = zeros(5,10);",
    "    sparse_matrix(1:4:end,1:2:end) = 1;",
    "    sparse_matrix(2:4,2:2:end) = 1;",
    "    sparse_matrix = sparse_matrix.*reshape((1:50) + j*(51:100),5,10);",
    "    sparse_matrix = sparse(sparse_matrix);",
    "",
    NULL
};

static const char *helptest_write_empty_2d_numeric[] = {
    "TEST: write_empty_2d_numeric",
    "",
    "Usage: test_mat write_empty_2d_numeric",
    "",
    "Writes an empty array named a to a MAT file. The class of the variable",
    "is double, or set by the -c option. The MAT file is the default file",
    "version, or set by the -v option. If the MAT file is version 5,",
    "compression can be enabled using the -z option if built with zlib",
    "library.",
    "",
    "MATLAB code to generate expected data",
    "",
    "    classtype = 'double';",
    "    a = cast([],classtype);",
    "",
    NULL
};

static const char *helptest_write_char[] = {
    "TEST: write_char",
    "",
    "Usage: test_mat write_char",
    "",
    "Writes a variable named a to a MAT file. The variable is a 2d character",
    "array of dimensions 4x26. The MAT file is the default file version, or",
    "set by the -v option. If the MAT file is version 5, compression can be",
    "enabled using the -z option if built with zlib library",
    "",
    "MATLAB code to generate expected data",
    "",
    "    a = ['abcdefghijklmnopqrstuvwxyz';",
    "         'ABCDEFGHIJKLMNOPQRSTUVWXYZ';",
    "         '1234567890!@#$%^&*()-_=+`~';",
    "         '[{]}\\|;:''\",<.>/?          '];",
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

static const char *helptest_write_struct_2d_numeric[] = {
    "TEST: write_struct_2d_numeric",
    "",
    "Usage: test_mat write_struct_2d_numeric",
    "",
    "Writes a variable named a to a MAT file. The variable is a structure",
    "array with 2d real numeric array fields. The class of the variable is",
    "double, or set by the -c option. The MAT file is the default file",
    "version, or set by the -v option. If the MAT file is version 5,",
    "compression can be enabled using the -z option if built with zlib library",
    "",
    "MATLAB code to generate expected data",
    "",
    "    classtype = 'double';",
    "    a(1).field1 = cast(reshape(1:12,3,4),classtype);",
    "    a(1).field2 = cast(reshape(13:24,3,4),classtype);",
    "    a(2).field1 = cast(reshape(25:36,3,4),classtype);",
    "    a(2).field2 = cast(reshape(37:48,3,4),classtype);",
    "",
    NULL
};

static const char *helptest_write_struct_complex_2d_numeric[] = {
    "TEST: write_struct_complex_2d_numeric",
    "",
    "Usage: test_mat write_struct_complex_2d_numeric",
    "",
    "Writes a variable named a to a MAT file. The variable is a structure",
    "array with 2d complex numeric array fields. The class of the variable is",
    "double, or set by the -c option. The MAT file is the default file",
    "version, or set by the -v option. If the MAT file is version 5,",
    "compression can be enabled using the -z option if built with zlib library",
    "",
    "MATLAB code to generate expected data",
    "",
    "    classtype = 'double';",
    "    a(1).field1 = cast(reshape((1:12)+j*(51:62),3,4),classtype);",
    "    a(1).field2 = cast(reshape((13:24)+j*(63:74),3,4),classtype);",
    "    a(2).field1 = cast(reshape((25:36)+j*(75:86),3,4),classtype);",
    "    a(2).field2 = cast(reshape((37:48)+j*(87:98),3,4),classtype);",
    "",
    NULL
};

static const char *helptest_write_empty_struct[] = {
    "TEST: write_empty_struct",
    "",
    "Usage: test_mat write_empty_struct",
    "",
    "Writes an empty structure to the file test_write_empty_struct.mat",
    "The MAT file is the default file version, or set by the -v option. If",
    "the MAT file is version 5, compression can be enabled using the -z",
    "option if built with zlib library.",
    "",
    "MATLAB code to generate expected data",
    "",
    "    var1 = repmat(struct,0,1);",
    "    var2 = repmat(struct('field1',zeros(0,0),'field2',zeros(0,0)),0,1);",
    "    var3 = struct('field1',zeros(0,1),'field2',zeros(0,1));",
    "    var4(1).field1 = zeros(0,1);",
    "    var4(1).field2 = repmat(' ',0,1);",
    "    var4(2).field1 = repmat(struct,0,1);",
    "    var4(2).field2 = repmat({zeros(0,0)},0,1);",
    "",
    NULL
};

static const char *helptest_write_cell_2d_numeric[] = {
    "TEST: write_cell_2d_numeric",
    "",
    "Usage: test_mat write_cell_2d_numeric",
    "",
    "Writes a variable named a to a MAT file. The variable is a cell array",
    "with 2d real numeric array fields. The class of the variable is",
    "double, or set by the -c option. The MAT file is the default file",
    "version, or set by the -v option. If the MAT file is version 5,",
    "compression can be enabled using the -z option if built with zlib library",
    "",
    "MATLAB code to generate expected data",
    "",
    "    classtype = 'double';",
    "    a = {cast(reshape(1:12,3,4),classtype);",
    "         cast(reshape(13:24,3,4),classtype);",
    "         cast(reshape(25:36,3,4),classtype);",
    "         cast(reshape(37:48,3,4),classtype);}",
    "",
    NULL
};

static const char *helptest_write_cell_complex_2d_numeric[] = {
    "TEST: write_cell_complex_2d_numeric",
    "",
    "Usage: test_mat write_cell_complex_2d_numeric",
    "",
    "Writes a variable named a to a MAT file. The variable is a cell array",
    "with 2d complex numeric array fields. The class of the variable is",
    "double, or set by the -c option. The MAT file is the default file",
    "version, or set by the -v option. If the MAT file is version 5,",
    "compression can be enabled using the -z option if built with zlib library",
    "",
    "MATLAB code to generate expected data",
    "",
    "    classtype = 'double';",
    "    a = {cast(reshape((1:12)+j*(51:62),3,4),classtype);",
    "         cast(reshape((13:24)+j*(63:74),3,4),classtype);",
    "         cast(reshape((25:36)+j*(75:86),3,4),classtype);",
    "         cast(reshape((37:48)+j*(87:98),3,4),classtype);}",
    "",
    NULL
};

static const char *helptest_write_empty_cell[] = {
    "TEST: write_empty_cell",
    "",
    "Usage: test_mat write_empty_cell",
    "",
    "Writes an empty cell array to the file test_write_empty_cell.mat",
    "The MAT file is the default file version, or set by the -v option. If",
    "the MAT file is version 5, compression can be enabled using the -z",
    "option if built with zlib library.",
    "",
    "MATLAB code to generate expected data",
    "",
    "    var1 = cell(0,1);",
    "    var2 = {zeros(0,1);zeros(0,1)};",
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
    else if ( !strcmp(test,"write_2d_numeric") )
        Mat_Help(helptest_write_2d_numeric);
    else if ( !strcmp(test,"write_complex_2d_numeric") )
        Mat_Help(helptest_write_complex_2d_numeric);
    else if ( !strcmp(test,"write_sparse") )
        Mat_Help(helptest_write_sparse);
    else if ( !strcmp(test,"write_complex_sparse") )
        Mat_Help(helptest_write_complex_sparse);
    else if ( !strcmp(test,"write_empty_2d_numeric") )
        Mat_Help(helptest_write_empty_2d_numeric);
    else if ( !strcmp(test,"write_char") )
        Mat_Help(helptest_write_char);
    else if ( !strcmp(test,"write_struct_2d_numeric") )
        Mat_Help(helptest_write_struct_2d_numeric);
    else if ( !strcmp(test,"write_struct_complex_2d_numeric") )
        Mat_Help(helptest_write_struct_complex_2d_numeric);
    else if ( !strcmp(test,"write_empty_struct") )
        Mat_Help(helptest_write_empty_struct);
    else if ( !strcmp(test,"write_cell_2d_numeric") )
        Mat_Help(helptest_write_cell_2d_numeric);
    else if ( !strcmp(test,"write_cell_complex_2d_numeric") )
        Mat_Help(helptest_write_cell_complex_2d_numeric);
    else if ( !strcmp(test,"write_empty_cell") )
        Mat_Help(helptest_write_empty_cell);
    else if ( !strcmp(test,"writeinf") )
        Mat_Help(helptest_writeinf);
    else if ( !strcmp(test,"writenan") )
        Mat_Help(helptest_writenan);
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
test_write_2d_numeric(enum matio_classes matvar_class, char *output_name)
{
    size_t dims[2] = {5,10};
    int    err = 0, i;
    double    d[50];
    float     f[50];
    mat_int32_t   i32[50];
    mat_uint32_t ui32[50];
    mat_int16_t   i16[50];
    mat_uint16_t ui16[50];
    mat_int8_t    i8[50];
    mat_uint8_t  ui8[50];
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
       ui32[i] = i+1;
        i16[i] = i+1;
       ui16[i] = i+1;
         i8[i] = i+1;
        ui8[i] = i+1;
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

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( !mat ) {
        return 1;
    }

    switch (matvar_class) {
        case MAT_C_DOUBLE:
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_DOUBLE,2,dims,d,0);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
        case MAT_C_SINGLE:
             matvar = Mat_VarCreate("a",matvar_class,MAT_T_SINGLE,2,dims,f,0);
             Mat_VarWrite(mat,matvar,compression);
             Mat_VarFree(matvar);
            break;
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_INT64,2,dims,i64,0);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_UINT64,2,dims,ui64,0);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
#endif
        case MAT_C_INT32:
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_INT32,2,dims,i32,0);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
        case MAT_C_UINT32:
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_UINT32,2,dims,ui32,0);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
        case MAT_C_INT16:
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_INT16,2,dims,i16,0);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
        case MAT_C_UINT16:
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_UINT16,2,dims,ui16,0);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
        case MAT_C_INT8:
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_INT8,2,dims,i8,0);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
        case MAT_C_UINT8:
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_UINT8,2,dims,ui8,0);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
    }

    Mat_Close(mat);

    return err;
}

static int
test_write_complex_2d_numeric(enum matio_classes matvar_class,char *output_name)
{
    size_t dims[2] = {5,10};
    int    err = 0, i;
    double    d_real[50], d_imag[50];
    float     f_real[50], f_imag[50];
    mat_int32_t   i32_real[50], i32_imag[50];
    mat_uint32_t ui32_real[50], ui32_imag[50];
    mat_int16_t   i16_real[50], i16_imag[50];
    mat_uint16_t ui16_real[50], ui16_imag[50];
    mat_int8_t    i8_real[50], i8_imag[50];
    mat_uint8_t  ui8_real[50], ui8_imag[50];
#ifdef HAVE_MAT_INT64_T
    mat_int64_t i64_real[50], i64_imag[50];
#endif
#ifdef HAVE_MAT_UINT64_T
    mat_uint64_t ui64_real[50], ui64_imag[50];
#endif
    struct ComplexSplit z = {NULL,NULL};
    mat_t *mat;
    matvar_t *matvar;

    for ( i = 0; i < 50; i++ ) {
          d_real[i] = i+1;
          d_imag[i] = i+51;
          f_real[i] = i+1;
          f_imag[i] = i+51;
        i32_real[i] = i+1;
        i32_imag[i] = i+51;
       ui32_real[i] = i+1;
       ui32_imag[i] = i+51;
        i16_real[i] = i+1;
        i16_imag[i] = i+51;
       ui16_real[i] = i+1;
       ui16_imag[i] = i+51;
         i8_real[i] = i+1;
         i8_imag[i] = i+51;
        ui8_real[i] = i+1;
        ui8_imag[i] = i+51;
#ifdef HAVE_MAT_INT64_T
        i64_real[i] = i+1;
        i64_imag[i] = i+51;
#endif
#ifdef HAVE_MAT_UINT64_T
        ui64_real[i] = i+1;
        ui64_imag[i] = i+51;
#endif
    }

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( !mat ) {
        return 1;
    }

    switch (matvar_class) {
        case MAT_C_DOUBLE:
            z.Re = d_real;
            z.Im = d_imag;
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_DOUBLE,2,dims,&z,
                                   MAT_F_COMPLEX);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
        case MAT_C_SINGLE:
            z.Re = f_real;
            z.Im = f_imag;
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_SINGLE,2,dims,&z,
                                   MAT_F_COMPLEX);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
            z.Re = i64_real;
            z.Im = i64_imag;
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_INT64,2,dims,&z,
                                   MAT_F_COMPLEX);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
            z.Re = ui64_real;
            z.Im = ui64_imag;
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_UINT64,2,dims,&z,
                                   MAT_F_COMPLEX);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
#endif
        case MAT_C_INT32:
            z.Re = i32_real;
            z.Im = i32_imag;
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_INT32,2,dims,&z,
                                   MAT_F_COMPLEX);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
        case MAT_C_UINT32:
            z.Re = ui32_real;
            z.Im = ui32_imag;
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_UINT32,2,dims,&z,
                                   MAT_F_COMPLEX);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
        case MAT_C_INT16:
            z.Re = i16_real;
            z.Im = i16_imag;
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_INT16,2,dims,&z,
                                   MAT_F_COMPLEX);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
        case MAT_C_UINT16:
            z.Re = ui16_real;
            z.Im = ui16_imag;
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_UINT16,2,dims,&z,
                                   MAT_F_COMPLEX);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
        case MAT_C_INT8:
            z.Re = i8_real;
            z.Im = i8_imag;
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_INT8,2,dims,&z,
                                   MAT_F_COMPLEX);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
        case MAT_C_UINT8:
            z.Re = ui8_real;
            z.Im = ui8_imag;
            matvar = Mat_VarCreate("a",matvar_class,MAT_T_UINT8,2,dims,&z,
                                   MAT_F_COMPLEX);
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
            break;
    }

    Mat_Close(mat);

    return err;
}

static int
test_write_empty_2d_numeric(enum matio_classes matvar_class,char *output_name)
{
    int       err = 0;
    mat_t    *mat;
    matvar_t *matvar;
    size_t    dims[2] = {0,10};
    enum matio_types matvar_datatype = MAT_T_UNKNOWN;

    switch ( matvar_class ) {
        case MAT_C_DOUBLE:
            matvar_datatype = MAT_T_DOUBLE;
            break;
        case MAT_C_SINGLE:
            matvar_datatype = MAT_T_SINGLE;
            break;
        case MAT_C_INT64:
            matvar_datatype = MAT_T_INT64;
            break;
        case MAT_C_UINT64:
            matvar_datatype = MAT_T_UINT64;
            break;
        case MAT_C_INT32:
            matvar_datatype = MAT_T_INT32;
            break;
        case MAT_C_UINT32:
            matvar_datatype = MAT_T_UINT32;
            break;
        case MAT_C_INT16:
            matvar_datatype = MAT_T_INT16;
            break;
        case MAT_C_UINT16:
            matvar_datatype = MAT_T_UINT16;
            break;
        case MAT_C_INT8:
            matvar_datatype = MAT_T_INT8;
            break;
        case MAT_C_UINT8:
            matvar_datatype = MAT_T_UINT8;
            break;
    }

    mat = Mat_CreateVer("test_write_empty_2d_numeric.mat",NULL,mat_file_ver);
    if ( mat != NULL ) {
        matvar = Mat_VarCreate("empty",matvar_class,matvar_datatype,2,dims,
                     NULL,0);
        Mat_VarWrite(mat,matvar,compression);
        Mat_VarFree(matvar);
        Mat_Close(mat);
    } else {
        err = 1;
    }

    return err;
}

static int
test_write_char(char *output_name)
{
    char     *str = "aA1[bB2{cC3]dD4}eE5\\fF6|gG7;hH8:iI9'jJ0\"kK!,lL@<"
                    "mM#.nN$>oO%/pP^?qQ& rR* sS( tT) uU- vV_ wW= xX+ yY` zZ~ ";
    int       err = 0, i;
    size_t    dims[2];
    mat_t    *mat;
    matvar_t *matvar;

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( mat ) {
        dims[0]   = 4;
        dims[1]   = 26;
        matvar = Mat_VarCreate("a",MAT_C_CHAR,MAT_T_UINT8,2,
                    dims,str,MEM_CONSERVE);
        Mat_VarWrite(mat,matvar,compression);
        Mat_Close(mat);
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
test_write_empty_struct(char *output_name)
{
    size_t  dims[2] = {0,0};
    int    err = 0, i;
    mat_t     *mat;
    matvar_t *matvar[5], *struct_matvar, *substruct_matvar;

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( mat ) {
        /* Write an empty structure with no fields */
        matvar[0] = NULL;
        dims[0] = 0;
        dims[1] = 1;
        struct_matvar = Mat_VarCreate("var1",MAT_C_STRUCT,MAT_T_STRUCT,
                                      2,dims,matvar,0);
        Mat_VarWrite(mat,struct_matvar,compression);
        Mat_VarFree(struct_matvar);

        /* Write empty structure with 2 fields */
        matvar[0] = Mat_VarCreate("field1",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,NULL,0);
        matvar[1] = Mat_VarCreate("field2",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,NULL,0);
        matvar[2] = NULL;
        dims[0] = 0;
        dims[1] = 1;
        struct_matvar = Mat_VarCreate("var2",MAT_C_STRUCT,MAT_T_STRUCT,
                                      2,dims,matvar,0);
        Mat_VarWrite(mat,struct_matvar,compression);
        Mat_VarFree(struct_matvar);

        /* Write scalar structure with empty fields */
        matvar[0] = Mat_VarCreate("field1",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,NULL,0);
        matvar[1] = Mat_VarCreate("field2",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,NULL,0);
        matvar[2] = NULL;
        dims[0] = 1;
        dims[1] = 1;
        struct_matvar = Mat_VarCreate("var3",MAT_C_STRUCT,MAT_T_STRUCT,
                                      2,dims,matvar,0);
        Mat_VarWrite(mat,struct_matvar,compression);
        Mat_VarFree(struct_matvar);

        /* Write scalar structure with empty fields */
        dims[0] = 0;
        dims[1] = 1;
        matvar[0] = Mat_VarCreate("field1",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,NULL,0);
        matvar[1] = Mat_VarCreate("field2",MAT_C_CHAR,MAT_T_UINT8,2,
                       dims,NULL,0);
        matvar[2] = Mat_VarCreate("field1",MAT_C_STRUCT,MAT_T_STRUCT,2,
                       dims,NULL,0);
        matvar[3] = Mat_VarCreate("field2",MAT_C_CELL,MAT_T_CELL,2,
                       dims,NULL,0);
        matvar[4] = NULL;
        dims[0] = 2;
        dims[1] = 1;
        struct_matvar = Mat_VarCreate("var4",MAT_C_STRUCT,MAT_T_STRUCT,
                                      2,dims,matvar,0);
        Mat_VarWrite(mat,struct_matvar,compression);
        Mat_VarFree(struct_matvar);

        Mat_Close(mat);
    }
    return err;
}

static int
test_write_struct_2d_numeric(enum matio_classes matvar_class,
    char *output_name)
{
    size_t dims[2] = {5,10};
    int    err = 0, i;
    double    d[50];
    float     f[50];
    mat_int32_t   i32[50];
    mat_uint32_t ui32[50];
    mat_int16_t   i16[50];
    mat_uint16_t ui16[50];
    mat_int8_t    i8[50];
    mat_uint8_t  ui8[50];
#ifdef HAVE_MAT_INT64_T
    mat_int64_t i64[50];
#endif
#ifdef HAVE_MAT_UINT64_T
    mat_uint64_t ui64[50];
#endif
    void *data[4] = {NULL,NULL,NULL,NULL};
    mat_t *mat;
    matvar_t *matvar[5], *struct_matvar;
    enum matio_types data_type;

    for ( i = 0; i < 50; i++ ) {
          d[i] = i+1;
          f[i] = i+1;
        i32[i] = i+1;
       ui32[i] = i+1;
        i16[i] = i+1;
       ui16[i] = i+1;
         i8[i] = i+1;
        ui8[i] = i+1;
#ifdef HAVE_MAT_INT64_T
        i64[i] = i+1;
#endif
#ifdef HAVE_MAT_UINT64_T
        ui64[i] = i+1;
#endif
    }

    switch (matvar_class) {
        case MAT_C_DOUBLE:
            data[0] = d;
            data[1] = d+12;
            data[2] = d+24;
            data[3] = d+36;
            data_type = MAT_T_DOUBLE;
            break;
        case MAT_C_SINGLE:
            data[0] = f;
            data[1] = f+12;
            data[2] = f+24;
            data[3] = f+36;
            data_type = MAT_T_SINGLE;
            break;
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
            data[0] = i64;
            data[1] = i64+12;
            data[2] = i64+24;
            data[3] = i64+36;
            data_type = MAT_T_INT64;
            break;
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
            data[0] = ui64;
            data[1] = ui64+12;
            data[2] = ui64+24;
            data[3] = ui64+36;
            data_type = MAT_T_UINT64;
            break;
#endif
        case MAT_C_INT32:
            data[0] = i32;
            data[1] = i32+12;
            data[2] = i32+24;
            data[3] = i32+36;
            data_type = MAT_T_INT32;
            break;
        case MAT_C_UINT32:
            data[0] = ui32;
            data[1] = ui32+12;
            data[2] = ui32+24;
            data[3] = ui32+36;
            data_type = MAT_T_UINT32;
            break;
        case MAT_C_INT16:
            data[0] = i16;
            data[1] = i16+12;
            data[2] = i16+24;
            data[3] = i16+36;
            data_type = MAT_T_INT16;
            break;
        case MAT_C_UINT16:
            data[0] = ui16;
            data[1] = ui16+12;
            data[2] = ui16+24;
            data[3] = ui16+36;
            data_type = MAT_T_UINT16;
            break;
        case MAT_C_INT8:
            data[0] = i8;
            data[1] = i8+12;
            data[2] = i8+24;
            data[3] = i8+36;
            data_type = MAT_T_INT8;
            break;
        case MAT_C_UINT8:
            data[0] = ui8;
            data[1] = ui8+12;
            data[2] = ui8+24;
            data[3] = ui8+36;
            data_type = MAT_T_UINT8;
            break;
    }

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( !mat ) {
        return 1;
    }

    dims[0] = 3;
    dims[1] = 4;
    matvar[0] = Mat_VarCreate("field1",matvar_class,data_type,2,
                   dims,data[0],MEM_CONSERVE);
    matvar[1] = Mat_VarCreate("field2",matvar_class,data_type,2,
                   dims,data[1],MEM_CONSERVE);
    matvar[2] = Mat_VarCreate("field1",matvar_class,data_type,2,
                   dims,data[2],MEM_CONSERVE);
    matvar[3] = Mat_VarCreate("field2",matvar_class,data_type,2,
                   dims,data[3],MEM_CONSERVE);
    matvar[4] = NULL;
    dims[0] = 2;
    dims[1] = 1;
    struct_matvar = Mat_VarCreate("a",MAT_C_STRUCT,MAT_T_STRUCT,2,dims,
                                  matvar,0);
    Mat_VarWrite(mat,struct_matvar,compression);
    Mat_VarFree(struct_matvar);

    Mat_Close(mat);

    return err;
}

static int
test_write_struct_complex_2d_numeric(enum matio_classes matvar_class,
    char *output_name)
{
    size_t dims[2] = {5,10};
    int    err = 0, i;
    double    d_real[50], d_imag[50];
    float     f_real[50], f_imag[50];
    mat_int32_t   i32_real[50], i32_imag[50];
    mat_uint32_t ui32_real[50], ui32_imag[50];
    mat_int16_t   i16_real[50], i16_imag[50];
    mat_uint16_t ui16_real[50], ui16_imag[50];
    mat_int8_t    i8_real[50], i8_imag[50];
    mat_uint8_t  ui8_real[50], ui8_imag[50];
#ifdef HAVE_MAT_INT64_T
    mat_int64_t i64_real[50], i64_imag[50];
#endif
#ifdef HAVE_MAT_UINT64_T
    mat_uint64_t ui64_real[50], ui64_imag[50];
#endif
    struct ComplexSplit data[4] = {NULL,NULL};
    mat_t *mat;
    matvar_t *matvar[5], *struct_matvar;
    enum matio_types data_type;

    for ( i = 0; i < 50; i++ ) {
          d_real[i] = i+1;
          d_imag[i] = i+51;
          f_real[i] = i+1;
          f_imag[i] = i+51;
        i32_real[i] = i+1;
        i32_imag[i] = i+51;
       ui32_real[i] = i+1;
       ui32_imag[i] = i+51;
        i16_real[i] = i+1;
        i16_imag[i] = i+51;
       ui16_real[i] = i+1;
       ui16_imag[i] = i+51;
         i8_real[i] = i+1;
         i8_imag[i] = i+51;
        ui8_real[i] = i+1;
        ui8_imag[i] = i+51;
#ifdef HAVE_MAT_INT64_T
        i64_real[i] = i+1;
        i64_imag[i] = i+51;
#endif
#ifdef HAVE_MAT_UINT64_T
        ui64_real[i] = i+1;
        ui64_imag[i] = i+51;
#endif
    }

    switch (matvar_class) {
        case MAT_C_DOUBLE:
            data[0].Re = d_real;
            data[0].Im = d_imag;
            data[1].Re = d_real+12;
            data[1].Im = d_imag+12;
            data[2].Re = d_real+24;
            data[2].Im = d_imag+24;
            data[3].Re = d_real+36;
            data[3].Im = d_imag+36;
            data_type = MAT_T_DOUBLE;
            break;
        case MAT_C_SINGLE:
            data[0].Re = f_real;
            data[0].Im = f_imag;
            data[1].Re = f_real+12;
            data[1].Im = f_imag+12;
            data[2].Re = f_real+24;
            data[2].Im = f_imag+24;
            data[3].Re = f_real+36;
            data[3].Im = f_imag+36;
            data_type = MAT_T_SINGLE;
            break;
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
            data[0].Re = i64_real;
            data[0].Im = i64_imag;
            data[1].Re = i64_real+12;
            data[1].Im = i64_imag+12;
            data[2].Re = i64_real+24;
            data[2].Im = i64_imag+24;
            data[3].Re = i64_real+36;
            data[3].Im = i64_imag+36;
            data_type = MAT_T_INT64;
            break;
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
            data[0].Re = ui64_real;
            data[0].Im = ui64_imag;
            data[1].Re = ui64_real+12;
            data[1].Im = ui64_imag+12;
            data[2].Re = ui64_real+24;
            data[2].Im = ui64_imag+24;
            data[3].Re = ui64_real+36;
            data[3].Im = ui64_imag+36;
            data_type = MAT_T_UINT64;
            break;
#endif
        case MAT_C_INT32:
            data[0].Re = i32_real;
            data[0].Im = i32_imag;
            data[1].Re = i32_real+12;
            data[1].Im = i32_imag+12;
            data[2].Re = i32_real+24;
            data[2].Im = i32_imag+24;
            data[3].Re = i32_real+36;
            data[3].Im = i32_imag+36;
            data_type = MAT_T_INT32;
            break;
        case MAT_C_UINT32:
            data[0].Re = ui32_real;
            data[0].Im = ui32_imag;
            data[1].Re = ui32_real+12;
            data[1].Im = ui32_imag+12;
            data[2].Re = ui32_real+24;
            data[2].Im = ui32_imag+24;
            data[3].Re = ui32_real+36;
            data[3].Im = ui32_imag+36;
            data_type = MAT_T_UINT32;
            break;
        case MAT_C_INT16:
            data[0].Re = i16_real;
            data[0].Im = i16_imag;
            data[1].Re = i16_real+12;
            data[1].Im = i16_imag+12;
            data[2].Re = i16_real+24;
            data[2].Im = i16_imag+24;
            data[3].Re = i16_real+36;
            data[3].Im = i16_imag+36;
            data_type = MAT_T_INT16;
            break;
        case MAT_C_UINT16:
            data[0].Re = ui16_real;
            data[0].Im = ui16_imag;
            data[1].Re = ui16_real+12;
            data[1].Im = ui16_imag+12;
            data[2].Re = ui16_real+24;
            data[2].Im = ui16_imag+24;
            data[3].Re = ui16_real+36;
            data[3].Im = ui16_imag+36;
            data_type = MAT_T_UINT16;
            break;
        case MAT_C_INT8:
            data[0].Re = i8_real;
            data[0].Im = i8_imag;
            data[1].Re = i8_real+12;
            data[1].Im = i8_imag+12;
            data[2].Re = i8_real+24;
            data[2].Im = i8_imag+24;
            data[3].Re = i8_real+36;
            data[3].Im = i8_imag+36;
            data_type = MAT_T_INT8;
            break;
        case MAT_C_UINT8:
            data[0].Re = ui8_real;
            data[0].Im = ui8_imag;
            data[1].Re = ui8_real+12;
            data[1].Im = ui8_imag+12;
            data[2].Re = ui8_real+24;
            data[2].Im = ui8_imag+24;
            data[3].Re = ui8_real+36;
            data[3].Im = ui8_imag+36;
            data_type = MAT_T_UINT8;
            break;
    }

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( !mat ) {
        return 1;
    }

    dims[0] = 3;
    dims[1] = 4;
    matvar[0] = Mat_VarCreate("field1",matvar_class,data_type,2,
                   dims,data,MEM_CONSERVE | MAT_F_COMPLEX);
    matvar[1] = Mat_VarCreate("field2",matvar_class,data_type,2,
                   dims,data+1,MEM_CONSERVE | MAT_F_COMPLEX);
    matvar[2] = Mat_VarCreate("field1",matvar_class,data_type,2,
                   dims,data+2,MEM_CONSERVE | MAT_F_COMPLEX);
    matvar[3] = Mat_VarCreate("field2",matvar_class,data_type,2,
                   dims,data+3,MEM_CONSERVE | MAT_F_COMPLEX);
    matvar[4] = NULL;
    dims[0] = 2;
    dims[1] = 1;
    struct_matvar = Mat_VarCreate("a",MAT_C_STRUCT,MAT_T_STRUCT,2,dims,
                                  matvar,0);
    Mat_VarWrite(mat,struct_matvar,compression);
    Mat_VarFree(struct_matvar);

    Mat_Close(mat);

    return err;
}

static int
test_write_empty_cell(char *output_name)
{
    size_t  dims[2] = {0,0};
    int    err = 0, i;
    mat_t     *mat;
    matvar_t *matvar[5], *cell_matvar, *struct_matvar, *substruct_matvar;

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( mat ) {
        /* Write an empty cell */
        matvar[0] = NULL;
        dims[0] = 0;
        dims[1] = 1;
        cell_matvar = Mat_VarCreate("var1",MAT_C_CELL,MAT_T_CELL,2,dims,NULL,0);
        Mat_VarWrite(mat,cell_matvar,compression);
        Mat_VarFree(cell_matvar);

        /* Write cell with empty element */
        matvar[0] = Mat_VarCreate("field1",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,NULL,0);
        matvar[1] = Mat_VarCreate("field2",MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                       dims,NULL,0);
        matvar[2] = NULL;
        dims[0] = 2;
        dims[1] = 1;
        cell_matvar = Mat_VarCreate("var2",MAT_C_CELL,MAT_T_CELL,2,dims,
                                    matvar,0);
        Mat_VarWrite(mat,cell_matvar,compression);
        Mat_VarFree(cell_matvar);

        Mat_Close(mat);
    }
    return err;
}

static int
test_write_cell_2d_numeric(enum matio_classes matvar_class,
    char *output_name)
{
    size_t dims[2] = {5,10};
    int    err = 0, i;
    double    d[50];
    float     f[50];
    mat_int32_t   i32[50];
    mat_uint32_t ui32[50];
    mat_int16_t   i16[50];
    mat_uint16_t ui16[50];
    mat_int8_t    i8[50];
    mat_uint8_t  ui8[50];
#ifdef HAVE_MAT_INT64_T
    mat_int64_t i64[50];
#endif
#ifdef HAVE_MAT_UINT64_T
    mat_uint64_t ui64[50];
#endif
    void *data[4] = {NULL,NULL,NULL,NULL};
    mat_t *mat;
    matvar_t *matvar[5], *cell_matvar;
    enum matio_types data_type;

    for ( i = 0; i < 50; i++ ) {
          d[i] = i+1;
          f[i] = i+1;
        i32[i] = i+1;
       ui32[i] = i+1;
        i16[i] = i+1;
       ui16[i] = i+1;
         i8[i] = i+1;
        ui8[i] = i+1;
#ifdef HAVE_MAT_INT64_T
        i64[i] = i+1;
#endif
#ifdef HAVE_MAT_UINT64_T
        ui64[i] = i+1;
#endif
    }

    switch (matvar_class) {
        case MAT_C_DOUBLE:
            data[0] = d;
            data[1] = d+12;
            data[2] = d+24;
            data[3] = d+36;
            data_type = MAT_T_DOUBLE;
            break;
        case MAT_C_SINGLE:
            data[0] = f;
            data[1] = f+12;
            data[2] = f+24;
            data[3] = f+36;
            data_type = MAT_T_SINGLE;
            break;
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
            data[0] = i64;
            data[1] = i64+12;
            data[2] = i64+24;
            data[3] = i64+36;
            data_type = MAT_T_INT64;
            break;
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
            data[0] = ui64;
            data[1] = ui64+12;
            data[2] = ui64+24;
            data[3] = ui64+36;
            data_type = MAT_T_UINT64;
            break;
#endif
        case MAT_C_INT32:
            data[0] = i32;
            data[1] = i32+12;
            data[2] = i32+24;
            data[3] = i32+36;
            data_type = MAT_T_INT32;
            break;
        case MAT_C_UINT32:
            data[0] = ui32;
            data[1] = ui32+12;
            data[2] = ui32+24;
            data[3] = ui32+36;
            data_type = MAT_T_UINT32;
            break;
        case MAT_C_INT16:
            data[0] = i16;
            data[1] = i16+12;
            data[2] = i16+24;
            data[3] = i16+36;
            data_type = MAT_T_INT16;
            break;
        case MAT_C_UINT16:
            data[0] = ui16;
            data[1] = ui16+12;
            data[2] = ui16+24;
            data[3] = ui16+36;
            data_type = MAT_T_UINT16;
            break;
        case MAT_C_INT8:
            data[0] = i8;
            data[1] = i8+12;
            data[2] = i8+24;
            data[3] = i8+36;
            data_type = MAT_T_INT8;
            break;
        case MAT_C_UINT8:
            data[0] = ui8;
            data[1] = ui8+12;
            data[2] = ui8+24;
            data[3] = ui8+36;
            data_type = MAT_T_UINT8;
            break;
    }

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( !mat ) {
        return 1;
    }

    dims[0] = 3;
    dims[1] = 4;
    matvar[0] = Mat_VarCreate(NULL,matvar_class,data_type,2,
                   dims,data[0],MEM_CONSERVE);
    matvar[1] = Mat_VarCreate(NULL,matvar_class,data_type,2,
                   dims,data[1],MEM_CONSERVE);
    matvar[2] = Mat_VarCreate(NULL,matvar_class,data_type,2,
                   dims,data[2],MEM_CONSERVE);
    matvar[3] = Mat_VarCreate(NULL,matvar_class,data_type,2,
                   dims,data[3],MEM_CONSERVE);
    matvar[4] = NULL;
    dims[0] = 4;
    dims[1] = 1;
    cell_matvar = Mat_VarCreate("a",MAT_C_CELL,MAT_T_CELL,2,dims,
                                  matvar,0);
    Mat_VarWrite(mat,cell_matvar,compression);
    Mat_VarFree(cell_matvar);

    Mat_Close(mat);

    return err;
}

static int
test_write_cell_complex_2d_numeric(enum matio_classes matvar_class,
    char *output_name)
{
    size_t dims[2] = {5,10};
    int    err = 0, i;
    double    d_real[50], d_imag[50];
    float     f_real[50], f_imag[50];
    mat_int32_t   i32_real[50], i32_imag[50];
    mat_uint32_t ui32_real[50], ui32_imag[50];
    mat_int16_t   i16_real[50], i16_imag[50];
    mat_uint16_t ui16_real[50], ui16_imag[50];
    mat_int8_t    i8_real[50], i8_imag[50];
    mat_uint8_t  ui8_real[50], ui8_imag[50];
#ifdef HAVE_MAT_INT64_T
    mat_int64_t i64_real[50], i64_imag[50];
#endif
#ifdef HAVE_MAT_UINT64_T
    mat_uint64_t ui64_real[50], ui64_imag[50];
#endif
    struct ComplexSplit data[4] = {NULL,NULL};
    mat_t *mat;
    matvar_t *matvar[5], *cell_matvar;
    enum matio_types data_type;

    for ( i = 0; i < 50; i++ ) {
          d_real[i] = i+1;
          d_imag[i] = i+51;
          f_real[i] = i+1;
          f_imag[i] = i+51;
        i32_real[i] = i+1;
        i32_imag[i] = i+51;
       ui32_real[i] = i+1;
       ui32_imag[i] = i+51;
        i16_real[i] = i+1;
        i16_imag[i] = i+51;
       ui16_real[i] = i+1;
       ui16_imag[i] = i+51;
         i8_real[i] = i+1;
         i8_imag[i] = i+51;
        ui8_real[i] = i+1;
        ui8_imag[i] = i+51;
#ifdef HAVE_MAT_INT64_T
        i64_real[i] = i+1;
        i64_imag[i] = i+51;
#endif
#ifdef HAVE_MAT_UINT64_T
        ui64_real[i] = i+1;
        ui64_imag[i] = i+51;
#endif
    }

    switch (matvar_class) {
        case MAT_C_DOUBLE:
            data[0].Re = d_real;
            data[0].Im = d_imag;
            data[1].Re = d_real+12;
            data[1].Im = d_imag+12;
            data[2].Re = d_real+24;
            data[2].Im = d_imag+24;
            data[3].Re = d_real+36;
            data[3].Im = d_imag+36;
            data_type = MAT_T_DOUBLE;
            break;
        case MAT_C_SINGLE:
            data[0].Re = f_real;
            data[0].Im = f_imag;
            data[1].Re = f_real+12;
            data[1].Im = f_imag+12;
            data[2].Re = f_real+24;
            data[2].Im = f_imag+24;
            data[3].Re = f_real+36;
            data[3].Im = f_imag+36;
            data_type = MAT_T_SINGLE;
            break;
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
            data[0].Re = i64_real;
            data[0].Im = i64_imag;
            data[1].Re = i64_real+12;
            data[1].Im = i64_imag+12;
            data[2].Re = i64_real+24;
            data[2].Im = i64_imag+24;
            data[3].Re = i64_real+36;
            data[3].Im = i64_imag+36;
            data_type = MAT_T_INT64;
            break;
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
            data[0].Re = ui64_real;
            data[0].Im = ui64_imag;
            data[1].Re = ui64_real+12;
            data[1].Im = ui64_imag+12;
            data[2].Re = ui64_real+24;
            data[2].Im = ui64_imag+24;
            data[3].Re = ui64_real+36;
            data[3].Im = ui64_imag+36;
            data_type = MAT_T_UINT64;
            break;
#endif
        case MAT_C_INT32:
            data[0].Re = i32_real;
            data[0].Im = i32_imag;
            data[1].Re = i32_real+12;
            data[1].Im = i32_imag+12;
            data[2].Re = i32_real+24;
            data[2].Im = i32_imag+24;
            data[3].Re = i32_real+36;
            data[3].Im = i32_imag+36;
            data_type = MAT_T_INT32;
            break;
        case MAT_C_UINT32:
            data[0].Re = ui32_real;
            data[0].Im = ui32_imag;
            data[1].Re = ui32_real+12;
            data[1].Im = ui32_imag+12;
            data[2].Re = ui32_real+24;
            data[2].Im = ui32_imag+24;
            data[3].Re = ui32_real+36;
            data[3].Im = ui32_imag+36;
            data_type = MAT_T_UINT32;
            break;
        case MAT_C_INT16:
            data[0].Re = i16_real;
            data[0].Im = i16_imag;
            data[1].Re = i16_real+12;
            data[1].Im = i16_imag+12;
            data[2].Re = i16_real+24;
            data[2].Im = i16_imag+24;
            data[3].Re = i16_real+36;
            data[3].Im = i16_imag+36;
            data_type = MAT_T_INT16;
            break;
        case MAT_C_UINT16:
            data[0].Re = ui16_real;
            data[0].Im = ui16_imag;
            data[1].Re = ui16_real+12;
            data[1].Im = ui16_imag+12;
            data[2].Re = ui16_real+24;
            data[2].Im = ui16_imag+24;
            data[3].Re = ui16_real+36;
            data[3].Im = ui16_imag+36;
            data_type = MAT_T_UINT16;
            break;
        case MAT_C_INT8:
            data[0].Re = i8_real;
            data[0].Im = i8_imag;
            data[1].Re = i8_real+12;
            data[1].Im = i8_imag+12;
            data[2].Re = i8_real+24;
            data[2].Im = i8_imag+24;
            data[3].Re = i8_real+36;
            data[3].Im = i8_imag+36;
            data_type = MAT_T_INT8;
            break;
        case MAT_C_UINT8:
            data[0].Re = ui8_real;
            data[0].Im = ui8_imag;
            data[1].Re = ui8_real+12;
            data[1].Im = ui8_imag+12;
            data[2].Re = ui8_real+24;
            data[2].Im = ui8_imag+24;
            data[3].Re = ui8_real+36;
            data[3].Im = ui8_imag+36;
            data_type = MAT_T_UINT8;
            break;
    }

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( !mat ) {
        return 1;
    }

    dims[0] = 3;
    dims[1] = 4;
    matvar[0] = Mat_VarCreate(NULL,matvar_class,data_type,2,
                   dims,data,MEM_CONSERVE | MAT_F_COMPLEX);
    matvar[1] = Mat_VarCreate(NULL,matvar_class,data_type,2,
                   dims,data+1,MEM_CONSERVE | MAT_F_COMPLEX);
    matvar[2] = Mat_VarCreate(NULL,matvar_class,data_type,2,
                   dims,data+2,MEM_CONSERVE | MAT_F_COMPLEX);
    matvar[3] = Mat_VarCreate(NULL,matvar_class,data_type,2,
                   dims,data+3,MEM_CONSERVE | MAT_F_COMPLEX);
    matvar[4] = NULL;
    dims[0] = 4;
    dims[1] = 1;
    cell_matvar = Mat_VarCreate("a",MAT_C_CELL,MAT_T_CELL,2,dims,matvar,0);
    Mat_VarWrite(mat,cell_matvar,compression);
    Mat_VarFree(cell_matvar);

    Mat_Close(mat);

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
test_write_sparse(enum matio_classes matvar_class,char *output_name)
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
    sparse.data  = NULL;

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( !mat )
        return 1;

    switch (matvar_class) {
        case MAT_C_DOUBLE:
            sparse.data  = d;
            break;
    }

    if ( NULL != sparse.data) {
        matvar = Mat_VarCreate("sparse_matrix",MAT_C_SPARSE,
                       MAT_T_DOUBLE,2,dims,&sparse,MEM_CONSERVE);
        if ( matvar != NULL ) {
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
        } else {
            Mat_Critical("test_writesparse: Couldn't create matlab variable");
            err = 1;
        }
    } else {
        err = 1;
    }

    Mat_Close(mat);

    return err;
}

static int
test_write_complex_sparse(enum matio_classes matvar_class,char *output_name)
{
    int    err = 0, i;
    size_t dims[2] = {5,10};
    double    d_real[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,37,
                            38,39,41,45,47,48,49},
              d_imag[25] = {51,55,57,58,59,61,65,67,68,69,71,75,77,78,79,81,85,
                            87,88,89,91,95,97,98,99};
    mat_int32_t  ir[25] = {0,4,1,2,3,0,4,1,2,3,0,4,1,2,3,0,4,1,2,3,0,4,1,2,3};
    mat_int32_t  jc[11] = {0,2,5,7,10,12,15,17,20,22,25};
    mat_t *mat;
    matvar_t *matvar;
    sparse_t  sparse = {0,};
    struct ComplexSplit z = {NULL,NULL};

    sparse.nzmax = 25;
    sparse.nir   = 25;
    sparse.ir    = ir;
    sparse.njc   = 11;
    sparse.jc    = jc;
    sparse.ndata = 25;
    sparse.data  = NULL;

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( !mat )
        return 1;

    switch (matvar_class) {
        case MAT_C_DOUBLE:
            z.Re = d_real;
            z.Im = d_imag;
            sparse.data  = &z;
            break;
    }

    if ( NULL != sparse.data ) {
        matvar = Mat_VarCreate("sparse_matrix",MAT_C_SPARSE,MAT_T_DOUBLE,2,
                               dims,&sparse,MAT_F_COMPLEX | MEM_CONSERVE);
        if ( matvar != NULL ) {
            Mat_VarWrite(mat,matvar,compression);
            Mat_VarFree(matvar);
        } else {
            Mat_Critical("test_write_compressed_sparse: Couldn't create "
                         "matlab variable");
            err = 1;
        }
    } else {
        err = 1;
    }

    Mat_Close(mat);

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
    enum matio_classes matvar_class = MAT_C_DOUBLE;
    char *output_name = NULL;

    Mat_LogInit(prog_name);

    while ((c = getopt_long(argc,argv,optstring,options,NULL)) != EOF) {
        switch (c) {
            case 'c':
                if ( !strcmp(optarg,"double") )
                    matvar_class = MAT_C_DOUBLE;
                else if ( !strcmp(optarg,"single") )
                    matvar_class = MAT_C_SINGLE;
                else if ( !strcmp(optarg,"int64") )
                    matvar_class = MAT_C_INT64;
                else if ( !strcmp(optarg,"uint64") )
                    matvar_class = MAT_C_UINT64;
                else if ( !strcmp(optarg,"int32") )
                    matvar_class = MAT_C_INT32;
                else if ( !strcmp(optarg,"uint32") )
                    matvar_class = MAT_C_UINT32;
                else if ( !strcmp(optarg,"int16") )
                    matvar_class = MAT_C_INT16;
                else if ( !strcmp(optarg,"uint16") )
                    matvar_class = MAT_C_UINT16;
                else if ( !strcmp(optarg,"int8") )
                    matvar_class = MAT_C_INT8;
                else if ( !strcmp(optarg,"uint8") )
                    matvar_class = MAT_C_UINT8;
                else {
                    fprintf(stderr,"Unrecognized MAT variable class '%s'",
                            optarg);
                    exit(EXIT_FAILURE);
                }
                break;
            case 'o':
                output_name = optarg;
                break;
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
        } else if ( !strcasecmp(argv[k],"write_2d_numeric") ) {
            k++;
            if ( NULL == output_name )
                output_name = "test_write_2d_numeric.mat";
            err += test_write_2d_numeric(matvar_class,output_name);
            ntests++;
        } else if ( !strcasecmp(argv[k],"write_complex_2d_numeric") ) {
            k++;
            if ( NULL == output_name )
                output_name = "test_write_complex_2d_numeric.mat";
            err += test_write_complex_2d_numeric(matvar_class,output_name);
            ntests++;
        } else if ( !strcasecmp(argv[k],"write_empty_2d_numeric") ) {
            k++;
            if ( NULL == output_name )
                output_name = "test_write_empty_2d_numeric.mat";
            err += test_write_empty_2d_numeric(matvar_class,output_name);
            ntests++;
        } else if ( !strcasecmp(argv[k],"write_char") ) {
            k++;
            if ( NULL == output_name )
                output_name = "test_write_char.mat";
            err += test_write_char(output_name);
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
        } else if ( !strcasecmp(argv[k],"write_struct_2d_numeric") ) {
            k++;
            if ( NULL == output_name )
                output_name = "test_write_struct_2d_numeric.mat";
            err += test_write_struct_2d_numeric(matvar_class,output_name);
            ntests++;
        } else if ( !strcasecmp(argv[k],"write_struct_complex_2d_numeric") ) {
            k++;
            if ( NULL == output_name )
                output_name = "test_write_struct_complex_2d_numeric.mat";
            err += test_write_struct_complex_2d_numeric(matvar_class,
                                                        output_name);
            ntests++;
        } else if ( !strcasecmp(argv[k],"write_empty_struct") ) {
            k++;
            if ( NULL == output_name )
                output_name = "test_write_empty_struct.mat";
            err += test_write_empty_struct(output_name);
            ntests++;
        } else if ( !strcasecmp(argv[k],"write_cell_2d_numeric") ) {
            k++;
            if ( NULL == output_name )
                output_name = "test_write_cell_2d_numeric.mat";
            err += test_write_cell_2d_numeric(matvar_class,output_name);
            ntests++;
        } else if ( !strcasecmp(argv[k],"write_cell_complex_2d_numeric") ) {
            k++;
            if ( NULL == output_name )
                output_name = "test_write_cell_complex_2d_numeric.mat";
            err += test_write_cell_complex_2d_numeric(matvar_class,
                                                        output_name);
            ntests++;
        } else if ( !strcasecmp(argv[k],"write_empty_cell") ) {
            k++;
            if ( NULL == output_name )
                output_name = "test_write_empty_cell.mat";
            err += test_write_empty_cell(output_name);
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
        } else if ( !strcasecmp(argv[k],"write_sparse") ) {
            k++;
            if ( NULL == output_name )
                output_name = "test_write_sparse.mat";
            err += test_write_sparse(matvar_class,output_name);
            ntests++;
        } else if ( !strcasecmp(argv[k],"write_complex_sparse") ) {
            k++;
            if ( NULL == output_name )
                output_name = "test_write_sparse_complex.mat";
            err += test_write_complex_sparse(matvar_class,output_name);
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

    return 0;
}
