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
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <getopt.h>
#include "matio_private.h"
#if !defined(HAVE_STRCASECMP)
#   define strcasecmp(a,b) strcmp(a,b)
#endif
#ifdef _MSC_VER
#   define SIZE_T_FMTSTR "Iu"
#else
#   define SIZE_T_FMTSTR "zu"
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
static enum matio_compression compression  = MAT_COMPRESSION_NONE;

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
"write_2d_logical         - Write a 2D logical array to a matlab file.",
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
"write_struct_2d_logical         - Write a structure with 2D logical arrays",
"                                  to a matlab file.",
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
"write_cell_2d_logical         - Write a cell array with 2D logical array",
"                                fields to a matlab file.",
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


static const char *helptest_write_2d_logical[] = {
    "TEST: write_2d_logical",
    "",
    "Usage: test_mat write_2d_logical",
    "",
    "Writes a several variables to a MAT file. The variables are 2d logical",
    "arrays. Variables l1, l2, l4, and l8 if 64-bit integers are available are",
    " the same except the logical source data are different integer sizes. The",
    "MAT file is the default file version, or set by the -v option. If the MAT",
    "file is version 5, compression can be enabled using the -z option if",
    "built with zlib library.",
    "",
    "MATLAB code to generate expected data",
    "",
    "    l0 = false(0,10);",
    "    l1 = logical(mod(reshape(0:49,5,10),2));",
    "    l2 = logical(mod(reshape(0:49,5,10),2));",
    "    l4 = logical(mod(reshape(0:49,5,10),2));",
    "    l8 = logical(mod(reshape(0:49,5,10),2));",
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
    "Reads variable_name from FILE and prints out it's information and data",
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

static const char *helptest_write_struct_2d_logical[] = {
    "TEST: write_struct_2d_logical",
    "",
    "Usage: test_mat write_struct_2d_logical",
    "",
    "Writes a variable named a to a MAT file. The variable is a structure",
    "array with 2d logical array fields. The MAT file is the default file",
    "version, or set by the -v option. If the MAT file is version 5,",
    "compression can be enabled using the -z option if built with zlib library",
    "",
    "MATLAB code to generate expected data",
    "",
    "    a(1).field1 = logical(mod(reshape(0:49,5,10),2));",
    "    a(1).field2 = ~a(1).field1;",
    "    a(2).field1 = false(0,5);",
    "    a(2).field2 = tril(true(5));",
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

static const char *helptest_write_cell_2d_logical[] = {
    "TEST: write_cell_2d_logical",
    "",
    "Usage: test_mat write_cell_2d_logical",
    "",
    "Writes a variable named a to a MAT file. The variable is a cell array",
    "with 2d logical array fields. The MAT file is the default file",
    "version, or set by the -v option. If the MAT file is version 5,",
    "compression can be enabled using the -z option if built with zlib library",
    "",
    "MATLAB code to generate expected data",
    "",
    "    a = {reshape((1:12),3,4);reshape((13:24),3,4);...",
    "         reshape((25:36),3,4);reshape((37:48),3,4);}",
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
    "Reads header information for variable_name from the Matlab v4 MAT file",
    "FILE and prints it out to the screen.",
    "",
    NULL
};

static const char *helptest_sub2ind[] = {
    "TEST: sub2ind",
    "",
    "Usage: test_mat sub2ind",
    "",
    "  Calculates a linear (single) index from a set of subscript indicies.",
    "  The size of the array used is [256,256,124].  The 1-relative indicies",
    "  are (233,74,1).  Therefore, the calculated linear index should be",
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
    "  index used is 18921.  Therefore, the calculated subscripts should be",
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
    else if ( !strcmp(test,"write_2d_logical") )
        Mat_Help(helptest_write_2d_logical);
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
    else if ( !strcmp(test,"write_struct_2d_logical") )
        Mat_Help(helptest_write_struct_2d_logical);
    else if ( !strcmp(test,"write_empty_struct") )
        Mat_Help(helptest_write_empty_struct);
    else if ( !strcmp(test,"write_cell_2d_numeric") )
        Mat_Help(helptest_write_cell_2d_numeric);
    else if ( !strcmp(test,"write_cell_complex_2d_numeric") )
        Mat_Help(helptest_write_cell_complex_2d_numeric);
    else if ( !strcmp(test,"write_cell_2d_logical") )
        Mat_Help(helptest_write_cell_2d_logical);
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
test_write_2d_logical(char *output_name)
{
    size_t dims[2] = {5,10};
    int    err = 0, i;
#ifdef HAVE_MAT_UINT64_T
    mat_uint64_t   l8[50];
#endif
    mat_uint32_t   l4[50];
    mat_uint16_t   l2[50];
    mat_uint8_t    l1[50];
    mat_t *mat;
    matvar_t *matvar;

    for ( i = 0; i < 50; i++ ) {
        l1[i] = i % 2;
        l2[i] = i % 2;
        l4[i] = i % 2;
#ifdef HAVE_MAT_UINT64_T
        l8[i] = i % 2;
#endif
    }

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( !mat ) {
        return 1;
    }

#ifdef HAVE_MAT_UINT64_T
    matvar = Mat_VarCreate("l8",MAT_C_UINT64,MAT_T_UINT64,2,dims,l8,MAT_F_LOGICAL);
    Mat_VarWrite(mat,matvar,compression);
    Mat_VarFree(matvar);
#endif
    matvar = Mat_VarCreate("l4",MAT_C_UINT32,MAT_T_UINT32,2,dims,l4,MAT_F_LOGICAL);
    Mat_VarWrite(mat,matvar,compression);
    Mat_VarFree(matvar);
    matvar = Mat_VarCreate("l2",MAT_C_UINT16,MAT_T_UINT16,2,dims,l2,MAT_F_LOGICAL);
    Mat_VarWrite(mat,matvar,compression);
    Mat_VarFree(matvar);
    matvar = Mat_VarCreate("l1",MAT_C_UINT8,MAT_T_UINT8,2,dims,l1,MAT_F_LOGICAL);
    Mat_VarWrite(mat,matvar,compression);
    Mat_VarFree(matvar);
    dims[0] = 0;
    matvar = Mat_VarCreate("l0",MAT_C_UINT8,MAT_T_UINT8,2,dims,NULL,MAT_F_LOGICAL);
    Mat_VarWrite(mat,matvar,compression);
    Mat_VarFree(matvar);

    Mat_Close(mat);

    return err;
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
    mat_complex_split_t z = {NULL,NULL},s = {NULL,NULL};
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
        default:
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
    mat_complex_split_t z = {NULL,NULL};
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
        default:
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
        default:
            return 1;
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
    int       err = 0;
    size_t    dims[2];
    mat_t    *mat;
    matvar_t *matvar;

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( mat ) {
        dims[0]   = 4;
        dims[1]   = 26;
        matvar = Mat_VarCreate("a",MAT_C_CHAR,MAT_T_UINT8,2,
                    dims,str,MAT_F_DONT_COPY_DATA);
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
    int    err = 0;
    mat_t     *mat;
    matvar_t *matvar[5], *struct_matvar;

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
test_write_struct_2d_logical(char *output_name)
{
    size_t dims[2] = {5,10};
    int    err = 0, i, j;
    mat_uint32_t odd[50];
    mat_uint16_t even[50];
    mat_uint8_t  lower_tri[25] = {0,};
    mat_t *mat;
    matvar_t *matvar[5], *struct_matvar;

    for ( i = 0; i < 50; i++ ) {
        odd[i] = i % 2;
        even[i] = !odd[i];
    }

    for ( i = 0; i < 5; i++ )
        for ( j = i; j < 5; j++ )
            lower_tri[j+5*i] = 1;

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( !mat ) {
        return 1;
    }

    matvar[0] = Mat_VarCreate("field1",MAT_C_UINT32,MAT_T_UINT32,2,
                              dims,odd,MAT_F_DONT_COPY_DATA | MAT_F_LOGICAL);
    matvar[1] = Mat_VarCreate("field2",MAT_C_UINT16,MAT_T_UINT16,2,
                              dims,even,MAT_F_DONT_COPY_DATA | MAT_F_LOGICAL);
    dims[0] = 0; dims[1] = 5;
    matvar[2] = Mat_VarCreate("field1",MAT_C_UINT8,MAT_T_UINT8,2,
                              dims,NULL,MAT_F_DONT_COPY_DATA | MAT_F_LOGICAL);
    dims[0] = 5; dims[1] = 5;
    matvar[3] = Mat_VarCreate("field2",MAT_C_UINT8,MAT_T_UINT8,2,dims,
                              lower_tri,MAT_F_DONT_COPY_DATA | MAT_F_LOGICAL);
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
        default:
            return 1;
    }

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( !mat ) {
        return 1;
    }

    dims[0] = 3;
    dims[1] = 4;
    matvar[0] = Mat_VarCreate("field1",matvar_class,data_type,2,
                   dims,data[0],MAT_F_DONT_COPY_DATA);
    matvar[1] = Mat_VarCreate("field2",matvar_class,data_type,2,
                   dims,data[1],MAT_F_DONT_COPY_DATA);
    matvar[2] = Mat_VarCreate("field1",matvar_class,data_type,2,
                   dims,data[2],MAT_F_DONT_COPY_DATA);
    matvar[3] = Mat_VarCreate("field2",matvar_class,data_type,2,
                   dims,data[3],MAT_F_DONT_COPY_DATA);
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
    mat_complex_split_t data[4] = {NULL,NULL};
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
        default:
            return 1;
    }

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( !mat ) {
        return 1;
    }

    dims[0] = 3;
    dims[1] = 4;
    matvar[0] = Mat_VarCreate("field1",matvar_class,data_type,2,
                   dims,data,MAT_F_DONT_COPY_DATA | MAT_F_COMPLEX);
    matvar[1] = Mat_VarCreate("field2",matvar_class,data_type,2,
                   dims,data+1,MAT_F_DONT_COPY_DATA | MAT_F_COMPLEX);
    matvar[2] = Mat_VarCreate("field1",matvar_class,data_type,2,
                   dims,data+2,MAT_F_DONT_COPY_DATA | MAT_F_COMPLEX);
    matvar[3] = Mat_VarCreate("field2",matvar_class,data_type,2,
                   dims,data+3,MAT_F_DONT_COPY_DATA | MAT_F_COMPLEX);
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
    int    err = 0;
    mat_t     *mat;
    matvar_t *matvar[5], *cell_matvar;

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
test_write_cell_2d_logical(char *output_name)
{
    size_t dims[2] = {5,5};
    int    err = 0, i, j;
    mat_uint32_t   upper_tri[25] = {0,};
    mat_uint16_t   lower_tri[25] = {0,};
    mat_uint8_t    eye[25] = {0,};
    mat_t *mat;
    matvar_t *matvar[5] = {NULL,}, *cell_matvar;

    for ( i = 0; i < 5; i++ ) {
        eye[5*i+i] = 1;
        for ( j = 0; j <= i; j++ )
            upper_tri[j+5*i] = 1;
        for ( j = i; j < 5; j++ )
            lower_tri[j+5*i] = 1;
    }

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( !mat ) {
        return 1;
    }

    matvar[0] = Mat_VarCreate(NULL,MAT_C_UINT32,MAT_T_UINT32,2,dims,upper_tri,
                              MAT_F_LOGICAL);
    matvar[1] = Mat_VarCreate(NULL,MAT_C_UINT16,MAT_T_UINT16,2,dims,lower_tri,
                              MAT_F_LOGICAL);
    matvar[2] = Mat_VarCreate(NULL,MAT_C_UINT8,MAT_T_UINT8,2,dims,eye,
                              MAT_F_LOGICAL);
    dims[0] = 0;
    matvar[3] = Mat_VarCreate(NULL,MAT_C_UINT8,MAT_T_UINT8,2,dims,NULL,
                              MAT_F_LOGICAL);

    dims[0] = 4;
    dims[1] = 1;
    cell_matvar = Mat_VarCreate("a",MAT_C_CELL,MAT_T_CELL,2,dims,matvar,0);
    Mat_VarWrite(mat,cell_matvar,compression);
    Mat_VarFree(cell_matvar);
    Mat_Close(mat);

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
        default:
            return 1;
    }

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( !mat ) {
        return 1;
    }

    dims[0] = 3;
    dims[1] = 4;
    matvar[0] = Mat_VarCreate(NULL,matvar_class,data_type,2,
                   dims,data[0],MAT_F_DONT_COPY_DATA);
    matvar[1] = Mat_VarCreate(NULL,matvar_class,data_type,2,
                   dims,data[1],MAT_F_DONT_COPY_DATA);
    matvar[2] = Mat_VarCreate(NULL,matvar_class,data_type,2,
                   dims,data[2],MAT_F_DONT_COPY_DATA);
    matvar[3] = Mat_VarCreate(NULL,matvar_class,data_type,2,
                   dims,data[3],MAT_F_DONT_COPY_DATA);
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
    mat_complex_split_t data[4] = {NULL,NULL};
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
        default:
            return 1;
    }

    mat = Mat_CreateVer(output_name,NULL,mat_file_ver);
    if ( !mat ) {
        return 1;
    }

    dims[0] = 3;
    dims[1] = 4;
    matvar[0] = Mat_VarCreate(NULL,matvar_class,data_type,2,
                   dims,data,MAT_F_DONT_COPY_DATA | MAT_F_COMPLEX);
    matvar[1] = Mat_VarCreate(NULL,matvar_class,data_type,2,
                   dims,data+1,MAT_F_DONT_COPY_DATA | MAT_F_COMPLEX);
    matvar[2] = Mat_VarCreate(NULL,matvar_class,data_type,2,
                   dims,data+2,MAT_F_DONT_COPY_DATA | MAT_F_COMPLEX);
    matvar[3] = Mat_VarCreate(NULL,matvar_class,data_type,2,
                   dims,data+3,MAT_F_DONT_COPY_DATA | MAT_F_COMPLEX);
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
                            MAT_T_STRUCT,3,dims,struct_fields,MAT_F_DONT_COPY_DATA);
        Mat_VarWrite(mat,struct_matvar,compression);
        /* Reset data to NULL so the fields are not free'd */
        struct_matvar->data = NULL;
        Mat_VarFree(struct_matvar);
        dims[0] = 1;
        struct_matvar = Mat_VarCreate("struct_null_fields",MAT_C_STRUCT,
                            MAT_T_STRUCT,2,dims,struct_fields,MAT_F_DONT_COPY_DATA);
        Mat_VarWrite(mat,struct_matvar,compression);
        /* Reset data to NULL so the fields are not free'd */
        struct_matvar->data = NULL;
        Mat_VarFree(struct_matvar);
        dims[0] = 0;
        cell_matvar = Mat_VarCreate("cell_null",MAT_C_CELL,MAT_T_CELL,2,
                            dims,NULL,MAT_F_DONT_COPY_DATA);
        Mat_VarWrite(mat,cell_matvar,compression);
        Mat_VarFree(cell_matvar);

        dims[0] = 3;
        cell_matvar = Mat_VarCreate("cell_null_cells",MAT_C_CELL,MAT_T_CELL,2,
                            dims,struct_fields,MAT_F_DONT_COPY_DATA);
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
test_struct_api_create(void)
{
    size_t dims[2] = {5,10};
    int    err = 0;
    matvar_t *matvar;
    size_t num_fields = 2;
    const char *fieldnames[2] = {"field1","field2"};

    dims[0] = 2;
    dims[1] = 1;
    matvar = Mat_VarCreateStruct("a", 2, dims, fieldnames, num_fields);
    Mat_VarPrint(matvar, 1);
    Mat_VarFree(matvar);

    matvar = Mat_VarCreateStruct("b", 2, dims, NULL, 0);
    Mat_VarPrint(matvar, 1);
    Mat_VarFree(matvar);

    dims[0] = 0; dims[1] = 0;
    matvar = Mat_VarCreateStruct("c", 2, dims, fieldnames, num_fields);
    Mat_VarPrint(matvar, 1);
    Mat_VarFree(matvar);

    return err;
}

static int
test_struct_api_setfield(void)
{
    size_t dims[2];
    int    err = 0;
    double    data1[2] = {0,1}, data2[3] = {2,3,4}, data3[3] = {5,6,7},
              data4[2] = {8,9};
    matvar_t *fields[5], *matvar;
    const size_t num_fields = 2;
    const char *fieldnames[2] = {"field1","field2"};

    dims[0] = 2; dims[1] = 1;
    fields[0] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                              dims,data1,MAT_F_DONT_COPY_DATA);
    dims[0] = 3; dims[1] = 1;
    fields[1] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                              dims,data2,MAT_F_DONT_COPY_DATA);
    dims[0] = 1; dims[1] = 3;
    fields[2] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                              dims,data3,MAT_F_DONT_COPY_DATA);
    dims[0] = 1; dims[1] = 2;
    fields[3] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                              dims,data4,MAT_F_DONT_COPY_DATA);
    dims[0] = 2; dims[1] = 1;
    matvar = Mat_VarCreateStruct("a", 2, dims, fieldnames, num_fields);
    Mat_VarSetStructFieldByName(matvar, "field1", 0, fields[0]);
    Mat_VarSetStructFieldByName(matvar, "field2", 0, fields[1]);
    Mat_VarSetStructFieldByName(matvar, "field1", 1, fields[2]);
    Mat_VarSetStructFieldByName(matvar, "field2", 1, fields[3]);
    Mat_VarPrint(matvar,1);
    /* Set data to NULL so the fields are not free'd */
    matvar->data = NULL;
    Mat_VarFree(matvar);

    dims[0] = 2; dims[1] = 1;
    matvar = Mat_VarCreateStruct("b", 2, dims, fieldnames, num_fields);
    Mat_VarSetStructFieldByIndex(matvar, 0, 0, fields[3]);
    Mat_VarSetStructFieldByIndex(matvar, 1, 0, fields[2]);
    Mat_VarSetStructFieldByIndex(matvar, 0, 1, fields[1]);
    Mat_VarSetStructFieldByIndex(matvar, 1, 1, fields[0]);
    Mat_VarPrint(matvar,1);
    Mat_VarFree(matvar);

    return err;
}

static int
test_struct_api_getfieldnames(void)
{
    size_t dims[2];
    int    err = 0;
    matvar_t *matvar;
    const unsigned num_fields = 4;
    const char *fieldnames[4] = {"field1","field2","field3","field4"};
    unsigned nfields, i;
    char * const *fieldnames2;

    dims[0] = 2; dims[1] = 1;
    matvar      = Mat_VarCreateStruct("a", 2, dims, fieldnames, num_fields);
    nfields     = Mat_VarGetNumberOfFields(matvar);
    fieldnames2 = Mat_VarGetStructFieldnames(matvar);
    printf("Fieldnames of \"a\":\n");
    if ( nfields < 1 ) {
        printf("  None\n");
    } else {
        for ( i = 0; i < nfields; i++ )
            printf("  %3d. %s\n",i, fieldnames2[i]);
    }
    Mat_VarFree(matvar);

    matvar      = Mat_VarCreateStruct("b", 2, dims, NULL, 0);
    nfields     = Mat_VarGetNumberOfFields(matvar);
    fieldnames2 = Mat_VarGetStructFieldnames(matvar);
    printf("Fieldnames of \"b\":\n");
    if ( nfields < 1 ) {
        printf("  None\n");
    } else {
        for ( i = 0; i < nfields; i++ )
            printf("  %3d. %s\n",i, fieldnames2[i]);
    }
    Mat_VarFree(matvar);

    nfields     = Mat_VarGetNumberOfFields(NULL);
    fieldnames2 = Mat_VarGetStructFieldnames(NULL);
    printf("Fieldnames of \"NULL\":\n");
    if ( nfields < 1 ) {
        printf("  None\n");
    } else {
        for ( i = 0; i < nfields; i++ )
            printf("  %3d. %s\n",i, fieldnames2[i]);
    }

    return err;
}

static int
test_struct_api_addfield(void)
{
    size_t    dims[2];
    double    data1[2] = {0,1}, data2[3] = {2,3,4}, data3[3] = {5,6,7},
              data4[2] = {8,9};
    matvar_t *fields[5], *matvar;

    dims[0] = 2; dims[1] = 1;
    matvar = Mat_VarCreateStruct("a", 2, dims, NULL, 0);

    dims[0] = 2; dims[1] = 1;
    fields[0] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                              dims,data1,MAT_F_DONT_COPY_DATA);
    dims[0] = 3; dims[1] = 1;
    fields[1] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                              dims,data2,MAT_F_DONT_COPY_DATA);
    dims[0] = 1; dims[1] = 3;
    fields[2] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                              dims,data3,MAT_F_DONT_COPY_DATA);
    dims[0] = 1; dims[1] = 2;
    fields[3] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                              dims,data4,MAT_F_DONT_COPY_DATA);
    Mat_VarAddStructField(matvar, "field1");
    Mat_VarSetStructFieldByName(matvar, "field1", 0, fields[0]);
    Mat_VarSetStructFieldByName(matvar, "field1", 1, fields[2]);
    Mat_VarAddStructField(matvar, "field2");
    Mat_VarSetStructFieldByName(matvar, "field2", 0, fields[1]);
    Mat_VarSetStructFieldByName(matvar, "field2", 1, fields[3]);
    Mat_VarPrint(matvar,1);

    Mat_VarFree(matvar);

    return 0;
}

static int
test_struct_api_getlinear(void)
{
    size_t dims[2];
    int    err = 0, i;
    double    r[12] = {0,1,2,3,4,5,6,7,8,9,10,11},
              c[12] = {12,13,14,15,16,17,18,19,20,21,22,23};
    mat_complex_split_t z[12];
    matvar_t *field, *matvar, *matvar2;
    const size_t num_fields = 3;
    const char *fieldnames[3] = {"r","c","z"};

    dims[0] = 3;
    dims[1] = 4;
    matvar = Mat_VarCreateStruct("a", 2, dims, fieldnames, num_fields);

    dims[0] = 1; dims[1] = 1;

    for ( i = 0; i < 12; i++ ) {
        field = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                              dims,r+i,MAT_F_DONT_COPY_DATA);
        Mat_VarSetStructFieldByName(matvar, "r", i, field);
        field = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                              dims,c+i,MAT_F_DONT_COPY_DATA);
        Mat_VarSetStructFieldByName(matvar, "c", i, field);
        z[i].Re = r+i;
        z[i].Im = c+i;
        field = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                              dims,z+i,MAT_F_DONT_COPY_DATA|MAT_F_COMPLEX);
        Mat_VarSetStructFieldByName(matvar, "z", i, field);
    }

    /* Read the second row of data */
    matvar2 = Mat_VarGetStructsLinear(matvar, 1, 3, 4, 0);
    Mat_VarPrint(matvar2,1);
    Mat_VarFree(matvar2);

    /* Read the first column of data */
    matvar2 = Mat_VarGetStructsLinear(matvar, 0, 1, 3, 0);
    Mat_VarPrint(matvar2,1);
    Mat_VarFree(matvar2);

    /* Read diagonal */
    matvar2 = Mat_VarGetStructsLinear(matvar, 0, 4, 3, 0);
    Mat_VarPrint(matvar2,1);
    Mat_VarFree(matvar2);

    Mat_VarFree(matvar);

    return err;
}

static int
test_struct_api_get(void)
{
    size_t dims[4];
    int    err = 0, i, start[4], stride[4], edge[4];
    double    r[360] = {0,}, c[360] = {0,};
    matvar_t *field, *matvar, *matvar2;
    const size_t num_fields = 2;
    const char *fieldnames[3] = {"r","c"};

    dims[0] = 3;
    dims[1] = 4;
    dims[2] = 5;
    dims[3] = 6;
    matvar = Mat_VarCreateStruct("a", 4, dims, fieldnames, num_fields);

    dims[0] = 1; dims[1] = 1;

    for ( i = 0; i < 360; i++ ) {
        r[i] = i+1;
        c[i] = -(i+1);
        field = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                              dims,r+i,MAT_F_DONT_COPY_DATA);
        Mat_VarSetStructFieldByName(matvar, "r", i, field);
        field = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                              dims,c+i,MAT_F_DONT_COPY_DATA);
        Mat_VarSetStructFieldByName(matvar, "c", i, field);
    }

    /* Read a(2,2:3,1:2:5,2:4:end) - MATLAB 1-relative indices */
    start[0]  = 1; start[1]  = 1; start[2]  = 0; start[3]  = 1;
    stride[0] = 0; stride[1] = 1; stride[2] = 2; stride[3] = 4;
    edge[0]   = 1; edge[1]   = 2; edge[2]   = 3; edge[3]   = 2;
    matvar2 = Mat_VarGetStructs(matvar, start,stride,edge,0);
    Mat_VarPrint(matvar2,1);
    Mat_VarFree(matvar2);

    Mat_VarFree(matvar);

    return err;
}

static int
test_cell_api_set(void)
{
    size_t dims[2];
    double    data[10] = {0,1,2,3,4,5,6,7,8,9};
    matvar_t *cells[10], *matvar, *prev_cell;

    dims[0] = 2; dims[1] = 3;
    matvar = Mat_VarCreate("a", MAT_C_CELL, MAT_T_CELL, 2, dims, NULL, 0);
    dims[0] = 1; dims[1] = 1;
    cells[0] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                              dims,data,MAT_F_DONT_COPY_DATA);
    cells[1] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                             dims,data+1,MAT_F_DONT_COPY_DATA);
    cells[2] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                             dims,data+2,MAT_F_DONT_COPY_DATA);
    cells[3] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                             dims,data+3,MAT_F_DONT_COPY_DATA);
    cells[4] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                             dims,data+4,MAT_F_DONT_COPY_DATA);
    cells[5] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                             dims,data+5,MAT_F_DONT_COPY_DATA);
    cells[6] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                             dims,data+6,MAT_F_DONT_COPY_DATA);
    cells[7] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                             dims,data+7,MAT_F_DONT_COPY_DATA);
    cells[8] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                             dims,data+8,MAT_F_DONT_COPY_DATA);
    cells[9] = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                             dims,data+9,MAT_F_DONT_COPY_DATA);
    Mat_VarSetCell(matvar, 0, cells[0]);
    Mat_VarSetCell(matvar, 1, cells[1]);
    Mat_VarSetCell(matvar, 2, cells[2]);
    Mat_VarSetCell(matvar, 3, cells[3]);
    Mat_VarSetCell(matvar, 4, cells[4]);
    Mat_VarSetCell(matvar, 5, cells[5]);
    Mat_VarSetCell(matvar, 6, cells[6]);
    Mat_VarPrint(matvar,1);
    /* Change the first row */
    prev_cell = Mat_VarSetCell(matvar, 0, cells[7]);
    if ( prev_cell != cells[0] ) {
        fprintf(stderr, "Previous cell element is not expected element\n");
        Mat_VarFree(matvar);
        return 1;
    }
    Mat_VarFree(prev_cell);
    prev_cell = Mat_VarSetCell(matvar, 2, cells[8]);
    if ( prev_cell != cells[2] ) {
        fprintf(stderr, "Previous cell element is not expected element\n");
        Mat_VarFree(matvar);
        return 1;
    }
    Mat_VarFree(prev_cell);
    prev_cell = Mat_VarSetCell(matvar, 4, cells[9]);
    if ( prev_cell != cells[4] ) {
        fprintf(stderr, "Previous cell element is not expected element\n");
        Mat_VarFree(matvar);
        return 1;
    }
    Mat_VarFree(prev_cell);
    Mat_VarPrint(matvar, 1);
    Mat_VarFree(matvar);

    return 0;
}

static int
test_cell_api_getlinear(void)
{
    size_t dims[2], i;
    double    r[4] = {0,1,2,3},c[4] = {4,5,6,7};
    mat_complex_split_t z[4];
    matvar_t *cell, *matvar, **cells;

    dims[0] = 3; dims[1] = 4;
    matvar = Mat_VarCreate("a", MAT_C_CELL, MAT_T_CELL, 2, dims, NULL, 0);
    dims[0] = 1; dims[1] = 1;
    for ( i = 0; i < 4; i++ ) {
        cell = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                             dims,r+i,MAT_F_DONT_COPY_DATA);
        Mat_VarSetCell(matvar, 3*i, cell);
        cell = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                             dims,c+i,MAT_F_DONT_COPY_DATA);
        Mat_VarSetCell(matvar, 3*i+1, cell);
        z[i].Re = r+i;
        z[i].Im = c+i;
        cell = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                             dims,z+i,MAT_F_COMPLEX|MAT_F_DONT_COPY_DATA);
        Mat_VarSetCell(matvar, 3*i+2, cell);
    }
    /* Get the first row */
    cells = Mat_VarGetCellsLinear(matvar, 0, 3, 4);
    if ( NULL != cells ) {
        for (i = 0; i < 4; i++ )
            Mat_VarPrint(cells[i], 1);
        free(cells);
    }

    /* Get the second row */
    cells = Mat_VarGetCellsLinear(matvar, 1, 3, 4);
    if ( NULL != cells ) {
        for (i = 0; i < 4; i++ )
            Mat_VarPrint(cells[i], 1);
        free(cells);
    }
    /* Get the third row */
    cells = Mat_VarGetCellsLinear(matvar, 2, 3, 4);
    if ( NULL != cells ) {
        for (i = 0; i < 4; i++ )
            Mat_VarPrint(cells[i], 1);
        free(cells);
    }
    /* Get the middle two columns */
    cells = Mat_VarGetCellsLinear(matvar, 3, 1, 6);
    if ( NULL != cells ) {
        for (i = 0; i < 6; i++ )
            Mat_VarPrint(cells[i], 1);
        free(cells);
    }

    Mat_VarFree(matvar);
    return 0;
}

static int
test_cell_api_getcells(void)
{
    size_t dims[4];
    int    i, start[4], stride[4], edge[4];
    double    x[360] = {0,};
    matvar_t *cell, *matvar, **matvar2;

    dims[0] = 3;
    dims[1] = 4;
    dims[2] = 5;
    dims[3] = 6;
    matvar = Mat_VarCreate("a", MAT_C_CELL, MAT_T_CELL, 4, dims, NULL, 0);

    dims[0] = 1; dims[1] = 1;

    for ( i = 0; i < 360; i++ ) {
        x[i] = i+1;
        cell = Mat_VarCreate(NULL,MAT_C_DOUBLE,MAT_T_DOUBLE,2,
                              dims,x+i,MAT_F_DONT_COPY_DATA);
        Mat_VarSetCell(matvar, i, cell);
    }

    /* Read a(2,2:3,1:2:5,2:4:end) - MATLAB 1-relative indices */
    start[0]  = 1; start[1]  = 1; start[2]  = 0; start[3]  = 1;
    stride[0] = 0; stride[1] = 1; stride[2] = 2; stride[3] = 4;
    edge[0]   = 1; edge[1]   = 2; edge[2]   = 3; edge[3]   = 2;
    matvar2 = Mat_VarGetCells(matvar, start, stride, edge);
    if ( NULL != matvar2 ) {
        for ( i = 0; i < 12; i++ )
            Mat_VarPrint(matvar2[i],1);
        free(matvar2);
    }
    Mat_VarFree(matvar);

    return 0;
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
                    field = Mat_VarGetStructField(matvar,&index,MAT_BY_INDEX,0);
                    err = (field == NULL) ? 1 : 0;
                    if ( !err )
                        Mat_VarPrint( field, 0);
                    break;
                default:
                    field = Mat_VarGetStructField(matvar,fieldname,
                                MAT_BY_NAME,0);
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
    int       idata[60]={0,};
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
                       dims,data,MAT_F_DONT_COPY_DATA);
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
                       dims,data,MAT_F_DONT_COPY_DATA);
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
    int    err = 0;
    size_t dims[2] = {5,10};
    mat_int32_t  ir[25] = {0,4,1,2,3,0,4,1,2,3,0,4,1,2,3,0,4,1,2,3,0,4,1,2,3};
    mat_int32_t  jc[11] = {0,2,5,7,10,12,15,17,20,22,25};
    mat_t *mat;
    matvar_t *matvar;
    mat_sparse_t  sparse = {0,};
    enum matio_types data_type;
    double    d[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,37,38,39,
                       41,45,47,48,49};
    float     f[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,37,38,39,
                       41,45,47,48,49};
    mat_int32_t   i32[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,37,
                             38,39,41,45,47,48,49};
    mat_uint32_t ui32[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,37,
                             38,39,41,45,47,48,49};
    mat_int16_t   i16[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,37,
                             38,39,41,45,47,48,49};
    mat_uint16_t ui16[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,37,
                             38,39,41,45,47,48,49};
    mat_int8_t    i8[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,37,
                            38,39,41,45,47,48,49};
    mat_uint8_t  ui8[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,37,
                            38,39,41,45,47,48,49};
#ifdef HAVE_MAT_INT64_T
    mat_int64_t i64[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,37,
                           38,39,41,45,47,48,49};
#endif
#ifdef HAVE_MAT_UINT64_T
    mat_uint64_t ui64[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,37,
                             38,39,41,45,47,48,49};
#endif

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
            data_type = MAT_T_DOUBLE;
            break;
        case MAT_C_SINGLE:
            sparse.data  = f;
            data_type = MAT_T_SINGLE;
            break;
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
            sparse.data  = i64;
            data_type = MAT_T_INT64;
            break;
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
            sparse.data  = ui64;
            data_type = MAT_T_UINT64;
            break;
#endif
        case MAT_C_INT32:
            sparse.data  = i32;
            data_type = MAT_T_INT32;
            break;
        case MAT_C_UINT32:
            sparse.data  = ui32;
            data_type = MAT_T_UINT32;
            break;
        case MAT_C_INT16:
            sparse.data  = i16;
            data_type = MAT_T_INT16;
            break;
        case MAT_C_UINT16:
            sparse.data  = ui16;
            data_type = MAT_T_UINT16;
            break;
        case MAT_C_INT8:
            sparse.data  = i8;
            data_type = MAT_T_INT8;
            break;
        case MAT_C_UINT8:
            sparse.data  = ui8;
            data_type = MAT_T_UINT8;
            break;
        default:
            err = 1;
            break;
    }

    if ( NULL != sparse.data) {
        matvar = Mat_VarCreate("sparse_matrix",MAT_C_SPARSE,
                       data_type,2,dims,&sparse,MAT_F_DONT_COPY_DATA);
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
    int    err = 0;
    size_t dims[2] = {5,10};
    mat_int32_t  ir[25] = {0,4,1,2,3,0,4,1,2,3,0,4,1,2,3,0,4,1,2,3,0,4,1,2,3};
    mat_int32_t  jc[11] = {0,2,5,7,10,12,15,17,20,22,25};
    mat_t *mat;
    matvar_t *matvar;
    mat_sparse_t  sparse = {0,};
    mat_complex_split_t z = {NULL,NULL};
    enum matio_types data_type;
    double    d_real[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,37,
                            38,39,41,45,47,48,49},
              d_imag[25] = {51,55,57,58,59,61,65,67,68,69,71,75,77,78,79,81,85,
                            87,88,89,91,95,97,98,99};
    float     f_real[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,37,
                            38,39,41,45,47,48,49},
              f_imag[25] = {51,55,57,58,59,61,65,67,68,69,71,75,77,78,79,81,85,
                            87,88,89,91,95,97,98,99};
    mat_int32_t i32_real[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,
                                37,38,39,41,45,47,48,49},
                i32_imag[25] = {51,55,57,58,59,61,65,67,68,69,71,75,77,78,79,81,
                                85,87,88,89,91,95,97,98,99};
    mat_uint32_t ui32_real[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,
                                  37,38,39,41,45,47,48,49},
                 ui32_imag[25] = {51,55,57,58,59,61,65,67,68,69,71,75,77,78,79,
                                  81,85,87,88,89,91,95,97,98,99};
    mat_int16_t i16_real[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,
                                37,38,39,41,45,47,48,49},
                i16_imag[25] = {51,55,57,58,59,61,65,67,68,69,71,75,77,78,79,81,
                                85,87,88,89,91,95,97,98,99};
    mat_uint16_t ui16_real[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,
                                  37,38,39,41,45,47,48,49},
                 ui16_imag[25] = {51,55,57,58,59,61,65,67,68,69,71,75,77,78,79,
                                  81,85,87,88,89,91,95,97,98,99};
    mat_int8_t i8_real[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,
                              37,38,39,41,45,47,48,49},
               i8_imag[25] = {51,55,57,58,59,61,65,67,68,69,71,75,77,78,79,81,
                              85,87,88,89,91,95,97,98,99};
    mat_uint8_t ui8_real[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,
                                37,38,39,41,45,47,48,49},
                ui8_imag[25] = {51,55,57,58,59,61,65,67,68,69,71,75,77,78,79,
                                81,85,87,88,89,91,95,97,98,99};
#ifdef HAVE_MAT_INT64_T
    mat_int64_t i64_real[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,
                                37,38,39,41,45,47,48,49},
                i64_imag[25] = {51,55,57,58,59,61,65,67,68,69,71,75,77,78,79,81,
                                85,87,88,89,91,95,97,98,99};
#endif
#ifdef HAVE_MAT_UINT64_T
    mat_uint64_t ui64_real[25] = {1,5,7,8,9,11,15,17,18,19,21,25,27,28,29,31,35,
                                  37,38,39,41,45,47,48,49},
                 ui64_imag[25] = {51,55,57,58,59,61,65,67,68,69,71,75,77,78,79,
                                  81,85,87,88,89,91,95,97,98,99};
#endif

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
            data_type = MAT_T_DOUBLE;
            break;
        case MAT_C_SINGLE:
            z.Re = f_real;
            z.Im = f_imag;
            sparse.data  = &z;
            data_type = MAT_T_SINGLE;
            break;
#ifdef HAVE_MAT_INT64_T
        case MAT_C_INT64:
            z.Re = i64_real;
            z.Im = i64_imag;
            sparse.data  = &z;
            data_type = MAT_T_INT64;
            break;
#endif
#ifdef HAVE_MAT_UINT64_T
        case MAT_C_UINT64:
            z.Re = ui64_real;
            z.Im = ui64_imag;
            sparse.data  = &z;
            data_type = MAT_T_UINT64;
            break;
#endif
        case MAT_C_INT32:
            z.Re = i32_real;
            z.Im = i32_imag;
            sparse.data  = &z;
            data_type = MAT_T_INT32;
            break;
        case MAT_C_UINT32:
            z.Re = ui32_real;
            z.Im = ui32_imag;
            sparse.data  = &z;
            data_type = MAT_T_UINT32;
            break;
        case MAT_C_INT16:
            z.Re = i16_real;
            z.Im = i16_imag;
            sparse.data  = &z;
            data_type = MAT_T_INT16;
            break;
        case MAT_C_UINT16:
            z.Re = ui16_real;
            z.Im = ui16_imag;
            sparse.data  = &z;
            data_type = MAT_T_UINT16;
            break;
        case MAT_C_INT8:
            z.Re = i8_real;
            z.Im = i8_imag;
            sparse.data  = &z;
            data_type = MAT_T_INT8;
            break;
        case MAT_C_UINT8:
            z.Re = ui8_real;
            z.Im = ui8_imag;
            sparse.data  = &z;
            data_type = MAT_T_UINT8;
            break;
       default:
            err = 1;
            break;
    }

    if ( NULL != sparse.data ) {
        matvar = Mat_VarCreate("sparse_matrix",MAT_C_SPARSE,data_type,2,
                               dims,&sparse,MAT_F_COMPLEX|MAT_F_DONT_COPY_DATA);
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
    matvar_t *matvar;
    enum matio_classes matvar_class = MAT_C_DOUBLE;
    char *output_name = NULL;
    int version[3];

    Mat_GetLibraryVersion(version, version+1, version+2);
    if ( MATIO_MAJOR_VERSION != version[0] ||
         MATIO_MINOR_VERSION != version[1] ||
         MATIO_RELEASE_LEVEL != version[2] ) {
        fprintf(stderr,"matio version in header does not match runtime "
                "version\n");
        return EXIT_FAILURE;
    }

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
                } else if ( !strcmp(optarg,"4") ) {
                    mat_file_ver = MAT_FT_MAT4;
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
                printf("%s %s\nWritten by Christopher Hulbert\n\n"
                       "Copyright(C) 2006-2016 Christopher C. Hulbert\n",
                       prog_name,PACKAGE_VERSION);
                exit(EXIT_SUCCESS);
            case 'z':
                compression = MAT_COMPRESSION_ZLIB;
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
                    Mat_VarWrite(mat,matvar,compression);
                Mat_Close(mat);
                Mat_Close(mat2);
            }
            ntests++;
        } else if ( !strcasecmp(argv[k],"delete") ) {
            k++;
            err += test_delete(argv[k],argv[k+1]);
            k+= 2;
            ntests++;
        } else if ( !strcasecmp(argv[k],"write_2d_logical") ) {
            k++;
            if ( NULL == output_name )
                output_name = "test_write_2d_logical.mat";
            err += test_write_2d_logical(output_name);
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
        } else if ( !strcasecmp(argv[k],"write_struct_2d_logical") ) {
            k++;
            if ( NULL == output_name )
                output_name = "test_write_struct_2d_logical.mat";
            err += test_write_struct_2d_logical(output_name);
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
        } else if ( !strcasecmp(argv[k],"write_cell_2d_logical") ) {
            k++;
            if ( NULL == output_name )
                output_name = "test_write_cell_2d_logical.mat";
            err += test_write_cell_2d_logical(output_name);
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
        } else if ( !strcasecmp(argv[k],"struct_api_create") ) {
            k++;
            err += test_struct_api_create();
            ntests++;
        } else if ( !strcasecmp(argv[k],"struct_api_setfield") ) {
            k++;
            err += test_struct_api_setfield();
            ntests++;
        } else if ( !strcasecmp(argv[k],"struct_api_getfieldnames") ) {
            k++;
            err += test_struct_api_getfieldnames();
            ntests++;
        } else if ( !strcasecmp(argv[k],"struct_api_addfield") ) {
            k++;
            err += test_struct_api_addfield();
            ntests++;
        } else if ( !strcasecmp(argv[k],"struct_api_getlinear") ) {
            k++;
            err += test_struct_api_getlinear();
            ntests++;
        } else if ( !strcasecmp(argv[k],"struct_api_get") ) {
            k++;
            err += test_struct_api_get();
            ntests++;
        } else if ( !strcasecmp(argv[k],"cell_api_set") ) {
            k++;
            err += test_cell_api_set();
            ntests++;
        } else if ( !strcasecmp(argv[k],"cell_api_getlinear") ) {
            k++;
            err += test_cell_api_getlinear();
            ntests++;
        } else if ( !strcasecmp(argv[k],"cell_api_getcells") ) {
            k++;
            err += test_cell_api_getcells();
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
		if ( matvar == NULL ) {
		  err++;
		} else {
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
		}
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
            size_t *subs, dims[3] = {256,256,124};

            subs = Mat_CalcSubscripts2(3,dims,18921-1);
            Mat_Message("%" SIZE_T_FMTSTR,"%" SIZE_T_FMTSTR,"%" SIZE_T_FMTSTR,subs[0],subs[1],subs[2]);
            free(subs);
            k++;
            ntests++;
        } else if ( !strcasecmp(argv[k],"sub2ind") ) {
            size_t dims[3] = {256,256,124}, index[3] = {233,74,1};
            size_t linear_index = 0;
            int err;

            err = Mat_CalcSingleSubscript2(3,dims,index,&linear_index);
            Mat_Message("%" SIZE_T_FMTSTR,linear_index);
            k++;
            ntests++;
        } else {
            Mat_Critical("Unrecognized test %s", argv[k]);
            k++;
            break;
        }
    }

    return err;
}
