/*
 * Copyright (c) 2015-2026, The matio contributors
 * Copyright (c) 2005-2014, Christopher C. Hulbert
 * All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include "matioConfig.h"
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

/* snprintf.c */
#if !HAVE_SNPRINTF
int rpl_snprintf(char *, size_t, const char *, ...);
#define mat_snprintf rpl_snprintf
#else
#define mat_snprintf snprintf
#endif /* !HAVE_SNPRINTF */

static const char *optstring = "df:hvo:HV";
static struct option options[] = {
    {"data", no_argument, NULL, 'd'},         {"format", required_argument, NULL, 'f'},
    {"human", no_argument, NULL, 'h'},        {"verbose", optional_argument, NULL, 'v'},
    {"help", no_argument, NULL, 'H'},         {"version", no_argument, NULL, 'V'},
    {"output", required_argument, NULL, 'o'}, {NULL, 0, NULL, 0}};

static const char *helpstr[] = {"",
                                "Usage: matdump [OPTIONS] mat_file [var1 var2 ...]",
                                "",
                                "Dumps the variables of the MAT file using libmatio",
                                "",
                                "OPTIONS",
                                "-d,--data         Print data with header information",
                                "-f,--format whos  Turn on 'whos' display mode",
                                "-h,--human        Human-readable output",
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
static void print_whos(const matvar_t *matvar);
static void print_default(const matvar_t *matvar);

/* Helper: print MAT_C_CHAR data as a string (handles UTF-8 and UTF-16) */
static void
print_char_as_string(const matvar_t *matvar)
{
    if ( NULL == matvar || NULL == matvar->data || matvar->class_type != MAT_C_CHAR )
        return;
    if ( matvar->data_type == MAT_T_UINT16 || matvar->data_type == MAT_T_UTF16 ) {
        const mat_uint16_t *u16 = (const mat_uint16_t *)matvar->data;
        size_t len = matvar->nbytes / sizeof(mat_uint16_t);
        size_t i;
        for ( i = 0; i < len; i++ ) {
            if ( u16[i] < 128 )
                putchar((char)u16[i]);
            else
                printf("\\u%04X", u16[i]);
        }
    } else {
        size_t len = matvar->nbytes / matvar->data_size;
        printf("%.*s", (int)len, (const char *)matvar->data);
    }
}

/* Helper: get length of a MAT_C_CHAR variable as a plain ASCII string */
static size_t
get_char_strlen(const matvar_t *matvar)
{
    if ( NULL == matvar || NULL == matvar->data || matvar->class_type != MAT_C_CHAR )
        return 0;
    if ( matvar->data_type == MAT_T_UINT16 || matvar->data_type == MAT_T_UTF16 )
        return matvar->nbytes / sizeof(mat_uint16_t);
    return matvar->nbytes / matvar->data_size;
}

/* Helper: snprintf a char field into buf, returns length written */
static int
snprint_char(char *buf, size_t bufsz, const matvar_t *matvar)
{
    size_t len, i;
    int pos = 0;
    if ( NULL == matvar || NULL == matvar->data || matvar->class_type != MAT_C_CHAR )
        return 0;
    if ( matvar->data_type == MAT_T_UINT16 || matvar->data_type == MAT_T_UTF16 ) {
        const mat_uint16_t *u16 = (const mat_uint16_t *)matvar->data;
        len = matvar->nbytes / sizeof(mat_uint16_t);
        for ( i = 0; i < len && (size_t)pos < bufsz - 1; i++ ) {
            if ( u16[i] < 128 )
                buf[pos++] = (char)u16[i];
            else {
                int n = mat_snprintf(buf + pos, bufsz - pos, "\\u%04X", u16[i]);
                if ( n > 0 )
                    pos += n;
            }
        }
    } else {
        len = matvar->nbytes / matvar->data_size;
        for ( i = 0; i < len && (size_t)pos < bufsz - 1; i++ )
            buf[pos++] = ((const char *)matvar->data)[i];
    }
    buf[pos] = '\0';
    return pos;
}

#if defined(MCOS) && MCOS
/* Helper: check if a uint32 column is an MCOS-encoded reference */
static int
is_mcos_encoded(const matvar_t *col)
{
    if ( col != NULL && col->class_type == MAT_C_UINT32 && col->data != NULL ) {
        const mat_uint32_t *u32 = (const mat_uint32_t *)col->data;
        size_t n = 1;
        int r;
        for ( r = 0; r < col->rank; r++ )
            n *= col->dims[r];
        if ( n >= 2 && u32[0] == 0xDD000000u )
            return 1;
    }
    return 0;
}

/* Column type identifiers for table display */
#define COL_TYPE_NUMERIC 0
#define COL_TYPE_STRING 1
#define COL_TYPE_DATETIME 2
#define COL_TYPE_CATEG 3
#define COL_TYPE_CHAR 4
#define COL_TYPE_DURATION 5
#define COL_TYPE_CELL 6
#define COL_TYPE_UNKNOWN 7

#define TABLE_MAX_COLS 64
#define TABLE_CELL_BUF 256

/** @brief Extract one string from a packed uint64 MCOS string object
 *
 * The "any" field in a string object is packed UTF-16LE in uint64:
 * u64[0..3]: header (ndims info, ncols, nstrs, flags)
 * u64[4..4+nstrs-1]: length of each string in UTF-16 chars
 * Packed UTF-16LE character data follows at 2 bytes per char.
 *
 * @param any   The "any" field (uint64 array)
 * @param row   Row index (0-based)
 * @param buf   Output buffer
 * @param bufsz Buffer size
 * @return Number of chars written (not counting NUL)
 */
static int
snprint_mcos_string_row(const matvar_t *any, int row, char *buf, size_t bufsz)
{
    const mat_uint64_t *u64;
    size_t n = 1, nstrs, header_len, char_offset, slen, j;
    const unsigned char *bytes;
    int pos = 0, r;

    if ( any == NULL || any->class_type != MAT_C_UINT64 || any->data == NULL )
        return 0;

    u64 = (const mat_uint64_t *)any->data;
    for ( r = 0; r < any->rank; r++ )
        n *= any->dims[r];

    if ( n <= 4 )
        return 0;

    nstrs = (size_t)u64[2];
    if ( (size_t)row >= nstrs )
        return 0;

    header_len = 4 + nstrs;
    if ( header_len >= n )
        return 0;

    bytes = (const unsigned char *)&u64[header_len];

    /* Compute char offset for this row */
    char_offset = 0;
    for ( j = 0; j < (size_t)row; j++ )
        char_offset += (size_t)u64[4 + j];

    slen = (size_t)u64[4 + row];

    for ( j = 0; j < slen && (size_t)pos < bufsz - 1; j++ ) {
        size_t byte_idx = (char_offset + j) * 2;
        mat_uint16_t ch =
            (mat_uint16_t)(bytes[byte_idx] | ((mat_uint16_t)bytes[byte_idx + 1] << 8));
        if ( ch >= 32 && ch < 127 )
            buf[pos++] = (char)ch;
        else if ( ch == 0 )
            break;
        else {
            int nn = mat_snprintf(buf + pos, bufsz - pos, "\\u%04X", ch);
            if ( nn > 0 )
                pos += nn;
        }
    }
    buf[pos] = '\0';
    return pos;
}

/** @brief Format a quoted MCOS string row into a buffer.
 *
 * Wraps the result of snprint_mcos_string_row in double quotes.
 */
static int
snprint_quoted_string_row(const matvar_t *any, int row, char *buf, size_t bufsz)
{
    int len;
    if ( bufsz < 3 ) {
        if ( bufsz > 0 )
            buf[0] = '\0';
        return 0;
    }
    buf[0] = '"';
    len = snprint_mcos_string_row(any, row, buf + 1, bufsz - 2);
    buf[1 + len] = '"';
    buf[2 + len] = '\0';
    return 2 + len;
}

/** @brief Format a datetime value from milliseconds since Unix epoch
 *
 * Converts MATLAB datetime.data (milliseconds since 1970-01-01 UTC)
 * to dd-MMM-yyyy format.
 *
 * @param ms    Millisecond value (since Unix epoch)
 * @param buf   Output buffer
 * @param bufsz Buffer size
 * @return Number of chars written
 */
static int
snprint_datetime_ms(double ms, char *buf, size_t bufsz)
{
    static const char *month_names[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
    static const int days_in_month[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    double sec = ms / 1000.0;
    int days, year, month, day, is_leap, dim;

    /* Convert seconds since Unix epoch to days, rounding toward zero */
    if ( sec >= 0 )
        days = (int)(sec / 86400.0);
    else
        days = (int)(sec / 86400.0) - 1;

    /* Start from 1970-01-01 and advance by years */
    year = 1970;
    while ( days > 0 ) {
        is_leap = (year % 4 == 0 && (year % 100 != 0 || year % 400 == 0));
        dim = is_leap ? 366 : 365;
        if ( days < dim )
            break;
        days -= dim;
        year++;
    }
    while ( days < 0 ) {
        year--;
        is_leap = (year % 4 == 0 && (year % 100 != 0 || year % 400 == 0));
        days += is_leap ? 366 : 365;
    }

    /* Advance by months */
    is_leap = (year % 4 == 0 && (year % 100 != 0 || year % 400 == 0));
    month = 0;
    while ( month < 11 ) {
        dim = days_in_month[month];
        if ( month == 1 && is_leap )
            dim++;
        if ( days < dim )
            break;
        days -= dim;
        month++;
    }
    day = days + 1;

    return mat_snprintf(buf, bufsz, "%02d-%s-%04d", day, month_names[month], year);
}

/** @brief Extract a category name for a given row
 *
 * @param cat   Categorical object
 * @param row   Row index (0-based)
 * @param buf   Output buffer
 * @param bufsz Buffer size
 * @return Number of chars written
 */
static int
snprint_categorical_row(const matvar_t *cat, int row, char *buf, size_t bufsz)
{
    matvar_t *names = Mat_VarGetStructFieldByName(cat, "categoryNames", 0);
    matvar_t *codes = Mat_VarGetStructFieldByName(cat, "codes", 0);
    mat_uint8_t code;
    matvar_t **cells;
    size_t ncats;

    if ( names == NULL || names->class_type != MAT_C_CELL || names->data == NULL || codes == NULL ||
         codes->data == NULL )
        return 0;

    cells = (matvar_t **)names->data;
    ncats = names->nbytes / names->data_size;

    if ( codes->data_type == MAT_T_UINT8 )
        code = ((mat_uint8_t *)codes->data)[row];
    else if ( codes->data_type == MAT_T_UINT16 )
        code = (mat_uint8_t)((mat_uint16_t *)codes->data)[row];
    else
        return 0;

    if ( code > 0 && code <= ncats && cells[code - 1] != NULL &&
         cells[code - 1]->class_type == MAT_C_CHAR ) {
        return snprint_char(buf, bufsz, cells[code - 1]);
    } else if ( code == 0 ) {
        return mat_snprintf(buf, bufsz, "<undefined>");
    }

    return mat_snprintf(buf, bufsz, "<%d>", code);
}

/** @brief Format a duration value from milliseconds
 *
 * Shows duration as hh:mm:ss or d days hh:mm:ss.
 *
 * @param ms    Duration in milliseconds
 * @param buf   Output buffer
 * @param bufsz Buffer size
 * @return Number of chars written
 */
static int
snprint_duration_ms(double ms, char *buf, size_t bufsz)
{
    double total_sec = ms / 1000.0;
    int hours, minutes, seconds;
    int neg = 0;

    if ( total_sec < 0 ) {
        neg = 1;
        total_sec = -total_sec;
    }

    hours = (int)(total_sec / 3600.0);
    total_sec -= hours * 3600.0;
    minutes = (int)(total_sec / 60.0);
    total_sec -= minutes * 60.0;
    seconds = (int)total_sec;

    if ( neg )
        return mat_snprintf(buf, bufsz, "-%02d:%02d:%02d", hours, minutes, seconds);
    return mat_snprintf(buf, bufsz, "%02d:%02d:%02d", hours, minutes, seconds);
}

/** @brief Snprint a single value from an arbitrary matvar for table display
 *
 * Handles double, char, objects (string/datetime/duration/categorical),
 * structs (prints '{struct}'), cells, etc.
 *
 * @param v     The matvar_t cell element
 * @param buf   Output buffer
 * @param bufsz Buffer size
 * @return Number of chars written
 */
static int
snprint_cell_value(const matvar_t *v, char *buf, size_t bufsz)
{
    if ( v == NULL )
        return mat_snprintf(buf, bufsz, "[]");

    if ( v->class_type == MAT_C_DOUBLE && v->data != NULL ) {
        return mat_snprintf(buf, bufsz, "%g", *(const double *)v->data);
    } else if ( v->class_type == MAT_C_SINGLE && v->data != NULL ) {
        return mat_snprintf(buf, bufsz, "%g", (double)*(const float *)v->data);
    } else if ( v->class_type == MAT_C_CHAR && v->data != NULL ) {
        int pos = 0;
        if ( (size_t)pos < bufsz - 1 )
            buf[pos++] = '\"';
        pos += snprint_char(buf + pos, bufsz - pos, v);
        if ( (size_t)pos < bufsz - 1 )
            buf[pos++] = '\"';
        buf[pos] = '\0';
        return pos;
    } else if ( v->class_type == MAT_C_OBJECT ) {
        const char *cls = Mat_VarGetClassName(v);
        if ( cls != NULL && 0 == strcmp(cls, "datetime") ) {
            const matvar_t *data_f = Mat_VarGetStructFieldByName(v, "data", 0);
            if ( data_f != NULL && data_f->class_type == MAT_C_DOUBLE && data_f->data != NULL )
                return snprint_datetime_ms(*(const double *)data_f->data, buf, bufsz);
        } else if ( cls != NULL &&
                    (0 == strcmp(cls, "duration") || 0 == strcmp(cls, "calendarDuration")) ) {
            const matvar_t *millis = Mat_VarGetStructFieldByName(v, "millis", 0);
            if ( millis != NULL && millis->class_type == MAT_C_DOUBLE && millis->data != NULL )
                return snprint_duration_ms(*(const double *)millis->data, buf, bufsz);
        } else if ( cls != NULL && 0 == strcmp(cls, "string") ) {
            const matvar_t *any = Mat_VarGetStructFieldByName(v, "any", 0);
            return snprint_quoted_string_row(any, 0, buf, bufsz);
        } else if ( cls != NULL && 0 == strcmp(cls, "categorical") ) {
            return snprint_categorical_row(v, 0, buf, bufsz);
        }
        if ( cls != NULL )
            return mat_snprintf(buf, bufsz, "[%s]", cls);
        return mat_snprintf(buf, bufsz, "[object]");
    } else if ( v->class_type == MAT_C_STRUCT ) {
        return mat_snprintf(buf, bufsz, "{struct}");
    } else if ( v->class_type == MAT_C_EMPTY ) {
        return mat_snprintf(buf, bufsz, "[]");
    }
    return mat_snprintf(buf, bufsz, "...");
}

/** @brief Identify the display type of a table column
 *
 * @param col  The column matvar_t
 * @return COL_TYPE_* constant
 */
static int
get_table_col_type(const matvar_t *col)
{
    if ( col == NULL )
        return COL_TYPE_UNKNOWN;

    if ( col->class_type == MAT_C_DOUBLE || col->class_type == MAT_C_SINGLE )
        return COL_TYPE_NUMERIC;

    if ( col->class_type == MAT_C_CHAR )
        return COL_TYPE_CHAR;

    if ( col->class_type == MAT_C_OBJECT ) {
        const char *cls = Mat_VarGetClassName(col);
        if ( cls != NULL ) {
            if ( 0 == strcmp(cls, "string") )
                return COL_TYPE_STRING;
            if ( 0 == strcmp(cls, "datetime") )
                return COL_TYPE_DATETIME;
            if ( 0 == strcmp(cls, "categorical") )
                return COL_TYPE_CATEG;
            if ( 0 == strcmp(cls, "duration") || 0 == strcmp(cls, "calendarDuration") )
                return COL_TYPE_DURATION;
        }
    }

    if ( col->class_type == MAT_C_CELL )
        return COL_TYPE_CELL;

    return COL_TYPE_UNKNOWN;
}

/** @brief Format one table cell into a buffer.
 *
 * @param col       Column variable
 * @param col_type  COL_TYPE_* constant
 * @param row       Row index
 * @param buf       Output buffer
 * @param bufsz     Buffer size
 * @return Number of characters written (excluding NUL).
 */
static int
snprint_table_cell(const matvar_t *col, int col_type, int row, char *buf, size_t bufsz)
{
    switch ( col_type ) {
        case COL_TYPE_NUMERIC:
            if ( col->class_type == MAT_C_DOUBLE && col->data != NULL )
                return mat_snprintf(buf, bufsz, "%g", ((const double *)col->data)[row]);
            if ( col->class_type == MAT_C_SINGLE && col->data != NULL )
                return mat_snprintf(buf, bufsz, "%g", (double)((const float *)col->data)[row]);
            break;

        case COL_TYPE_STRING: {
            const matvar_t *any = Mat_VarGetStructFieldByName(col, "any", 0);
            return snprint_quoted_string_row(any, row, buf, bufsz);
        }

        case COL_TYPE_DATETIME: {
            const matvar_t *data_f = Mat_VarGetStructFieldByName(col, "data", 0);
            if ( data_f != NULL && data_f->class_type == MAT_C_DOUBLE && data_f->data != NULL )
                return snprint_datetime_ms(((const double *)data_f->data)[row], buf, bufsz);
            break;
        }

        case COL_TYPE_CATEG:
            return snprint_categorical_row(col, row, buf, bufsz);

        case COL_TYPE_CHAR:
            if ( col->data != NULL && row == 0 ) {
                int len;
                buf[0] = '"';
                len = snprint_char(buf + 1, bufsz > 2 ? bufsz - 2 : 0, col);
                if ( (size_t)(2 + len) < bufsz ) {
                    buf[1 + len] = '"';
                    buf[2 + len] = '\0';
                }
                return 2 + len;
            }
            buf[0] = '\0';
            return 0;

        case COL_TYPE_DURATION: {
            const matvar_t *millis = Mat_VarGetStructFieldByName(col, "millis", 0);
            if ( millis != NULL && millis->class_type == MAT_C_DOUBLE && millis->data != NULL )
                return snprint_duration_ms(((const double *)millis->data)[row], buf, bufsz);
            break;
        }

        case COL_TYPE_CELL:
            if ( col->data != NULL ) {
                matvar_t **cell_elems = (matvar_t **)col->data;
                return snprint_cell_value(cell_elems[row], buf, bufsz);
            }
            break;

        default:
            break;
    }
    return mat_snprintf(buf, bufsz, "...");
}

/* Print human-readable table in MATLAB-style tabular format */
static void
print_human_table(const matvar_t *matvar)
{
    matvar_t *varnames_var, *data_var;
    const matvar_t *props;
    matvar_t *f;
    int nrows = 0;
    int nvars = 0;
    int col_width[TABLE_MAX_COLS];
    int col_type[TABLE_MAX_COLS];
    char hdr[TABLE_MAX_COLS][TABLE_CELL_BUF];
    matvar_t **cols = NULL;
    matvar_t **vnames = NULL;
    int ncols_actual = 0;
    int i, row;

    f = Mat_VarGetStructFieldByName(matvar, "nrows", 0);
    if ( f != NULL && f->class_type == MAT_C_DOUBLE && f->data != NULL )
        nrows = (int)*(double *)f->data;
    f = Mat_VarGetStructFieldByName(matvar, "nvars", 0);
    if ( f != NULL && f->class_type == MAT_C_DOUBLE && f->data != NULL )
        nvars = (int)*(double *)f->data;

    /* Print header */
    printf("\n  %s = %dx%d table\n\n", matvar->name ? matvar->name : "ans", nrows, nvars);

    varnames_var = Mat_VarGetStructFieldByName(matvar, "varnames", 0);
    data_var = Mat_VarGetStructFieldByName(matvar, "data", 0);

    if ( NULL == varnames_var || varnames_var->class_type != MAT_C_CELL ||
         NULL == varnames_var->data )
        return;
    vnames = (matvar_t **)varnames_var->data;

    if ( NULL != data_var && data_var->class_type == MAT_C_CELL && NULL != data_var->data ) {
        cols = (matvar_t **)data_var->data;
        ncols_actual = (int)(data_var->nbytes / data_var->data_size);
    }

    if ( nvars > TABLE_MAX_COLS ) {
        Mat_Warning("Table has %d columns, only displaying first %d", nvars, TABLE_MAX_COLS);
        nvars = TABLE_MAX_COLS;
    }

    /* Pass 1: determine column headers, types, and widths */
    for ( i = 0; i < nvars; i++ ) {
        int w;
        hdr[i][0] = '\0';
        if ( vnames[i] != NULL && vnames[i]->class_type == MAT_C_CHAR )
            snprint_char(hdr[i], TABLE_CELL_BUF, vnames[i]);
        w = (int)strlen(hdr[i]);
        if ( w < 4 )
            w = 4;
        col_width[i] = w;
        col_type[i] = COL_TYPE_UNKNOWN;

        if ( printdata && i < ncols_actual && cols[i] != NULL ) {
            int j;
            col_type[i] = get_table_col_type(cols[i]);
            for ( j = 0; j < nrows; j++ ) {
                char tmp[TABLE_CELL_BUF];
                int len = snprint_table_cell(cols[i], col_type[i], j, tmp, sizeof(tmp));
                if ( len > col_width[i] )
                    col_width[i] = len;
            }
        }
    }

    /* Pass 2: print column headers */
    printf("    ");
    for ( i = 0; i < nvars; i++ ) {
        if ( i > 0 )
            printf("    ");
        if ( col_type[i] == COL_TYPE_NUMERIC )
            printf("%*s", col_width[i], hdr[i]);
        else
            printf("%-*s", col_width[i], hdr[i]);
    }
    printf("\n");

    /* Print underlines */
    printf("    ");
    for ( i = 0; i < nvars; i++ ) {
        int j;
        if ( i > 0 )
            printf("    ");
        for ( j = 0; j < col_width[i]; j++ )
            putchar('_');
    }
    printf("\n\n");

    /* Pass 3: print data rows */
    if ( !printdata || ncols_actual == 0 )
        return;

    for ( row = 0; row < nrows; row++ ) {
        printf("    ");
        for ( i = 0; i < nvars; i++ ) {
            char tmp[TABLE_CELL_BUF];
            if ( i > 0 )
                printf("    ");
            if ( i >= ncols_actual || cols[i] == NULL ) {
                printf("%-*s", col_width[i], "...");
                continue;
            }
            snprint_table_cell(cols[i], col_type[i], row, tmp, sizeof(tmp));
            if ( col_type[i] == COL_TYPE_NUMERIC )
                printf("%*s", col_width[i], tmp);
            else
                printf("%-*s", col_width[i], tmp);
        }
        printf("\n");
    }

    /* Print description from props if available */
    props = Mat_VarGetStructFieldByName(matvar, "props", 0);
    if ( props != NULL && props->class_type == MAT_C_STRUCT ) {
        const matvar_t *desc = Mat_VarGetStructFieldByName(props, "Description", 0);
        if ( desc != NULL && desc->class_type == MAT_C_CHAR && desc->data != NULL &&
             desc->nbytes > 0 ) {
            printf("\n  Description: '");
            print_char_as_string(desc);
            printf("'\n");
        }
    }
}

/* Print human-readable string object */
static void
print_human_string(const matvar_t *matvar)
{
    const matvar_t *any = Mat_VarGetStructFieldByName(matvar, "any", 0);
    Mat_Message("%s = string:", matvar->name ? matvar->name : "");
    if ( any != NULL && any->class_type == MAT_C_UINT64 && any->data != NULL ) {
        const mat_uint64_t *u64 = (const mat_uint64_t *)any->data;
        size_t n = 1;
        int r;
        for ( r = 0; r < any->rank; r++ )
            n *= any->dims[r];
        if ( n > 4 ) {
            size_t nstrs = (size_t)u64[2];
            size_t header_len = 4 + nstrs;
            if ( header_len < n && nstrs > 0 && nstrs < 10000 ) {
                size_t s;
                for ( s = 0; s < nstrs && s < 20; s++ ) {
                    char buf[256];
                    snprint_quoted_string_row(any, (int)s, buf, sizeof(buf));
                    printf("  %s\n", buf);
                }
                if ( nstrs > 20 )
                    printf("  ... (%" SIZE_T_FMTSTR " strings total)\n", nstrs);
            }
        }
    }
}

/* Print human-readable datetime or duration object */
static void
print_human_millis(const matvar_t *matvar, const char *label, const char *field)
{
    matvar_t *f = Mat_VarGetStructFieldByName(matvar, field, 0);

    Mat_Message("%s = %s:", matvar->name ? matvar->name : "", label);
    if ( f != NULL && f->class_type == MAT_C_DOUBLE && f->data != NULL ) {
        const double *d = (const double *)f->data;
        size_t n = 1;
        int r;
        for ( r = 0; r < f->rank; r++ )
            n *= f->dims[r];
        if ( n > 0 ) {
            size_t i;
            printf("  ");
            for ( i = 0; i < n && i < 10; i++ ) {
                if ( i > 0 )
                    printf(", ");
                printf("%g ms", d[i]);
            }
            if ( n > 10 )
                printf(", ... (%" SIZE_T_FMTSTR " total)", n);
            printf("\n");
        }
    }
}

/* Print human-readable datetime object */
static void
print_human_datetime(const matvar_t *matvar)
{
    print_human_millis(matvar, "datetime", "data");
}

/* Print human-readable categorical object */
static void
print_human_categorical(const matvar_t *matvar)
{
    matvar_t *names = Mat_VarGetStructFieldByName(matvar, "categoryNames", 0);
    matvar_t *codes = Mat_VarGetStructFieldByName(matvar, "codes", 0);

    Mat_Message("%s = categorical:", matvar->name ? matvar->name : "");
    if ( names != NULL && names->class_type == MAT_C_CELL && names->data != NULL ) {
        matvar_t **cells = (matvar_t **)names->data;
        size_t ncats = names->nbytes / names->data_size;
        size_t i;
        printf("  Categories: {");
        for ( i = 0; i < ncats; i++ ) {
            if ( i > 0 )
                printf(", ");
            if ( cells[i] != NULL && cells[i]->class_type == MAT_C_CHAR )
                print_char_as_string(cells[i]);
        }
        printf("}\n");

        /* Map codes to names */
        if ( printdata && codes != NULL && codes->data != NULL ) {
            size_t n = 1;
            int r;
            for ( r = 0; r < codes->rank; r++ )
                n *= codes->dims[r];
            if ( n > 0 ) {
                printf("  Values: ");
                for ( i = 0; i < n && i < 10; i++ ) {
                    mat_uint8_t code = ((mat_uint8_t *)codes->data)[i];
                    if ( i > 0 )
                        printf(", ");
                    if ( code > 0 && code <= ncats && cells[code - 1] != NULL &&
                         cells[code - 1]->class_type == MAT_C_CHAR ) {
                        print_char_as_string(cells[code - 1]);
                    } else if ( code == 0 ) {
                        printf("<undefined>");
                    } else {
                        printf("<%d>", code);
                    }
                }
                if ( n > 10 )
                    printf(", ... (%" SIZE_T_FMTSTR " total)", n);
                printf("\n");
            }
        }
    }
}

/* Print human-readable containers.Map object */
static void
print_human_map(const matvar_t *matvar)
{
    const matvar_t *ser, *keys, *values, *kt, *vt;
    size_t nkeys = 0;

    ser = Mat_VarGetStructFieldByName(matvar, "serialization", 0);
    if ( ser == NULL || ser->class_type != MAT_C_STRUCT ) {
        Mat_Message("%s = Map (empty)", matvar->name ? matvar->name : "ans");
        return;
    }

    keys = Mat_VarGetStructFieldByName(ser, "keys", 0);
    values = Mat_VarGetStructFieldByName(ser, "values", 0);
    kt = Mat_VarGetStructFieldByName(ser, "keyType", 0);
    vt = Mat_VarGetStructFieldByName(ser, "valueType", 0);

    if ( keys != NULL && keys->class_type == MAT_C_CELL && keys->data != NULL )
        nkeys = keys->nbytes / keys->data_size;

    printf("  %s = Map (", matvar->name ? matvar->name : "ans");
    if ( kt != NULL && kt->class_type == MAT_C_CHAR && kt->data != NULL )
        print_char_as_string(kt);
    printf(" -> ");
    if ( vt != NULL && vt->class_type == MAT_C_CHAR && vt->data != NULL )
        print_char_as_string(vt);
    printf(") with %" SIZE_T_FMTSTR " %s\n", nkeys, nkeys == 1 ? "entry" : "entries");

    if ( !printdata || nkeys == 0 || keys == NULL || values == NULL )
        return;

    {
        matvar_t **kcells = (matvar_t **)keys->data;
        matvar_t **vcells = (values->class_type == MAT_C_CELL && values->data != NULL)
                                ? (matvar_t **)values->data
                                : NULL;
        size_t nvals = (vcells != NULL) ? values->nbytes / values->data_size : 0;
        size_t i;

        for ( i = 0; i < nkeys; i++ ) {
            char kbuf[TABLE_CELL_BUF], vbuf[TABLE_CELL_BUF];
            snprint_cell_value(kcells[i], kbuf, sizeof(kbuf));
            if ( vcells != NULL && i < nvals )
                snprint_cell_value(vcells[i], vbuf, sizeof(vbuf));
            else
                mat_snprintf(vbuf, sizeof(vbuf), "...");
            printf("    %s : %s\n", kbuf, vbuf);
        }
    }
}

/* Print human-readable dictionary object */
static void
print_human_dictionary(const matvar_t *matvar)
{
    const matvar_t *data, *key_var, *val_var, *uncfg;
    size_t nkeys = 0;

    data = Mat_VarGetStructFieldByName(matvar, "data", 0);
    if ( data == NULL || data->class_type != MAT_C_STRUCT ) {
        Mat_Message("%s = dictionary (empty)", matvar->name ? matvar->name : "ans");
        return;
    }

    /* Check for unconfigured empty dictionary */
    uncfg = Mat_VarGetStructFieldByName(data, "Unconfigured", 0);
    if ( uncfg != NULL && uncfg->class_type == MAT_C_UINT8 && uncfg->data != NULL &&
         *(mat_uint8_t *)uncfg->data != 0 ) {
        printf("  %s = dictionary with unset key and value types\n",
               matvar->name ? matvar->name : "ans");
        return;
    }

    key_var = Mat_VarGetStructFieldByName(data, "Key", 0);
    val_var = Mat_VarGetStructFieldByName(data, "Value", 0);

    /* Count keys */
    if ( key_var != NULL ) {
        if ( key_var->class_type == MAT_C_CELL && key_var->data != NULL )
            nkeys = key_var->nbytes / key_var->data_size;
        else if ( key_var->class_type == MAT_C_DOUBLE && key_var->data != NULL )
            nkeys = key_var->dims[0];
        else if ( key_var->class_type == MAT_C_OBJECT ) {
            /* String key: count from "any" field */
            matvar_t *any = Mat_VarGetStructFieldByName(key_var, "any", 0);
            if ( any != NULL && any->class_type == MAT_C_UINT64 && any->data != NULL ) {
                const mat_uint64_t *u64 = (const mat_uint64_t *)any->data;
                size_t n = 1;
                int r;
                for ( r = 0; r < any->rank; r++ )
                    n *= any->dims[r];
                if ( n > 4 )
                    nkeys = (size_t)u64[2];
            }
        }
    }

    printf("  %s = dictionary with %" SIZE_T_FMTSTR " %s\n", matvar->name ? matvar->name : "ans",
           nkeys, nkeys == 1 ? "entry" : "entries");

    if ( !printdata || nkeys == 0 )
        return;

    {
        size_t i;
        for ( i = 0; i < nkeys; i++ ) {
            char kbuf[TABLE_CELL_BUF], vbuf[TABLE_CELL_BUF];

            /* Format key */
            if ( key_var->class_type == MAT_C_CELL ) {
                matvar_t **kcells = (matvar_t **)key_var->data;
                snprint_cell_value(kcells[i], kbuf, sizeof(kbuf));
            } else if ( key_var->class_type == MAT_C_DOUBLE && key_var->data != NULL ) {
                mat_snprintf(kbuf, sizeof(kbuf), "%g", ((const double *)key_var->data)[i]);
            } else if ( key_var->class_type == MAT_C_OBJECT ) {
                const char *kcls = Mat_VarGetClassName(key_var);
                if ( kcls != NULL && 0 == strcmp(kcls, "string") ) {
                    const matvar_t *any = Mat_VarGetStructFieldByName(key_var, "any", 0);
                    snprint_quoted_string_row(any, (int)i, kbuf, sizeof(kbuf));
                } else {
                    mat_snprintf(kbuf, sizeof(kbuf), "[%s]", kcls ? kcls : "object");
                }
            } else {
                mat_snprintf(kbuf, sizeof(kbuf), "...");
            }

            /* Format value */
            if ( val_var == NULL ) {
                mat_snprintf(vbuf, sizeof(vbuf), "...");
            } else if ( val_var->class_type == MAT_C_CELL && val_var->data != NULL ) {
                matvar_t **vcells = (matvar_t **)val_var->data;
                size_t nvals = val_var->nbytes / val_var->data_size;
                if ( i < nvals )
                    snprint_cell_value(vcells[i], vbuf, sizeof(vbuf));
                else
                    mat_snprintf(vbuf, sizeof(vbuf), "...");
            } else if ( val_var->class_type == MAT_C_DOUBLE && val_var->data != NULL ) {
                mat_snprintf(vbuf, sizeof(vbuf), "%g", ((const double *)val_var->data)[i]);
            } else if ( val_var->class_type == MAT_C_OBJECT ) {
                const char *vcls = Mat_VarGetClassName(val_var);
                if ( vcls != NULL && 0 == strcmp(vcls, "string") ) {
                    const matvar_t *any = Mat_VarGetStructFieldByName(val_var, "any", 0);
                    snprint_quoted_string_row(any, (int)i, vbuf, sizeof(vbuf));
                } else {
                    mat_snprintf(vbuf, sizeof(vbuf), "[%s]", vcls ? vcls : "object");
                }
            } else {
                mat_snprintf(vbuf, sizeof(vbuf), "...");
            }

            printf("    %s : %s\n", kbuf, vbuf);
        }
    }
}

/* Print human-readable MCOS object. Returns 1 if handled, 0 to fall through. */
static int
print_human_object(const matvar_t *matvar)
{
    const char *cls;
    if ( matvar == NULL || matvar->class_type != MAT_C_OBJECT )
        return 0;
    cls = Mat_VarGetClassName(matvar);
    if ( cls == NULL )
        return 0;

    if ( 0 == strcmp(cls, "table") || 0 == strcmp(cls, "timetable") ) {
        print_human_table(matvar);
        return 1;
    } else if ( 0 == strcmp(cls, "string") ) {
        print_human_string(matvar);
        return 1;
    } else if ( 0 == strcmp(cls, "datetime") ) {
        print_human_datetime(matvar);
        return 1;
    } else if ( 0 == strcmp(cls, "duration") || 0 == strcmp(cls, "calendarDuration") ) {
        print_human_millis(matvar, "duration", "millis");
        return 1;
    } else if ( 0 == strcmp(cls, "categorical") ) {
        print_human_categorical(matvar);
        return 1;
    } else if ( 0 == strcmp(cls, "Map") ) {
        print_human_map(matvar);
        return 1;
    } else if ( 0 == strcmp(cls, "dictionary") ) {
        print_human_dictionary(matvar);
        return 1;
    } else {
        /* Unknown MCOS class: print summary with field names */
        int nfields = Mat_VarGetNumberOfFields(matvar);
        Mat_Message("%s = %s (%d fields)", matvar->name ? matvar->name : "", cls, nfields);
        if ( printdata && nfields > 0 ) {
            char *const *fieldnames = Mat_VarGetStructFieldnames(matvar);
            if ( fieldnames != NULL ) {
                int i;
                for ( i = 0; i < nfields; i++ ) {
                    matvar_t *fld = Mat_VarGetStructFieldByName(matvar, fieldnames[i], 0);
                    printf("    .%s", fieldnames[i]);
                    if ( fld == NULL ) {
                        printf(" = (null)\n");
                    } else if ( fld->class_type == MAT_C_DOUBLE && fld->data != NULL &&
                                fld->rank == 2 && fld->dims[0] == 1 && fld->dims[1] == 1 ) {
                        printf(" = %g\n", *(double *)fld->data);
                    } else if ( fld->class_type == MAT_C_CHAR && fld->data != NULL ) {
                        printf(" = '");
                        print_char_as_string(fld);
                        printf("'\n");
                    } else {
                        size_t n = 1;
                        int r;
                        for ( r = 0; r < fld->rank; r++ )
                            n *= fld->dims[r];
                        printf(": [%" SIZE_T_FMTSTR " %s]\n", n, mxclass[fld->class_type]);
                    }
                }
            }
        }
        return 1;
    }
}
#endif /* MCOS */

static void (*printfunc)(const matvar_t *matvar) = NULL;

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
slab_get_rank(char *open, const char *close)
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
slab_get_select(char *open, const char *close, int rank, int *start, int *stride, int *edge)
{
    char *ptr = open;
    const char *valptr = open + 1;
    int dim = 0;
    int nvals = 0;
    do {
        ptr++;
        if ( *ptr == ',' ) {
            if ( nvals == 2 ) {
                *ptr = '\0';
                if ( !strcmp(valptr, "end") ) {
                    edge[dim] = -1;
                } else {
                    edge[dim] = (int)strtol(valptr, NULL, 10);
                }
            } else if ( nvals == 1 ) {
                *ptr = '\0';
                if ( !strcmp(valptr, "end") ) {
                    edge[dim] = -1;
                } else {
                    edge[dim] = (int)strtol(valptr, NULL, 10);
                }
            } else if ( nvals == 0 ) {
                *ptr = '\0';
                if ( !strcmp(valptr, "end") ) {
                    start[dim] = -1;
                    edge[dim] = -1;
                } else {
                    int i = (int)strtol(valptr, NULL, 10);
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
                int i = (int)strtol(valptr, NULL, 10);
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
                int i = (int)strtol(valptr, NULL, 10);
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
slab_select_valid(int rank, int *start, const int *stride, int *edge, matvar_t *matvar)
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
    int err = 1, j, done = 0;
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
                    int i;
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
            const char *varname = next_tok_pos + 1;
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
                matvar_t **cells;

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
                    matvar_t *cell = Mat_VarGetCell(matvar, j);
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
                    int i;
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
print_whos(const matvar_t *matvar)
{
    size_t nbytes;

    if ( print_whos_first ) {
        printf("%-20s       %-10s     %-10s     %-18s\n\n", "Name", "Size", "Bytes", "Class");
        print_whos_first = 0;
    }
    printf("%-20s", matvar->name);
    if ( matvar->rank > 0 ) {
        int cnt = 0, i;
        char size[32] = {
            '\0',
        };
        printf("%8" SIZE_T_FMTSTR, matvar->dims[0]);
        for ( i = 1; i < matvar->rank; i++ ) {
            if ( ceil(log10((double)matvar->dims[i])) + 1 < 32 ) {
                cnt += mat_snprintf(size + cnt, sizeof(size) - cnt, "x%" SIZE_T_FMTSTR,
                                    matvar->dims[i]);
                if ( cnt >= sizeof(size) ) {
                    break;
                }
            }
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
default_printf_func(int log_level, const char *message)
{
    size_t i;

    for ( i = 0; i < indent; i++ )
        printf("    ");
    printf("%s\n", message);
}

static void
print_default_number(enum matio_types type, const void *data)
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
print_default_numeric_2d(const matvar_t *matvar)
{
    size_t i, j, stride;

    if ( NULL == matvar->data )
        return;

    stride = Mat_SizeOf(matvar->data_type);
    if ( matvar->isComplex ) {
        const mat_complex_split_t *complex_data = (mat_complex_split_t *)matvar->data;
        const char *rp = (char *)complex_data->Re;
        const char *ip = (char *)complex_data->Im;
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
        const char *data = (char *)matvar->data;
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
print_default_numeric_3d(const matvar_t *matvar)
{
    size_t i, j, k, l, stride;

    if ( NULL == matvar->data )
        return;

    stride = Mat_SizeOf(matvar->data_type);
    if ( matvar->isComplex ) {
        const mat_complex_split_t *complex_data = (mat_complex_split_t *)matvar->data;
        const char *rp = (char *)complex_data->Re;
        const char *ip = (char *)complex_data->Im;
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
        const char *data = (char *)matvar->data;
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
print_default(const matvar_t *matvar)
{
    if ( NULL == matvar )
        return;

#if defined(MCOS) && MCOS
    /* In human-readable mode, skip unnamed variables (internal MCOS subsystem data) */
    if ( human_readable && (NULL == matvar->name || '\0' == matvar->name[0]) )
        return;
#endif

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
            if ( human_readable && matvar->name )
                Mat_Message("%s =", matvar->name);
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
                    matvar_t **fields = (matvar_t **)matvar->data;
                    if ( NULL != fields ) {
                        size_t j;
                        for ( j = 0; j < nfields * nmemb; j++ )
                            print_default(fields[j]);
                    }
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
                matvar_t **cells = (matvar_t **)matvar->data;
                if ( NULL != cells ) {
                    size_t j;
                    for ( j = 0; j < ncells; j++ )
                        print_default(cells[j]);
                }
            }
            indent--;
            Mat_Message("}");
            break;
        }
        case MAT_C_OBJECT:
#if defined(MCOS) && MCOS
            if ( human_readable && print_human_object(matvar) )
                break;
#endif
            Mat_VarPrint(matvar, printdata);
            break;
        case MAT_C_OPAQUE:
            if ( human_readable ) {
                const char *cls = Mat_VarGetClassName(matvar);
                if ( matvar->name && matvar->name[0] )
                    Mat_Message("%s [%s]", matvar->name, cls ? cls : "opaque");
            } else {
                Mat_VarPrint(matvar, printdata);
            }
            break;
        default:
            Mat_Message("Empty");
    }
}

static void
redirect_output(const char *output)
{
    if ( output != NULL )
        if ( freopen(output, "w", stdout) == NULL )
            fprintf(stderr, "Unable to open %s for writing. Using stdout instead.", output);
}

int
main(int argc, char *argv[])
{
    const char *prog_name = "matdump";
    int c, err = EXIT_SUCCESS;
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
            case 'o':
                redirect_output(optarg);
                break;
            case 'H':
                Mat_Help(helpstr);
                /* Note: Mat_Help() calls exit() */
            case 'V':
                printf(
                    "%s %s\nWritten by Christopher Hulbert\n\n"
                    "Copyright(C) 2015-2026, The matio contributors\n"
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
        int i;
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
