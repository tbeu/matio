/** @file io.c
 * MAT File I/O Utility Functions
 */
/*
 * Copyright (C) 2005-2008   Christopher C. Hulbert
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
#include <matio.h>

#if !defined(HAVE_VA_COPY) && defined(HAVE___VA_COPY)
#    define va_copy(d,s) __va_copy(d,s)
#elif !defined(HAVE_VA_COPY)
#    define va_copy(d,s) memcpy(&(d),&(s),sizeof(va_list))
#endif
#ifndef HAVE_VSNPRINTF
#    define vsnprintf mat_vsnprintf
#    ifdef  __cplusplus
         extern "C" int vsnprintf(char *,size_t,const char *,va_list);
#    else
         extern int vsnprintf(char *,size_t,const char *,va_list);
#    endif
#endif
#ifndef HAVE_SNPRINTF
#    define snprintf mat_snprintf
#    ifdef  __cplusplus
         extern "C" int snprintf(char *str,size_t size,const char *format,...);
#    else
         extern int snprintf(char *str,size_t size,const char *format,...);
#    endif
#endif
#ifndef HAVE_VASPRINTF
#    define vasprintf mat_vasprintf
#endif
#ifndef HAVE_ASPRINTF
#    define asprintf mat_asprintf
#endif

/** @cond 0 */
#define LOG_LEVEL_ERROR    1
#define LOG_LEVEL_CRITICAL 1 << 1
#define LOG_LEVEL_WARNING  1 << 2
#define LOG_LEVEL_MESSAGE  1 << 3
#define LOG_LEVEL_DEBUG    1 << 4
/** @endcond */

static void (*logfunc)(int log_level, char *message ) = NULL;
static char *progname = NULL;

/** @brief Allocates and prints to a new string
 *
 * @ingroup mat_util
 * @param format format string
 * @param ap variable argument list
 * @return Newly allocated string with format printed to it
 */
char *
strdup_vprintf(const char* format, va_list ap)
{
  va_list ap2;
  int size;
  char* buffer;

  va_copy(ap2, ap);
  size = vsnprintf(NULL, 0, format, ap2)+1;
  va_end(ap2);

  buffer = malloc(size+1);
  if ( !buffer )
      return NULL;

  vsnprintf(buffer, size, format, ap);
  return buffer;
}

/** @brief Allocates and prints to a new string using printf format
 *
 * @ingroup mat_util
 * @param format format string
 * @return Pointer to resulting string, or NULL if there was an error
 */
char *
strdup_printf(const char* format, ...)
{
  char* buffer;
  va_list ap;
  va_start(ap, format);
  buffer = strdup_vprintf(format, ap);
  va_end(ap);
  return buffer;
}

static void
matio_error_func( int log_level, char *message )
{

    if ( progname ) {
        if ( log_level & LOG_LEVEL_CRITICAL) {
            fprintf(stderr,"-E- %s: %s\n", progname, message);
            fflush(stderr);
        } else if ( log_level & LOG_LEVEL_ERROR ) {
            fprintf(stderr,"-E- %s: %s\n", progname, message);
            fflush(stderr);
            abort();
        } else if ( log_level & LOG_LEVEL_WARNING ) {
            fprintf(stderr,"-W- %s: %s\n", progname, message);
            fflush(stderr);
        } else if ( log_level & LOG_LEVEL_DEBUG ) {
            fprintf(stderr,"-D- %s: %s\n", progname, message);
            fflush(stderr);
        } else if ( log_level & LOG_LEVEL_MESSAGE ) {
            fprintf(stdout,"%s\n", message);
            fflush(stdout);
        }
    } else {
        if ( log_level & LOG_LEVEL_CRITICAL) {
            fprintf(stderr,"-E- : %s\n", message);
            fflush(stderr);
        } else if ( log_level & LOG_LEVEL_ERROR ) {
            fprintf(stderr,"-E- : %s\n", message);
            fflush(stderr);
            abort();
        } else if ( log_level & LOG_LEVEL_WARNING ) {
            fprintf(stderr,"-W- : %s\n", message);
            fflush(stderr);
        } else if ( log_level & LOG_LEVEL_DEBUG ) {
            fprintf(stderr,"-D- : %s\n", message);
            fflush(stderr);
        } else if ( log_level & LOG_LEVEL_MESSAGE ) {
            fprintf(stdout,"%s\n", message);
            fflush(stdout);
        }
    }

}

static void
mat_log(int loglevel, const char *format, va_list ap)
{
    char* buffer;

    if ( !logfunc ) return;
    buffer = strdup_vprintf(format, ap);
    (*logfunc)(loglevel,buffer);
    free(buffer);
    return;
}


/** @var debug
 *  @brief holds the verbose level set in @ref SetVerbose
 *  This variable is used to determine if information should be printed to
 *  the screen
 *  @ingroup mat_util
 */
static int debug = 0;

/** @var verbose
 *  @brief holds the verbose level set in @ref SetVerbose
 *  This variable is used to determine if information should be printed to
 *  the screen
 *  @ingroup mat_util
 */
static int verbose = 0;
/** @var silent
 *  @brief holds the silent level set in @ref SetVerbose
 *  If set, all output which is not an error is not displayed regardless
 *  of verbose level
 *  @ingroup mat_util
 */
static int silent = 0;
/** @brief Sets verbose parameters
 *
 *  Sets the verbose level and silent level.  These values are used by
 *  programs to determine what information should be printed to the screen
 *  @ingroup mat_util
 *  @param verb sets logging verbosity level
 *  @param s sets logging silent level
 */
int
Mat_SetVerbose( int verb, int s )
{

    verbose = verb;
    silent  = s;

    return 0;
}

/** @brief Sets verbose parameters
 *
 *  Sets the verbose level and silent level.  These values are used by
 *  programs to determine what information should be printed to the screen
 *  @ingroup mat_util
 *  @param verb sets logging verbosity level
 *  @param s sets logging silent level
 */
int
Mat_SetDebug( int d )
{
    debug = d;
    return 0;
}

/** @brief Log a message unless silent
 *
 * Logs the message unless the silent option is set (See @ref SetVerbose).
 * To log a message based on the verbose level, use @ref Mat_VerbMessage
 * @ingroup mat_util
 * @param format message format
 */
int Mat_Message( const char *format, ... )
{
    va_list ap;

    if ( silent ) return 0;
    if ( !logfunc ) return 0;

    va_start(ap, format );
    mat_log(LOG_LEVEL_MESSAGE, format, ap );
    va_end(ap);
    return 0;
}

/** @brief Log a message based on verbose level
 *
 *  If @e level is less than or equal to the set verbose level, the message
 *  is printed.  If the level is higher than the set verbose level nothing
 *  is displayed.
 *  @ingroup mat_util
 *  @param level verbose level
 *  @param format message format
 */
int Mat_DebugMessage( int level, const char *format, ... )
{
    va_list ap;

    if ( silent ) return 0;
    if ( level > debug ) return 0;

    va_start(ap, format );
    mat_log(LOG_LEVEL_DEBUG, format, ap );
    va_end(ap);
    return 0;
}

/** @brief Log a message based on verbose level
 *
 *  If @e level is less than or equal to the set verbose level, the message
 *  is printed.  If the level is higher than the set verbose level nothing
 *  is displayed.
 *  @ingroup mat_util
 *  @param level verbose level
 *  @param format message format
 */
int Mat_VerbMessage( int level, const char *format, ... )
{
    va_list ap;

    if ( silent ) return 0;
    if ( level > verbose ) return 0;

    va_start(ap, format );
    mat_log(LOG_LEVEL_MESSAGE, format, ap );
    va_end(ap);
    return 0;
}

/** @brief Logs a Critical message and returns to the user
 *
 * Logs a Critical message and returns to the user.  If the program should
 * stop running, use @ref Mat_Error
 * @ingroup mat_util
 * @param format format string identical to printf format
 * @param ... arguments to the format string
 */
void Mat_Critical( const char *format, ... )
{
    va_list ap;

    va_start(ap, format );
    mat_log(LOG_LEVEL_CRITICAL, format, ap );
    va_end(ap);
}

/** @brief Logs a Critical message and aborts the program
 *
 * Logs an Error message and aborts
 * @ingroup mat_util
 * @param format format string identical to printf format
 * @param ... arguments to the format string
 */
void Mat_Error( const char *format, ... )
{
    va_list ap;

    va_start(ap, format );
    mat_log( LOG_LEVEL_ERROR, format, ap );
    va_end(ap);
}

/** @brief Prints a helpstring to stdout and exits with status 1
 *
 * Prints the array of strings to stdout and exits with status 1.  The array
 * of strings should have NULL as its last element
 * @code
 * char *helpstr[] = {"My Help string line1","My help string line 2",NULL};
 * Mat_Help(helpstr);
 * @endcode
 * @ingroup mat_util
 * @param helpstr array of strings with NULL as its last element
 */
void Mat_Help( const char *helpstr[] )
{
    int i;
    for (i = 0; helpstr[i] != NULL; i++)
        printf("%s\n",helpstr[i]);
    exit(EXIT_SUCCESS);
}

/** @brief Closes the logging system
 *
 * @ingroup mat_util
 * @retval 1
 */
int
Mat_LogClose( void )
{
    logfunc = NULL;
    return 1;
}

/** @brief Intializes the logging system
 *
 * @ingroup mat_util
 * @param prog_name Name of the program initializing the logging functions
 * @return 0 on success
 */
int
Mat_LogInit( char *prog_name )
{
    logfunc = &matio_error_func;

    verbose = 0;
    silent  = 0;

    return 0;
}

/** @brief Intializes the logging system
 *
 * @ingroup mat_util
 * @param prog_name Name of the program initializing the logging functions
 * @param log_func pointer to the function to do the logging
 * @return 0 on success
 */
int
Mat_LogInitFunc(char *prog_name,void (*log_func)(int log_level,char *message))
{
    logfunc = log_func;
    progname = prog_name;

    verbose = 0;
    silent  = 0;
    return 0;
}

/** @brief Prints a warning message to stdout
 *
 * Logs a warning message then returns
 * @ingroup mat_util
 * @param format format string identical to printf format
 * @param ... arguments to the format string
 */
void
Mat_Warning( const char *format, ... )
{
    va_list ap;

    va_start(ap, format );
    mat_log(LOG_LEVEL_WARNING, format, ap );
    va_end(ap);
}

/** @brief Calculate the size of MAT data types
 *
 * @ingroup mat_util
 * @param data_type Data type enumeration
 * @return size of the data type in bytes
 */
size_t
Mat_SizeOf(enum matio_types data_type)
{
    switch (data_type) {
        case MAT_T_DOUBLE:
            return sizeof(double);
        case MAT_T_SINGLE:
            return sizeof(float);
#ifdef HAVE_MAT_INT64_T
        case MAT_T_INT64:
            return sizeof(mat_int64_t);
#endif
#ifdef HAVE_MAT_INT64_T
        case MAT_T_UINT64:
            return sizeof(mat_uint64_t);
#endif
        case MAT_T_INT32:
            return sizeof(mat_int32_t);
        case MAT_T_UINT32:
            return sizeof(mat_uint32_t);
        case MAT_T_INT16:
            return sizeof(mat_int16_t);
        case MAT_T_UINT16:
            return sizeof(mat_uint16_t);
        case MAT_T_INT8:
            return sizeof(mat_int8_t);
        case MAT_T_UINT8:
            return sizeof(mat_uint8_t);
        default:
            return 0;
    }
}
