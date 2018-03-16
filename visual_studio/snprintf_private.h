/*
 * Copyright (c) 2012-2018, Christopher C. Hulbert
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

/* Define to 1 if you have the `asprintf' function. */
#undef HAVE_ASPRINTF

/* Define to 1 if the system has the type `intmax_t'. */
#if defined(_MSC_VER) && _MSC_VER >= 1600
#define HAVE_INTMAX_T 1
#else
#undef HAVE_INTMAX_T
#endif

/* Define to 1 if you have the <inttypes.h> header file. */
#if defined(_MSC_VER) && _MSC_VER >= 1800
#define HAVE_INTTYPES_H 1
#else
#undef HAVE_INTTYPES_H
#endif

/* Define to 1 if you have the `localeconv' function. */
#define HAVE_LOCALECONV 1

/* Define to 1 if you have the <locale.h> header file. */
#define HAVE_LOCALE_H 1

/* Define to 1 if the system has the type `long double'. */
#if defined(_MSC_VER) && _MSC_VER >= 1300
#define HAVE_LONG_DOUBLE 1
#else
#undef HAVE_LONG_DOUBLE
#endif

/* Define to 1 if the system has the type `long long int'. */
#if defined(_MSC_VER) && _MSC_VER >= 1300
#define HAVE_LONG_LONG_INT 1
#else
#undef HAVE_LONG_LONG_INT
#endif

/* Define to 1 if the system has the type `ptrdiff_t'. */
#define HAVE_PTRDIFF_T 1

/* Define to 1 if you have a C99 compliant `snprintf' function. */
#if defined(_MSC_VER) && _MSC_VER >= 1900
#define HAVE_SNPRINTF 1
#else
#undef HAVE_SNPRINTF
#endif

/* Define to 1 if you have the <stdarg.h> header file. */
#define HAVE_STDARG_H 1

/* Define to 1 if you have the <stddef.h> header file. */
#define HAVE_STDDEF_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#if defined(_MSC_VER) && _MSC_VER >= 1600
#define HAVE_STDINT_H 1
#else
#undef HAVE_STDINT_H
#endif

/* Have the <stdlib.h> header file */
#define HAVE_STDLIB_H 1

/* Define to 1 if `decimal_point' is member of `struct lconv'. */
#define HAVE_STRUCT_LCONV_DECIMAL_POINT 1

/* Define to 1 if `thousands_sep' is member of `struct lconv'. */
#define HAVE_STRUCT_LCONV_THOUSANDS_SEP 1

/* Define to 1 if the system has the type `uintmax_t'. */
#if defined(_MSC_VER) && _MSC_VER >= 1600
#define HAVE_UINTMAX_T 1
#else
#undef HAVE_UINTMAX_T
#endif

/* Define to 1 if the system has the type `uintptr_t'. */
#define HAVE_UINTPTR_T 1

/* Define to 1 if the system has the type `unsigned long long int'. */
#if defined(_MSC_VER) && _MSC_VER >= 1300
#define HAVE_UNSIGNED_LONG_LONG_INT 1
#else
#undef HAVE_UNSIGNED_LONG_LONG_INT
#endif

/* Define to 1 if you have the `vasprintf' function. */
#undef HAVE_VASPRINTF

/* Define to 1 if you have the `va_copy' function or macro. */
#if defined(_MSC_VER) && _MSC_VER >= 1800
#define HAVE_VA_COPY 1
#else
#undef HAVE_VA_COPY
#endif

/* Define to 1 if you have a C99 compliant `vsnprintf' function. */
#if defined(_MSC_VER) && _MSC_VER >= 1900
#define HAVE_VSNPRINTF 1
#else
#undef HAVE_VSNPRINTF
#endif

/* Define to 1 if you have the `__va_copy' function or macro. */
#undef HAVE___VA_COPY
