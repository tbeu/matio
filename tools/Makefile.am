## Makefile.am -- Process this file with automake to produce Makefile.in
##
## Copyright (c) 2015-2026, The matio contributors
## Copyright (c) 2005-2014, Christopher C. Hulbert
## All rights reserved.
##
## SPDX-License-Identifier: BSD-2-Clause
##
VPATH = @srcdir@

if NEED_GETOPT
    GETOPT_CFLAGS = -I$(top_builddir)/getopt
    GETOPT_LIBS   = $(top_builddir)/getopt/libgetopt.la
endif

SNPRINTF_LIBS = $(top_builddir)/snprintf/libsnprintf.la

AM_CFLAGS = -I$(top_srcdir)/src $(GETOPT_CFLAGS) $(HDF5_CFLAGS) $(ZLIB_CFLAGS) \
            $(LT_CFLAGS)
AM_LDFLAGS = $(LT_LDFLAGS) $(FCLDFLAGS)

bin_PROGRAMS = matdump

matdump_SOURCES = matdump.c 
matdump_LDADD   = $(top_builddir)/src/libmatio.la $(HDF5_LIBS) $(ZLIB_LIBS) \
                  $(GETOPT_LIBS) $(SNPRINTF_LIBS)
