## Makefile.am -- Process this file with automake to produce Makefile.in
##
## Copyright (c) 2015-2026, The matio contributors
## Copyright (c) 2005-2014, Christopher C. Hulbert
## All rights reserved.
##
## SPDX-License-Identifier: BSD-2-Clause
##
VPATH = @srcdir@

LT_CURRENT=14
LT_REVISION=0
LT_AGE=0

AM_CFLAGS  = -I. $(HDF5_CFLAGS) $(ZLIB_CFLAGS) $(LT_CFLAGS)
AM_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) $(LT_LDFLAGS)

if HAVE_ZLIB
    ZLIB_SRC = inflate.c
endif

SNPRINTF_LIBS          = $(top_builddir)/snprintf/libsnprintf.la

nodist_noinst_HEADERS  = matioConfig.h
nodist_include_HEADERS = matio_pubconf.h
include_HEADERS        = matio.h
noinst_HEADERS         = matio_private.h mat4.h mat5.h mat73.h \
                         read_data_impl.h safe-math.h
lib_LTLIBRARIES        = libmatio.la
libmatio_la_SOURCES    = endian.c io.c $(ZLIB_SRC) read_data.c \
                         mat5.c mat4.c mat.c matvar_cell.c matvar_struct.c
libmatio_la_LIBADD     = $(HDF5_LIBS) $(ZLIB_LIBS) $(SNPRINTF_LIBS)
libmatio_la_LDFLAGS    = -no-undefined -export-symbols @srcdir@/matio.sym $(AM_LDFLAGS)

if MAT73
    libmatio_la_SOURCES += mat73.c
endif

EXTRA_DIST = matio.sym

dosubst = sed -e 's,[@]PACKAGE[@],$(PACKAGE),g' \
              -e 's,[@]VERSION[@],$(VERSION),g' \
              -e 's,\/,\\,g'

doc:


print:
	@list='$(libmatio_a_SOURCES)'; \
	rm -f libmatio.ps; \
	curdir=`pwd`; \
	cd $(srcdir); \
	for f in $$list; do \
	    enscript -2 -G -r -Ec --color -o - $$f >> $$curdir/libmatio.ps;\
	done; \
	cd $$curdir
