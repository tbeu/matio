## Makefile.am -- Process this file with automake to produce Makefile.in
##
## Copyright (c) 2015-2022, The matio contributors
## Copyright (c) 2005-2014, Christopher C. Hulbert
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##
## 1. Redistributions of source code must retain the above copyright notice, this
##    list of conditions and the following disclaimer.
##
## 2. Redistributions in binary form must reproduce the above copyright notice,
##    this list of conditions and the following disclaimer in the documentation
##    and/or other materials provided with the distribution.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
## IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
## DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
## SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
## CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
## OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
VPATH = @srcdir@

LT_CURRENT=11
LT_REVISION=4
LT_AGE=0

AM_CFLAGS  = -I. $(HDF5_CFLAGS) $(ZLIB_CFLAGS) $(LT_CFLAGS)
AM_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) $(LT_LDFLAGS)

if HAVE_ZLIB
    ZLIB_SRC = inflate.c
endif

nodist_noinst_HEADERS  = matioConfig.h
nodist_include_HEADERS = matio_pubconf.h
include_HEADERS        = matio.h
noinst_HEADERS         = matio_private.h mat4.h mat5.h mat73.h \
                         read_data_impl.h safe-math.h
lib_LTLIBRARIES        = libmatio.la
libmatio_la_SOURCES    = snprintf.c endian.c io.c $(ZLIB_SRC) read_data.c \
                         mat5.c mat4.c mat.c matvar_cell.c matvar_struct.c
libmatio_la_LIBADD     = $(HDF5_LIBS) $(ZLIB_LIBS)
libmatio_la_LDFLAGS    = -no-undefined -export-symbols @srcdir@/matio.sym $(AM_LDFLAGS)

if MAT73
    libmatio_la_SOURCES+= mat73.c
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
