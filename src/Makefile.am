## Makefile.am -- Process this file with automake to produce Makefile.in
##
## Copyright (C) 2005-2011   Christopher C. Hulbert
##
##  This library is free software; you can redistribute it and/or
##  modify it under the terms of the GNU Lesser General Public
##  License as published by the Free Software Foundation; either
##  version 2.1 of the License, or (at your option) any later version.
##
##  This library is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
##  Lesser General Public License for more details.
##
##  You should have received a copy of the GNU Lesser General Public
##  License along with this library; if not, write to the Free Software
##  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
##
VPATH = @srcdir@

LT_CURRENT=2
LT_REVISION=0
LT_AGE=0

AM_CFLAGS  = -I. $(HDF5_CFLAGS) $(ZLIB_CFLAGS) $(LT_CFLAGS)
AM_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE) $(LT_LDFLAGS)

if HAVE_ZLIB
    ZLIB_SRC = inflate.c
endif

nodist_noinst_HEADERS  = matioConfig.h
nodist_include_HEADERS = matio_pubconf.h
include_HEADERS        = matio.h
noinst_HEADERS         = matio_private.h mat4.h mat5.h mat73.h
lib_LTLIBRARIES        = libmatio.la
libmatio_la_SOURCES    = snprintf.c endian.c io.c $(ZLIB_SRC) read_data.c \
                         mat5.c mat4.c mat.c
libmatio_la_LIBADD     = $(ZLIB_LIBS)

if MAT73
libmatio_la_SOURCES+= mat73.c
endif

if ENABLE_FORTRAN
AM_FCFLAGS              = -I. -I$(top_builddir)/src/fortran
AM_LDFLAGS             +=  $(FCLDFLAGS)
libmatio_la_SOURCES    +=  fortran/matio_internal.c fortran/matio.f90
nodist_include_HEADERS +=  $(top_builddir)/src/matio.mod
noinst_HEADERS         += fortran/create.f90 fortran/read_data.f90 \
                          fortran/write_data.f90 fortran/write.f90
endif

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
