# automake file for scats
# $Revision: 1.1.2.1 $ $State: Exp $
# $Date: 2005/07/01 17:59:20 $

AUTOMAKE_OPTIONS = foreign 1.8 no-dependencies
VPATH = @srcdir@

AM_CFLAGS  = -I. $(ZLIB_CFLAGS)
AM_LDFLAGS =

if HAVE_ZLIB
    ZLIB_SRC = inflate.c
endif

include_HEADERS     = matio.h matioConfig.h
lib_LTLIBRARIES     = libmatio.la
libmatio_la_SOURCES = snprintf.c endian.c io.c $(ZLIB_SRC) read_data.c \
                      mat5.c mat4.c mat.c

if ENABLE_FORTRAN
AM_FCFLAGS           = -I. -I$(top_builddir)/src/fortran
AM_LDFLAGS          +=  $(FCLDFLAGS)
libmatio_la_SOURCES +=  fortran/matio_internal.c fortran/matio.f90
include_HEADERS     +=  $(top_builddir)/src/matio.mod
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
