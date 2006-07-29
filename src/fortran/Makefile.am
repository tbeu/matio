# automake file for scats
# $Revision: 1.1.2.1 $ $State: Exp $
# $Date: 2005/07/01 17:59:20 $

AUTOMAKE_OPTIONS = foreign 1.8 no-dependencies
VPATH = @srcdir@

AM_CFLAGS  = -I. -I$(top_srcdir)/../src -I$(top_builddir)/../src
AM_FCFLAGS = -I$(top_builddir)/src
AM_LDFLAGS = $(FCLDFLAGS) -avoid-version

include_HEADERS = $(top_builddir)/src/matio.mod
lib_LTLIBRARIES      = libfmatio.la
libfmatio_la_SOURCES = matio_internal.c matio.f90

print:
	@list='$(libmatio_a_SOURCES)'; \
	rm -f libmatio.ps; \
	curdir=`pwd`; \
	cd $(srcdir); \
	for f in $$list; do \
	    enscript -2 -G -r -Ec --color -o - $$f >> $$curdir/libmatio.ps;\
	done; \
	cd $$curdir
