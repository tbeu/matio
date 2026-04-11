## Makefile.am -- Process this file with automake to produce Makefile.in
##
## Copyright (c) 2015-2026, The matio contributors
## Copyright (c) 2005-2014, Christopher C. Hulbert
## All rights reserved.
##
## SPDX-License-Identifier: BSD-2-Clause
##
VPATH = @srcdir@

AM_CFLAGS  = -I. -I$(top_srcdir)/../src -I$(top_builddir)/../src
AM_FCFLAGS = -I$(top_builddir)/src
AM_LDFLAGS = $(FCLDFLAGS) -avoid-version

include_HEADERS = $(top_builddir)/src/matio.mod
lib_LTLIBRARIES      = libfmatio.la
libfmatio_la_SOURCES = matio_internal.c matio.f90

print:
	@list='$(libfmatio_a_SOURCES)'; \
	rm -f libfmatio.ps; \
	curdir=`pwd`; \
	cd $(srcdir); \
	for f in $$list; do \
	    enscript -2 -G -r -Ec --color -o - $$f >> $$curdir/libfmatio.ps;\
	done; \
	cd $$curdir
