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
