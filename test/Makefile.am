## Makefile.am -- Process this file with automake to produce Makefile.in
##
## Copyright (C) 2005-2008   Christopher C. Hulbert
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
AUTOMAKE_OPTIONS = foreign 1.8 no-dependencies
VPATH = @srcdir@

if NEED_GETOPT
GETOPT_CFLAGS = -I$(top_builddir)/getopt/lib
GETOPT_LIBS   = $(top_builddir)/getopt/lib/libgnu.la
endif

AM_CFLAGS   = -I$(top_srcdir)/src $(GETOPT_CFLAGS) $(HDF5_CFLAGS) $(ZLIB_CFLAGS)
AM_LDFLAGS  = $(FCLDFLAGS)
TEST_LIBS   = $(top_builddir)/src/libmatio.la $(GETOPT_LIBS) $(HDF5_LIBS) $(ZLIB_LIBS)
TEST_LFLAGS = -L$(top_builddir)/src

bin_PROGRAMS = test_mat test_snprintf

test_mat_SOURCES = test_mat.c
test_mat_LDADD   = $(TEST_LIBS)
test_mat_LDFLAGS = $(TEST_LFLAGS)

test_snprintf_SOURCES = test_snprintf.c
test_snprintf_LDADD   = $(TEST_LIBS)
test_snprintf_LDFLAGS = $(TEST_LFLAGS)

if ENABLE_FORTRAN
bin_PROGRAMS += test_matf

test_matf_SOURCES = test_matf.f90
test_matf_CFLAGS  = -I$(top_builddir)/src -I$(top_builddir)/src/fortran
test_matf_FCFLAGS = -I$(top_builddir)/src -I$(top_builddir)/src/fortran
test_matf_LDFLAGS = -L$(top_builddir)/src -L$(top_builddir)/src/fortran
test_matf_LDADD   = $(TEST_LIBS)
endif
