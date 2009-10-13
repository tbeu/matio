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

AM_CFLAGS = -I$(top_srcdir)/src $(GETOPT_CFLAGS) $(HDF5_CFLAGS) $(ZLIB_CFLAGS) \
            $(LT_CFLAGS)
AM_LDFLAGS = $(LT_LDFLAGS) $(FCLDFLAGS)

bin_PROGRAMS = matdump

matdump_SOURCES = matdump.c
matdump_LDADD   = $(top_builddir)/src/libmatio.la $(GETOPT_LIBS) \
                  $(HDF5_LIBS) $(ZLIB_LIBS)
