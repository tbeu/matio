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

ACLOCAL_AMFLAGS = -I config -I getopt/m4

datadir = $(prefix)/share/matio
EXTRA_DIST = matio.pc.in bootstrap patches/inflate.patch \
             share/test_file.mat share/test_file_v6.mat

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = matio.pc
$(pkgconfig_DATA): config.status


if DOCS_INSTALL
DOCS=doxygen
endif

if NEED_GETOPT
GETOPT = getopt/lib
endif

SUBDIRS = src $(GETOPT) tools test $(DOCS)

dosubst = sed -e 's,[@]PACKAGE[@],$(PACKAGE),g' \
              -e 's,[@]VERSION[@],$(VERSION),g' \
              -e 's,\/,\\,g'

doc:
	

print:
	subdirs='$(SUBDIRS)'; \
	here=`pwd`;\
	for f in $$subdirs; do \
	    cd $$f; \
	    make print;\
	    cd $$here;\
	done
