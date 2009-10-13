## Makefile.am -- Process this file with automake to produce Makefile.in
##
## Copyright (C) 2007-2008   Christopher C. Hulbert
##
## This library is free software; you can redistribute it and/or
## modify it under the terms of the GNU Lesser General Public
## License as published by the Free Software Foundation; either
## version 2.1 of the License, or (at your option) any later version.
##
## This library is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public
## License along with this library; if not, write to the Free Software
## Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
##
AUTOMAKE_OPTIONS = foreign 1.8 no-dependencies
VPATH = @srcdir@

docdir = $(prefix)/share/doc/matio
doc_DATA = $(top_builddir)/doxygen/dev_api/latex/libmatio_dev.pdf

doc: $(doc_DATA)

$(top_builddir)/doxygen/dev_api/latex/libmatio_dev.pdf:
	(cat $(srcdir)/doxygen.config; echo "INPUT=$(top_srcdir)/src";echo "LATEX_HEADER=$(srcdir)/../header.tex"; echo "IMAGE_PATH=$(srcdir)") | doxygen -
	cd latex;../$(srcdir)/../format_api.sh;../$(srcdir)/../textopdf.sh
	mv latex/refman.pdf latex/libmatio_dev.pdf


print:
