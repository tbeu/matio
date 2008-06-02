##  Makefile.am
##
##        GNU Automake Makefile
##
##  Process this file with GNU automake to produce a Makefile.in.

AUTOMAKE_OPTIONS = foreign 1.8 no-dependencies
VPATH = @srcdir@

EXTRA_DIST = header.tex doxygen.config format_api.sh textopdf.sh

docdir = $(prefix)/share/doc/matio
doc_DATA = $(top_builddir)/doxygen/latex/libmatio.pdf

doc: $(doc_DATA)

$(top_builddir)/doxygen/latex/libmatio.pdf:
	(cat $(srcdir)/doxygen.config; echo "INPUT=$(top_srcdir)/src";echo "LATEX_HEADER=$(srcdir)/header.tex"; echo "EXCLUDE=$(top_srcdir)/scats"; echo "IMAGE_PATH=$(srcdir)"; echo "PROJECT_NUMBER=$(PACKAGE_VERSION)") | doxygen -
	cd latex;../$(srcdir)/format_api.sh;../$(srcdir)/textopdf.sh
#	cd latex;../$(srcdir)/textopdf.sh
	mv latex/refman.pdf latex/libmatio.pdf

print:
