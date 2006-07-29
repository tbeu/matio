##  Makefile.am
##
##        GNU Automake Makefile
##
##  Process this file with GNU automake to produce a Makefile.in.

AUTOMAKE_OPTIONS = foreign 1.8 no-dependencies
VPATH = @srcdir@

docdir = $(prefix)/doc
doc_DATA = $(top_builddir)/doxygen/latex/libmatio.pdf

doc: $(doc_DATA)

$(top_builddir)/doxygen/latex/libmatio.pdf:
	(cat $(srcdir)/doxygen.config; echo "INPUT=$(top_srcdir)/src";echo "LATEX_HEADER=$(srcdir)/header.tex"; echo "EXCLUDE=$(top_srcdir)/scats"; echo "IMAGE_PATH=$(srcdir)") | doxygen -
	cd latex;../$(srcdir)/format_api.sh;../$(srcdir)/textopdf.sh
#	cd latex;../$(srcdir)/textopdf.sh
	mv latex/refman.pdf latex/libmatio.pdf

print:
