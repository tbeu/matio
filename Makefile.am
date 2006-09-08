# automake file for scats

AUTOMAKE_OPTIONS = foreign 1.8 no-dependencies
VPATH = @srcdir@

ACLOCAL_AMFLAGS = -Iconfig


EXTRA_DIST = matio.pc.in
data_DATA  = share/test_file.mat share/test_file_v6.mat

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = matio.pc
$(pkgconfig_DATA): config.status


if DOCS_INSTALL
DOCS=doxygen
endif

if BUILD_TEST
TEST=test
endif

SUBDIRS = src $(TEST) $(DOCS)

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
