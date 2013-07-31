## Makefile.am -- Process this file with automake to produce Makefile.in
##
## Copyright (C) 2005-2011   Christopher C. Hulbert
##
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##
##    1. Redistributions of source code must retain the above copyright notice,
##       this list of conditions and the following disclaimer.
##
##    2. Redistributions in binary form must reproduce the above copyright
##       notice, this list of conditions and the following disclaimer in the
##       documentation and/or other materials provided with the distribution.
##
## THIS SOFTWARE IS PROVIDED BY CHRISTOPHER C. HULBERT ``AS IS'' AND ANY EXPRESS
## OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
## OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
## EVENT SHALL CHRISTOPHER C. HULBERT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
## INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
## (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
## LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
## ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
## SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
VPATH = @srcdir@

TESTSUITE_AT = $(srcdir)/testsuite.at \
               $(srcdir)/mat4_read.at \
               $(srcdir)/mat4_read_be.at \
               $(srcdir)/mat5_uncompressed_read_le.at \
               $(srcdir)/mat5_uncompressed_read_be.at \
               $(srcdir)/mat5_uncompressed_write.at \
               $(srcdir)/mat5_compressed_read_le.at \
               $(srcdir)/mat5_compressed_read_be.at \
               $(srcdir)/mat5_compressed_write.at \
               $(srcdir)/mat73_read_le.at \
               $(srcdir)/mat73_read_be.at \
               $(srcdir)/mat73_write.at \
               $(srcdir)/mat73_compressed_write.at \
               $(srcdir)/struct_api.at \
               $(srcdir)/cell_api.at \
               $(srcdir)/mat4_readslab_be.at \
               $(srcdir)/mat4_readslab_le.at \
               $(srcdir)/mat5_compressed_readslab_be.at \
               $(srcdir)/mat5_compressed_readslab_le.at \
               $(srcdir)/mat5_uncompressed_readslab_be.at \
               $(srcdir)/mat5_uncompressed_readslab_le.at \
               $(srcdir)/mat73_readslab_be.at \
               $(srcdir)/mat73_readslab_le.at \
               $(srcdir)/mat5_compressed_extended_sparse.at \
               $(srcdir)/mat5_uncompressed_extended_sparse.at \
               $(srcdir)/mat73_compressed_extended_sparse.at \
               $(srcdir)/mat73_uncompressed_extended_sparse.at
TESTSUITE = $(srcdir)/testsuite

TEST_DATAFILES = $(srcdir)/datasets/matio_test_cases.m \
                 $(srcdir)/datasets/matio_test_cases_compressed_be.mat \
                 $(srcdir)/datasets/matio_test_cases_compressed_le.mat \
                 $(srcdir)/datasets/matio_test_cases_hdf_be.mat \
                 $(srcdir)/datasets/matio_test_cases_hdf_le.mat \
                 $(srcdir)/datasets/matio_test_cases_uncompressed_be.mat \
                 $(srcdir)/datasets/matio_test_cases_uncompressed_le.mat \
                 $(srcdir)/datasets/matio_test_cases_v4_be.mat \
                 $(srcdir)/datasets/matio_test_cases_v4_le.mat \
                 $(srcdir)/datasets/small_v4_be.mat \
                 $(srcdir)/datasets/small_v4_le.mat
if MAT73
MAT73 = 1
else
MAT73 = 0
endif

if HAVE_ZLIB
COMPRESSION_ZLIB = 1
else
COMPRESSION_ZLIB = 0
endif

if EXTENDED_SPARSE
EXTENDED_SPARSE = 1
else
EXTENDED_SPARSE = 0
endif

CHECK_ENVIRONMENT = GREP=$(GREP) MAT73=$(MAT73) MATLABEXE=$(MATLABEXE) \
                    HAVE_UINT64=$(HAVE_UINT64) HAVE_INT64=$(HAVE_INT64) \
                    EXTENDED_SPARSE=$(EXTENDED_SPARSE) \
                    COMPRESSION_ZLIB=$(COMPRESSION_ZLIB) \
                    LIBTOOL="$(top_builddir)/libtool"

EXTRA_DIST = $(TESTSUITE_AT) $(TESTSUITE) $(srcdir)/package.m4 \
             $(TEST_DATAFILES)

if NEED_GETOPT
GETOPT_CFLAGS = -I$(top_builddir)/getopt
GETOPT_LIBS   = $(top_builddir)/getopt/libgetopt.la
endif

AM_CFLAGS   = -I$(top_srcdir)/src $(GETOPT_CFLAGS) $(HDF5_CFLAGS) $(ZLIB_CFLAGS)
AM_LDFLAGS  = $(FCLDFLAGS)
TEST_LIBS   = $(top_builddir)/src/libmatio.la $(GETOPT_LIBS) $(HDF5_LIBS) $(ZLIB_LIBS)
TEST_LFLAGS = -L$(top_builddir)/src

noinst_PROGRAMS = test_mat test_snprintf

test_mat_SOURCES = test_mat.c
test_mat_LDADD   = $(TEST_LIBS)
test_mat_LDFLAGS = $(TEST_LFLAGS)

test_snprintf_SOURCES = test_snprintf.c
test_snprintf_LDADD   = $(TEST_LIBS)
test_snprintf_LDFLAGS = $(TEST_LFLAGS)

atconfig: $(top_builddir)/config.status
	cd $(top_builddir) && ./config.status tests/$@

check-local: atconfig $(TESTSUITE)
	$(SHELL) $(TESTSUITE) $(CHECK_ENVIRONMENT) $(TESTSUITEFLAGS)

installcheck-local: atconfig $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' AUTOTEST_PATH='$(bindir)' $(TESTSUITEFLAGS)

clean-local:
	test ! -f '$(TESTSUITE)' || $(SHELL) '$(TESTSUITE)' --clean

$(TESTSUITE): $(srcdir)/package.m4 $(TESTSUITE_AT)
	$(AUTOTEST) -I '$(srcdir)' $@.at -o $@.tmp
	mv $@.tmp $@

$(srcdir)/package.m4: $(top_srcdir)/configure.ac
	{ \
	  echo '# Signature of the current package.'; \
	  echo 'm4_define([AT_PACKAGE_NAME],      [$(PACKAGE_NAME)])'; \
	  echo 'm4_define([AT_PACKAGE_TARNAME],   [$(PACKAGE_TARNAME)])'; \
	  echo 'm4_define([AT_PACKAGE_VERSION],   [$(PACKAGE_VERSION)])'; \
	  echo 'm4_define([AT_PACKAGE_STRING],    [$(PACKAGE_STRING)])'; \
	  echo 'm4_define([AT_PACKAGE_BUGREPORT], [$(PACKAGE_BUGREPORT)])'; \
	} >$(srcdir)/package.m4
