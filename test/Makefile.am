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

TESTSUITE_AT = $(srcdir)/testsuite.at \
               $(srcdir)/mat4_read.at \
               $(srcdir)/mat5_uncompressed_read_le.at \
               $(srcdir)/mat5_uncompressed_read_be.at \
               $(srcdir)/mat5_uncompressed_write.at \
               $(srcdir)/mat5_compressed_read_le.at \
               $(srcdir)/mat5_compressed_read_be.at \
               $(srcdir)/mat5_compressed_write.at \
               $(srcdir)/mat73_read_le.at \
               $(srcdir)/mat73_read_be.at \
               $(srcdir)/mat73_write.at
TESTSUITE = $(srcdir)/testsuite

TEST_DATAFILES = $(srcdir)/datasets/matio_test_cases.m \
                 $(srcdir)/datasets/matio_test_cases_compressed_be.mat \
                 $(srcdir)/datasets/matio_test_cases_compressed_le.mat \
                 $(srcdir)/datasets/matio_test_cases_hdf_be.mat \
                 $(srcdir)/datasets/matio_test_cases_hdf_le.mat \
                 $(srcdir)/datasets/matio_test_cases_uncompressed_be.mat \
                 $(srcdir)/datasets/matio_test_cases_uncompressed_le.mat \
                 $(srcdir)/datasets/matio_test_cases_v4_be.mat \
                 $(srcdir)/datasets/matio_test_cases_v4_le.mat
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

CHECK_ENVIRONMENT = GREP=$(GREP) MAT73=$(MAT73) MATLABEXE=$(MATLABEXE) \
                    HAVE_UINT64=$(HAVE_UINT64) HAVE_INT64=$(HAVE_INT64) \
                    COMPRESSION_ZLIB=$(COMPRESSION_ZLIB)

EXTRA_DIST = $(TESTSUITE_AT) $(TESTSUITE) $(srcdir)/package.m4 \
             $(TEST_DATAFILES)

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
