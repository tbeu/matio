## Makefile.am -- Process this file with automake to produce Makefile.in
##
## Copyright (C) 2005-2016   Christopher C. Hulbert
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
               $(srcdir)/mat4_read_be.at \
               $(srcdir)/mat4_read_le.at \
               $(srcdir)/mat4_write.at \
               $(srcdir)/mat4_readwrite.at \
               $(srcdir)/mat5_uncompressed_read_be.at \
               $(srcdir)/mat5_uncompressed_read_le.at \
               $(srcdir)/mat5_uncompressed_write.at \
               $(srcdir)/mat5_uncompressed_readwrite.at \
               $(srcdir)/mat5_compressed_read_be.at \
               $(srcdir)/mat5_compressed_read_le.at \
               $(srcdir)/mat5_compressed_write.at \
               $(srcdir)/mat5_compressed_readwrite.at \
               $(srcdir)/mat73_read_be.at \
               $(srcdir)/mat73_read_le.at \
               $(srcdir)/mat73_write.at \
               $(srcdir)/mat73_readwrite.at \
               $(srcdir)/mat73_compressed_write.at \
               $(srcdir)/mat73_compressed_readwrite.at \
               $(srcdir)/struct_api.at \
               $(srcdir)/cell_api.at \
               $(srcdir)/subscript_api.at \
               $(srcdir)/mat4_readslab_be.at \
               $(srcdir)/mat4_readslab_le.at \
               $(srcdir)/mat5_compressed_readslab_be.at \
               $(srcdir)/mat5_compressed_readslab_le.at \
               $(srcdir)/mat5_uncompressed_readslab_be.at \
               $(srcdir)/mat5_uncompressed_readslab_le.at \
               $(srcdir)/mat73_readslab_be.at \
               $(srcdir)/mat73_readslab_le.at \
               $(srcdir)/mat4_extended_sparse.at \
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
                 $(srcdir)/datasets/small_v4_le.mat \
                 $(srcdir)/datasets/large_struct_compressed_le.mat \
                 $(srcdir)/matlab/test_write_2d_logical.m \
                 $(srcdir)/matlab/test_write_2d_numeric.m \
                 $(srcdir)/matlab/test_write_cell_2d_logical.m \
                 $(srcdir)/matlab/test_write_cell_2d_numeric.m \
                 $(srcdir)/matlab/test_write_cell_complex_2d_numeric.m \
                 $(srcdir)/matlab/test_write_cell_empty_struct.m \
                 $(srcdir)/matlab/test_write_char.m \
                 $(srcdir)/matlab/test_write_complex_2d_numeric.m \
                 $(srcdir)/matlab/test_write_empty.m \
                 $(srcdir)/matlab/test_write_empty_cell.m \
                 $(srcdir)/matlab/test_write_empty_struct.m \
                 $(srcdir)/matlab/test_write_sparse.m \
                 $(srcdir)/matlab/test_write_sparse_complex.m \
                 $(srcdir)/matlab/test_write_struct_2d_logical.m \
                 $(srcdir)/matlab/test_write_struct_2d_numeric.m \
                 $(srcdir)/matlab/test_write_struct_char.m \
                 $(srcdir)/matlab/test_write_struct_complex_2d_numeric.m \
                 $(srcdir)/results/cell_api_getcells.out \
                 $(srcdir)/results/cell_api_getlinear.out \
                 $(srcdir)/results/cell_api_set.out \
                 $(srcdir)/results/dump-large_struct_compressed.out \
                 $(srcdir)/results/dump-large_struct_compressed_32.out \
                 $(srcdir)/results/dump-mat_copy-4.out \
                 $(srcdir)/results/dump-mat_copy-4_32.out \
                 $(srcdir)/results/dump-mat_copy-5.out \
                 $(srcdir)/results/dump-mat_copy-5_32.out \
                 $(srcdir)/results/dump-mat_copy-73.out \
                 $(srcdir)/results/dump-mat_copy-73_32.out \
                 $(srcdir)/results/dump-mat_copy-partial.out \
                 $(srcdir)/results/dump-var1.out \
                 $(srcdir)/results/dump-var1-2.out \
                 $(srcdir)/results/dump-var2x-field2-1.out \
                 $(srcdir)/results/dump-var2x-field2-2.out \
                 $(srcdir)/results/dump-var2x-field2-3.out \
                 $(srcdir)/results/dump-var3x-field2-1.out \
                 $(srcdir)/results/dump-var3x-field2-2.out \
                 $(srcdir)/results/dump-var3x-field2-3.out \
                 $(srcdir)/results/dump-var7x-3.out \
                 $(srcdir)/results/dump-var7x-4.out \
                 $(srcdir)/results/dump-var8x-3.out \
                 $(srcdir)/results/dump-var8x-4.out \
                 $(srcdir)/results/dump-var11.out \
                 $(srcdir)/results/dump-var11-2.out \
                 $(srcdir)/results/dump-var70-1.out \
                 $(srcdir)/results/dump-var70-2.out \
                 $(srcdir)/results/dump-var71-1.out \
                 $(srcdir)/results/dump-var71-2.out \
                 $(srcdir)/results/dump-var72-1.out \
                 $(srcdir)/results/dump-var72-2.out \
                 $(srcdir)/results/dump-var73-1.out \
                 $(srcdir)/results/dump-var73-2.out \
                 $(srcdir)/results/dump-var74-1.out \
                 $(srcdir)/results/dump-var74-2.out \
                 $(srcdir)/results/dump-var75-1.out \
                 $(srcdir)/results/dump-var75-2.out \
                 $(srcdir)/results/dump-var76-1.out \
                 $(srcdir)/results/dump-var76-2.out \
                 $(srcdir)/results/dump-var77-1.out \
                 $(srcdir)/results/dump-var77-2.out \
                 $(srcdir)/results/dump-var78-1.out \
                 $(srcdir)/results/dump-var78-2.out \
                 $(srcdir)/results/dump-var78-3.out \
                 $(srcdir)/results/dump-var78-4.out \
                 $(srcdir)/results/dump-var79-1.out \
                 $(srcdir)/results/dump-var79-2.out \
                 $(srcdir)/results/dump-var79-3.out \
                 $(srcdir)/results/dump-var79-4.out \
                 $(srcdir)/results/dump-var80-1.out \
                 $(srcdir)/results/dump-var80-2.out \
                 $(srcdir)/results/dump-var81-1.out \
                 $(srcdir)/results/dump-var81-2.out \
                 $(srcdir)/results/dump-var82-1.out \
                 $(srcdir)/results/dump-var82-2.out \
                 $(srcdir)/results/dump-var83-1.out \
                 $(srcdir)/results/dump-var83-2.out \
                 $(srcdir)/results/dump-var84-1.out \
                 $(srcdir)/results/dump-var84-2.out \
                 $(srcdir)/results/dump-var85-1.out \
                 $(srcdir)/results/dump-var85-2.out \
                 $(srcdir)/results/dump-var86-1.out \
                 $(srcdir)/results/dump-var86-2.out \
                 $(srcdir)/results/dump-var86-3.out \
                 $(srcdir)/results/dump-var86-4.out \
                 $(srcdir)/results/dump-var87-1.out \
                 $(srcdir)/results/dump-var87-2.out \
                 $(srcdir)/results/dump-var87-3.out \
                 $(srcdir)/results/dump-var87-4.out \
                 $(srcdir)/results/dump-var88-1.out \
                 $(srcdir)/results/dump-var88-2.out \
                 $(srcdir)/results/dump-var88-3.out \
                 $(srcdir)/results/dump-var88-4.out \
                 $(srcdir)/results/dump-var89-1.out \
                 $(srcdir)/results/dump-var89-2.out \
                 $(srcdir)/results/dump-var89-3.out \
                 $(srcdir)/results/dump-var89-4.out \
                 $(srcdir)/results/getstructfield-large_struct_compressed.out \
                 $(srcdir)/results/ind2sub.out \
                 $(srcdir)/results/read-var1.out \
                 $(srcdir)/results/read-var2.out \
                 $(srcdir)/results/read-var3.out \
                 $(srcdir)/results/read-var4.out \
                 $(srcdir)/results/read-var5.out \
                 $(srcdir)/results/read-var6.out \
                 $(srcdir)/results/read-var7.out \
                 $(srcdir)/results/read-var8.out \
                 $(srcdir)/results/read-var9.out \
                 $(srcdir)/results/read-var10.out \
                 $(srcdir)/results/read-var11.out \
                 $(srcdir)/results/read-var12.out \
                 $(srcdir)/results/read-var13.out \
                 $(srcdir)/results/read-var14.out \
                 $(srcdir)/results/read-var15.out \
                 $(srcdir)/results/read-var16.out \
                 $(srcdir)/results/read-var17.out \
                 $(srcdir)/results/read-var18.out \
                 $(srcdir)/results/read-var19.out \
                 $(srcdir)/results/read-var20.out \
                 $(srcdir)/results/read-var21.out \
                 $(srcdir)/results/read-var22.out \
                 $(srcdir)/results/read-var23.out \
                 $(srcdir)/results/read-var24.out \
                 $(srcdir)/results/read-var25.out \
                 $(srcdir)/results/read-var26.out \
                 $(srcdir)/results/read-var27.out \
                 $(srcdir)/results/read-var28.out \
                 $(srcdir)/results/read-var29.out \
                 $(srcdir)/results/read-var30.out \
                 $(srcdir)/results/read-var31.out \
                 $(srcdir)/results/read-var32.out \
                 $(srcdir)/results/read-var33.out \
                 $(srcdir)/results/read-var34.out \
                 $(srcdir)/results/read-var35.out \
                 $(srcdir)/results/read-var36.out \
                 $(srcdir)/results/read-var37.out \
                 $(srcdir)/results/read-var38.out \
                 $(srcdir)/results/read-var39.out \
                 $(srcdir)/results/read-var40.out \
                 $(srcdir)/results/read-var41.out \
                 $(srcdir)/results/read-var42.out \
                 $(srcdir)/results/read-var43.out \
                 $(srcdir)/results/read-var44.out \
                 $(srcdir)/results/read-var45.out \
                 $(srcdir)/results/read-var46.out \
                 $(srcdir)/results/read-var47.out \
                 $(srcdir)/results/read-var48.out \
                 $(srcdir)/results/read-var49.out \
                 $(srcdir)/results/read-var50.out \
                 $(srcdir)/results/read-var51.out \
                 $(srcdir)/results/read-var52.out \
                 $(srcdir)/results/read-var53.out \
                 $(srcdir)/results/read-var54.out \
                 $(srcdir)/results/read-var55.out \
                 $(srcdir)/results/read-var56.out \
                 $(srcdir)/results/read-var57.out \
                 $(srcdir)/results/read-var58.out \
                 $(srcdir)/results/read-var59.out \
                 $(srcdir)/results/read-var60.out \
                 $(srcdir)/results/read-var61.out \
                 $(srcdir)/results/read-var62.out \
                 $(srcdir)/results/read-var63.out \
                 $(srcdir)/results/read-var64.out \
                 $(srcdir)/results/read-var65.out \
                 $(srcdir)/results/read-var66.out \
                 $(srcdir)/results/read-var67.out \
                 $(srcdir)/results/read-var68.out \
                 $(srcdir)/results/read-var69.out \
                 $(srcdir)/results/read-var90.out \
                 $(srcdir)/results/read-var91.out \
                 $(srcdir)/results/read-var92.out \
                 $(srcdir)/results/read-x.out \
                 $(srcdir)/results/readvar-write_cell_empty_struct-var1.out \
                 $(srcdir)/results/readvar-write_char-a.out \
                 $(srcdir)/results/readvar-write_complex_sparse-sparse_matrix-double.out \
                 $(srcdir)/results/readvar-write_complex_sparse-sparse_matrix-int8.out \
                 $(srcdir)/results/readvar-write_complex_sparse-sparse_matrix-int16.out \
                 $(srcdir)/results/readvar-write_complex_sparse-sparse_matrix-int32.out \
                 $(srcdir)/results/readvar-write_complex_sparse-sparse_matrix-int64.out \
                 $(srcdir)/results/readvar-write_complex_sparse-sparse_matrix-single.out \
                 $(srcdir)/results/readvar-write_complex_sparse-sparse_matrix-uint8.out \
                 $(srcdir)/results/readvar-write_complex_sparse-sparse_matrix-uint16.out \
                 $(srcdir)/results/readvar-write_complex_sparse-sparse_matrix-uint32.out \
                 $(srcdir)/results/readvar-write_complex_sparse-sparse_matrix-uint64.out \
                 $(srcdir)/results/readvar-write_empty_2d_numeric-empty.out \
                 $(srcdir)/results/readvar-write_empty_cell-var1.out \
                 $(srcdir)/results/readvar-write_empty_cell-var2.out \
                 $(srcdir)/results/readvar-write_empty_struct-var1.out \
                 $(srcdir)/results/readvar-write_empty_struct-var2.out \
                 $(srcdir)/results/readvar-write_empty_struct-var3.out \
                 $(srcdir)/results/readvar-write_empty_struct-var4.out \
                 $(srcdir)/results/readvar-write_sparse-sparse_matrix-double.out \
                 $(srcdir)/results/readvar-write_sparse-sparse_matrix-int8.out \
                 $(srcdir)/results/readvar-write_sparse-sparse_matrix-int16.out \
                 $(srcdir)/results/readvar-write_sparse-sparse_matrix-int32.out \
                 $(srcdir)/results/readvar-write_sparse-sparse_matrix-int64.out \
                 $(srcdir)/results/readvar-write_sparse-sparse_matrix-single.out \
                 $(srcdir)/results/readvar-write_sparse-sparse_matrix-uint8.out \
                 $(srcdir)/results/readvar-write_sparse-sparse_matrix-uint16.out \
                 $(srcdir)/results/readvar-write_sparse-sparse_matrix-uint32.out \
                 $(srcdir)/results/readvar-write_sparse-sparse_matrix-uint64.out \
                 $(srcdir)/results/readvar-writeinf-d.out \
                 $(srcdir)/results/readvar-writeinf-d-win.out \
                 $(srcdir)/results/readvar-writenan-d.out \
                 $(srcdir)/results/readvar-writeslab-d.out \
                 $(srcdir)/results/readvar-writeslab-f.out \
                 $(srcdir)/results/readvar-writeslab-i.out \
                 $(srcdir)/results/struct_api_addfield.out \
                 $(srcdir)/results/struct_api_create.out \
                 $(srcdir)/results/struct_api_get.out \
                 $(srcdir)/results/struct_api_getfieldnames.out \
                 $(srcdir)/results/struct_api_getlinear.out \
                 $(srcdir)/results/struct_api_setfield.out \
                 $(srcdir)/results/sub2ind.out \
                 $(srcdir)/results/write_2d_numeric-73-l2-logical.out \
                 $(srcdir)/results/write_2d_numeric-73-l4-logical.out \
                 $(srcdir)/results/write_2d_numeric-double.out \
                 $(srcdir)/results/write_2d_numeric-int8.out \
                 $(srcdir)/results/write_2d_numeric-int16.out \
                 $(srcdir)/results/write_2d_numeric-int32.out \
                 $(srcdir)/results/write_2d_numeric-int64.out \
                 $(srcdir)/results/write_2d_numeric-l1-logical.out \
                 $(srcdir)/results/write_2d_numeric-l2-logical.out \
                 $(srcdir)/results/write_2d_numeric-l4-logical.out \
                 $(srcdir)/results/write_2d_numeric-single.out \
                 $(srcdir)/results/write_2d_numeric-uint8.out \
                 $(srcdir)/results/write_2d_numeric-uint16.out \
                 $(srcdir)/results/write_2d_numeric-uint32.out \
                 $(srcdir)/results/write_2d_numeric-uint64.out \
                 $(srcdir)/results/write_cell_2d_numeric-double.out \
                 $(srcdir)/results/write_cell_2d_numeric-int8.out \
                 $(srcdir)/results/write_cell_2d_numeric-int16.out \
                 $(srcdir)/results/write_cell_2d_numeric-int32.out \
                 $(srcdir)/results/write_cell_2d_numeric-int64.out \
                 $(srcdir)/results/write_cell_2d_numeric-single.out \
                 $(srcdir)/results/write_cell_2d_numeric-uint8.out \
                 $(srcdir)/results/write_cell_2d_numeric-uint16.out \
                 $(srcdir)/results/write_cell_2d_numeric-uint32.out \
                 $(srcdir)/results/write_cell_2d_numeric-uint64.out \
                 $(srcdir)/results/write_cell_complex_2d_numeric-double.out \
                 $(srcdir)/results/write_cell_complex_2d_numeric-int8.out \
                 $(srcdir)/results/write_cell_complex_2d_numeric-int16.out \
                 $(srcdir)/results/write_cell_complex_2d_numeric-int32.out \
                 $(srcdir)/results/write_cell_complex_2d_numeric-int64.out \
                 $(srcdir)/results/write_cell_complex_2d_numeric-single.out \
                 $(srcdir)/results/write_cell_complex_2d_numeric-uint8.out \
                 $(srcdir)/results/write_cell_complex_2d_numeric-uint16.out \
                 $(srcdir)/results/write_cell_complex_2d_numeric-uint32.out \
                 $(srcdir)/results/write_cell_complex_2d_numeric-uint64.out \
                 $(srcdir)/results/write_cell_numeric-73-logical.out \
                 $(srcdir)/results/write_cell_numeric-logical.out \
                 $(srcdir)/results/write_complex_2d_numeric-double.out \
                 $(srcdir)/results/write_complex_2d_numeric-int8.out \
                 $(srcdir)/results/write_complex_2d_numeric-int16.out \
                 $(srcdir)/results/write_complex_2d_numeric-int32.out \
                 $(srcdir)/results/write_complex_2d_numeric-int64.out \
                 $(srcdir)/results/write_complex_2d_numeric-single.out \
                 $(srcdir)/results/write_complex_2d_numeric-uint8.out \
                 $(srcdir)/results/write_complex_2d_numeric-uint16.out \
                 $(srcdir)/results/write_complex_2d_numeric-uint32.out \
                 $(srcdir)/results/write_complex_2d_numeric-uint64.out \
                 $(srcdir)/results/write_struct_2d_numeric-73-logical.out \
                 $(srcdir)/results/write_struct_2d_numeric-double.out \
                 $(srcdir)/results/write_struct_2d_numeric-int8.out \
                 $(srcdir)/results/write_struct_2d_numeric-int16.out \
                 $(srcdir)/results/write_struct_2d_numeric-int32.out \
                 $(srcdir)/results/write_struct_2d_numeric-int64.out \
                 $(srcdir)/results/write_struct_2d_numeric-logical.out \
                 $(srcdir)/results/write_struct_2d_numeric-single.out \
                 $(srcdir)/results/write_struct_2d_numeric-uint8.out \
                 $(srcdir)/results/write_struct_2d_numeric-uint16.out \
                 $(srcdir)/results/write_struct_2d_numeric-uint32.out \
                 $(srcdir)/results/write_struct_2d_numeric-uint64.out \
                 $(srcdir)/results/write_struct_char.out \
                 $(srcdir)/results/write_struct_complex_2d_numeric-double.out \
                 $(srcdir)/results/write_struct_complex_2d_numeric-int8.out \
                 $(srcdir)/results/write_struct_complex_2d_numeric-int16.out \
                 $(srcdir)/results/write_struct_complex_2d_numeric-int32.out \
                 $(srcdir)/results/write_struct_complex_2d_numeric-int64.out \
                 $(srcdir)/results/write_struct_complex_2d_numeric-single.out \
                 $(srcdir)/results/write_struct_complex_2d_numeric-uint8.out \
                 $(srcdir)/results/write_struct_complex_2d_numeric-uint16.out \
                 $(srcdir)/results/write_struct_complex_2d_numeric-uint32.out \
                 $(srcdir)/results/write_struct_complex_2d_numeric-uint64.out
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
