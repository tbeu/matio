## Makefile.am -- Process this file with automake to produce Makefile.in
##
## Copyright (c) 2015-2022, The matio contributors
## Copyright (c) 2005-2014, Christopher C. Hulbert
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##
## 1. Redistributions of source code must retain the above copyright notice, this
##    list of conditions and the following disclaimer.
##
## 2. Redistributions in binary form must reproduce the above copyright notice,
##    this list of conditions and the following disclaimer in the documentation
##    and/or other materials provided with the distribution.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
## IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
## DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
## SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
## CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
## OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
VPATH = @srcdir@

tests_dir=$(srcdir)/tests
datasets_dir=$(srcdir)/datasets
matlab_dir=$(srcdir)/matlab
results_dir=$(srcdir)/results

TESTSUITE_AT = $(srcdir)/testsuite.at \
               $(tests_dir)/mat4_read_be.at \
               $(tests_dir)/mat4_read_le.at \
               $(tests_dir)/mat4_write.at \
               $(tests_dir)/mat4_readwrite.at \
               $(tests_dir)/mat5_uncompressed_read_be.at \
               $(tests_dir)/mat5_uncompressed_read_le.at \
               $(tests_dir)/mat5_uncompressed_write.at \
               $(tests_dir)/mat5_uncompressed_readwrite.at \
               $(tests_dir)/mat5_compressed_read_be.at \
               $(tests_dir)/mat5_compressed_read_le.at \
               $(tests_dir)/mat5_compressed_write.at \
               $(tests_dir)/mat5_compressed_readwrite.at \
               $(tests_dir)/mat73_read_be.at \
               $(tests_dir)/mat73_compressed_read_le.at \
               $(tests_dir)/mat73_uncompressed_read_le.at \
               $(tests_dir)/mat73_write.at \
               $(tests_dir)/mat73_writeappend.at \
               $(tests_dir)/mat73_compressed_readwrite.at \
               $(tests_dir)/mat73_uncompressed_readwrite.at \
               $(tests_dir)/mat73_compressed_write.at \
               $(tests_dir)/mat73_compressed_writeappend.at \
               $(tests_dir)/mat73_compressed_readwrite.at \
               $(tests_dir)/struct_api.at \
               $(tests_dir)/cell_api.at \
               $(tests_dir)/subscript_api.at \
               $(tests_dir)/mat4_readslab_be.at \
               $(tests_dir)/mat4_readslab_le.at \
               $(tests_dir)/mat5_compressed_readslab_be.at \
               $(tests_dir)/mat5_compressed_readslab_le.at \
               $(tests_dir)/mat5_uncompressed_readslab_be.at \
               $(tests_dir)/mat5_uncompressed_readslab_le.at \
               $(tests_dir)/mat73_readslab_be.at \
               $(tests_dir)/mat73_compressed_readslab_le.at \
               $(tests_dir)/mat73_uncompressed_readslab_le.at \
               $(tests_dir)/mat4_extended_sparse.at \
               $(tests_dir)/mat5_compressed_extended_sparse.at \
               $(tests_dir)/mat5_uncompressed_extended_sparse.at \
               $(tests_dir)/mat73_compressed_extended_sparse.at \
               $(tests_dir)/mat73_uncompressed_extended_sparse.at

TESTSUITE = $(srcdir)/testsuite

TEST_DATAFILES = $(datasets_dir)/large_struct_compressed_le.mat \
                 $(datasets_dir)/matio_test_cases.m \
                 $(datasets_dir)/matio_test_cases.py \
                 $(datasets_dir)/matio_test_cases_compressed_be.mat \
                 $(datasets_dir)/matio_test_cases_compressed_le.mat \
                 $(datasets_dir)/matio_test_cases_hdf_be.mat \
                 $(datasets_dir)/matio_test_cases_compressed_hdf_le.mat \
                 $(datasets_dir)/matio_test_cases_uncompressed_hdf_le.mat \
                 $(datasets_dir)/matio_test_cases_uncompressed_be.mat \
                 $(datasets_dir)/matio_test_cases_uncompressed_le.mat \
                 $(datasets_dir)/matio_test_cases_v4_be.mat \
                 $(datasets_dir)/matio_test_cases_v4_le.mat \
                 $(datasets_dir)/packed_field_name_compressed_le.mat \
                 $(datasets_dir)/packed_field_name_uncompressed_le.mat \
                 $(datasets_dir)/small_v4_be.mat \
                 $(datasets_dir)/small_v4_le.mat \
                 $(matlab_dir)/test_write_2d_logical.m \
                 $(matlab_dir)/test_write_2d_numeric.m \
                 $(matlab_dir)/test_write_cell_2d_logical.m \
                 $(matlab_dir)/test_write_cell_2d_numeric.m \
                 $(matlab_dir)/test_write_cell_complex_2d_numeric.m \
                 $(matlab_dir)/test_write_cell_empty_struct.m \
                 $(matlab_dir)/test_write_char_unicode.m \
                 $(matlab_dir)/test_write_char_utf8.m \
                 $(matlab_dir)/test_write_char.m \
                 $(matlab_dir)/test_write_complex_2d_numeric.m \
                 $(matlab_dir)/test_write_empty.m \
                 $(matlab_dir)/test_write_empty_cell.m \
                 $(matlab_dir)/test_write_empty_struct.m \
                 $(matlab_dir)/test_write_sparse.m \
                 $(matlab_dir)/test_write_sparse_complex.m \
                 $(matlab_dir)/test_write_struct_2d_logical.m \
                 $(matlab_dir)/test_write_struct_2d_numeric.m \
                 $(matlab_dir)/test_write_struct_char.m \
                 $(matlab_dir)/test_write_struct_complex_2d_numeric.m \
                 $(results_dir)/cell_api_getcells.out \
                 $(results_dir)/cell_api_getlinear.out \
                 $(results_dir)/cell_api_set.out \
                 $(results_dir)/dir-4.out \
                 $(results_dir)/dir-4_be.out \
                 $(results_dir)/dir_le.out \
                 $(results_dir)/dir-5_be.out \
                 $(results_dir)/dir-73_be.out \
                 $(results_dir)/dump-large_struct_compressed.out \
                 $(results_dir)/dump-large_struct_compressed_32.out \
                 $(results_dir)/dump-mat_copy-4.out \
                 $(results_dir)/dump-mat_copy-4_32.out \
                 $(results_dir)/dump-mat_copy-5.out \
                 $(results_dir)/dump-mat_copy-5_32.out \
                 $(results_dir)/dump-mat_copy-73.out \
                 $(results_dir)/dump-mat_copy-73_32.out \
                 $(results_dir)/dump-mat_copy-partial.out \
                 $(results_dir)/dump-var1.out \
                 $(results_dir)/dump-var1-2.out \
                 $(results_dir)/dump-var2x-field2-1.out \
                 $(results_dir)/dump-var2x-field2-2.out \
                 $(results_dir)/dump-var2x-field2-3.out \
                 $(results_dir)/dump-var3x-field2-1.out \
                 $(results_dir)/dump-var3x-field2-2.out \
                 $(results_dir)/dump-var3x-field2-3.out \
                 $(results_dir)/dump-var7x-3.out \
                 $(results_dir)/dump-var7x-4.out \
                 $(results_dir)/dump-var8x-3.out \
                 $(results_dir)/dump-var8x-4.out \
                 $(results_dir)/dump-var11.out \
                 $(results_dir)/dump-var11-2.out \
                 $(results_dir)/dump-var70-1.out \
                 $(results_dir)/dump-var70-2.out \
                 $(results_dir)/dump-var71-1.out \
                 $(results_dir)/dump-var71-2.out \
                 $(results_dir)/dump-var72-1.out \
                 $(results_dir)/dump-var72-2.out \
                 $(results_dir)/dump-var73-1.out \
                 $(results_dir)/dump-var73-2.out \
                 $(results_dir)/dump-var74-1.out \
                 $(results_dir)/dump-var74-2.out \
                 $(results_dir)/dump-var75-1.out \
                 $(results_dir)/dump-var75-2.out \
                 $(results_dir)/dump-var76-1.out \
                 $(results_dir)/dump-var76-2.out \
                 $(results_dir)/dump-var77-1.out \
                 $(results_dir)/dump-var77-2.out \
                 $(results_dir)/dump-var78-1.out \
                 $(results_dir)/dump-var78-2.out \
                 $(results_dir)/dump-var78-3.out \
                 $(results_dir)/dump-var78-4.out \
                 $(results_dir)/dump-var79-1.out \
                 $(results_dir)/dump-var79-2.out \
                 $(results_dir)/dump-var79-3.out \
                 $(results_dir)/dump-var79-4.out \
                 $(results_dir)/dump-var80-1.out \
                 $(results_dir)/dump-var80-2.out \
                 $(results_dir)/dump-var81-1.out \
                 $(results_dir)/dump-var81-2.out \
                 $(results_dir)/dump-var82-1.out \
                 $(results_dir)/dump-var82-2.out \
                 $(results_dir)/dump-var83-1.out \
                 $(results_dir)/dump-var83-2.out \
                 $(results_dir)/dump-var84-1.out \
                 $(results_dir)/dump-var84-2.out \
                 $(results_dir)/dump-var85-1.out \
                 $(results_dir)/dump-var85-2.out \
                 $(results_dir)/dump-var86-1.out \
                 $(results_dir)/dump-var86-2.out \
                 $(results_dir)/dump-var86-3.out \
                 $(results_dir)/dump-var86-4.out \
                 $(results_dir)/dump-var87-1.out \
                 $(results_dir)/dump-var87-2.out \
                 $(results_dir)/dump-var87-3.out \
                 $(results_dir)/dump-var87-4.out \
                 $(results_dir)/dump-var88-1.out \
                 $(results_dir)/dump-var88-2.out \
                 $(results_dir)/dump-var88-3.out \
                 $(results_dir)/dump-var88-4.out \
                 $(results_dir)/dump-var89-1.out \
                 $(results_dir)/dump-var89-2.out \
                 $(results_dir)/dump-var89-3.out \
                 $(results_dir)/dump-var89-4.out \
                 $(results_dir)/getstructfield-large_struct_compressed.out \
                 $(results_dir)/ind2sub.out \
                 $(results_dir)/read-packed_field_name.out \
                 $(results_dir)/readslab-var1.out \
                 $(results_dir)/readslab-var11.out \
                 $(results_dir)/readslab-var28.out \
                 $(results_dir)/readslab-var38.out \
                 $(results_dir)/read-var1.out \
                 $(results_dir)/read-var2.out \
                 $(results_dir)/read-var3.out \
                 $(results_dir)/read-var4.out \
                 $(results_dir)/read-var5.out \
                 $(results_dir)/read-var6.out \
                 $(results_dir)/read-var7.out \
                 $(results_dir)/read-var8.out \
                 $(results_dir)/read-var9.out \
                 $(results_dir)/read-var10.out \
                 $(results_dir)/read-var11.out \
                 $(results_dir)/read-var12.out \
                 $(results_dir)/read-var13.out \
                 $(results_dir)/read-var14.out \
                 $(results_dir)/read-var15.out \
                 $(results_dir)/read-var16.out \
                 $(results_dir)/read-var17.out \
                 $(results_dir)/read-var18.out \
                 $(results_dir)/read-var19.out \
                 $(results_dir)/read-var20.out \
                 $(results_dir)/read-var21.out \
                 $(results_dir)/read-var22.out \
                 $(results_dir)/read-var23.out \
                 $(results_dir)/read-var24.out \
                 $(results_dir)/read-var24-5.out \
                 $(results_dir)/read-var24-73.out \
                 $(results_dir)/read-var25.out \
                 $(results_dir)/read-var26.out \
                 $(results_dir)/read-var27.out \
                 $(results_dir)/read-var27-5.out \
                 $(results_dir)/read-var28.out \
                 $(results_dir)/read-var29.out \
                 $(results_dir)/read-var30.out \
                 $(results_dir)/read-var31.out \
                 $(results_dir)/read-var32.out \
                 $(results_dir)/read-var33.out \
                 $(results_dir)/read-var34.out \
                 $(results_dir)/read-var35.out \
                 $(results_dir)/read-var36.out \
                 $(results_dir)/read-var37.out \
                 $(results_dir)/read-var38.out \
                 $(results_dir)/read-var39.out \
                 $(results_dir)/read-var40.out \
                 $(results_dir)/read-var41.out \
                 $(results_dir)/read-var42.out \
                 $(results_dir)/read-var43.out \
                 $(results_dir)/read-var44.out \
                 $(results_dir)/read-var45.out \
                 $(results_dir)/read-var46.out \
                 $(results_dir)/read-var47.out \
                 $(results_dir)/read-var48.out \
                 $(results_dir)/read-var49.out \
                 $(results_dir)/read-var50.out \
                 $(results_dir)/read-var50-5.out \
                 $(results_dir)/read-var51.out \
                 $(results_dir)/read-var52.out \
                 $(results_dir)/read-var53.out \
                 $(results_dir)/read-var54.out \
                 $(results_dir)/read-var55.out \
                 $(results_dir)/read-var56.out \
                 $(results_dir)/read-var57.out \
                 $(results_dir)/read-var58.out \
                 $(results_dir)/read-var59.out \
                 $(results_dir)/read-var60.out \
                 $(results_dir)/read-var61.out \
                 $(results_dir)/read-var62.out \
                 $(results_dir)/read-var63.out \
                 $(results_dir)/read-var64.out \
                 $(results_dir)/read-var65.out \
                 $(results_dir)/read-var65-5.out \
                 $(results_dir)/read-var66.out \
                 $(results_dir)/read-var66-5.out \
                 $(results_dir)/read-var67.out \
                 $(results_dir)/read-var68.out \
                 $(results_dir)/read-var69.out \
                 $(results_dir)/read-var69-5.out \
                 $(results_dir)/read-var69-73.out \
                 $(results_dir)/read-var90.out \
                 $(results_dir)/read-var91.out \
                 $(results_dir)/read-var92.out \
                 $(results_dir)/read-var95-5.out \
                 $(results_dir)/read-var95-73.out \
                 $(results_dir)/read-x.out \
                 $(results_dir)/readvar-write_cell_empty_struct-var1.out \
                 $(results_dir)/readvar-write_char_unicode-a-5.out \
                 $(results_dir)/readvar-write_char_unicode-a-73.out \
                 $(results_dir)/readvar-write_char_utf8-a-5.out \
                 $(results_dir)/readvar-write_char-a.out \
                 $(results_dir)/readvar-write_char-a-5.out \
                 $(results_dir)/readvar-write_char-a-73.out \
                 $(results_dir)/readvar-write_complex_sparse-sparse_matrix-double.out \
                 $(results_dir)/readvar-write_complex_sparse-sparse_matrix-int8.out \
                 $(results_dir)/readvar-write_complex_sparse-sparse_matrix-int16.out \
                 $(results_dir)/readvar-write_complex_sparse-sparse_matrix-int32.out \
                 $(results_dir)/readvar-write_complex_sparse-sparse_matrix-int64.out \
                 $(results_dir)/readvar-write_complex_sparse-sparse_matrix-single.out \
                 $(results_dir)/readvar-write_complex_sparse-sparse_matrix-uint8.out \
                 $(results_dir)/readvar-write_complex_sparse-sparse_matrix-uint16.out \
                 $(results_dir)/readvar-write_complex_sparse-sparse_matrix-uint32.out \
                 $(results_dir)/readvar-write_complex_sparse-sparse_matrix-uint64.out \
                 $(results_dir)/readvar-write_empty_2d_numeric-empty.out \
                 $(results_dir)/readvar-write_empty_cell-var1.out \
                 $(results_dir)/readvar-write_empty_cell-var2.out \
                 $(results_dir)/readvar-write_empty_struct-var1.out \
                 $(results_dir)/readvar-write_empty_struct-var2.out \
                 $(results_dir)/readvar-write_empty_struct-var3.out \
                 $(results_dir)/readvar-write_empty_struct-var4.out \
                 $(results_dir)/readvar-write_empty_struct-var4-5.out \
                 $(results_dir)/readvar-write_sparse-sparse_matrix-double.out \
                 $(results_dir)/readvar-write_sparse-sparse_matrix-int8.out \
                 $(results_dir)/readvar-write_sparse-sparse_matrix-int16.out \
                 $(results_dir)/readvar-write_sparse-sparse_matrix-int32.out \
                 $(results_dir)/readvar-write_sparse-sparse_matrix-int64.out \
                 $(results_dir)/readvar-write_sparse-sparse_matrix-single.out \
                 $(results_dir)/readvar-write_sparse-sparse_matrix-uint8.out \
                 $(results_dir)/readvar-write_sparse-sparse_matrix-uint16.out \
                 $(results_dir)/readvar-write_sparse-sparse_matrix-uint32.out \
                 $(results_dir)/readvar-write_sparse-sparse_matrix-uint64.out \
                 $(results_dir)/readvar-writeinf-d.out \
                 $(results_dir)/readvar-writeinf-d-win.out \
                 $(results_dir)/readvar-writenan-d.out \
                 $(results_dir)/readvar-writeslab-d.out \
                 $(results_dir)/readvar-writeslab-f.out \
                 $(results_dir)/readvar-writeslab-i.out \
                 $(results_dir)/struct_api_addfield.out \
                 $(results_dir)/struct_api_create.out \
                 $(results_dir)/struct_api_get.out \
                 $(results_dir)/struct_api_getfieldnames.out \
                 $(results_dir)/struct_api_getlinear.out \
                 $(results_dir)/struct_api_setfield.out \
                 $(results_dir)/sub2ind.out \
                 $(results_dir)/write_2d_numeric-73-l2-logical.out \
                 $(results_dir)/write_2d_numeric-73-l4-logical.out \
                 $(results_dir)/write_2d_numeric-double.out \
                 $(results_dir)/write_2d_numeric-int8.out \
                 $(results_dir)/write_2d_numeric-int16.out \
                 $(results_dir)/write_2d_numeric-int32.out \
                 $(results_dir)/write_2d_numeric-int64.out \
                 $(results_dir)/write_2d_numeric-l1-logical.out \
                 $(results_dir)/write_2d_numeric-l2-logical.out \
                 $(results_dir)/write_2d_numeric-l4-logical.out \
                 $(results_dir)/write_2d_numeric-single.out \
                 $(results_dir)/write_2d_numeric-uint8.out \
                 $(results_dir)/write_2d_numeric-uint16.out \
                 $(results_dir)/write_2d_numeric-uint32.out \
                 $(results_dir)/write_2d_numeric-uint64.out \
                 $(results_dir)/write_cell_2d_numeric-double.out \
                 $(results_dir)/write_cell_2d_numeric-int8.out \
                 $(results_dir)/write_cell_2d_numeric-int16.out \
                 $(results_dir)/write_cell_2d_numeric-int32.out \
                 $(results_dir)/write_cell_2d_numeric-int64.out \
                 $(results_dir)/write_cell_2d_numeric-single.out \
                 $(results_dir)/write_cell_2d_numeric-uint8.out \
                 $(results_dir)/write_cell_2d_numeric-uint16.out \
                 $(results_dir)/write_cell_2d_numeric-uint32.out \
                 $(results_dir)/write_cell_2d_numeric-uint64.out \
                 $(results_dir)/write_cell_complex_2d_numeric-double.out \
                 $(results_dir)/write_cell_complex_2d_numeric-int8.out \
                 $(results_dir)/write_cell_complex_2d_numeric-int16.out \
                 $(results_dir)/write_cell_complex_2d_numeric-int32.out \
                 $(results_dir)/write_cell_complex_2d_numeric-int64.out \
                 $(results_dir)/write_cell_complex_2d_numeric-single.out \
                 $(results_dir)/write_cell_complex_2d_numeric-uint8.out \
                 $(results_dir)/write_cell_complex_2d_numeric-uint16.out \
                 $(results_dir)/write_cell_complex_2d_numeric-uint32.out \
                 $(results_dir)/write_cell_complex_2d_numeric-uint64.out \
                 $(results_dir)/write_cell_numeric-73-logical.out \
                 $(results_dir)/write_cell_numeric-logical.out \
                 $(results_dir)/write_complex_2d_numeric-double.out \
                 $(results_dir)/write_complex_2d_numeric-int8.out \
                 $(results_dir)/write_complex_2d_numeric-int16.out \
                 $(results_dir)/write_complex_2d_numeric-int32.out \
                 $(results_dir)/write_complex_2d_numeric-int64.out \
                 $(results_dir)/write_complex_2d_numeric-single.out \
                 $(results_dir)/write_complex_2d_numeric-uint8.out \
                 $(results_dir)/write_complex_2d_numeric-uint16.out \
                 $(results_dir)/write_complex_2d_numeric-uint32.out \
                 $(results_dir)/write_complex_2d_numeric-uint64.out \
                 $(results_dir)/write_struct_2d_numeric-73-logical.out \
                 $(results_dir)/write_struct_2d_numeric-double.out \
                 $(results_dir)/write_struct_2d_numeric-int8.out \
                 $(results_dir)/write_struct_2d_numeric-int16.out \
                 $(results_dir)/write_struct_2d_numeric-int32.out \
                 $(results_dir)/write_struct_2d_numeric-int64.out \
                 $(results_dir)/write_struct_2d_numeric-logical.out \
                 $(results_dir)/write_struct_2d_numeric-single.out \
                 $(results_dir)/write_struct_2d_numeric-uint8.out \
                 $(results_dir)/write_struct_2d_numeric-uint16.out \
                 $(results_dir)/write_struct_2d_numeric-uint32.out \
                 $(results_dir)/write_struct_2d_numeric-uint64.out \
                 $(results_dir)/write_struct_char.out \
                 $(results_dir)/write_struct_char-5.out \
                 $(results_dir)/write_struct_complex_2d_numeric-double.out \
                 $(results_dir)/write_struct_complex_2d_numeric-int8.out \
                 $(results_dir)/write_struct_complex_2d_numeric-int16.out \
                 $(results_dir)/write_struct_complex_2d_numeric-int32.out \
                 $(results_dir)/write_struct_complex_2d_numeric-int64.out \
                 $(results_dir)/write_struct_complex_2d_numeric-single.out \
                 $(results_dir)/write_struct_complex_2d_numeric-uint8.out \
                 $(results_dir)/write_struct_complex_2d_numeric-uint16.out \
                 $(results_dir)/write_struct_complex_2d_numeric-uint32.out \
                 $(results_dir)/write_struct_complex_2d_numeric-uint64.out
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

if IS_WIN32
IS_WIN32 = 1
else
IS_WIN32 = 0
endif

CHECK_ENVIRONMENT = GREP=$(GREP) MAT73=$(MAT73) MATLABEXE=$(MATLABEXE) \
                    HAVE_UINT64=$(HAVE_UINT64) HAVE_INT64=$(HAVE_INT64) \
                    EXTENDED_SPARSE=$(EXTENDED_SPARSE) \
                    COMPRESSION_ZLIB=$(COMPRESSION_ZLIB) \
                    IS_WIN32=$(IS_WIN32) \
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
	$(AUTOTEST) -I '$(srcdir)' -I '$(tests_dir)' $@.at -o $@.tmp
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
