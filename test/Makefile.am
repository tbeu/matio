## Makefile.am -- Process this file with automake to produce Makefile.in
##
## Copyright (c) 2015-2026, The matio contributors
## Copyright (c) 2005-2014, Christopher C. Hulbert
## All rights reserved.
##
## SPDX-License-Identifier: BSD-2-Clause
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
               $(tests_dir)/mat73_uncompressed_extended_sparse.at \
               $(tests_dir)/mat5_uncompressed_read_mcos.at \
               $(tests_dir)/mat5_compressed_read_mcos.at \
               $(tests_dir)/mat73_read_mcos.at

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
                 $(datasets_dir)/struct_nullpad_class_name_hdf_le.mat \
                 $(datasets_dir)/small_v4_be.mat \
                 $(datasets_dir)/small_v4_le.mat \
                 $(datasets_dir)/bugv6.mat \
                 $(datasets_dir)/bugv7.mat \
                 $(datasets_dir)/test_basic_v73.mat \
                 $(datasets_dir)/test_class_alias.mat \
                 $(datasets_dir)/test_corrupted_mcos_object_metadata.mat \
                 $(datasets_dir)/test_corrupted_subsystem.mat \
                 $(datasets_dir)/test_dynamic_v7.mat \
                 $(datasets_dir)/test_dynamic_v73.mat \
                 $(datasets_dir)/test_enum_v7.mat \
                 $(datasets_dir)/test_enum_v73.mat \
                 $(datasets_dir)/test_figure.fig \
                 $(datasets_dir)/test_function_handles_v7.mat \
                 $(datasets_dir)/test_function_handles_v73.mat \
                 $(datasets_dir)/test_maps_v7.mat \
                 $(datasets_dir)/test_maps_v73.mat \
                 $(datasets_dir)/test_scattered_interpolant_v6.mat \
                 $(datasets_dir)/test_scattered_interpolant_v7.mat \
                 $(datasets_dir)/test_scattered_interpolant_v7.3.mat \
                 $(datasets_dir)/test_string_v7.mat \
                 $(datasets_dir)/test_string_v73.mat \
                 $(datasets_dir)/test_strings_v7.mat \
                 $(datasets_dir)/test_table_all_v6.mat \
                 $(datasets_dir)/test_table_all_v7.mat \
                 $(datasets_dir)/test_table_all_v7.3.mat \
                 $(datasets_dir)/test_table_v6.mat \
                 $(datasets_dir)/test_table_v7.mat \
                 $(datasets_dir)/test_table_v7.3.mat \
                 $(datasets_dir)/test_tables_v7.mat \
                 $(datasets_dir)/test_tables_v73.mat \
                 $(datasets_dir)/test_time_v7.mat \
                 $(datasets_dir)/test_time_v73.mat \
                 $(datasets_dir)/test_type_systems_v7.mat \
                 $(datasets_dir)/test_type_systems_v73.mat \
                 $(datasets_dir)/test_user_defined_v7.mat \
                 $(datasets_dir)/test_user_defined_v73.mat \
                 $(matlab_dir)/test_write_1d_logical.m \
                 $(matlab_dir)/test_write_2d_logical.m \
                 $(matlab_dir)/test_write_1d_numeric.m \
                 $(matlab_dir)/test_write_2d_numeric.m \
                 $(matlab_dir)/test_write_cell_2d_logical.m \
                 $(matlab_dir)/test_write_cell_2d_numeric.m \
                 $(matlab_dir)/test_write_cell_complex_2d_numeric.m \
                 $(matlab_dir)/test_write_cell_empty_struct.m \
                 $(matlab_dir)/test_write_char_unicode_1d.m \
                 $(matlab_dir)/test_write_char_utf8_1d.m \
                 $(matlab_dir)/test_write_char_1d.m \
                 $(matlab_dir)/test_write_char_unicode_2d.m \
                 $(matlab_dir)/test_write_char_utf8_2d.m \
                 $(matlab_dir)/test_write_char_2d.m \
                 $(matlab_dir)/test_write_complex_1d_numeric.m \
                 $(matlab_dir)/test_write_complex_2d_numeric.m \
                 $(matlab_dir)/test_write_empty_2d_numeric.m \
                 $(matlab_dir)/test_write_empty_cell.m \
                 $(matlab_dir)/test_write_empty_struct.m \
                 $(matlab_dir)/test_write_sparse.m \
                 $(matlab_dir)/test_write_sparse_complex.m \
                 $(matlab_dir)/test_write_allzero_sparse.m \
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
                 $(results_dir)/read-nullpad_class_name.out \
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
                 $(results_dir)/readvar-write_char_unicode_1d-a-5.out \
                 $(results_dir)/readvar-write_char_unicode_1d-a-73.out \
                 $(results_dir)/readvar-write_char_utf8_1d-a-5.out \
                 $(results_dir)/readvar-write_char_1d-a.out \
                 $(results_dir)/readvar-write_char_1d-a-5.out \
                 $(results_dir)/readvar-write_char_1d-a-73.out \
                 $(results_dir)/readvar-write_char_unicode_2d-a-5.out \
                 $(results_dir)/readvar-write_char_unicode_2d-a-73.out \
                 $(results_dir)/readvar-write_char_utf8_2d-a-5.out \
                 $(results_dir)/readvar-write_char_2d-a.out \
                 $(results_dir)/readvar-write_char_2d-a-5.out \
                 $(results_dir)/readvar-write_char_2d-a-73.out \
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
                 $(results_dir)/readvar-write_allzero_sparse-sparse_matrix-double.out \
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
                 $(results_dir)/write_1d_numeric-73-l2-logical.out \
                 $(results_dir)/write_1d_numeric-73-l4-logical.out \
                 $(results_dir)/write_1d_numeric-double.out \
                 $(results_dir)/write_1d_numeric-int8.out \
                 $(results_dir)/write_1d_numeric-int16.out \
                 $(results_dir)/write_1d_numeric-int32.out \
                 $(results_dir)/write_1d_numeric-int64.out \
                 $(results_dir)/write_1d_numeric-l1-logical.out \
                 $(results_dir)/write_1d_numeric-l2-logical.out \
                 $(results_dir)/write_1d_numeric-l4-logical.out \
                 $(results_dir)/write_1d_numeric-single.out \
                 $(results_dir)/write_1d_numeric-uint8.out \
                 $(results_dir)/write_1d_numeric-uint16.out \
                 $(results_dir)/write_1d_numeric-uint32.out \
                 $(results_dir)/write_1d_numeric-uint64.out \
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
                 $(results_dir)/write_complex_1d_numeric-double.out \
                 $(results_dir)/write_complex_1d_numeric-int8.out \
                 $(results_dir)/write_complex_1d_numeric-int16.out \
                 $(results_dir)/write_complex_1d_numeric-int32.out \
                 $(results_dir)/write_complex_1d_numeric-int64.out \
                 $(results_dir)/write_complex_1d_numeric-single.out \
                 $(results_dir)/write_complex_1d_numeric-uint8.out \
                 $(results_dir)/write_complex_1d_numeric-uint16.out \
                 $(results_dir)/write_complex_1d_numeric-uint32.out \
                 $(results_dir)/write_complex_1d_numeric-uint64.out \
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
                 $(results_dir)/write_struct_complex_2d_numeric-uint64.out \
                 $(results_dir)/read-mcos-bugv6-la.out \
                 $(results_dir)/read-mcos-bugv7-la.out \
                 $(results_dir)/read-mcos-class_alias-obj.out \
                 $(results_dir)/read-mcos-class_alias-obj1.out \
                 $(results_dir)/read-mcos-dynamic_v7-obj.out \
                 $(results_dir)/read-mcos-dynamic_v73-obj.out \
                 $(results_dir)/read-mcos-enum_v7-enum_array.out \
                 $(results_dir)/read-mcos-enum_v7-enum_nested.out \
                 $(results_dir)/read-mcos-enum_v7-enum_scalar.out \
                 $(results_dir)/read-mcos-enum_v7-enum_uint32.out \
                 $(results_dir)/read-mcos-enum_v73-enum_array.out \
                 $(results_dir)/read-mcos-enum_v73-enum_nested.out \
                 $(results_dir)/read-mcos-enum_v73-enum_scalar.out \
                 $(results_dir)/read-mcos-enum_v73-enum_uint32.out \
                 $(results_dir)/read-mcos-figure-hgM_070000.out \
                 $(results_dir)/read-mcos-figure-hgS_070000.out \
                 $(results_dir)/read-mcos-figure-meta_data.out \
                 $(results_dir)/read-mcos-function_handles_v7-anonymous_fh.out \
                 $(results_dir)/read-mcos-function_handles_v7-builtin_fh.out \
                 $(results_dir)/read-mcos-function_handles_v7-class_fh.out \
                 $(results_dir)/read-mcos-function_handles_v7-custom_fh.out \
                 $(results_dir)/read-mcos-function_handles_v7-nested_fh.out \
                 $(results_dir)/read-mcos-function_handles_v73-anonymous_fh.out \
                 $(results_dir)/read-mcos-function_handles_v73-builtin_fh.out \
                 $(results_dir)/read-mcos-function_handles_v73-class_fh.out \
                 $(results_dir)/read-mcos-function_handles_v73-custom_fh.out \
                 $(results_dir)/read-mcos-function_handles_v73-nested_fh.out \
                 $(results_dir)/read-mcos-maps_v7-dict_cell_keys.out \
                 $(results_dir)/read-mcos-maps_v7-dict_cell_vals.out \
                 $(results_dir)/read-mcos-maps_v7-dict_empty.out \
                 $(results_dir)/read-mcos-maps_v7-dict_numeric_keys.out \
                 $(results_dir)/read-mcos-maps_v7-dict_string_keys.out \
                 $(results_dir)/read-mcos-maps_v7-dict_val_scalar.out \
                 $(results_dir)/read-mcos-maps_v7-map_char_keys.out \
                 $(results_dir)/read-mcos-maps_v7-map_empty.out \
                 $(results_dir)/read-mcos-maps_v7-map_numeric_keys.out \
                 $(results_dir)/read-mcos-maps_v7-map_string_keys.out \
                 $(results_dir)/read-mcos-maps_v73-dict_cell_keys.out \
                 $(results_dir)/read-mcos-maps_v73-dict_cell_vals.out \
                 $(results_dir)/read-mcos-maps_v73-dict_empty.out \
                 $(results_dir)/read-mcos-maps_v73-dict_numeric_keys.out \
                 $(results_dir)/read-mcos-maps_v73-dict_string_keys.out \
                 $(results_dir)/read-mcos-maps_v73-dict_val_scalar.out \
                 $(results_dir)/read-mcos-maps_v73-map_char_keys.out \
                 $(results_dir)/read-mcos-maps_v73-map_empty.out \
                 $(results_dir)/read-mcos-maps_v73-map_numeric_keys.out \
                 $(results_dir)/read-mcos-maps_v73-map_string_keys.out \
                 $(results_dir)/read-mcos-scattered_interpolant_v6-F.out \
                 $(results_dir)/read-mcos-scattered_interpolant_v7-F.out \
                 $(results_dir)/read-mcos-scattered_interpolant_v73-F.out \
                 $(results_dir)/read-mcos-string_v7-string_array.out \
                 $(results_dir)/read-mcos-string_v7-string_empty.out \
                 $(results_dir)/read-mcos-string_v7-string_scalar.out \
                 $(results_dir)/read-mcos-string_v73-string_array.out \
                 $(results_dir)/read-mcos-string_v73-string_empty.out \
                 $(results_dir)/read-mcos-string_v73-string_scalar.out \
                 $(results_dir)/read-mcos-strings_v7-a.out \
                 $(results_dir)/read-mcos-strings_v7-b.out \
                 $(results_dir)/read-mcos-strings_v7-c.out \
                 $(results_dir)/read-mcos-strings_v7-cell.out \
                 $(results_dir)/read-mcos-strings_v7-char.out \
                 $(results_dir)/read-mcos-strings_v7-ee.out \
                 $(results_dir)/read-mcos-strings_v7-s.out \
                 $(results_dir)/read-mcos-strings_v7-s1.out \
                 $(results_dir)/read-mcos-strings_v7-s2.out \
                 $(results_dir)/read-mcos-strings_v7-str_last.out \
                 $(results_dir)/read-mcos-table_all_v6-Comment.out \
                 $(results_dir)/read-mcos-table_all_v6-Customer.out \
                 $(results_dir)/read-mcos-table_all_v6-Date.out \
                 $(results_dir)/read-mcos-table_all_v6-FlightNum.out \
                 $(results_dir)/read-mcos-table_all_v6-Rating.out \
                 $(results_dir)/read-mcos-table_all_v7-Comment.out \
                 $(results_dir)/read-mcos-table_all_v7-Customer.out \
                 $(results_dir)/read-mcos-table_all_v7-Date.out \
                 $(results_dir)/read-mcos-table_all_v7-FlightNum.out \
                 $(results_dir)/read-mcos-table_all_v7-Rating.out \
                 $(results_dir)/read-mcos-table_all_v73-Comment.out \
                 $(results_dir)/read-mcos-table_all_v73-Customer.out \
                 $(results_dir)/read-mcos-table_all_v73-Date.out \
                 $(results_dir)/read-mcos-table_all_v73-FlightNum.out \
                 $(results_dir)/read-mcos-table_all_v73-Rating.out \
                 $(results_dir)/read-mcos-table_v6-T.out \
                 $(results_dir)/read-mcos-table_v7-T.out \
                 $(results_dir)/read-mcos-table_v73-T.out \
                 $(results_dir)/read-mcos-tables_v7-cat_3D.out \
                 $(results_dir)/read-mcos-tables_v7-cat_array.out \
                 $(results_dir)/read-mcos-tables_v7-cat_empty.out \
                 $(results_dir)/read-mcos-tables_v7-cat_from_numeric.out \
                 $(results_dir)/read-mcos-tables_v7-cat_missing.out \
                 $(results_dir)/read-mcos-tables_v7-cat_mixed_case.out \
                 $(results_dir)/read-mcos-tables_v7-cat_ordered.out \
                 $(results_dir)/read-mcos-tables_v7-cat_scalar.out \
                 $(results_dir)/read-mcos-tables_v7-cat_string.out \
                 $(results_dir)/read-mcos-tables_v7-cat_unordered.out \
                 $(results_dir)/read-mcos-tables_v7-table_empty.out \
                 $(results_dir)/read-mcos-tables_v7-table_from_cell.out \
                 $(results_dir)/read-mcos-tables_v7-table_multi_col_data.out \
                 $(results_dir)/read-mcos-tables_v7-table_numeric.out \
                 $(results_dir)/read-mcos-tables_v7-table_row_names.out \
                 $(results_dir)/read-mcos-tables_v7-table_strings.out \
                 $(results_dir)/read-mcos-tables_v7-table_time.out \
                 $(results_dir)/read-mcos-tables_v7-table_var_names.out \
                 $(results_dir)/read-mcos-tables_v7-table_with_attrs.out \
                 $(results_dir)/read-mcos-tables_v7-table_with_objects.out \
                 $(results_dir)/read-mcos-tables_v7-timetable_datetime.out \
                 $(results_dir)/read-mcos-tables_v7-timetable_duration.out \
                 $(results_dir)/read-mcos-tables_v7-timetable_empty.out \
                 $(results_dir)/read-mcos-tables_v7-timetable_from_duration.out \
                 $(results_dir)/read-mcos-tables_v7-timetable_from_sample_rate.out \
                 $(results_dir)/read-mcos-tables_v7-timetable_from_starttime_datetime.out \
                 $(results_dir)/read-mcos-tables_v7-timetable_from_starttime_duration.out \
                 $(results_dir)/read-mcos-tables_v7-timetable_multi_col.out \
                 $(results_dir)/read-mcos-tables_v7-timetable_var_names.out \
                 $(results_dir)/read-mcos-tables_v7-timetable_with_attrs.out \
                 $(results_dir)/read-mcos-tables_v73-cat_3D.out \
                 $(results_dir)/read-mcos-tables_v73-cat_array.out \
                 $(results_dir)/read-mcos-tables_v73-cat_empty.out \
                 $(results_dir)/read-mcos-tables_v73-cat_from_numeric.out \
                 $(results_dir)/read-mcos-tables_v73-cat_missing.out \
                 $(results_dir)/read-mcos-tables_v73-cat_mixed_case.out \
                 $(results_dir)/read-mcos-tables_v73-cat_ordered.out \
                 $(results_dir)/read-mcos-tables_v73-cat_scalar.out \
                 $(results_dir)/read-mcos-tables_v73-cat_string.out \
                 $(results_dir)/read-mcos-tables_v73-cat_unordered.out \
                 $(results_dir)/read-mcos-tables_v73-table_empty.out \
                 $(results_dir)/read-mcos-tables_v73-table_from_cell.out \
                 $(results_dir)/read-mcos-tables_v73-table_multi_col_data.out \
                 $(results_dir)/read-mcos-tables_v73-table_numeric.out \
                 $(results_dir)/read-mcos-tables_v73-table_row_names.out \
                 $(results_dir)/read-mcos-tables_v73-table_strings.out \
                 $(results_dir)/read-mcos-tables_v73-table_time.out \
                 $(results_dir)/read-mcos-tables_v73-table_var_names.out \
                 $(results_dir)/read-mcos-tables_v73-table_with_attrs.out \
                 $(results_dir)/read-mcos-tables_v73-table_with_objects.out \
                 $(results_dir)/read-mcos-tables_v73-timetable_datetime.out \
                 $(results_dir)/read-mcos-tables_v73-timetable_duration.out \
                 $(results_dir)/read-mcos-tables_v73-timetable_empty.out \
                 $(results_dir)/read-mcos-tables_v73-timetable_from_duration.out \
                 $(results_dir)/read-mcos-tables_v73-timetable_from_sample_rate.out \
                 $(results_dir)/read-mcos-tables_v73-timetable_from_starttime_datetime.out \
                 $(results_dir)/read-mcos-tables_v73-timetable_from_starttime_duration.out \
                 $(results_dir)/read-mcos-tables_v73-timetable_multi_col.out \
                 $(results_dir)/read-mcos-tables_v73-timetable_var_names.out \
                 $(results_dir)/read-mcos-tables_v73-timetable_with_attrs.out \
                 $(results_dir)/read-mcos-time_v7-cdur_array.out \
                 $(results_dir)/read-mcos-time_v7-cdur_days.out \
                 $(results_dir)/read-mcos-time_v7-cdur_days_and_months.out \
                 $(results_dir)/read-mcos-time_v7-cdur_days_and_qtrs.out \
                 $(results_dir)/read-mcos-time_v7-cdur_empty.out \
                 $(results_dir)/read-mcos-time_v7-cdur_millis.out \
                 $(results_dir)/read-mcos-time_v7-cdur_months_and_years.out \
                 $(results_dir)/read-mcos-time_v7-cdur_weeks.out \
                 $(results_dir)/read-mcos-time_v7-dt_array.out \
                 $(results_dir)/read-mcos-time_v7-dt_basic.out \
                 $(results_dir)/read-mcos-time_v7-dt_empty.out \
                 $(results_dir)/read-mcos-time_v7-dt_fmt.out \
                 $(results_dir)/read-mcos-time_v7-dt_tz.out \
                 $(results_dir)/read-mcos-time_v7-dt_vector.out \
                 $(results_dir)/read-mcos-time_v7-dur_array.out \
                 $(results_dir)/read-mcos-time_v7-dur_days.out \
                 $(results_dir)/read-mcos-time_v7-dur_empty.out \
                 $(results_dir)/read-mcos-time_v7-dur_h.out \
                 $(results_dir)/read-mcos-time_v7-dur_hms.out \
                 $(results_dir)/read-mcos-time_v7-dur_m.out \
                 $(results_dir)/read-mcos-time_v7-dur_s.out \
                 $(results_dir)/read-mcos-time_v7-dur_years.out \
                 $(results_dir)/read-mcos-time_v73-cdur_array.out \
                 $(results_dir)/read-mcos-time_v73-cdur_days.out \
                 $(results_dir)/read-mcos-time_v73-cdur_days_and_months.out \
                 $(results_dir)/read-mcos-time_v73-cdur_days_and_qtrs.out \
                 $(results_dir)/read-mcos-time_v73-cdur_empty.out \
                 $(results_dir)/read-mcos-time_v73-cdur_millis.out \
                 $(results_dir)/read-mcos-time_v73-cdur_months_and_years.out \
                 $(results_dir)/read-mcos-time_v73-cdur_weeks.out \
                 $(results_dir)/read-mcos-time_v73-dt_array.out \
                 $(results_dir)/read-mcos-time_v73-dt_basic.out \
                 $(results_dir)/read-mcos-time_v73-dt_empty.out \
                 $(results_dir)/read-mcos-time_v73-dt_fmt.out \
                 $(results_dir)/read-mcos-time_v73-dt_tz.out \
                 $(results_dir)/read-mcos-time_v73-dt_vector.out \
                 $(results_dir)/read-mcos-time_v73-dur_array.out \
                 $(results_dir)/read-mcos-time_v73-dur_days.out \
                 $(results_dir)/read-mcos-time_v73-dur_empty.out \
                 $(results_dir)/read-mcos-time_v73-dur_h.out \
                 $(results_dir)/read-mcos-time_v73-dur_hms.out \
                 $(results_dir)/read-mcos-time_v73-dur_m.out \
                 $(results_dir)/read-mcos-time_v73-dur_s.out \
                 $(results_dir)/read-mcos-time_v73-dur_years.out \
                 $(results_dir)/read-mcos-type_systems_v7-handletype.out \
                 $(results_dir)/read-mcos-type_systems_v7-javatype.out \
                 $(results_dir)/read-mcos-type_systems_v73-handletype.out \
                 $(results_dir)/read-mcos-type_systems_v73-javatype.out \
                 $(results_dir)/read-mcos-user_defined_v7-obj_array.out \
                 $(results_dir)/read-mcos-user_defined_v7-obj_handle_1.out \
                 $(results_dir)/read-mcos-user_defined_v7-obj_handle_2.out \
                 $(results_dir)/read-mcos-user_defined_v7-obj_no_vals.out \
                 $(results_dir)/read-mcos-user_defined_v7-obj_with_default_val.out \
                 $(results_dir)/read-mcos-user_defined_v7-obj_with_nested_props.out \
                 $(results_dir)/read-mcos-user_defined_v7-obj_with_vals.out \
                 $(results_dir)/read-mcos-user_defined_v73-obj_array.out \
                 $(results_dir)/read-mcos-user_defined_v73-obj_handle_1.out \
                 $(results_dir)/read-mcos-user_defined_v73-obj_handle_2.out \
                 $(results_dir)/read-mcos-user_defined_v73-obj_no_vals.out \
                 $(results_dir)/read-mcos-user_defined_v73-obj_with_default_val.out \
                 $(results_dir)/read-mcos-user_defined_v73-obj_with_nested_props.out \
                 $(results_dir)/read-mcos-user_defined_v73-obj_with_vals.out \
                 $(results_dir)/dump-mcos-bugv6.out \
                 $(results_dir)/dump-mcos-bugv6-hd.out \
                 $(results_dir)/dump-mcos-bugv7.out \
                 $(results_dir)/dump-mcos-bugv7-hd.out \
                 $(results_dir)/dump-mcos-class_alias.out \
                 $(results_dir)/dump-mcos-class_alias-hd.out \
                 $(results_dir)/dump-mcos-dynamic_v7.out \
                 $(results_dir)/dump-mcos-dynamic_v7-hd.out \
                 $(results_dir)/dump-mcos-dynamic_v73.out \
                 $(results_dir)/dump-mcos-dynamic_v73-hd.out \
                 $(results_dir)/dump-mcos-enum_v7.out \
                 $(results_dir)/dump-mcos-enum_v7-hd.out \
                 $(results_dir)/dump-mcos-enum_v73.out \
                 $(results_dir)/dump-mcos-enum_v73-hd.out \
                 $(results_dir)/dump-mcos-figure.out \
                 $(results_dir)/dump-mcos-figure-hd.out \
                 $(results_dir)/dump-mcos-function_handles_v7.out \
                 $(results_dir)/dump-mcos-function_handles_v7-hd.out \
                 $(results_dir)/dump-mcos-maps_v7.out \
                 $(results_dir)/dump-mcos-maps_v7-hd.out \
                 $(results_dir)/dump-mcos-maps_v73.out \
                 $(results_dir)/dump-mcos-maps_v73-hd.out \
                 $(results_dir)/dump-mcos-scattered_interpolant_v6.out \
                 $(results_dir)/dump-mcos-scattered_interpolant_v6-hd.out \
                 $(results_dir)/dump-mcos-scattered_interpolant_v7.out \
                 $(results_dir)/dump-mcos-scattered_interpolant_v7-hd.out \
                 $(results_dir)/dump-mcos-scattered_interpolant_v73.out \
                 $(results_dir)/dump-mcos-scattered_interpolant_v73-hd.out \
                 $(results_dir)/dump-mcos-string_v7.out \
                 $(results_dir)/dump-mcos-string_v7-hd.out \
                 $(results_dir)/dump-mcos-string_v73.out \
                 $(results_dir)/dump-mcos-string_v73-hd.out \
                 $(results_dir)/dump-mcos-strings_v7.out \
                 $(results_dir)/dump-mcos-strings_v7-hd.out \
                 $(results_dir)/dump-mcos-table_all_v6.out \
                 $(results_dir)/dump-mcos-table_all_v6-hd.out \
                 $(results_dir)/dump-mcos-table_all_v7.out \
                 $(results_dir)/dump-mcos-table_all_v7-hd.out \
                 $(results_dir)/dump-mcos-table_all_v73.out \
                 $(results_dir)/dump-mcos-table_all_v73-hd.out \
                 $(results_dir)/dump-mcos-table_v6.out \
                 $(results_dir)/dump-mcos-table_v6-hd.out \
                 $(results_dir)/dump-mcos-table_v7.out \
                 $(results_dir)/dump-mcos-table_v7-hd.out \
                 $(results_dir)/dump-mcos-table_v73.out \
                 $(results_dir)/dump-mcos-table_v73-hd.out \
                 $(results_dir)/dump-mcos-tables_v7.out \
                 $(results_dir)/dump-mcos-tables_v7-hd.out \
                 $(results_dir)/dump-mcos-time_v7.out \
                 $(results_dir)/dump-mcos-time_v7-hd.out \
                 $(results_dir)/dump-mcos-time_v73.out \
                 $(results_dir)/dump-mcos-time_v73-hd.out \
                 $(results_dir)/dump-mcos-type_systems_v7.out \
                 $(results_dir)/dump-mcos-type_systems_v7-hd.out \
                 $(results_dir)/dump-mcos-type_systems_v73.out \
                 $(results_dir)/dump-mcos-type_systems_v73-hd.out \
                 $(results_dir)/dump-mcos-user_defined_v7.out \
                 $(results_dir)/dump-mcos-user_defined_v7-hd.out \
                 $(results_dir)/dump-mcos-user_defined_v73.out \
                 $(results_dir)/dump-mcos-user_defined_v73-hd.out
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

if MATIO_MCOS
    MCOS = 1
else
    MCOS = 0
endif

CHECK_ENVIRONMENT = GREP=$(GREP) MAT73=$(MAT73) MATLABEXE=$(MATLABEXE) \
                    HAVE_UINT64=$(HAVE_UINT64) HAVE_INT64=$(HAVE_INT64) \
                    EXTENDED_SPARSE=$(EXTENDED_SPARSE) \
                    COMPRESSION_ZLIB=$(COMPRESSION_ZLIB) \
                    MCOS=$(MCOS) \
                    IS_WIN32=$(IS_WIN32) \
                    LIBTOOL="$(top_builddir)/libtool"

EXTRA_DIST = $(TESTSUITE_AT) $(TESTSUITE) $(srcdir)/package.m4 \
             $(TEST_DATAFILES) $(srcdir)/convert_at_to_ctest.py

if NEED_GETOPT
    GETOPT_CFLAGS = -I$(top_builddir)/getopt
    GETOPT_LIBS   = $(top_builddir)/getopt/libgetopt.la
endif

SNPRINTF_LIBS = $(top_builddir)/snprintf/libsnprintf.la

AM_CFLAGS   = -I$(top_srcdir)/src $(GETOPT_CFLAGS) $(HDF5_CFLAGS) $(ZLIB_CFLAGS)
AM_LDFLAGS  = $(FCLDFLAGS)
TEST_LIBS   = $(top_builddir)/src/libmatio.la $(GETOPT_LIBS) $(HDF5_LIBS) $(ZLIB_LIBS)
TEST_LFLAGS = -L$(top_builddir)/src

noinst_PROGRAMS = test_mat test_snprintf

test_mat_SOURCES = test_mat.c
test_mat_LDADD   = $(TEST_LIBS)
test_mat_LDFLAGS = $(TEST_LFLAGS)

test_snprintf_SOURCES = test_snprintf.c
test_snprintf_LDADD   = $(SNPRINTF_LIBS)
test_snprintf_LDFLAGS = -L$(top_builddir)/snprintf

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
