## Makefile.am -- Process this file with automake to produce Makefile.in
##
## Copyright (c) 2015-2026, The matio contributors
## Copyright (c) 2005-2014, Christopher C. Hulbert
## All rights reserved.
##
## SPDX-License-Identifier: BSD-2-Clause
##
VPATH = @srcdir@

ACLOCAL_AMFLAGS = -I config

datadir = $(prefix)/share/matio
VS_FILES = visual_studio/libmatio/libmatio.vcproj \
           visual_studio/libmatio/libmatio.vcxproj \
           visual_studio/libmatio/libmatio.vcxproj.filters \
           visual_studio/matdump/matdump.vcproj \
           visual_studio/matdump/matdump.vcxproj \
           visual_studio/matdump/matdump.vcxproj.filters \
           visual_studio/test_mat/test_mat.vcxproj \
           visual_studio/test_mat/test_mat.vcxproj.filters \
           visual_studio/matio.def \
           visual_studio/matio_vs2008.sln \
           visual_studio/matio.sln \
           visual_studio/matioConfig.h \
           visual_studio/matio_pubconf.h \
           visual_studio/matio.rc \
           visual_studio/stdint_msvc.h

CMAKE_FILES = CMakeLists.txt \
              conanfile.py \
              cmake/compilerOptions.cmake \
              cmake/conan.cmake \
              cmake/conan_provider.cmake \
              cmake/getopt.cmake \
              cmake/options.cmake \
              cmake/run_matlab_test.cmake \
              cmake/run_matlab_cmd.cmake \
              cmake/src.cmake \
              cmake/staticAnalyzers.cmake \
              cmake/test.cmake \
              cmake/thirdParties.cmake \
              cmake/tools.cmake \
              cmake/matio_pubconf.cmake.in \
              cmake/matioConfig.cmake.in

EXTRA_DIST = matio.pc.in \
             autogen.sh \
             share/test_file.mat \
             share/test_file_v6.mat \
             share/test_file_hdf5.mat \
             $(CMAKE_FILES) \
             $(VS_FILES)

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = matio.pc
$(pkgconfig_DATA): config.status

if NEED_GETOPT
    GETOPT = getopt
endif

SUBDIRS = snprintf src $(GETOPT) tools test documentation

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
