## Makefile.am -- Process this file with automake to produce Makefile.in
##
## Copyright (C) 2005-2017   Christopher C. Hulbert
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
           visual_studio/matio.rc

EXTRA_DIST = matio.pc.in autogen.sh patches/inflate.patch \
             share/test_file.mat share/test_file_v6.mat $(VS_FILES)

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = matio.pc
$(pkgconfig_DATA): config.status

if NEED_GETOPT
GETOPT=getopt
endif

SUBDIRS = src $(GETOPT) tools test documentation

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
