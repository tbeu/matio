## Makefile.am -- Process this file with automake to produce Makefile.in
##
## Copyright (c) 2015-2025, The matio contributors
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

AM_CFLAGS  = -I. -I$(top_srcdir)/../src -I$(top_builddir)/../src
AM_FCFLAGS = -I$(top_builddir)/src
AM_LDFLAGS = $(FCLDFLAGS) -avoid-version

include_HEADERS = $(top_builddir)/src/matio.mod
lib_LTLIBRARIES      = libfmatio.la
libfmatio_la_SOURCES = matio_internal.c matio.f90

print:
	@list='$(libfmatio_a_SOURCES)'; \
	rm -f libfmatio.ps; \
	curdir=`pwd`; \
	cd $(srcdir); \
	for f in $$list; do \
	    enscript -2 -G -r -Ec --color -o - $$f >> $$curdir/libfmatio.ps;\
	done; \
	cd $$curdir
