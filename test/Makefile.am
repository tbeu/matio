AUTOMAKE_OPTIONS = foreign 1.8 no-dependencies
VPATH = @srcdir@

AM_CFLAGS = $(all_includes) -I$(top_srcdir)/src $(ZLIB_CFLAGS)
AM_LDFLAGS = $(FCLDFLAGS)
TEST_LIBS= -lmatio $(ZLIB_LIBS)
TEST_LFLAGS = $(all_libraries) -L$(top_builddir)/src

bin_PROGRAMS = test_mat test_snprintf

test_mat_SOURCES = test_mat.c
test_mat_LDADD   = -lmatio $(ZLIB_LIBS) $(all_libraries)
test_mat_LDFLAGS = $(TEST_LFLAGS)

test_snprintf_SOURCES = test_snprintf.c
test_snprintf_LDADD   = -lmatio $(ZLIB_LIBS) $(all_libraries)
test_snprintf_LDFLAGS = $(TEST_LFLAGS)

if ENABLE_FORTRAN
bin_PROGRAMS += test_matf

test_matf_SOURCES = test_matf.f90
test_matf_CFLAGS  = -I$(top_builddir)/src -I$(top_builddir)/src/fortran
test_matf_FCFLAGS = -I$(top_builddir)/src -I$(top_builddir)/src/fortran
test_matf_LDFLAGS = -L$(top_builddir)/src -L$(top_builddir)/src/fortran
test_matf_LDADD   = -lmatio $(ZLIB_LIBS)
endif
