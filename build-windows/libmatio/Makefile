top_builddir = ..
top_srcdir = ..\..\src
srcdir = ..\..\src

CFLAGS  = -I$(top_builddir) $(ZLIB_CFLAGS) $(CFLAGS)
FCFLAGS = -I$(top_builddir) $(FCFLAGS)

.SUFFIXES : .c .obj .lib

.c.obj:
	$(CC) -o $@ $(CFLAGS) $<

libmatio_lib_SOURCES = snprintf.c endian.c io.c read_data.c inflate.c \
                       mat5.c mat4.c mat.c

!IF $(BUILD_FORTRAN)
include_HEADERS=$(srcdir)\matio.h ..\matioConfig.h matio.mod
libfmatio_lib_SOURCES = fortran\matio_internal.c fortran\matio.f90
!ELSE
include_HEADERS=$(srcdir)\matio.h ..\matioConfig.h
libfmatio_lib_SOURCES = 
!ENDIF

libmatio.lib:
	for %f in ($(libmatio_lib_SOURCES)) do $(CC) -c $(CFLAGS) $(srcdir)\%f
	for %f in ($(libfmatio_lib_SOURCES)) do $(FC) -c $(FCFLAGS) $(srcdir)\%f
	$(LD) -out:$@ *.obj

install:
	if not exist $(EXEC_PREFIX)\lib mkdir $(EXEC_PREFIX)\lib
	copy *.lib $(EXEC_PREFIX)\lib
	if not exist $(EXEC_PREFIX)\include mkdir $(EXEC_PREFIX)\include
	for %f in ($(include_HEADERS)) do copy %f $(EXEC_PREFIX)\include

clean:
	del *.obj
	del *.lib
	del *.mod
