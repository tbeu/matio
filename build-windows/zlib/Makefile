# Makefile for zlib -- Microsoft (Visual) C
#
# Authors:
#   Cosmin Truta, 11-Mar-2003
#   Christian Spieler, 19-Mar-2003
#
# Last updated:
#   Cosmin Truta, 27-Aug-2003
#
# Usage:
#   nmake -f win32/Makefile.msc            (standard build)
#   nmake -f win32/Makefile.msc LOC=-DFOO  (nonstandard build)
#   nmake -f win32/Makefile.msc LOC=-DASMV OBJA=match.obj  (use ASM code)


# optional build flags
LOC =

srcdir = ..\..\zlib

# variables
STATICLIB = zlib.lib
SHAREDLIB = zlib1.dll
IMPLIB    = zdll.lib

# CC = cl
AS = ml
# LD = link
AR = lib
RC = rc
CFLAGS  = -nologo -MD -O2 $(LOC)
ASFLAGS = -coff
LDFLAGS = -nologo -release
ARFLAGS = -nologo
RCFLAGS = /dWIN32 /r

OBJS = adler32.obj compress.obj crc32.obj deflate.obj gzio.obj infback.obj \
       inffast.obj inflate.obj inftrees.obj trees.obj uncompr.obj zutil.obj
SRCS = adler32.c compress.c crc32.c deflate.c gzio.c infback.c \
       inffast.c inflate.c inftrees.c trees.c uncompr.c zutil.c
OBJA =

include_HEADERS = $(srcdir)\zconf.h $(srcdir)\zlib.h

# targets
all: $(STATICLIB)
#     example.exe minigzip.exe example_d.exe minigzip_d.exe

$(STATICLIB): $(OBJS) $(OBJA)
	for %f in ($(SRCS)) do $(CC) -c $(CFLAGS) $(srcdir)\%f
	$(AR) $(ARFLAGS) -out:$@ $(OBJS) $(OBJA)

$(IMPLIB): $(SHAREDLIB)

$(SHAREDLIB): $(srcdir)\win32\zlib.def $(OBJS) $(OBJA) zlib1.res
	$(LD) $(LDFLAGS) -def:win32/zlib.def -dll -implib:$(IMPLIB) \
	  -out:$@ $(OBJS) $(OBJA) zlib1.res

example.exe: example.obj $(STATICLIB)
	$(LD) $(LDFLAGS) example.obj $(STATICLIB)

minigzip.exe: minigzip.obj $(STATICLIB)
	$(LD) $(LDFLAGS) minigzip.obj $(STATICLIB)

example_d.exe: example.obj $(IMPLIB)
	$(LD) $(LDFLAGS) -out:$@ example.obj $(IMPLIB)

minigzip_d.exe: minigzip.obj $(IMPLIB)
	$(LD) $(LDFLAGS) -out:$@ minigzip.obj $(IMPLIB)

.c.obj:
	$(CC) -c $(CFLAGS) $<

.asm.obj:
	$(AS) -c $(ASFLAGS) $<

adler32.obj: $(srcdir)\adler32.c $(srcdir)\zlib.h $(srcdir)\zconf.h

compress.obj: $(srcdir)\compress.c $(srcdir)\zlib.h $(srcdir)\zconf.h

crc32.obj: $(srcdir)\crc32.c $(srcdir)\zlib.h $(srcdir)\zconf.h $(srcdir)\crc32.h

deflate.obj: $(srcdir)\deflate.c $(srcdir)\deflate.h $(srcdir)\zutil.h $(srcdir)\zlib.h $(srcdir)\zconf.h

gzio.obj: $(srcdir)\gzio.c $(srcdir)\zutil.h $(srcdir)\zlib.h $(srcdir)\zconf.h

infback.obj: $(srcdir)\infback.c $(srcdir)\zutil.h $(srcdir)\zlib.h $(srcdir)\zconf.h $(srcdir)\inftrees.h $(srcdir)\inflate.h \
             $(srcdir)\inffast.h $(srcdir)\inffixed.h

inffast.obj: $(srcdir)\inffast.c $(srcdir)\zutil.h $(srcdir)\zlib.h $(srcdir)\zconf.h $(srcdir)\inftrees.h $(srcdir)\inflate.h \
             $(srcdir)\inffast.h

inflate.obj: $(srcdir)\inflate.c $(srcdir)\zutil.h $(srcdir)\zlib.h $(srcdir)\zconf.h $(srcdir)\inftrees.h $(srcdir)\inflate.h \
             $(srcdir)\inffast.h $(srcdir)\inffixed.h

inftrees.obj: $(srcdir)\inftrees.c $(srcdir)\zutil.h $(srcdir)\zlib.h $(srcdir)\zconf.h $(srcdir)\inftrees.h

trees.obj: $(srcdir)\trees.c $(srcdir)\zutil.h $(srcdir)\zlib.h $(srcdir)\zconf.h $(srcdir)\deflate.h $(srcdir)\trees.h

uncompr.obj: $(srcdir)\uncompr.c $(srcdir)\zlib.h $(srcdir)\zconf.h

zutil.obj: $(srcdir)\zutil.c $(srcdir)\zutil.h $(srcdir)\zlib.h $(srcdir)\zconf.h

example.obj: $(srcdir)\example.c $(srcdir)\zlib.h $(srcdir)\zconf.h

minigzip.obj: $(srcdir)\minigzip.c $(srcdir)\zlib.h $(srcdir)\zconf.h

zlib1.res: win32/zlib1.rc
	$(RC) $(RCFLAGS) /fo$@ win32/zlib1.rc


# testing
test: example.exe minigzip.exe
	example
	echo hello world | minigzip | minigzip -d

testdll: example_d.exe minigzip_d.exe
	example_d
	echo hello world | minigzip_d | minigzip_d -d

install:
	if not exist $(EXEC_PREFIX)\lib mkdir $(EXEC_PREFIX)\lib
	copy *.lib $(EXEC_PREFIX)\lib
	if not exist $(EXEC_PREFIX)\include mkdir $(EXEC_PREFIX)\include
	for %f in ($(include_HEADERS)) do copy %f $(EXEC_PREFIX)\include


# cleanup
clean:
	-del $(STATICLIB)
	-del $(SHAREDLIB)
	-del $(IMPLIB)
	-del *.obj
	-del *.res
	-del *.exp
	-del *.exe
	-del foo.gz
