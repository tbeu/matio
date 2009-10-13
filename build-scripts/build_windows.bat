set CC=cl
set FC=ifort
set LD=LIB
set prefix=c:\cygwin\home\chulbert\matio_v1.1.4
set exec_prefix=c:\cygwin\home\chulbert\matio_v1.1.4

REM Set to 1 to build the fortran interface, otherwise set to 0
set build_fortran=1
REM Set build_zlib to 1 to build the zlib with matio, or set to 0 and uncomment
REM the ZLIB_CFLAGS and ZLIB_LIBS if zlib has already been built
set build_zlib=1
REM set ZLIB_CFLAGS=-Ic:\Matlab71\extern\include
REM set ZLIB_LIBS=-LIBPATH:"c:\Matlab71\extern\lib\win32\microsoft\msvc71" libmat.lib libmx.lib libmex.lib

cd build-windows
nmake
nmake install
cd ..
