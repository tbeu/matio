REM Set to 1 to build the fortran interface, otherwise set to 0
set build_fortran=1
REM Set build_zlib to 1 to build the zlib with matio, or set to 0 and uncomment
REM the ZLIB_CFLAGS and ZLIB_LIBS if zlib has already been built
set build_zlib=1
REM set ZLIB_CFLAGS=-Ic:\Matlab71\extern\include
REM set ZLIB_LIBS=-LIBPATH:"c:\Matlab71\extern\lib\win32\microsoft\msvc71" libmat.lib libmx.lib libmex.lib

cd build-windows
nmake clean
cd ..
