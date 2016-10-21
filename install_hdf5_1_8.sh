#! /bin/sh 

svn co -q https://svn.hdfgroup.org/hdf5/branches/hdf5_1_8_17@29907
cd hdf5_1_8_17
./configure --quiet --enable-shared --enable-production=no --enable-debug=all --with-pic --disable-deprecated-symbols --disable-hl --with-default-api-version=v18 CFLAGS="-w"
make install -C src
