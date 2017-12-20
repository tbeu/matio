#! /bin/sh 

git clone --depth 1 --branch hdf5_1_8_20 https://git.hdfgroup.org/scm/hdffv/hdf5.git hdf5_1_8_20
cd hdf5_1_8_20
./configure --quiet --enable-shared --disable-production --enable-debug=all --with-pic --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-clear-file-buffers --disable-instrument --disable-parallel --disable-trace --with-default-api-version=v18 CFLAGS="-w"
make install -C src
