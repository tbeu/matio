#! /bin/sh 

svn co -q https://svn.hdfgroup.org/hdf5/branches/hdf5_1_10_0@29954
cd hdf5_1_10_0
./configure --quiet --enable-shared --enable-build-mode=debug --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-memory-alloc-sanity-check --disable-instrument --disable-parallel --disable-trace --disable-internal-debug --enable-optimization=debug --disable-asserts --with-pic --with-default-api-version=v110 CFLAGS="-w"
make install -C src
