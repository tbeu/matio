#! /bin/sh 

git clone --branch hdf5_1_10_1 https://git.hdfgroup.org/scm/hdffv/hdf5.git hdf5_1_10_1
cd hdf5_1_10_1
./configure --quiet --enable-shared --enable-build-mode=debug --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-memory-alloc-sanity-check --disable-instrument --disable-parallel --disable-trace --disable-internal-debug --enable-optimization=debug --disable-asserts --with-pic --with-default-api-version=v110 --with-zlib=$TRAVIS_BUILD_DIR/zlib CFLAGS="-w"
make install -C src
