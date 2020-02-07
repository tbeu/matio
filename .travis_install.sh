#!/bin/bash

if [[ "$HDF5_VERSION_DIR" == "hdf5_1_12_0" ]]; then
    git clone --depth 1 https://github.com/madler/zlib
    pushd zlib && ./configure --prefix="$TRAVIS_BUILD_DIR"/zlib --eprefix="$TRAVIS_BUILD_DIR"/zlib && make install && popd
    git clone --depth 1 --branch hdf5-1_12_0 https://git.hdfgroup.org/scm/hdffv/hdf5.git ${HDF5_VERSION_DIR}
    pushd ${HDF5_VERSION_DIR} && ./configure --quiet --enable-shared --enable-build-mode=debug --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-memory-alloc-sanity-check --disable-instrument --disable-parallel --disable-trace --disable-internal-debug --enable-optimization=debug --disable-asserts --disable-tests --disable-tools --with-pic --with-default-api-version=v112 --with-zlib="$TRAVIS_BUILD_DIR"/zlib CFLAGS="-w" && make install -C src && popd
fi

if [[ "$HDF5_VERSION_DIR" == "hdf5_1_10_6" ]]; then
    git clone --depth 1 https://github.com/madler/zlib
    pushd zlib && ./configure --prefix="$TRAVIS_BUILD_DIR"/zlib --eprefix="$TRAVIS_BUILD_DIR"/zlib && make install && popd
    git clone --depth 1 --branch hdf5-1_10_6 https://git.hdfgroup.org/scm/hdffv/hdf5.git ${HDF5_VERSION_DIR}
    pushd ${HDF5_VERSION_DIR} && ./configure --quiet --enable-shared --enable-build-mode=debug --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-memory-alloc-sanity-check --disable-instrument --disable-parallel --disable-trace --disable-internal-debug --enable-optimization=debug --disable-asserts --with-pic --with-default-api-version=v110 --with-zlib="$TRAVIS_BUILD_DIR"/zlib CFLAGS="-w" && make install -C src && popd
fi

if [[ "$HDF5_VERSION_DIR" == "hdf5_1_8_21" ]]; then
    git clone --depth 1 --branch hdf5-1_8_21 https://git.hdfgroup.org/scm/hdffv/hdf5.git ${HDF5_VERSION_DIR}
    pushd ${HDF5_VERSION_DIR} && ./configure --quiet --enable-shared --disable-production --enable-debug=all --with-pic --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-clear-file-buffers --disable-instrument --disable-parallel --disable-trace --with-default-api-version=v18 CFLAGS="-w" && make install -C src && popd
fi
