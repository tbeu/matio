#!/bin/bash

set -x #echo on

./autogen.sh
if [[ "$HDF5_VERSION_DIR" == "foo" ]]; then
    ./configure --quiet --enable-shared --enable-debug --enable-mat73=$ENABLE_MAT73 --enable-extended-sparse=$ENABLE_EXTENDED_SPARSE --with-zlib=$WITH_ZLIB --with-pic; CC=foo
fi
if [[ "$CC" == "gcc-4.8" ]]; then
    ./configure --quiet --enable-shared --enable-coverage --enable-debug --enable-mat73=$ENABLE_MAT73 --enable-extended-sparse=$ENABLE_EXTENDED_SPARSE --with-zlib=$WITH_ZLIB --with-pic --with-hdf5=$TRAVIS_BUILD_DIR/$HDF5_VERSION_DIR/hdf5
fi
if [[ "$CC" == "clang" ]]; then
    ./configure --quiet --enable-shared --enable-debug --enable-mat73=$ENABLE_MAT73 --enable-extended-sparse=$ENABLE_EXTENDED_SPARSE --with-zlib=$WITH_ZLIB --with-pic --with-hdf5=$TRAVIS_BUILD_DIR/$HDF5_VERSION_DIR/hdf5
fi
if [[ "$HDF5_VERSION_DIR" == "foo" ]]; then
    CC=$(which clang)
fi
