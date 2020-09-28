#!/bin/bash

set -x #echo on

if [[ "${USE_CMAKE:-no}" == "no" ]]; then
    ./autogen.sh
    if [[ "$HDF5_VERSION" == "foo" ]]; then
        ./configure --quiet --enable-shared --enable-debug --enable-mat73=$ENABLE_MAT73 --enable-extended-sparse=$ENABLE_EXTENDED_SPARSE --with-zlib=$WITH_ZLIB --with-pic; CC=foo
    fi
    if [[ "$CC" == "gcc-4.8" ]]; then
        ./configure --quiet --enable-shared --enable-coverage --enable-debug --enable-mat73=$ENABLE_MAT73 --enable-extended-sparse=$ENABLE_EXTENDED_SPARSE --with-zlib=$WITH_ZLIB --with-pic --with-hdf5=$TRAVIS_BUILD_DIR/hdf5-$HDF5_VERSION/hdf5
    fi
    if [[ "$CC" == "clang" ]]; then
        ./configure --quiet --enable-shared --enable-debug --enable-mat73=$ENABLE_MAT73 --enable-extended-sparse=$ENABLE_EXTENDED_SPARSE --with-zlib=$WITH_ZLIB --with-pic --with-hdf5=$TRAVIS_BUILD_DIR/hdf5-$HDF5_VERSION/hdf5
    fi
    if [[ "$HDF5_VERSION" == "foo" ]]; then
        CC=$(which clang)
    fi
fi
if [[ "${USE_CMAKE:-no}" == "yes" ]] &&  [[ "${USE_CONAN:-no}" == "no" ]]; then
    SRC_DIR=$TRAVIS_BUILD_DIR
    BUILD_DIR=$HOME/matio_cmake
    HDF5_DIR=$HOME/CMake-hdf5-$HDF5_VERSION/HDF5-$HDF5_VERSION-Linux/HDF_Group/HDF5/$HDF5_VERSION
    mkdir -p $BUILD_DIR
    pushd $BUILD_DIR
    cmake $SRC_DIR -DCMAKE_BUILD_TYPE=Debug \
                   -DCMAKE_PREFIX_PATH=$HDF5_DIR/share/cmake \
                   -DMATIO_EXTENDED_SPARSE=$ENABLE_EXTENDED_SPARSE \
                   -DMATIO_MAT73=$ENABLE_MAT73 \
                   -DMATIO_WITH_HDF5=$ENABLE_MAT73 \
                   -DMATIO_WITH_ZLIB=$WITH_ZLIB \
                   -DMATIO_SHARED=TRUE
    popd
fi
if [[ "${USE_CMAKE:-no}" == "yes" ]] &&  [[ "${USE_CONAN:-no}" == "yes" ]]; then
    SRC_DIR=$TRAVIS_BUILD_DIR
    BUILD_DIR=$HOME/matio_cmake
    mkdir -p $BUILD_DIR
    pushd $BUILD_DIR
    cmake $SRC_DIR -DCMAKE_BUILD_TYPE=Debug \
                   -DMATIO_EXTENDED_SPARSE=$ENABLE_EXTENDED_SPARSE \
                   -DMATIO_MAT73=$ENABLE_MAT73 \
                   -DMATIO_WITH_HDF5=$ENABLE_MAT73 \
                   -DMATIO_WITH_ZLIB=$WITH_ZLIB \
                   -DMATIO_SHARED=TRUE \
                   -DMATIO_USE_CONAN=TRUE
    popd
fi
