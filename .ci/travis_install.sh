#!/bin/bash

set -ex

if [[ "$ENABLE_MAT73" == "yes" ]]; then
    if [[ "${USE_CMAKE:-no}" == "no" ]] && [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
        git clone --branch v1.3.1 --depth 1 https://github.com/madler/zlib
        pushd zlib
        ./configure --prefix="$TRAVIS_BUILD_DIR"/zlib --eprefix="$TRAVIS_BUILD_DIR"/zlib
        make install -j8
        popd

        if [[ "$HDF5_VERSION" == "1.14" ]]; then
            HDF5_URL="https://github.com/HDFGroup/hdf5/releases/download/hdf5_${HDF5_VERSION}.${HDF5_PATCH_VERSION}/hdf5-${HDF5_VERSION}.${HDF5_PATCH_VERSION}.tar.gz"
        else
            HDF5_URL="https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-${HDF5_VERSION}/hdf5-${HDF5_VERSION}.${HDF5_PATCH_VERSION%-*}/src/hdf5-${HDF5_VERSION}.${HDF5_PATCH_VERSION}.tar.gz"
        fi

        wget --no-check-certificate -nv $HDF5_URL
        tar -xzf hdf5-*.tar.gz
        pushd hdf5-*

        if [[ "$HDF5_VERSION" == "1.14" ]]; then
            ./configure --quiet --enable-shared --enable-build-mode=debug --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-instrument --disable-parallel --disable-trace --disable-internal-debug --enable-optimization=debug --disable-asserts --disable-tests --disable-tools --with-pic --with-default-api-version=v114 --disable-dependency-tracking --with-zlib="$TRAVIS_BUILD_DIR"/zlib CFLAGS="-w"
        elif [[ "$HDF5_VERSION" == "1.12" ]]; then
            ./configure --quiet --enable-shared --enable-build-mode=debug --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-memory-alloc-sanity-check --disable-instrument --disable-parallel --disable-trace --disable-internal-debug --enable-optimization=debug --disable-asserts --disable-tests --disable-tools --with-pic --with-default-api-version=v112 --with-zlib="$TRAVIS_BUILD_DIR"/zlib CFLAGS="-w"
        elif [[ "$HDF5_VERSION" == "1.10" ]]; then
            ./configure --quiet --enable-shared --enable-build-mode=debug --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-memory-alloc-sanity-check --disable-instrument --disable-parallel --disable-trace --disable-internal-debug --enable-optimization=debug --disable-asserts --disable-tools --with-pic --with-default-api-version=v110 --with-zlib="$TRAVIS_BUILD_DIR"/zlib CFLAGS="-w"
        elif [[ "$HDF5_VERSION" == "1.8" ]]; then
            ./configure --quiet --enable-shared --disable-production --enable-debug=all --with-pic --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-clear-file-buffers --disable-instrument --disable-parallel --disable-trace --with-default-api-version=v18 --with-zlib="$TRAVIS_BUILD_DIR"/zlib CFLAGS="-w"
        fi

        make install -C src -j8
        popd
    fi
    if [[ "${USE_CMAKE:-no}" == "yes" ]] && [[ "${USE_CONAN:-no}" == "no" ]] && [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
        pushd $HOME
        HDF5_URL="https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-${HDF5_VERSION}/hdf5-${HDF5_VERSION}.${HDF5_PATCH_VERSION%-*}/src/CMake-hdf5-${HDF5_VERSION}.${HDF5_PATCH_VERSION}.tar.gz"
        wget --no-check-certificate -nv $HDF5_URL
        tar -xzf CMake-hdf5-$HDF5_VERSION.$HDF5_PATCH_VERSION.tar.gz
        cd CMake-hdf5-$HDF5_VERSION.$HDF5_PATCH_VERSION
        ctest -S HDF5config.cmake,BUILD_GENERATOR=Unix,LOCAL_SKIP_TEST=true -C Debug -j8
        tar -xzf HDF5-$HDF5_VERSION.$HDF5_PATCH_VERSION-Linux.tar.gz
        popd
    fi
fi

pushd $TRAVIS_BUILD_DIR
mkdir MSL
cd MSL
git init
git config core.sparseCheckout true
git remote add origin https://github.com/modelica/ModelicaStandardLibrary
echo "Modelica/Resources/Data/Tables/" >> .git/info/sparse-checkout
git fetch --depth 1 origin master
git checkout master
popd

pushd $TRAVIS_BUILD_DIR
mkdir matfilerw
cd matfilerw
git init
git config core.sparseCheckout true
git remote add origin https://github.com/diffplug/matfilerw
echo "src/test/resources/" >> .git/info/sparse-checkout
git fetch --depth 1 origin master
git checkout master
rm ./src/test/resources/simulink_tet_out.mat
rm ./src/test/resources/object.mat
rm ./src/test/resources/SPM.mat
popd
