#!/bin/bash

set -x #echo on

if [[ "${USE_CMAKE:-no}" == "no" ]]; then
    if [[ "$ENABLE_MAT73" == "yes" ]]; then
        if [[ "$HDF5_VERSION_DIR" == "hdf5_1_12_0" ]]; then
            git clone --depth 1 https://github.com/madler/zlib
            pushd zlib
            ./configure --prefix="$TRAVIS_BUILD_DIR"/zlib --eprefix="$TRAVIS_BUILD_DIR"/zlib
            make install
            popd
            
            git clone --depth 1 --branch hdf5-1_12_0 https://git.hdfgroup.org/scm/hdffv/hdf5.git $HDF5_VERSION_DIR
            pushd $HDF5_VERSION_DIR
            ./configure --quiet --enable-shared --enable-build-mode=debug --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-memory-alloc-sanity-check --disable-instrument --disable-parallel --disable-trace --disable-internal-debug --enable-optimization=debug --disable-asserts --disable-tests --disable-tools --with-pic --with-default-api-version=v112 --with-zlib="$TRAVIS_BUILD_DIR"/zlib CFLAGS="-w"
            make install -C src
            popd
        fi

        if [[ "$HDF5_VERSION_DIR" == "hdf5_1_10_6" ]]; then
            git clone --depth 1 https://github.com/madler/zlib
            pushd zlib
            ./configure --prefix="$TRAVIS_BUILD_DIR"/zlib --eprefix="$TRAVIS_BUILD_DIR"/zlib
            make install
            popd
            
            git clone --depth 1 --branch hdf5-1_10_6 https://git.hdfgroup.org/scm/hdffv/hdf5.git $HDF5_VERSION_DIR
            pushd $HDF5_VERSION_DIR
            ./configure --quiet --enable-shared --enable-build-mode=debug --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-memory-alloc-sanity-check --disable-instrument --disable-parallel --disable-trace --disable-internal-debug --enable-optimization=debug --disable-asserts --with-pic --with-default-api-version=v110 --with-zlib="$TRAVIS_BUILD_DIR"/zlib CFLAGS="-w"
            make install -C src
            popd
        fi

        if [[ "$HDF5_VERSION_DIR" == "hdf5_1_8_21" ]]; then
            git clone --depth 1 --branch hdf5-1_8_21 https://git.hdfgroup.org/scm/hdffv/hdf5.git $HDF5_VERSION_DIR
            pushd $HDF5_VERSION_DIR
            ./configure --quiet --enable-shared --disable-production --enable-debug=all --with-pic --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-clear-file-buffers --disable-instrument --disable-parallel --disable-trace --with-default-api-version=v18 CFLAGS="-w"
            make install -C src
            popd
        fi
    fi
else
    pushd $HOME
    HDF5_URL="https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-${HDF5_VERSION%.[0-9]}/hdf5-${HDF5_VERSION}/src/CMake-hdf5-${HDF5_VERSION}.tar.gz"
    wget --no-check-certificate -nv $HDF5_URL
    tar -xzf CMake-hdf5-$HDF5_VERSION.tar.gz
    cd CMake-hdf5-$HDF5_VERSION
    ctest -S HDF5config.cmake,BUILD_GENERATOR=Unix,LOCAL_SKIP_TEST=true -C Release
    tar -xzf HDF5-$HDF5_VERSION-Linux.tar.gz
    popd
fi

pushd $TRAVIS_BUILD_DIR
mkdir MSL
cd MSL
git init
git remote add -f origin https://github.com/modelica/ModelicaStandardLibrary
git config core.sparseCheckout true
echo "Modelica/Resources/Data/Tables/" >> .git/info/sparse-checkout
git pull origin master
popd

pushd $TRAVIS_BUILD_DIR
mkdir matfilerw
cd matfilerw
git init
git remote add -f origin https://github.com/diffplug/matfilerw
git config core.sparseCheckout true
echo "src/test/resources/" >> .git/info/sparse-checkout
git pull origin master
rm ./src/test/resources/simulink_tet_out.mat
rm ./src/test/resources/object.mat
rm ./src/test/resources/SPM.mat
popd
