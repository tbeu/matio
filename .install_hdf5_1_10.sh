#! /bin/sh 

git clone --depth=1 --branch hdf5_1_10_1 https://git.hdfgroup.org/scm/hdffv/hdf5.git hdf5_1_10_1
cd hdf5_1_10_1
# To get the same shared library name as with autoconf
sed -i -e's/-shared//' CMakeLists.txt
mkdir build
cd build
cmake -DHDF5_ENABLE_Z_LIB_SUPPORT=ON -DCMAKE_INSTALL_PREFIX=$TRAVIS_BUILD_DIR/hdf5 -DZLIB_DIR=$TRAVIS_BUILD_DIR/zlib -DBUILD_SHARED_LIBS=ON -DHDF5_BUILD_CPP_LIB=OFF -DHDF5_BUILD_EXAMPLES=OFF -DBUILD_TESTING:BOOL=OFF -DHDF5_BUILD_HL_LIB=OFF -DHDF5_BUILD_TOOLS=OFF -DHDF5_ENABLE_DEPRECATED_SYMBOLS=OFF ..
#./configure --quiet --enable-shared --enable-build-mode=debug --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-memory-alloc-sanity-check --disable-instrument --disable-parallel --disable-trace --disable-internal-debug --enable-optimization=debug --disable-asserts --with-pic --with-default-api-version=v110 --with-zlib=$TRAVIS_BUILD_DIR/zlib CFLAGS="-w"
make install
