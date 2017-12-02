#! /bin/sh 

git clone --depth 1 --branch hdf5_1_8_19 https://git.hdfgroup.org/scm/hdffv/hdf5.git hdf5_1_8_19
cd hdf5_1_8_19
# To get the same shared library name as with autoconf
sed -i -e's/-shared//' CMakeLists.txt
mkdir build
cd build
cmake -DHDF5_ENABLE_Z_LIB_SUPPORT=ON -DCMAKE_INSTALL_PREFIX=$TRAVIS_BUILD_DIR/hdf5 -DZLIB_DIR=$TRAVIS_BUILD_DIR/zlib -DBUILD_SHARED_LIBS=ON -DHDF5_BUILD_CPP_LIB=OFF -DHDF5_BUILD_EXAMPLES=OFF -DBUILD_TESTING:BOOL=OFF -DHDF5_BUILD_HL_LIB=OFF -HDF5_LIBSH_TARGET=hdf5 -DHDF5_BUILD_TOOLS=OFF -DHDF5_ENABLE_DEPRECATED_SYMBOLS=OFF ..
#./configure --quiet --enable-shared --disable-production --enable-debug=all --with-pic --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-clear-file-buffers --disable-instrument --disable-parallel --disable-trace --with-default-api-version=v18 CFLAGS="-w"
make install
