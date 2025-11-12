#!/bin/bash -eu
# Copyright 2019 Google Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
################################################################################

# build zlib
pushd "$SRC/zlib"
./configure --static --prefix="$WORK"
make -j$(nproc) CFLAGS="$CFLAGS -fPIC"
make install
popd

wget http://ftp.gnu.org/gnu/autoconf/autoconf-2.71.tar.gz
tar xvfvz autoconf-2.71.tar.gz
pushd autoconf-2.71
./configure
make
make install
popd

#build hdf5
pushd "$SRC"
cd hdf5
cmake -S . -B build -DBUILD_SHARED_LIBS=OFF -DHDF5_BUILD_EXAMPLES=OFF -DHDF5_BUILD_UTILS=OFF -DBUILD_LZ4_LIBRARY_SOURCE=OFF -DHDF5_BUILD_HL_LIB=OFF -DHDF5_BUILD_TOOLS=OFF -DHDF5_ENABLE_DEPRECATED_SYMBOLS=OFF -DHDF5_ENABLE_PREADWRITE=OFF -DHDF5_ENABLE_NONSTANDARD_FEATURES=OFF -DBUILD_TESTING=OFF -DHDF5_ENABLE_ALL_WARNINGS=OFF -DHDF5_ENABLE_ZLIB_SUPPORT=ON -DCMAKE_INSTALL_PREFIX="$WORK"
cmake --build build -- -j$(nproc)
cmake --build build --target install
popd

# build matio
./autogen.sh
./configure --prefix="$WORK" --disable-shared --with-hdf5="$WORK" --with-zlib="$WORK"
make -j$(nproc)
make install

MATIO_INCLUDE="$WORK/include"
MATIO_LIBS_NO_FUZZ="$WORK/lib/libmatio.a $WORK/lib/libhdf5.a $WORK/lib/libz.a"
MATIO_LIBS="$LIB_FUZZING_ENGINE $MATIO_LIBS_NO_FUZZ"

# build fuzzers
cd ./ossfuzz
for fuzzers in $(find . -name '*_fuzzer.cpp'); do
  base=$(basename -s .cpp $fuzzers)
  $CXX $CXXFLAGS -std=c++11 -I$MATIO_INCLUDE $fuzzers -o $OUT/$base $MATIO_LIBS
  zip -q -r ${base}_seed_corpus.zip ../share
done

find . -name '*_fuzzer.dict' -exec cp -v '{}' $OUT ';'
find . -name '*_fuzzer_seed_corpus.zip' -exec cp -v '{}' $OUT ';'
