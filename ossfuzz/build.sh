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

#build hdf5
pushd "$SRC"
tar -xvf hdf5-1.12.1.tar.gz
cd hdf5-1.12.1
./configure --disable-shared --disable-deprecated-symbols --disable-hl --disable-parallel --disable-trace --disable-internal-debug --disable-asserts --disable-tests --disable-tools --with-pic --with-zlib="$WORK" --prefix="$WORK"
make -j$(nproc)
make install
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
