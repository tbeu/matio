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

# build szip
tar -xvf szip.tar.gz
cd szip-2.1.1
./configure --disable-shared CFLAGS="-w"
make -C src
cd ..

# build project
./autogen.sh
./configure --with-hdf5=$HDF5_DIR
make -j$(nproc)
make install

# build fuzzers
cd ./ossfuzz
for fuzzers in $(find . -name '*_fuzzer.cpp'); do
  base=$(basename -s .cpp $fuzzers)
  $CXX $CXXFLAGS -std=c++11 -I../include \
    $fuzzers ../getopt/.libs/libgetopt.a \
    ../src/.libs/libmatio.a -o $OUT/$base $LIB_FUZZING_ENGINE $HDF5_DIR/libhdf5.a ../szip-2.1.1/src/.libs/libsz.a -lz
  zip -q -r ${base}_seed_corpus.zip ../share
done

find . -name '*_fuzzer.dict' -exec cp -v '{}' $OUT ';'
find . -name '*_fuzzer_seed_corpus.zip' -exec cp -v '{}' $OUT ';'
