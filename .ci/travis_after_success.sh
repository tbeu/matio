#!/bin/bash

set -ex

if [[ "$COVERITY_SCAN_BRANCH" == 1 ]] || [[ "$TRAVIS_OS_NAME" != "linux" ]] || [[ "$TRAVIS_CPU_ARCH" != "amd64" ]] || [[ "$CC" != "gcc-4.8" ]] || [[ "${USE_CMAKE:-no}" == "yes" ]]; then
    exit 0
fi

coveralls -e coverity_model.c -e getopt -e matfilerw -e MSL -e test/datasets/matio_test_cases.m -e test/matlab -e test/results -e ossfuzz -e patches -e share -e hdf5-$HDF5_VERSION.$HDF5_PATCH_VERSION -e visual_studio -e zlib --gcov /usr/bin/gcov-4.8 --gcov-options '\-lp'

if [[ "$HDF5_VERSION" != "1.8" ]] || [[ "$ENABLE_MAT73" != "yes" ]] || [[ "$ENABLE_EXTENDED_SPARSE" != "yes" ]] || [[ "$WITH_ZLIB" != "yes" ]] || [[ "$MAX_RANK" != 3 ]]; then
    exit 0
fi

make -C documentation pdf html MAKEINFOFLAGS=--no-split
make dist-gzip

if [[ "$TRAVIS_PULL_REQUEST" != "false" ]] || [[ "$TRAVIS_BRANCH" != "master" ]]; then
    exit 0
fi

curl -X POST -H "Authorization: Bearer $BBTOKEN" https://api.bitbucket.org/2.0/repositories/tbeu/downloads/downloads -F files=@./documentation/matio_user_guide.html -F files=@./documentation/matio_user_guide.pdf -F files=@./matio-1.5.25.tar.gz
