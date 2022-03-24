#!/bin/bash

set -x #echo on

if [[ "$COVERITY_SCAN_BRANCH" == 1 ]] || [[ "$TRAVIS_OS_NAME" != "linux" ]] || [[ "$TRAVIS_CPU_ARCH" != "amd64" ]] || [[ "$CC" != "gcc-4.8" ]] || [[ "${USE_CMAKE:-no}" == "yes" ]]; then
    exit 0
fi

coveralls -e coverity_model.c -e getopt -e matfilerw -e MSL -e test/datasets/matio_test_cases.m -e test/matlab -e test/results -e ossfuzz -e patches -e share -e hdf5-$HDF5_VERSION -e visual_studio -e zlib --gcov /usr/bin/gcov-4.8 --gcov-options '\-lp'

if [[ "$HDF5_VERSION" != "1.8.22" ]] || [[ "$ENABLE_MAT73" != "yes" ]] || [[ "$ENABLE_EXTENDED_SPARSE" != "yes" ]] || [[ "$WITH_ZLIB" != "yes" ]] || [[ "$MAX_RANK" != 3 ]]; then
    exit 0
fi

make -C documentation pdf html MAKEINFOFLAGS=--no-split
make dist-gzip

if [[ "$TRAVIS_PULL_REQUEST" != "false" ]] || [[ "$TRAVIS_BRANCH" != "master" ]]; then
    exit 0
fi

sh ./upload-to-bitbucket.sh tbeu $BBPASS /tbeu/downloads/downloads ./documentation/matio_user_guide.html
sh ./upload-to-bitbucket.sh tbeu $BBPASS /tbeu/downloads/downloads ./documentation/matio_user_guide.pdf
sh ./upload-to-bitbucket.sh tbeu $BBPASS /tbeu/downloads/downloads ./matio-1.5.22.tar.gz
