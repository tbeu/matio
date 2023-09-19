#!/bin/bash

set -ex

if [[ "$COVERITY_SCAN_BRANCH" != 1 ]] && [[ "$TRAVIS_OS_NAME" == "linux" ]] && [[ "$TRAVIS_CPU_ARCH" == "amd64" ]] && [[ "$CC" == "gcc-4.8" ]]; then
    pip install --upgrade pip
    pip install --user "urllib3>=1.26.16,<2" cpp-coveralls
fi

if [[ "${USE_CONAN:-no}" == "yes" ]]; then
    pip3 install --user "conan>=1.60.2,<2"
fi

if [[ "$TRAVIS_OS_NAME" == "osx" ]] && [[ "$HOMEBREW_DEPLOY" == "yes" ]]; then
    brew update
    brew upgrade zlib
    brew upgrade libtool
    brew install automake
    brew upgrade hdf5
fi
