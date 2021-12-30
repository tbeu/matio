# MATIO
MATLAB MAT file I/O library

## Status
[![Build Status](https://app.travis-ci.com/tbeu/matio.svg?branch=master)](https://app.travis-ci.com/tbeu/matio) [![Coverity Scan Build Status](https://scan.coverity.com/projects/7575/badge.svg)](https://scan.coverity.com/projects/tbeu-matio) [![Coverage Status](https://coveralls.io/repos/github/tbeu/matio/badge.svg?branch=master)](https://coveralls.io/github/tbeu/matio?branch=master) [![Build Status](https://ci.appveyor.com/api/projects/status/heqkwuqllbh573i5/branch/master?svg=true)](https://ci.appveyor.com/project/tbeu/matio/branch/master) [![FreeBSD Build Status](https://img.shields.io/cirrus/github/tbeu/matio?label=freebsd&logo=cirrus%20ci)](https://cirrus-ci.com/github/tbeu/matio) [![BCH Compliance](https://bettercodehub.com/edge/badge/tbeu/matio?branch=master)](https://bettercodehub.com/) [![Total Alerts](https://img.shields.io/lgtm/alerts/g/tbeu/matio.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/tbeu/matio/alerts/) [![Packaging status](https://repology.org/badge/tiny-repos/matio.svg)](https://repology.org/metapackage/matio/versions) [![Fuzzing Status](https://oss-fuzz-build-logs.storage.googleapis.com/badges/matio.svg)](https://bugs.chromium.org/p/oss-fuzz/issues/list?sort=-opened&can=1&q=proj:matio) [![Open Hub](https://img.shields.io/badge/Open-Hub-blue.svg)](https://www.openhub.net/p/matio)

## Table of Contents
1. [Introduction](#10-introduction)
  * 1.1[Contact](#11-contact)
  * 1.2 [Acknowledgements](#12-acknowledgements)
  * 1.3 [Contributing](#13-contributing)
  * 1.4 [Questions and Reporting Bugs](#14-questions-and-reporting-bugs)
2. [Building](#20-building)
  * 2.1 [Dependencies](#21-dependencies)
    * 2.1.1 [zlib](#211-zlib)
    * 2.1.2 [HDF5](#212-hdf5)
  * 2.2 [Building matio](#22-building-matio)
    * 2.2.1 [Quick Build Guide](#221-quick-build-guide)
    * 2.2.2 [Configure Options](#222-configure-options)
    * 2.2.3 [CMake build system](#223-cmake-build-system)
    * 2.2.4 [Visual Studio](#224-visual-studio)
    * 2.2.5 [Testsuite](#225-testsuite)
  * 2.3 [Platforms](#23-platforms)
3. [License](#30-license)

## 1.0 Introduction
Matio is an open-source C library for reading and writing binary MATLAB MAT files.
This library is designed for use by programs/libraries that do not have access or do not want to rely on MATLAB's shared libraries.

### 1.1 Contact
You can contact the maintainer through email at t-beu@users.sourceforge.net.

### 1.2 Acknowledgements
The following people/organizations have helped in the development of matio through patches, bug reports, and/or testing:

* [Don Zimmer](https://github.com/dpzimmer)
* [Greg Sjaardema](https://github.com/gsjaardema)
* [Jacco van Beek](https://sourceforge.net/u/jabe)
* [John Filo](https://github.com/jfilo)
* [Maarten Bent](https://github.com/MaartenBent)
* [Modelica Association](https://modelica.org/association)
* [Nelson](https://nelson-numerical-software.github.io/nelson-website/)
* [Nils Jannasch](https://github.com/NJannasch)
* [OpenMEEG](http://openmeeg.github.io/)
* [Scilab](http://www.scilab.org/)
* [Sébastien Villemot](https://github.com/svillemot)
* SGI in support of Interactive Supercomputing, Inc.
* [Steven Leibman](mailto:sleibman@alum.mit.edu)

### 1.3 Contributing
If you are interested in collaborations, contact the maintainer via email (see section [1.1](#11-contact)).

### 1.4 Questions and Reporting Bugs
Questions can be asked using the [forums](https://sourceforge.net/p/matio/discussion) on the sourceforge site hosting matio.

Bugs, enhancements, etc. should be submitted using one of the [trackers](https://sourceforge.net/p/matio/_list/tickets) on the sourceforge page.

## 2.0 Building
This section describes how to build matio. Section [2.1](#21-dependencies) describes the dependencies, section [2.2](#22-building-matio) how to build/test matio, and section [2.3](#23-platforms) documents the platforms matio has been tested on.

### 2.1 Dependencies
Matio has two optional dependencies. These are not required for the software to work, but without them some files may be unreadable. Zlib is required to read/write level 5 MAT files that use compression. HDF5 is required to work with newer MAT files that use the HDF5-format files.

#### 2.1.1 zlib
To support compressed MAT files, zlib version &ge; 1.2.3 is required. The zlib software can be downloaded from http://zlib.net/.

#### 2.1.2 HDF5
Support for MAT file version 7.3 requires the HDF5 library. This library can be downloaded from https://www.hdfgroup.org/. Matio requires HDF5 version &ge; 1.8.x. Neither deprecated HDF5 1.6.x API functions nor HDF5 higher-level functions are called.

* Building matio with HDF5 1.8.x requires configuration of HDF5 with default API version 1.8 (i.e. `--with-default-api-version=v18`).
* Building matio with HDF5 1.10.x requires configuration of HDF5 with either default API version 1.10 (i.e. `--with-default-api-version=v110`) or with deprecated API version 1.8 (i.e. `--with-default-api-version=v18`).
* Building matio with HDF5 1.12.x requires configuration of HDF5 with either default API version 1.12 (i.e. `--with-default-api-version=v112`), or with deprecated API version 1.10 (i.e. `--with-default-api-version=v110`) or with deprecated API version 1.8 (i.e. `--with-default-api-version=v18`).
* Building matio with HDF5 1.13.x requires configuration of HDF5 with either default API version 1.14 (i.e. `--with-default-api-version=v114`), or with deprecated API version 1.12 (i.e. `--with-default-api-version=v112`), or with deprecated API version 1.10 (i.e. `--with-default-api-version=v110`) or with deprecated API version 1.8 (i.e. `--with-default-api-version=v18`).

For Windows, the pre-compiled binaries can be used which also include a DLL of zlib to satisfy the zlib dependency.

### 2.2 Building matio
#### 2.2.1 Quick Build Guide
The primary method for building the software is with GNU autotools using `configure` followed by `make`. After building, the testsuite can be executed to test the software using `make check`. The software can be installed using `make install`. For example,
```console
git clone git://git.code.sf.net/p/matio/matio
cd matio
git submodule update --init  # for datasets used in unit tests
./autogen.sh
./configure
make
make check
make install
```
If any of the tests in the testsuite fail, you should report the failure using the tracker (see section [1.4](#14-questions-and-reporting-bugs)). You should attach the generated testsuite.log file to the bug report.

#### 2.2.2 Configure Options
The configure script used to build the software takes a number of options. This section describes the key options.

* `--enable-mat73=yes`
This flag enables the support for version 7.3 MAT files. The option only makes sense if built with HDF5 as support for version 7.3 files. It will be disabled if HDF5 is not available.
* `--enable-extended-sparse=yes`
This option enables extended sparse matrix data types not supported in MATLAB. MATLAB only supports double-precision sparse data. With this flag, matio will read sparse data with other types (i.e. single-precision and integer types).
* `--with-matlab=DIR`
This option specifies the directory (DIR) with the 'matlab' program. With this option, the testsuite will check that the MAT files written by matio can be read into MATLAB. Without this, the test will only check that matio can read the file written and if successful the test will be skipped. If matio can not read the file, the test will fail.
* `--with-zlib=DIR`
This option specifies the prefix where zlib is installed (see section [2.1.1](#211-zlib) for information about zlib).
* `--with-hdf5=DIR`
This option specifies the prefix where the HDF5 software is installed (see section [2.1.2](#212-hdf5) for information about HDF5).
* `--with-default-file-ver=version`
This option sets the default MAT file version (4,5,7.3) that will be used when writing. The default file version is used by the Mat_Create macro and the Mat_CreateVer function when MAT_FT_DEFAULT is used for the version argument.
* `--with-libdir-suffix=suffix`
This option specifies a suffix to apply to library directories when installing and looking for dependent libraries (i.e. HDF5 and zlib). For example, some multi-arch Linux distributions install 64-bit libraries into lib64 and 32-bit libraries into lib.

#### 2.2.3 CMake build system
The CMake build system is supported as an alternative build system, which usually consists of three steps for configuration, build and installation, for example,
```console
git clone git://git.code.sf.net/p/matio/matio
cd matio
cmake .
cmake --build .
cmake --install .
```
The following matio specific options for building with CMake are available.

* `MATIO_USE_CONAN:BOOL=OFF`
This option enables the Conan package manager to resolve the library dependencies.
* `MATIO_DEFAULT_FILE_VERSION:STRING=5`
This option sets the default MAT file version (4,5,7.3) that will be used when writing.
* `MATIO_EXTENDED_SPARSE:BOOL=ON`
This option enables extended sparse matrix data types not supported in MATLAB.
* `MATIO_MAT73:BOOL=ON`
This flag enables the support for version 7.3 MAT files.
* `MATIO_PIC:BOOL=ON`
This option enables position-independent code (PIC), i.e., compilation with the `-fPIC` flag. It is ignored for Visual Studio builds.
* `MATIO_SHARED:BOOL=ON`
This option builds the matio library as shared object (i.e., a dynamic link library on Windows).
* `MATIO_WITH_HDF5:BOOL=ON`
This option enables CMake to check for availability of the HDF5 library (see section [2.1.2](#212-hdf5) for information about HDF5).
* `MATIO_WITH_ZLIB:BOOL=ON`
This option enables CMake to check for availability of the zlib library (see section [2.1.1](#211-zlib) for information about zlib).

To help CMake find the HDF5 libraries, set environment variable `HDF5_DIR` to the `cmake/hdf5` directory (containing `hdf5-config.cmake`) inside the HDF5 build or installation directory, or call cmake with `-DHDF5_DIR="dir/to/hdf5/cmake/hdf5"`. Alternatively call CMake with `-DCMAKE_PREFIX_PATH="dir/to/hdf5/cmake"`. See the [HDF5 instructions](https://support.hdfgroup.org/HDF5/release/cmakebuild.html#compile) for more information. Using `hdf5-config` is recommended over using CMake's built-in `FindHDF5`, especially for static builds. CMake 3.10 or later is recommended.

#### 2.2.4 Visual Studio
Visual Studio solutions are provided as [matio_vs2008.sln](visual_studio/matio_vs2008.sln) for VS2008 and as [matio.sln](visual_studio/matio.sln) for VS2010 (and newer). The Debug and Release configurations of both solutions are set up to build a DLL of the matio library (libmatio.dll) and the matdump tool and assume HDF5 is available in the directory specified by the HDF5_DIR environment variable. It is assumed that the **shared** libraries of HDF5 (and zlib) are available. If the **static** libraries of HDF5 (and zlib) are installed/built the macro `H5_BUILT_AS_STATIC_LIB` needs to be defined (instead of `H5_BUILT_AS_DYNAMIC_LIB`). Furthermore, the Release Lib configuration of the VS2010 solution is set up to build a static LIB of the matio library (libmatio.lib) and assumes that the **static** libraries of HDF5 (and zlib) are installed/built.

* The VS2008 solution was tested with Visual Studio 2008 and the [HDF5 1.8.12 Visual Studio pre-built Windows binaries](https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.8/hdf5-1.8.12/bin/windows/) (vs9shared) including zlib.
* The VS2010 solution was tested with Visual Studio 2010 and the [HDF5 1.8.13 Visual Studio pre-built Windows binaries](https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.8/hdf5-1.8.13/bin/windows/) (VS2010-shared) including zlib.
* The VS2010 solution was tested with Visual Studio 2010 and the [HDF5 1.8.13 Visual Studio pre-built Windows binaries](https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.8/hdf5-1.8.13/bin/windows/) (VS2010-static) including zlib.
* The updated VS2010 solution was tested with Visual Studio 2015 and the [HDF5 1.8.16 Visual Studio pre-built Windows binaries](https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.8/hdf5-1.8.16/bin/windows/extra/) (vs2015-shared) including zlib.

#### 2.2.5 Testsuite
A testsuite is available when building with the GNU autotools. To run the testsuite, first configure and build matio. After building run `make check` to run the testsuite. If matio was built without zlib, the compressed variable tests will be skipped. If built without HDF5, the tests for version 7.3 MAT files will be skipped. If the path to the MATLAB application was not specified (`--with-matlab`), the write tests will fail if matio cannot read the file and skip if matio can read the file. The write tests will pass if MATLAB is available and can also read the file.

To report matio testsuite failures, compress the testsuite.log file in the test sub-directory of the build directory. Upload the compressed log file along with a bug report (see section [1.4](#14-questions-and-reporting-bugs) for information on reporting bugs).

### 2.3 Platforms
The library has been tested/used on Linux, Windows, and OS X including both little-endian and big-endian architecture.

## 3.0 License
This software is provided under a Simplified BSD license. See the [COPYING](COPYING) file for details on the license.

<sub>MATLAB is a registered trademark of The MathWorks, Inc.</sub>
