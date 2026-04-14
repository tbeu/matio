# Options

# Option to utilize Conan 1.x
option(MATIO_USE_CONAN "Use Conan 1.x to resolve library dependencies" OFF)

# Option to enable extended sparse matrix data types not supported in MATLAB
option(MATIO_EXTENDED_SPARSE "Enable extended sparse matrix data types not supported in MATLAB" ON)
set(EXTENDED_SPARSE ${MATIO_EXTENDED_SPARSE})

# Option to enable MCOS (MATLAB Class Object System) support
option(MATIO_MCOS "Enable MATLAB Class Object System (MCOS) support" ON)
set(MCOS ${MATIO_MCOS})

# Option to enable MAT v7.3 file support
option(MATIO_MAT73 "Enable support for version 7.3 MAT files" ON)
set(MAT73 ${MATIO_MAT73})

# Option to build static or shared
option(MATIO_SHARED "Build shared matio library, disable for static library" ON)

# Option to enable position-independent code (PIC)
option(MATIO_PIC "Enable position-independent code (PIC), i.e., compilation with the -fPIC flag" ON)

# Build with hdf5 support
option(MATIO_WITH_HDF5 "Check for hdf5 library" ON)

# Build with zlib support
option(MATIO_WITH_ZLIB "Check for zlib library" ON)

# Select what MAT file format version is used by default
set(MATIO_DEFAULT_FILE_VERSION "5" CACHE STRING "Default MAT file version")
set_property(CACHE MATIO_DEFAULT_FILE_VERSION PROPERTY STRINGS 4 5 7.3)

if(MATIO_DEFAULT_FILE_VERSION STREQUAL "4")
    set(MAT_FT_DEFAULT MAT_FT_MAT4)
elseif(MATIO_DEFAULT_FILE_VERSION STREQUAL "5")
    set(MAT_FT_DEFAULT MAT_FT_MAT5)
elseif(MATIO_DEFAULT_FILE_VERSION STREQUAL "7.3")
    set(MAT_FT_DEFAULT MAT_FT_MAT73)
else()
    message(ERROR "Unrecognized MAT file version")
endif()

if(${CMAKE_VERSION} VERSION_GREATER_EQUAL "3.10")
    # Option to enable static analysis with Cppcheck
    option(MATIO_ENABLE_CPPCHECK "Enable static analysis with Cppcheck." OFF)
endif()

# Option to enable matio testsuite
option(MATIO_BUILD_TESTING "Build matio testing" ON)

# Option to enable MATLAB verification of written MAT files
option(MATIO_MATLAB "Enable MATLAB verification of written MAT files" OFF)
set(MATIO_MATLAB_EXE "" CACHE FILEPATH "Path to local MATLAB executable")
set(MATIO_MATLAB_SSH_KEY "" CACHE FILEPATH "SSH private key for remote MATLAB host")
set(MATIO_MATLAB_SSH_HOST "" CACHE STRING "SSH user@host for remote MATLAB (e.g., user@host)")
set(MATIO_MATLAB_SSH_DIR "/tmp" CACHE STRING "Remote working directory for MATLAB test files")

set(BUILD_TESTING OFF)
if(MATIO_BUILD_TESTING)
    set(BUILD_TESTING ON)
endif()
