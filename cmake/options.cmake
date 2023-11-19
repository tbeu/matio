# Options

# Option to utilize Conan
option(MATIO_USE_CONAN "Use Conan to resolve library dependencies" OFF)

# Option to enable extended sparse matrix data types not supported in MATLAB
option(MATIO_EXTENDED_SPARSE "Enable extended sparse matrix data types not supported in MATLAB" ON)
set(EXTENDED_SPARSE ${MATIO_EXTENDED_SPARSE})

# Option to enable MAT v7.3 file support
option(MATIO_MAT73 "Enable support for version 7.3 MAT files" ON)
set(MAT73 ${MATIO_MAT73})

# Option to build static or shared
option(MATIO_SHARED "Build shared matio library, disable for static library" ON)

# Option to enable position-independent code (PIC)
option(MATIO_PIC "Enable position-independent code (PIC), i.e., compilation with the -fPIC flag" ON)

# Option to link the MSVC runtime library statically
option(MATIO_STATIC_LINK_CRT "Link the MSVC runtime library statically" OFF)

if(POLICY CMP0091)
    # CMake >= 3.15 has CMAKE_MSVC_RUNTIME_LIBRARY to set the MSVCC runtime library
    if(WIN32 AND CMAKE_GENERATOR MATCHES "Visual Studio .*|NMake .*")
        if(MATIO_STATIC_LINK_CRT)
            message(STATUS "Configuring to link the MSVC runtime library statically")
            set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
        else()
            message(STATUS "Configuring to link the MSVC runtime library dynamically")
            set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>DLL")
        endif()
    endif()
endif()

# Build with hdf5 support
option(MATIO_WITH_HDF5 "Check for HDF5 library" ON)

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
