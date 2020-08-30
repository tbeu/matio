if(MATIO_WITH_HDF5)
    find_package(HDF5 1.8)
    if(HDF5_FOUND)
        set(HAVE_HDF5 1)
    endif()
endif()

if(MATIO_WITH_ZLIB)
    find_package(ZLIB 1.2.3)
    if(ZLIB_FOUND)
        set(HAVE_ZLIB 1)
    endif()
endif()

if(HDF5_FOUND)
    add_library(HDF5::HDF5 INTERFACE IMPORTED)
    set_target_properties(HDF5::HDF5 PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${HDF5_INCLUDE_DIRS}"
        INTERFACE_LINK_LIBRARIES "${HDF5_LIBRARIES}"
    )
endif()

if(NOT HAVE_HDF5 AND MATIO_MAT73)
    message(FATAL_ERROR "MAT73 requires HDF5")
endif()
