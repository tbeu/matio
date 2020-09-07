if(CMAKE_VERSION VERSION_LESS 3.11)
    list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake/modules")
endif()

if(MATIO_WITH_HDF5)
    if(NOT DEFINED HDF5_USE_STATIC_LIBRARIES)
        if(MATIO_SHARED)
            set(HDF5_USE_STATIC_LIBRARIES FALSE)
        else()
            set(HDF5_USE_STATIC_LIBRARIES TRUE)
        endif()
    endif()

    find_package(HDF5)
    if(HDF5_FOUND)
        set(HDF_MIN_VER 1.8)
        if(HDF5_VERSION VERSION_LESS ${HDF_MIN_VER})
            message(FATAL_ERROR "Could NOT find HDF5: Found unsuitable version ${HDF5_VERSION}, but required is at least ${HDF_MIN_VER} (found ${HDF5_LIBRARIES})")
        endif()
        set(HAVE_HDF5 1)
    endif()
endif()

if(HDF5_FOUND)
    add_library(HDF5::HDF5 INTERFACE IMPORTED)
    if(HDF5_USE_STATIC_LIBRARIES AND TARGET hdf5::hdf5-static)
        # static target from hdf5 1.10 or 1.12 config
        target_link_libraries(HDF5::HDF5 INTERFACE hdf5::hdf5-static)
    elseif(NOT HDF5_USE_STATIC_LIBRARIES AND TARGET hdf5::hdf5-shared)
        # shared target from hdf5 1.10 or 1.12 config
        target_link_libraries(HDF5::HDF5 INTERFACE hdf5::hdf5-shared)
    elseif(TARGET hdf5)
        # target from hdf5 1.8 config
        target_link_libraries(HDF5::HDF5 INTERFACE hdf5)
        if(NOT HDF5_USE_STATIC_LIBRARIES)
            set_target_properties(HDF5::HDF5 PROPERTIES
                INTERFACE_COMPILE_DEFINITIONS "H5_BUILT_AS_DYNAMIC_LIB"
            )
        endif()
    else()
        # results from CMake FindHDF5
        set_target_properties(HDF5::HDF5 PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${HDF5_INCLUDE_DIRS}"
            INTERFACE_LINK_LIBRARIES "${HDF5_LIBRARIES}"
        )
    endif()
endif()

if(NOT HAVE_HDF5 AND MATIO_MAT73)
    message(FATAL_ERROR "MAT73 requires HDF5")
endif()


macro(matio_create_zlib target)
    add_library(ZLIB::ZLIB INTERFACE IMPORTED)
    target_link_libraries(ZLIB::ZLIB INTERFACE ${target})
    set(ZLIB_FOUND TRUE)
endmacro()

if(MATIO_WITH_ZLIB)
    if(HDF5_USE_STATIC_LIBRARIES AND TARGET zlib-static)
        matio_create_zlib(zlib-static)
    elseif(HDF5_USE_STATIC_LIBRARIES AND TARGET hdf5::zlib-static)
        matio_create_zlib(hdf5::zlib-static)
    elseif(NOT HDF5_USE_STATIC_LIBRARIES AND TARGET zlib-shared)
        matio_create_zlib(zlib-shared)
    elseif(NOT HDF5_USE_STATIC_LIBRARIES AND TARGET hdf5::zlib-shared)
        matio_create_zlib(hdf5::zlib-shared)
    elseif(TARGET zlib)
        matio_create_zlib(zlib)
    else()
        find_package(ZLIB 1.2.3)

        # try to find a static zlib version
        if((NOT ZLIB_FOUND OR NOT MATIO_SHARED) AND ZLIB_INCLUDE_DIRS AND NOT ZLIB_STATIC_LIBRARY)
            get_filename_component(ZLIB_DIR ${ZLIB_LIBRARY} DIRECTORY)
            find_library(ZLIB_STATIC_LIBRARY libzlib PATHS ${ZLIB_DIR} ${ZLIB_ROOT}/lib ${PROJECT_BINARY_DIR}/lib)
            if(ZLIB_STATIC_LIBRARY)
                if(NOT TARGET ZLIB::ZLIB)
                    set(ZLIB_FOUND TRUE)
                    add_library(ZLIB::ZLIB UNKNOWN IMPORTED)
                    set_target_properties(ZLIB::ZLIB PROPERTIES INTERFACE_INCLUDE_DIRECTORIES "${ZLIB_INCLUDE_DIRS}")
                endif()
            set_target_properties(ZLIB::ZLIB PROPERTIES IMPORTED_LOCATION "${ZLIB_STATIC_LIBRARY}")
            message(STATUS "Found ZLIB: ${ZLIB_STATIC_LIBRARY} (static library)")
            endif()
        endif()
    endif()

    if(ZLIB_FOUND)
        set(HAVE_ZLIB 1)
    endif()
endif()
