if(MATIO_USE_CONAN AND (MATIO_WITH_HDF5 OR MATIO_WITH_ZLIB))
    conan_add_remote(NAME conan-center URL https://conan.bintray.com)
endif()

if(MATIO_WITH_HDF5)
    if(NOT DEFINED HDF5_USE_STATIC_LIBRARIES)
        if(MATIO_SHARED)
            set(HDF5_USE_STATIC_LIBRARIES FALSE)
        else()
            set(HDF5_USE_STATIC_LIBRARIES TRUE)
        endif()
    endif()

    if(MATIO_USE_CONAN)
        if(HDF5_USE_STATIC_LIBRARIES)
            conan_cmake_run(REQUIRES "hdf5/[>=1.8 <1.13]" "zlib/[>=1.2.3]" BASIC_SETUP CMAKE_TARGETS OPTIONS hdf5:shared=False zlib:shared=False BUILD missing)
        else()
            conan_cmake_run(REQUIRES "hdf5/[>=1.8 <1.13]" "zlib/[>=1.2.3]" BASIC_SETUP CMAKE_TARGETS OPTIONS hdf5:shared=True zlib:shared=True BUILD missing)
        endif()
    else()
        find_package(HDF5 1.8 REQUIRED)
    endif()

    set(HAVE_HDF5 1)
    if(NOT TARGET HDF5::HDF5)
        add_library(HDF5::HDF5 INTERFACE IMPORTED)
        if(MATIO_USE_CONAN AND TARGET CONAN_PKG::hdf5)
            # target from Conan
            target_link_libraries(HDF5::HDF5 INTERFACE CONAN_PKG::hdf5)
        elseif(HDF5_USE_STATIC_LIBRARIES AND TARGET hdf5::hdf5-static)
            # static target from hdf5 1.10 or 1.12 config
            target_link_libraries(HDF5::HDF5 INTERFACE hdf5::hdf5-static)
        elseif(NOT HDF5_USE_STATIC_LIBRARIES AND TARGET hdf5::hdf5-shared)
            # shared target from hdf5 1.10 or 1.12 config
            target_link_libraries(HDF5::HDF5 INTERFACE hdf5::hdf5-shared)
        elseif(TARGET hdf5)
            # target from hdf5 1.8 config
            target_link_libraries(HDF5::HDF5 INTERFACE hdf5)
        else()
            # results from CMake FindHDF5
            set_target_properties(HDF5::HDF5 PROPERTIES
                INTERFACE_INCLUDE_DIRECTORIES "${HDF5_INCLUDE_DIRS}"
                INTERFACE_LINK_LIBRARIES "${HDF5_LIBRARIES}"
            )
        endif()
        if(NOT HDF5_USE_STATIC_LIBRARIES)
            set_target_properties(HDF5::HDF5 PROPERTIES
                INTERFACE_COMPILE_DEFINITIONS "H5_BUILT_AS_DYNAMIC_LIB"
            )
        endif()
    endif()
endif()

if(NOT HAVE_HDF5 AND MATIO_MAT73)
    message(FATAL_ERROR "MAT73 requires HDF5")
endif()


macro(matio_create_zlib target)
    if(NOT TARGET ZLIB::ZLIB)
        add_library(ZLIB::ZLIB INTERFACE IMPORTED)
        target_link_libraries(ZLIB::ZLIB INTERFACE ${target})
    endif()
endmacro()

if(MATIO_WITH_ZLIB)
    if(MATIO_USE_CONAN AND NOT MATIO_WITH_HDF5)
        conan_cmake_run(REQUIRES "zlib/[>=1.2.3]" BASIC_SETUP CMAKE_TARGETS OPTIONS BUILD missing)
    endif()

    if(MATIO_USE_CONAN AND TARGET CONAN_PKG::zlib)
        matio_create_zlib(CONAN_PKG::zlib)
    elseif(HDF5_USE_STATIC_LIBRARIES AND TARGET zlib-static)
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
        find_package(ZLIB 1.2.3 REQUIRED)
    endif()

    set(HAVE_ZLIB 1)
endif()
