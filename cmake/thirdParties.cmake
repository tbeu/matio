if(MATIO_USE_CONAN AND (MATIO_WITH_HDF5 OR MATIO_WITH_ZLIB))
    conan_add_remote(NAME conan-center URL https://center.conan.io VERIFY_SSL False)
endif()

if(MATIO_USE_CONAN)
    message(WARNING
        "MATIO_USE_CONAN for Conan 1.X is deprecated. "
        "Please update your build configuration to Conan 2.X to avoid using this option."
    )
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
        set(MATIO_CONAN_REQUIRES "hdf5/[>=1.8 <1.15]" "zlib/[>=1.2.3]")
        if(MATIO_ENABLE_CPPCHECK)
            list(APPEND MATIO_CONAN_REQUIRES "cppcheck/[>=2.16.0]")
        endif()
        if(HDF5_USE_STATIC_LIBRARIES)
            conan_cmake_run(
                REQUIRES ${MATIO_CONAN_REQUIRES}
                BASIC_SETUP CMAKE_TARGETS
                OPTIONS hdf5:shared=False zlib:shared=False
                BUILD missing
            )
        else()
            conan_cmake_run(
                REQUIRES ${MATIO_CONAN_REQUIRES}
                BASIC_SETUP CMAKE_TARGETS
                OPTIONS hdf5:shared=True zlib:shared=True
                BUILD missing
            )
        endif()
        set(HDF5_FOUND TRUE)
    else()
        find_package(HDF5)
        if(HDF5_FOUND)
            set(HDF_MIN_VER 1.8)
            if(HDF5_VERSION VERSION_LESS ${HDF_MIN_VER})
                message(FATAL_ERROR
                    "Could NOT find HDF5: Found unsuitable version ${HDF5_VERSION}, "
                    "but required is at least ${HDF_MIN_VER} (found ${HDF5_LIBRARIES})."
                )
            endif()
        endif()
    endif()
endif()

if(HDF5_FOUND)
    set(HAVE_HDF5 1)
    add_library(MATIO::HDF5 INTERFACE IMPORTED)
    if(MATIO_USE_CONAN AND TARGET CONAN_PKG::hdf5)
        # target from Conan
        target_link_libraries(MATIO::HDF5 INTERFACE CONAN_PKG::hdf5)
    elseif(HDF5_USE_STATIC_LIBRARIES AND TARGET hdf5::hdf5-static)
        # static target from hdf5 1.10 or 1.12 config
        target_link_libraries(MATIO::HDF5 INTERFACE hdf5::hdf5-static)
    elseif(NOT HDF5_USE_STATIC_LIBRARIES AND TARGET hdf5::hdf5-shared)
        # shared target from hdf5 1.10 or 1.12 config
        target_link_libraries(MATIO::HDF5 INTERFACE hdf5::hdf5-shared)
    elseif(TARGET hdf5)
        # target from hdf5 1.8 config
        target_link_libraries(MATIO::HDF5 INTERFACE hdf5)
    elseif(TARGET HDF5::HDF5)
        # target defined in CMake FindHDF5 (since 3.19)
        target_link_libraries(MATIO::HDF5 INTERFACE HDF5::HDF5)
    else()
        # results from CMake FindHDF5
        set_target_properties(MATIO::HDF5 PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${HDF5_INCLUDE_DIRS}"
            INTERFACE_LINK_LIBRARIES "${HDF5_LIBRARIES}"
        )
    endif()
    if(NOT HDF5_USE_STATIC_LIBRARIES)
        set_target_properties(MATIO::HDF5 PROPERTIES
            INTERFACE_COMPILE_DEFINITIONS "H5_BUILT_AS_DYNAMIC_LIB"
        )
    endif()
endif()

if(NOT HAVE_HDF5 AND MATIO_MAT73)
    message(FATAL_ERROR "MAT73 requires HDF5")
endif()

# Create the zlib target
macro(MATIO_CREATE_ZLIB target)
    add_library(MATIO::ZLIB INTERFACE IMPORTED)
    set_target_properties(MATIO::ZLIB PROPERTIES INTERFACE_LINK_LIBRARIES ${target})
    set(ZLIB_FOUND TRUE)
endmacro()

if(MATIO_WITH_ZLIB)
    if(MATIO_USE_CONAN AND NOT MATIO_WITH_HDF5)
        conan_cmake_run(REQUIRES "zlib/[>=1.2.3]" BASIC_SETUP CMAKE_TARGETS OPTIONS BUILD missing)
    endif()

    if(MATIO_USE_CONAN AND TARGET CONAN_PKG::zlib)
        MATIO_CREATE_ZLIB(CONAN_PKG::zlib)
    elseif(HDF5_USE_STATIC_LIBRARIES AND TARGET zlib-static)
        MATIO_CREATE_ZLIB(zlib-static)
    elseif(HDF5_USE_STATIC_LIBRARIES AND TARGET hdf5::zlib-static)
        MATIO_CREATE_ZLIB(hdf5::zlib-static)
    elseif(NOT HDF5_USE_STATIC_LIBRARIES AND TARGET zlib-shared)
        MATIO_CREATE_ZLIB(zlib-shared)
    elseif(NOT HDF5_USE_STATIC_LIBRARIES AND TARGET hdf5::zlib-shared)
        MATIO_CREATE_ZLIB(hdf5::zlib-shared)
    elseif(TARGET zlib)
        MATIO_CREATE_ZLIB(zlib)
    elseif(TARGET ZLIB::ZLIB)
        MATIO_CREATE_ZLIB(ZLIB::ZLIB)
    else()
        find_package(ZLIB 1.2.3)
        if(ZLIB_FOUND)
            MATIO_CREATE_ZLIB(ZLIB::ZLIB)
        endif()
    endif()

    if(ZLIB_FOUND)
        set(HAVE_ZLIB 1)
    endif()
endif()
