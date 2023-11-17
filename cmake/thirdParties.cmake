if(MATIO_USE_CONAN AND (MATIO_WITH_HDF5 OR MATIO_WITH_ZLIB))
    conan_add_remote(NAME conan-center URL https://center.conan.io VERIFY_SSL False)
endif()

if(WIN32)
    if(CMAKE_GENERATOR_PLATFORM STREQUAL "x64")
        set(MATIO_TRY_HDF_PATH_ROOT "$ENV{PROGRAMW6432}/HDF_Group")
    elseif(CMAKE_GENERATOR_PLATFORM STREQUAL "Win32" OR NOT CMAKE_GENERATOR_PLATFORM)
        set(PROGRAMFILES_X86 "PROGRAMFILES(x86)")
        set(MATIO_TRY_HDF_PATH_ROOT "$ENV{${PROGRAMFILES_X86}}/HDF_Group")
        unset(PROGRAMFILES_X86)
    endif()
    file(GLOB MATIO_TRY_SUB_DIRS RELATIVE ${MATIO_TRY_HDF_PATH_ROOT} ${MATIO_TRY_HDF_PATH_ROOT}/**/*)
    if(MATIO_TRY_SUB_DIRS)
        set(MATIO_TRY_HDF5_PATH)
        foreach(MATIO_TRY_SUB_DIR ${MATIO_TRY_SUB_DIRS})
            list(APPEND MATIO_TRY_HDF5_PATH "${MATIO_TRY_HDF_PATH_ROOT}/${MATIO_TRY_SUB_DIR}")
        endforeach()
    endif()
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
            conan_cmake_run(
                REQUIRES "hdf5/[>=1.8 <1.15]" "zlib/[>=1.2.3]"
                BASIC_SETUP CMAKE_TARGETS
                OPTIONS hdf5:shared=False zlib:shared=False
                BUILD missing
            )
        else()
            conan_cmake_run(
                REQUIRES "hdf5/[>=1.8 <1.15]" "zlib/[>=1.2.3]"
                BASIC_SETUP CMAKE_TARGETS
                OPTIONS hdf5:shared=True zlib:shared=True
                BUILD missing)
        endif()
        set(HDF5_FOUND TRUE)
    else()
        if(MATIO_TRY_HDF5_PATH)
            if(HDF5_ROOT)
                list(INSERT HDF5_ROOT 0 ${MATIO_TRY_HDF5_PATH})
            else()
                set(HDF5_ROOT ${MATIO_TRY_HDF5_PATH})
            endif()
        endif()
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


macro(matio_create_zlib target)
    add_library(MATIO::ZLIB INTERFACE IMPORTED)
    set_target_properties(MATIO::ZLIB PROPERTIES INTERFACE_LINK_LIBRARIES ${target})
    set(ZLIB_FOUND TRUE)
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
    elseif(TARGET ZLIB::ZLIB)
        matio_create_zlib(ZLIB::ZLIB)
    else()
        find_package(ZLIB 1.2.3)
        if(ZLIB_FOUND)
            matio_create_zlib(ZLIB::ZLIB)
        endif()
    endif()

    if(ZLIB_FOUND)
        set(HAVE_ZLIB 1)
    endif()
endif()
