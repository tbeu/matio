
#-----------------------------------------------
# packaging
#-----------------------------------------------

option(ENABLE_PACKAGING "Enable Packaging" ON)


if (ENABLE_PACKAGING)
    set(CPACK_GENERATOR "TGZ")

    # set variables
    set(CPACK_PACKAGE_NAME "libmatio")
    set(CPACK_PACKAGE_VERSION ${PROJECT_VERSION})

    set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_SOURCE_DIR}/README")
    set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")

    set(PACKAGE_ARCH_SHORT "Linux")
    if(WIN32)
        SET(PACKAGE_ARCH_SHORT "Win64")
        if(NOT CMAKE_CL_64)
            set(PACKAGE_ARCH_SHORT "Win32")
        endif()
    elseif(APPLE)
        set(PACKAGE_ARCH_SHORT "MacOSX")
    endif()

    set(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}-${PACKAGE_ARCH_SHORT}")

    # Following https://blog.quickmediasolutions.com/2017/11/24/using-windeployqt-with-cpack.html
    set(CMAKE_INSTALL_UCRT_LIBRARIES TRUE)
    # set(CMAKE_INSTALL_OPENMP_LIBRARIES TRUE)
    include(InstallRequiredSystemLibraries)
    include(CPack)

endif()
