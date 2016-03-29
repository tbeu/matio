#-----------------------------------------------
# packaging
#-----------------------------------------------

option(ENABLE_PACKAGING "Enable Packaging" ON)

if (CMAKE_C_COMPILER MATCHES gcc)
    exec_program(${CMAKE_C_COMPILER}
        ARGS -dumpversion
        OUTPUT_VARIABLE PACKAGE_COMPILER)
    set(PACKAGE_COMPILER gcc-${PACKAGE_COMPILER})
else()
    set(PACKAGE_COMPILER ${CMAKE_CXX_COMPILER})
endif()

if (UNIX AND NOT APPLE) # LINUX
    option(BUILD_RPM "Enable RPM Packaging" OFF)
endif()

#   Do packaging only if there is no other (upward) packaging already being done.

if (NOT PACKAGE_NAME)
    if (ENABLE_PACKAGING OR BUILD_RPM)

        include(InstallRequiredSystemLibraries)

        configure_file(${MATIO_SOURCE_DIR}/README.md  README.md  COPYONLY)
        configure_file(${MATIO_SOURCE_DIR}/COPYING COPYING.txt COPYONLY)

        set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "matio library for matlab IOs")
        set(CPACK_PACKAGE_VENDOR "Christopher Hulbert")
        set(CPACK_PACKAGE_DESCRIPTION_FILE "${MATIO_BINARY_DIR}/README.md")
        set(CPACK_RESOURCE_FILE_LICENSE "${MATIO_BINARY_DIR}/COPYING.txt")
        set(CPACK_PACKAGE_INSTALL_DIRECTORY "matio")
        set(CPACK_PACKAGE_CONTACT "cch@isl-inc.com")

        if ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "x86_64")
            set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE amd64)
            set(CPACK_RPM_PACKAGE_ARCHITECTURE x86_64)
        else()
            set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE i386)
            set(CPACK_RPM_PACKAGE_ARCHITECTURE i386)
        endif()

        set(PACKAGE_NAME "matio-${PACKAGE_VERSION_MAJOR}.${PACKAGE_VERSION_MINOR}.${PACKAGE_VERSION_PATCH}")
        if (UNIX)
            if (APPLE)
                set(PACKAGE_NAME ${PACKAGE_NAME}-MacOSX-Intel)
                if (BUILD_UNIVERSAL)
                    set(PACKAGE_NAME ${PACKAGE_NAME}-Universal)
                endif()
            else()
                set(PACKAGE_NAME ${PACKAGE_NAME}-Linux.${CPACK_RPM_PACKAGE_ARCHITECTURE})
            endif()
        else()
            set(PACKAGE_NAME ${PACKAGE_NAME}-win32-x86)
        endif()

        set(PACKAGE_NAME ${PACKAGE_NAME}-${PACKAGE_COMPILER})

        if (USE_OMP)
            set(PACKAGE_NAME ${PACKAGE_NAME}-OpenMP)
        endif()

        if (BUILD_SHARED_LIBS)
            if (PYTHON_WRAP)
                set(PACKAGE_NAME ${PACKAGE_NAME}-python)
            endif()
            set(PACKAGE_NAME ${PACKAGE_NAME}-shared)
        else()
            set(PACKAGE_NAME ${PACKAGE_NAME}-static)
        endif()

        set(CPACK_PACKAGE_FILE_NAME ${PACKAGE_NAME})

        if (WIN32)
            # There is a bug in NSIS that does not handle full unix paths properly. Make
            # sure there is at least one set of four (4) backlasshes.
            set(CPACK_NSIS_DISPLAY_NAME "matio library for matlab IOs")
            set(CPACK_NSIS_HELP_LINK "http:\\\\\\\\sourceforge.net/projects/matio/support")
            set(CPACK_NSIS_URL_INFO_ABOUT "https:\\\\\\\\gforge.inria.fr/projects/openmeeg/")
            set(CPACK_NSIS_CONTACT "cch@isl-inc.com")
            set(CPACK_NSIS_MODIFY_PATH ON)
        endif()

        set(CPACK_SOURCE_STRIP_FILES "")

        if (UNIX AND NOT APPLE)
            set(CPACK_GENERATOR "TGZ")
        endif()

        if (APPLE)
            set(CPACK_GENERATOR "PackageMaker;TGZ")
        endif()

        include(CPack)

        if (UNIX AND BUILD_RPM) # linux
            set(CPACK_GENERATOR "${CPACK_GENERATOR};RPM")
            if (CMAKE_VERSION VERSION_LESS "2.8")
                include(UseRPMTools)
                if (RPMTools_FOUND)
                    RPMTools_ADD_RPM_TARGETS(${PROJECT_NAME} "${PROJECT_SOURCE_DIR}/packaging/${PROJECT_NAME}.spec.in")
                endif()
            else()
                set(CPACK_RPM_PACKAGE_LICENSE "LGPL")
                set(CPACK_RPM_PACKAGE_DESCRIPTION  "libmatio is an open-source library for reading/writing Matlab MAT files.  This
    library is designed for use by programs/libraries that do not have access or
    do not want to rely on Matlab's libmat shared library.")
                set(CPACK_RPM_PACKAGE_GROUP "Libraries")
            endif()
        endif()
    endif()
endif()

#IF (ENABLE_PACKAGING AND WIN32)
#    INCLUDE(UseWin32dlls)
#ENDIF()
