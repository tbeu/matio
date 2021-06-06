configure_file(
    "${PROJECT_SOURCE_DIR}/cmake/matio_pubconf.cmake.in"
    "${PROJECT_BINARY_DIR}/src/matio_pubconf.h"
    ESCAPE_QUOTES @ONLY
)

configure_file(
    "${PROJECT_SOURCE_DIR}/cmake/matioConfig.cmake.in"
    "${PROJECT_BINARY_DIR}/src/matioConfig.h"
    ESCAPE_QUOTES @ONLY
)

set(MATIO_SOURCES
    ${PROJECT_SOURCE_DIR}/src/endian.c
    ${PROJECT_SOURCE_DIR}/src/mat.c
    ${PROJECT_SOURCE_DIR}/src/io.c
    ${PROJECT_SOURCE_DIR}/src/inflate.c
    ${PROJECT_SOURCE_DIR}/src/mat73.c
    ${PROJECT_SOURCE_DIR}/src/matvar_cell.c
    ${PROJECT_SOURCE_DIR}/src/matvar_struct.c
    ${PROJECT_SOURCE_DIR}/src/mat4.c
    ${PROJECT_SOURCE_DIR}/src/mat5.c
    ${PROJECT_SOURCE_DIR}/src/snprintf.c
    ${PROJECT_SOURCE_DIR}/src/read_data.c
    ${PROJECT_SOURCE_DIR}/src/mat5.h
    ${PROJECT_SOURCE_DIR}/src/mat73.h
    ${PROJECT_SOURCE_DIR}/src/matio_private.h
    ${PROJECT_SOURCE_DIR}/src/mat4.h
    ${PROJECT_SOURCE_DIR}/src/matio.h
    ${PROJECT_SOURCE_DIR}/visual_studio/matio.rc
    ${PROJECT_SOURCE_DIR}/visual_studio/matio.def
    ${PROJECT_BINARY_DIR}/src/matio_pubconf.h
    ${PROJECT_BINARY_DIR}/src/matioConfig.h
)
if(STDINT_MSVC)
    set(MATIO_SOURCES ${MATIO_SOURCES} ${PROJECT_SOURCE_DIR}/visual_studio/stdint_msvc.h)
endif()

if(USE_GNU_LINK_FLAGS OR USE_LLVM_MACOS_LINK_FLAGS)
    file(COPY "${PROJECT_SOURCE_DIR}/src/matio.sym" DESTINATION "${PROJECT_BINARY_DIR}/src")
    if(USE_GNU_LINK_FLAGS)
        set(MATIO_LINKER_FLAGS " -Wl,--no-undefined -Wl,--retain-symbols-file,${PROJECT_BINARY_DIR}/src/matio.sym")
    else()
        file(READ "${PROJECT_BINARY_DIR}/src/matio.sym" SYM_CONTENTS)
        string(REPLACE "Mat_" "_Mat_" SYM_CONTENTS ${SYM_CONTENTS})
        file(WRITE "${PROJECT_BINARY_DIR}/src/matio.sym" ${SYM_CONTENTS})
        set(MATIO_LINKER_FLAGS " -Wl,-undefined,error -Wl,-exported_symbols_list,${PROJECT_BINARY_DIR}/src/matio.sym")
    endif()
    string(APPEND CMAKE_SHARED_LINKER_FLAGS ${MATIO_LINKER_FLAGS})
endif()

if(MATIO_SHARED)
    add_library(${PROJECT_NAME} SHARED ${MATIO_SOURCES})
else()
    add_library(${PROJECT_NAME} STATIC ${MATIO_SOURCES})
endif()
add_library(${PROJECT_NAME}::${PROJECT_NAME} ALIAS ${PROJECT_NAME})

target_include_directories(${PROJECT_NAME}
    INTERFACE ${PROJECT_SOURCE_DIR}/src
    PUBLIC    ${PROJECT_BINARY_DIR}/src
)
if(STDINT_MSVC)
    target_include_directories(${PROJECT_NAME} PUBLIC ${PROJECT_SOURCE_DIR}/visual_studio)
endif()

if(HAVE_LIBM)
    target_link_libraries(${PROJECT_NAME} PUBLIC m)
endif()

if(MSVC)
    set_target_properties(${PROJECT_NAME} PROPERTIES OUTPUT_NAME lib${PROJECT_NAME})
endif()

if(HDF5_FOUND)
    target_link_libraries(${PROJECT_NAME} PUBLIC MATIO::HDF5)
endif()

if(ZLIB_FOUND)
    target_link_libraries(${PROJECT_NAME} PUBLIC MATIO::ZLIB)
endif()

set_target_properties(${PROJECT_NAME} PROPERTIES POSITION_INDEPENDENT_CODE ${MATIO_PIC})
if(MATIO_SHARED)
    # Convert matio_LIB_VERSIONINFO libtool version format into VERSION and SOVERSION
    # Convert from ":" separated into CMake list format using ";"
    string(REPLACE ":" ";" matio_LIB_VERSIONINFO ${matio_LIB_VERSIONINFO})
    list(GET matio_LIB_VERSIONINFO 0 matio_LIB_VERSION_CURRENT)
    list(GET matio_LIB_VERSIONINFO 1 matio_LIB_VERSION_REVISION)
    list(GET matio_LIB_VERSIONINFO 2 matio_LIB_VERSION_AGE)
    math(EXPR matio_LIB_VERSION_MAJOR "${matio_LIB_VERSION_CURRENT} - ${matio_LIB_VERSION_AGE}")
    set(matio_LIB_VERSION_MINOR "${matio_LIB_VERSION_AGE}")
    set(matio_LIB_VERSION_RELEASE "${matio_LIB_VERSION_REVISION}")
    set_target_properties(${PROJECT_NAME} PROPERTIES
        SOVERSION ${matio_LIB_VERSION_MAJOR}
        VERSION "${matio_LIB_VERSION_MAJOR}.${matio_LIB_VERSION_MINOR}.${matio_LIB_VERSION_RELEASE}"
    )
endif()

# specify the public headers
set(MATIO_PUBLIC_HEADERS
    ${PROJECT_SOURCE_DIR}/src/matio.h
    ${PROJECT_BINARY_DIR}/src/matio_pubconf.h
)
if(STDINT_MSVC)
    set(MATIO_PUBLIC_HEADERS ${MATIO_PUBLIC_HEADERS} ${PROJECT_SOURCE_DIR}/visual_studio/stdint_msvc.h)
endif()
set_target_properties(${PROJECT_NAME} PROPERTIES PUBLIC_HEADER "${MATIO_PUBLIC_HEADERS}")

# 'make install' to the correct locations (provided by GNUInstallDirs).
install(TARGETS ${PROJECT_NAME} EXPORT lib${PROJECT_NAME}
        PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
