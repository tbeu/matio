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

if(NOT MSVC)
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,--no-undefined")
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -Wl,--retain-symbols-file,${PROJECT_SOURCE_DIR}/src/matio.sym")
endif()

if(MATIO_SHARED)
    add_library(matio SHARED ${MATIO_SOURCES})
else()
    add_library(matio STATIC ${MATIO_SOURCES})
endif()
add_library(matio::matio ALIAS matio)

target_include_directories(matio
    INTERFACE ${PROJECT_SOURCE_DIR}/src
    PUBLIC    ${PROJECT_BINARY_DIR}/src
)
if(STDINT_MSVC)
    target_include_directories(matio PUBLIC ${PROJECT_SOURCE_DIR}/visual_studio)
endif()

if(HAVE_LIBM)
    target_link_libraries(matio PUBLIC m)
endif()

if(WIN32)
    set_target_properties(matio PROPERTIES OUTPUT_NAME libmatio)
endif()

if(HDF5_FOUND)
    target_link_libraries(matio PUBLIC HDF5::HDF5)
endif()

if(ZLIB_FOUND)
    target_link_libraries(matio PUBLIC ZLIB::ZLIB)
endif()

set_target_properties(matio PROPERTIES POSITION_INDEPENDENT_CODE ${MATIO_PIC})

# specify the public headers
set(MATIO_PUBLIC_HEADERS
    ${PROJECT_SOURCE_DIR}/src/matio.h
    ${PROJECT_BINARY_DIR}/src/matio_pubconf.h
)
if(STDINT_MSVC)
    set(MATIO_PUBLIC_HEADERS ${MATIO_PUBLIC_HEADERS} ${PROJECT_SOURCE_DIR}/visual_studio/stdint_msvc.h)
endif()
set_target_properties(matio PROPERTIES PUBLIC_HEADER "${MATIO_PUBLIC_HEADERS}")

# 'make install' to the correct locations (provided by GNUInstallDirs).
install(TARGETS matio EXPORT libmatio
        PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
