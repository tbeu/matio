configure_file(
  "${PROJECT_SOURCE_DIR}/cmake/matio_pubconf.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/src/matio_pubconf.h"
  ESCAPE_QUOTES @ONLY)

configure_file(
  "${PROJECT_SOURCE_DIR}/cmake/matioConfig.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/src/matioConfig.h"
  ESCAPE_QUOTES @ONLY)

set(src_SOURCES
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
  ${CMAKE_CURRENT_BINARY_DIR}/src/matio_pubconf.h
  ${CMAKE_CURRENT_BINARY_DIR}/src/matioConfig.h
)

add_library(matio STATIC ${src_SOURCES} )
target_include_directories(matio
    INTERFACE ${PROJECT_SOURCE_DIR}/src/
    PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/src/
)


if(NOT WIN32)
  target_link_libraries(matio PUBLIC m)
else()
  target_link_libraries(matio PUBLIC ${GETOPT_LIB})
  set_target_properties(matio PROPERTIES OUTPUT_NAME libmatio)
endif()

if(HDF5_FOUND)
  if(WIN32)
    target_link_libraries(matio
      PUBLIC hdf5::hdf5-static)
  else()
    target_link_libraries(matio
      PUBLIC HDF5::HDF5)
  endif()
endif()

if(ZLIB_FOUND)
  target_link_libraries(matio
      PUBLIC ZLIB::ZLIB
  )
endif()

# XXX not sure it's the right thing to do...
set_target_properties(matio PROPERTIES
  CXX_STANDARD_REQUIRED ON
  CXX_VISIBILITY_PRESET hidden
  VISIBILITY_INLINES_HIDDEN 1)


# This generates matio_export.h
include(GenerateExportHeader)
generate_export_header(matio)

set_target_properties(matio PROPERTIES PUBLIC_HEADER "${PROJECT_SOURCE_DIR}/src/matio.h;${CMAKE_CURRENT_BINARY_DIR}/src/matio_pubconf.h;${CMAKE_CURRENT_BINARY_DIR}/matio_export.h") # XXX: check whether matio_pubconf.h or matioConfig.h is the current strategy (one of the two is deprected)


# 'make install' to the correct locations (provided by GNUInstallDirs).
install(TARGETS matio EXPORT libmatio
        PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})
# install(EXPORT libmatio NAMESPACE matio:: DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake)
