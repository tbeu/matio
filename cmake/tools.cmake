add_executable(matdump ${PROJECT_SOURCE_DIR}/tools/matdump.c )
target_link_libraries(matdump matio)
if(NOT HAVE_GETOPT)
    target_link_libraries(matdump getopt)
endif()

install(TARGETS matdump
        PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
