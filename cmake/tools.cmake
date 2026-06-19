add_executable(matdump
    ${PROJECT_SOURCE_DIR}/tools/matdump.c
    ${PROJECT_SOURCE_DIR}/snprintf/snprintf.c
)
matio_set_common_properties(matdump)
target_link_libraries(matdump PRIVATE matio)
if(NOT HAVE_GETOPT)
    target_link_libraries(matdump PRIVATE getopt)
endif()

install(TARGETS matdump
        PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
