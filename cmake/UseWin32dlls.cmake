IF (WIN32 AND ENABLE_PACKAGING)

     # TODO.

#    SET(WIN32_MANIFEST "${CMAKE_SOURCE_DIR}/../win32addons/Microsoft.VC80.CRT.manifest")
#    SET(MSVCP80 "${OpenMEEG_SOURCE_DIR}/../win32addons/msvcp80.dll")
#    SET(MSVCR80 "${OpenMEEG_SOURCE_DIR}/../win32addons/msvcr80.dll")

    ADD_CUSTOM_TARGET(copy_dlls ALL
        COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/win32depends/
        COMMAND ${CMAKE_COMMAND} -E copy ${MSVCP80} ${CMAKE_BINARY_DIR}/win32depends/
        COMMAND ${CMAKE_COMMAND} -E copy ${MSVCR80} ${CMAKE_BINARY_DIR}/win32depends/
        COMMAND ${CMAKE_COMMAND} -E copy ${WIN32_MANIFEST} ${CMAKE_BINARY_DIR}/win32depends/
    )

    INSTALL(DIRECTORY ${MATIO_BINARY_DIR}/win32depends/ DESTINATION bin
              PATTERN "${MATIO_BINARY_DIR}/win32depends/*"
              PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
                          GROUP_EXECUTE GROUP_READ)

ENDIF()
