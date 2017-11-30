SET(DOC_FILES Mat_Close.3 Mat_CreateVer.3 Mat_GetFilename.3 Mat_GetLibraryVersion.3 Mat_GetVersion.3 Mat_Open.3 Mat_Rewind.3
              Mat_VarAddStructField.3 Mat_VarCreate.3 Mat_VarCreateStruct.3 Mat_VarFree.3 Mat_VarGetNumberOfFields.3
              Mat_VarGetStructFieldnames.3 Mat_VarGetStructs.3 Mat_VarGetStructsLinear.3 Mat_VarRead.3 Mat_VarReadInfo.3
              Mat_VarReadNext.3 Mat_VarReadNextInfo.3 Mat_VarWrite.3)

INSTALL(FILES ${DOC_FILES} DESTINATION share/man/man3)

ADD_CUSTOM_TARGET(user_documentation ALL)

FIND_PROGRAM(TEXI2PDF texi2pdf)
IF (TEXI2PDF)
    ADD_DEPENDENCIES(user_documentation user_documentation_pdf)
    ADD_CUSTOM_TARGET(user_documentation_pdf
                      COMMAND ${TEXI2PDF} ${CMAKE_CURRENT_SOURCE_DIR}/matio_user_guide.texi
                      DEPENDS intro.texi quick.texi matvar_structure.texi)
    INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/matio_user_guide.pdf DESTINATION share/doc/matio)
ENDIF()

FIND_PROGRAM(TEXI2HTML texi2html)
IF (TEXI2HTML)
    ADD_DEPENDENCIES(user_documentation user_documentation_html)
    ADD_CUSTOM_TARGET(user_documentation_html
                      COMMAND ${TEXI2HTML} ${CMAKE_CURRENT_SOURCE_DIR}/matio_user_guide.texi
                      DEPENDS intro.texi quick.texi matvar_structure.texi)
    INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/matio_user_guide.html DESTINATION share/doc/matio)
ENDIF()

FIND_PROGRAM(TEXI2DVI texi2dvi)
IF (TEXI2DVI)
    ADD_DEPENDENCIES(user_documentation user_documentation_ps user_documentation_dvi)
    ADD_CUSTOM_TARGET(user_documentation_ps
                      COMMAND ${TEXI2DVI} --ps ${CMAKE_CURRENT_SOURCE_DIR}/matio_user_guide.texi
                      DEPENDS intro.texi quick.texi matvar_structure.texi)

    ADD_CUSTOM_TARGET(user_documentation_dvi
                      COMMAND ${TEXI2DVI} --dvi ${CMAKE_CURRENT_SOURCE_DIR}/matio_user_guide.texi
                      DEPENDS intro.texi quick.texi matvar_structure.texi)
    INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/matio_user_guide.ps ${CMAKE_CURRENT_BINARY_DIR}/matio_user_guide.dvi DESTINATION share/doc/matio)
ENDIF()

SET(DOC_FILE_TO_CLEAN matio_user_guide.aux matio_user_guide.cp matio_user_guide.dvi 
                      matio_user_guide.fn matio_user_guide.html matio_user_guide.ky 
                      matio_user_guide.log matio_user_guide.pdf matio_user_guide.pg 
                      matio_user_guide.ps matio_user_guide.toc matio_user_guide.tp 
                      matio_user_guide.vr)

SET_DIRECTORY_PROPERTIES(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "${DOC_FILE_TO_CLEAN}")
