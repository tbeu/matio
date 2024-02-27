if(MATIO_ENABLE_CPPCHECK)
    find_program(CPPCHECK cppcheck)
    if(CPPCHECK)
        set(CMAKE_C_CPPCHECK ${CPPCHECK}
            --language=c
            --quiet
            --check-level=exhaustive
            --suppress=checkersReport
            --suppress=unusedFunction
            --suppress=unmatchedSuppression
            --suppress=missingInclude
            --suppress=missingIncludeSystem
            --enable=all
            --inline-suppr
            --inconclusive
        )
    else()
        message(SEND_ERROR "Cppcheck requested but executable not found.")
    endif()
endif()
