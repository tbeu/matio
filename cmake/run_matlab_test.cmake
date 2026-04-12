# Run a MATLAB test locally or via SSH
#
# Copyright (c) 2015-2026, The matio contributors
# Copyright (c) 2005-2014, Christopher C. Hulbert
# All rights reserved.
#
# SPDX-License-Identifier: BSD-2-Clause
#
# Required variables (passed via -D before -P):
#   MAT_FILE          - Local .mat file name (in working directory)
#   MAT_FILE_REMOTE   - Expected .mat filename (what .m script loads)
#   M_FILE            - Full path to the .m script
#   MATLAB_SCRIPT     - MATLAB script name (without .m)
#
# Local mode (MATIO_MATLAB_EXE is set):
#   MATIO_MATLAB_EXE  - Path to local MATLAB executable
#
# SSH mode (MATIO_MATLAB_EXE is empty):
#   MATIO_MATLAB_SSH_KEY  - SSH private key path
#   MATIO_MATLAB_SSH_HOST - SSH user@host
#
# Optional variables:
#   MATLAB_TYPE           - Type parameter for MATLAB script
#   MATIO_MATLAB_SSH_DIR  - Remote working directory (default: /tmp)

# Build MATLAB command
if(DEFINED MATLAB_TYPE AND NOT MATLAB_TYPE STREQUAL "")
    set(MATLAB_SETUP "type='${MATLAB_TYPE}';")
else()
    set(MATLAB_SETUP "")
endif()

if(DEFINED MATIO_MATLAB_EXE AND NOT MATIO_MATLAB_EXE STREQUAL "")
    # --- Local mode ---

    # Copy .mat file to expected name
    if(NOT MAT_FILE STREQUAL MAT_FILE_REMOTE)
        execute_process(
            COMMAND ${CMAKE_COMMAND} -E copy "${MAT_FILE}" "${MAT_FILE_REMOTE}"
            RESULT_VARIABLE COPY_RESULT
        )
        if(NOT COPY_RESULT EQUAL 0)
            message(FATAL_ERROR "Failed to copy ${MAT_FILE} to ${MAT_FILE_REMOTE}")
        endif()
    endif()

    # Copy .m file to working directory
    get_filename_component(M_FILE_NAME "${M_FILE}" NAME)
    execute_process(
        COMMAND ${CMAKE_COMMAND} -E copy "${M_FILE}" "${M_FILE_NAME}"
        RESULT_VARIABLE COPY_M_RESULT
    )
    if(NOT COPY_M_RESULT EQUAL 0)
        message(FATAL_ERROR "Failed to copy ${M_FILE}")
    endif()

    set(MATLAB_CMD "${MATLAB_SETUP}${MATLAB_SCRIPT}")
    execute_process(
        COMMAND "${MATIO_MATLAB_EXE}" -nosplash -nojvm -batch "${MATLAB_CMD}"
        OUTPUT_VARIABLE MATLAB_OUTPUT
        ERROR_VARIABLE MATLAB_ERROR
        RESULT_VARIABLE MATLAB_RESULT
    )
else()
    # --- SSH mode ---

    if(NOT DEFINED MATIO_MATLAB_SSH_KEY OR NOT DEFINED MATIO_MATLAB_SSH_HOST)
        message(FATAL_ERROR "Either MATIO_MATLAB_EXE or MATIO_MATLAB_SSH_KEY/MATIO_MATLAB_SSH_HOST must be set")
    endif()

    if(NOT DEFINED MATIO_MATLAB_SSH_DIR)
        set(MATIO_MATLAB_SSH_DIR "/tmp")
    endif()

    set(SSH_OPTS -i "${MATIO_MATLAB_SSH_KEY}"
        -o StrictHostKeyChecking=no
        -o UserKnownHostsFile=/dev/null
        -o LogLevel=ERROR)

    # SCP .mat file to remote (renamed to what the .m script expects)
    execute_process(
        COMMAND scp ${SSH_OPTS} "${MAT_FILE}" "${MATIO_MATLAB_SSH_HOST}:${MATIO_MATLAB_SSH_DIR}/${MAT_FILE_REMOTE}"
        RESULT_VARIABLE SCP_MAT_RESULT
    )
    if(NOT SCP_MAT_RESULT EQUAL 0)
        message(FATAL_ERROR "Failed to copy .mat file to remote host")
    endif()

    # SCP .m file to remote
    execute_process(
        COMMAND scp ${SSH_OPTS} "${M_FILE}" "${MATIO_MATLAB_SSH_HOST}:${MATIO_MATLAB_SSH_DIR}/"
        RESULT_VARIABLE SCP_M_RESULT
    )
    if(NOT SCP_M_RESULT EQUAL 0)
        message(FATAL_ERROR "Failed to copy .m file to remote host")
    endif()

    set(MATLAB_CMD "cd('${MATIO_MATLAB_SSH_DIR}');${MATLAB_SETUP}${MATLAB_SCRIPT}")
    execute_process(
        COMMAND ssh ${SSH_OPTS} "${MATIO_MATLAB_SSH_HOST}" "matlab -batch \"${MATLAB_CMD}\""
        OUTPUT_VARIABLE MATLAB_OUTPUT
        ERROR_VARIABLE MATLAB_ERROR
        RESULT_VARIABLE MATLAB_RESULT
    )
endif()

# Check for PASSED in output
string(FIND "${MATLAB_OUTPUT}" "PASSED" PASSED_POS)
if(PASSED_POS EQUAL -1)
    message(FATAL_ERROR "MATLAB test failed.\nOutput: ${MATLAB_OUTPUT}\nError: ${MATLAB_ERROR}")
endif()

message(STATUS "PASSED")
