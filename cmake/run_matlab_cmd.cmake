# Run a MATLAB command locally or via SSH
#
# Copyright (c) 2015-2026, The matio contributors
# All rights reserved.
#
# SPDX-License-Identifier: BSD-2-Clause
#
# Required variables (passed via -D before -P):
#   MATLAB_CMD        - MATLAB command to execute (default: ver)
#
# Local mode (MATIO_MATLAB_EXE is set):
#   MATIO_MATLAB_EXE  - Path to local MATLAB executable
#
# SSH mode (MATIO_MATLAB_EXE is empty):
#   MATIO_MATLAB_SSH_KEY  - SSH private key path
#   MATIO_MATLAB_SSH_HOST - SSH user@host

if(NOT DEFINED MATLAB_CMD OR MATLAB_CMD STREQUAL "")
    set(MATLAB_CMD "ver")
endif()

if(DEFINED MATIO_MATLAB_EXE AND NOT MATIO_MATLAB_EXE STREQUAL "")
    execute_process(
        COMMAND "${MATIO_MATLAB_EXE}" -nosplash -nojvm -batch "${MATLAB_CMD}"
        OUTPUT_VARIABLE MATLAB_OUTPUT
        ERROR_VARIABLE MATLAB_ERROR
        RESULT_VARIABLE MATLAB_RESULT
    )
else()
    if(NOT DEFINED MATIO_MATLAB_SSH_KEY OR NOT DEFINED MATIO_MATLAB_SSH_HOST)
        message(FATAL_ERROR "Either MATIO_MATLAB_EXE or MATIO_MATLAB_SSH_KEY/MATIO_MATLAB_SSH_HOST must be set")
    endif()

    set(SSH_OPTS -i "${MATIO_MATLAB_SSH_KEY}"
        -o StrictHostKeyChecking=no
        -o UserKnownHostsFile=/dev/null
        -o LogLevel=ERROR)

    execute_process(
        COMMAND ssh ${SSH_OPTS} "${MATIO_MATLAB_SSH_HOST}" "matlab -batch \"${MATLAB_CMD}\""
        OUTPUT_VARIABLE MATLAB_OUTPUT
        ERROR_VARIABLE MATLAB_ERROR
        RESULT_VARIABLE MATLAB_RESULT
    )
endif()

if(NOT MATLAB_RESULT EQUAL 0)
    message(FATAL_ERROR "MATLAB command failed.\nOutput: ${MATLAB_OUTPUT}\nError: ${MATLAB_ERROR}")
endif()

message("${MATLAB_OUTPUT}")
