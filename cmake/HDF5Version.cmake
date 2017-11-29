#   If the HDF5 config file already provided the variable HDF5_VERSION_STRING,
#   there is nothing to do.

if (NOT HDF5_VERSION_STRING)
    function(HDF5_GET_VERSION VARNAME)
        try_run(RRESULT CRESULT
                ${CMAKE_BINARY_DIR}/cmake
                ${CMAKE_CURRENT_SOURCE_DIR}/cmake/HDF5Version.c
                CMAKE_FLAGS -DINCLUDE_DIRECTORIES::STRING=${HDF5_INCLUDE_DIRS}
                RUN_OUTPUT_VARIABLE HDF5VERS)
        if (NOT ${CRESULT})
            message(FATAL "Unable to compile a simple hdf5 program. Check your installation.")
        endif()
        if (NOT ${RRESULT} EQUAL 0)
            message(FATAL "Executing a simple hdf5 program.")
        endif()
        set(${VARNAME} ${HDF5VERS} PARENT_SCOPE)
    endfunction()
    HDF5_GET_VERSION(HDF5_VERSION_STRING)
endif()
