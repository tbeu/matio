
find_package(Threads)

find_package(HDF5)
if (HDF5_FOUND)
   set(HAVE_HDF5 1)
endif()

find_package(ZLIB)

# FindHDF5.cmake does not expose a modern CMake Target

if (HDF5_FOUND AND NOT TARGET HDF5::HDF5)
    add_library(HDF5::HDF5 INTERFACE IMPORTED)
    set_target_properties(HDF5::HDF5 PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${HDF5_INCLUDE_DIRS}"
        INTERFACE_LINK_LIBRARIES "${HDF5_LIBRARIES}")
endif()
