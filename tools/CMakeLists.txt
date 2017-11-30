add_executable(matdump matdump.c)
target_link_libraries(matdump matio ${GETOPT_LIB} ${LIBM})
