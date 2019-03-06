enable_testing()

add_executable(test_mat ${PROJECT_SOURCE_DIR}/test/test_mat.c)
target_link_libraries(test_mat matio)

add_executable(test_snprintf ${PROJECT_SOURCE_DIR}/test/test_snprintf.c)
target_link_libraries(test_snprintf matio)
