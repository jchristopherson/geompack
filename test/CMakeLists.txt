include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

add_subdirectory(fortran_test_helper)

set(geompack_test_sources
    geompack_test.f90
    geompack_test_routines.f90
)

add_executable(geompack_test ${geompack_test_sources})
link_library(geompack_test ${PROJECT_NAME} ${PROJECT_INCLUDE_DIR})
link_library(geompack_test ${fortran_test_helper_LIBRARY} ${fortran_test_helper_INCLUDE_DIR})
add_test(
    NAME geompack_test
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:geompack_test>
)