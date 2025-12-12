set(geompack_test_sources
    geompack_test.f90
    geompack_test_routines.f90
)

find_package(fortran_test_helper QUIET)
if (NOT fortran_test_helper_FOUND)
    include(FetchContent)
    FetchContent_Declare(
        fortran_test_helper
        GIT_TAG "origin/main"
        GIT_REPOSITORY https://github.com/jchristopherson/fortran_test_helper
    )
    FetchContent_MakeAvailable(fortran_test_helper)
    set(FTH_LIBRARIES fortran_test_helper)
else()
    set(FTH_LIBRARIES fortran_test_helper::fortran_test_helper)
endif()

add_executable(geompack_test ${geompack_test_sources})
target_link_libraries(
    geompack_test
    geompack
    ${FTH_LIBRARIES}
)
add_test(
    NAME geompack_test
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:geompack_test>
)


if (${BUILD_SHARED_LIBS} AND WIN32)
    add_custom_command(
        TARGET geompack_test
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy 
        $<TARGET_RUNTIME_DLLS:geompack_test> 
        $<TARGET_FILE_DIR:geompack_test>
        COMMAND_EXPAND_LISTS
    )
endif()
