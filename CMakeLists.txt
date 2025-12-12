cmake_minimum_required(VERSION 3.24)
project(
    geompack
    LANGUAGES Fortran
    VERSION 1.0.3
)
set(CMAKE_Fortran_STANDARD 2018)
set(CMAKE_Fortran_STANDARD_REQUIRED TRUE)

add_subdirectory(configure)
add_subdirectory(src)

# Testing
option(BUILD_TESTING "Build GEOMPACK tests")
include(CTest)
message(STATUS "Build GEOMPACK tests: ${BUILD_TESTING}")
if (BUILD_TESTING)
    enable_testing()
    add_subdirectory(test)
endif()