# Locate the source directory
set(dir ${CMAKE_CURRENT_SOURCE_DIR})

# Define the source files
set(GEOMPACK_SOURCES
    ${dir}/geompack.f90
)
set(GEOMPACK_SOURCES ${GEOMPACK_SOURCES} PARENT_SCOPE)