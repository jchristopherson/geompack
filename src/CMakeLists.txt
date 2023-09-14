# Locate the source directory
set(dir ${CMAKE_CURRENT_SOURCE_DIR})

# Define the source files
set(GEOMPACK_SOURCES
    ${dir}/geompack.f90
    ${dir}/alpha_measure.f90
    ${dir}/angle_rad_2d.f90
    ${dir}/diaedg.f90
    ${dir}/get_unit.f90
    ${dir}/i4_modp.f90
    ${dir}/i4_swap.f90
    ${dir}/i4_wrap.f90
    ${dir}/i4mat_transpose_print.f90
    ${dir}/i4mat_transpose_print_some.f90
    ${dir}/i4vec_heap_d.f90
    ${dir}/i4vec_sort_heap_a.f90
    ${dir}/i4vec_sorted_unique.f90
    ${dir}/lrline.f90
    ${dir}/perm_check.f90
    ${dir}/perm_inverse.f90
    ${dir}/points_delaunay_naive_2d.f90
    ${dir}/points_hull_2d.f90
    ${dir}/quad_convex_random.f90
    ${dir}/r8_acos.f90
    ${dir}/r82vec_part_quick_a.f90
    ${dir}/r82vec_permute.f90
    ${dir}/r82vec_sort_heap_index_a.f90
    ${dir}/r82vec_sort_quick_a.f90
    ${dir}/r8mat_transpose_print.f90
    ${dir}/r8mat_transpose_print_some.f90
    ${dir}/r8mat_uniform_01.f90
    ${dir}/r8tris2.f90
    ${dir}/r8vec_eq.f90
    ${dir}/r8vec_gt.f90
    ${dir}/r8vec_lt.f90
    ${dir}/r8vec_print.f90
    ${dir}/r8vec_swap.f90
    ${dir}/swapec.f90
    ${dir}/timestamp.f90
    ${dir}/triangle_circumcenter_2d.f90
    ${dir}/triangulation_order3_plot.f90
    ${dir}/triangulation_order3_print.f90
    ${dir}/vbedg.f90
)
set(GEOMPACK_SOURCES ${GEOMPACK_SOURCES} PARENT_SCOPE)