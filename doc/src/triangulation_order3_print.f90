subroutine triangulation_order3_print ( node_num, triangle_num, node_xy, &
    triangle_node, triangle_neighbor )
  
  !*****************************************************************************80
  !
  !! TRIANGULATION_ORDER3_PRINT prints information about a Delaunay triangulation.
  !
  !  Discussion:
  !
  !    Triangulations created by R8TRIS2 include extra information encoded
  !    in the negative values of TRIANGLE_NEIGHBOR.
  !
  !    Because some of the nodes counted in NODE_NUM may not actually be
  !    used in the triangulation, I needed to compute the true number
  !    of vertices.  I added this calculation on 13 October 2001.
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license.
  !
  !  Modified:
  !
  !    26 November 2002
  !
  !  Author:
  !
  !    John Burkardt
  !
  !  Parameters:
  !
  !    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
  !
  !    Input, integer ( kind = 4 ) TRIANGLE_NUM, the number of triangles.
  !
  !    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
  !
  !    Input, integer ( kind = 4 ) TRIANGLE_NODE(3,TRIANGLE_NUM), the nodes that
  !    make up the triangles.
  !
  !    Input, integer ( kind = 4 ) TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), the
  !    triangle neighbors on each side.  If there is no triangle neighbor on a
  !    particular side, the value of TRIANGLE_NEIGHBOR should be negative.  If
  !    the triangulation data was created by R8TRIS2, then there is more
  !    information encoded in the negative values.
  !
    implicit none
  
    integer ( kind = 4 ), parameter :: dim_num = 2
    integer ( kind = 4 ) node_num
    integer ( kind = 4 ) triangle_num
  
    integer ( kind = 4 ) boundary_num
    integer ( kind = 4 ) i
    integer ( kind = 4 ) i4_wrap
    integer ( kind = 4 ) j
    integer ( kind = 4 ) k
    integer ( kind = 4 ) n1
    integer ( kind = 4 ) n2
    real ( kind = 8 ) node_xy(dim_num,node_num)
    integer ( kind = 4 ) s
    logical skip
    integer ( kind = 4 ) t
    integer ( kind = 4 ) triangle_node(3,triangle_num)
    integer ( kind = 4 ) triangle_neighbor(3,triangle_num)
    integer ( kind = 4 ), allocatable, dimension ( : ) :: vertex_list
    integer ( kind = 4 ) vertex_num
  
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRIANGULATION_ORDER3_PRINT'
    write ( *, '(a)' ) '  Information defining an order3 triangulation.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  The number of nodes is ', node_num
  
    call r8mat_transpose_print ( dim_num, node_num, node_xy, &
      '  Node coordinates' )
  
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  The number of triangles is ', triangle_num
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Sets of three nodes are used as vertices of'
    write ( *, '(a)' ) '  the triangles.  For each triangle, the nodes'
    write ( *, '(a)' ) '  are listed in counterclockwise order.'
  
    call i4mat_transpose_print ( 3, triangle_num, triangle_node, &
      '  Triangle nodes:' )
  
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  On each side of a given triangle, there is either'
    write ( *, '(a)' ) '  another triangle, or a piece of the convex hull.'
    write ( *, '(a)' ) '  For each triangle, we list the indices of the three'
    write ( *, '(a)' ) '  neighbors, or (if negative) the codes of the'
    write ( *, '(a)' ) '  segments of the convex hull.'
  
    call i4mat_transpose_print ( 3, triangle_num, triangle_neighbor, &
      '  Triangle neighbors' )
  !
  !  Determine the number of vertices.
  !
    allocate ( vertex_list(1:3*triangle_num) )
  
    vertex_list(1:3*triangle_num) = reshape ( triangle_node(1:3,1:triangle_num), &
      (/ 3*triangle_num /) )
  
    call i4vec_sort_heap_a ( 3*triangle_num, vertex_list )
  
    call i4vec_sorted_unique ( 3*triangle_num, vertex_list, vertex_num )
  
    deallocate ( vertex_list )
  !
  !  Determine the number of boundary points.
  !
    boundary_num = 2 * vertex_num - triangle_num - 2
  
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  The number of boundary points is ', boundary_num
  
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  The segments that make up the convex hull can be'
    write ( *, '(a)' ) '  determined from the negative entries of the triangle'
    write ( *, '(a)' ) '  neighbor list.'
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '     #   Tri  Side    N1    N2'
    write ( *, '(a)' ) ' '
  
    skip = .false.
  
    k = 0
  
    do i = 1, triangle_num
  
      do j = 1, 3
  
        if ( triangle_neighbor(j,i) < 0 ) then
          s = - triangle_neighbor(j,i)
          t = s / 3
  
          if ( t < 1 .or. triangle_num < t ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) '  Sorry, this data does not use the R8TRIS2'
            write ( *, '(a)' ) '  convention for convex hull segments.'
            skip = .true.
            exit
          end if
  
          s = mod ( s, 3 ) + 1
          k = k + 1
          n1 = triangle_node(s,t)
          n2 = triangle_node(i4_wrap(s+1,1,3),t)
          write ( *, '(2x,i4,2x,i4,2x,i4,2x,i4,2x,i4)' ) k, t, s, n1, n2
        end if
  
      end do
  
      if ( skip ) then
        exit
      end if
  
    end do
  
    return
end