subroutine vbedg ( x, y, node_num, node_xy, triangle_num, triangle_node, &
    triangle_neighbor, ltri, ledg, rtri, redg )
  
  !*****************************************************************************80
  !
  !! VBEDG determines which boundary edges are visible to a point.
  !
  !  Discussion:
  !
  !    The point (X,Y) is assumed to be outside the convex hull of the
  !    region covered by the 2D triangulation.
  !
  !  Modified:
  !
  !    25 August 2001
  !
  !  Author:
  !
  !    Original FORTRAN77 version by Barry Joe.
  !    FORTRAN90 version by John Burkardt.
  !
  !  Reference:
  !
  !    Barry Joe,
  !    GEOMPACK - a software package for the generation of meshes
  !    using geometric algorithms,
  !    Advances in Engineering Software,
  !    Volume 13, pages 325-331, 1991.
  !
  !  Parameters:
  !
  !    Input, real ( kind = 8 ) X, Y, the coordinates of a point outside the
  !    convex hull of the current triangulation.
  !
  !    Input, integer ( kind = 4 ) NODE_NUM, the number of points.
  !
  !    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the
  !    vertices.
  !
  !    Input, integer ( kind = 4 ) TRIANGLE_NUM, the number of triangles.
  !
  !    Input, integer ( kind = 4 ) TRIANGLE_NODE(3,TRIANGLE_NUM), the
  !    triangle incidence list.
  !
  !    Input, integer ( kind = 4 ) TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM), the
  !    triangle neighbor list; negative values are used for links of a
  !    counterclockwise linked list of boundary edges;
  !      LINK = -(3*I + J-1) where I, J = triangle, edge index.
  !
  !    Input/output, integer ( kind = 4 ) LTRI, LEDG.  If LTRI /= 0 then these
  !    values are assumed to be already computed and are not changed, else they
  !    are updated.  On output, LTRI is the index of boundary triangle to the
  !    left of the leftmost boundary triangle visible from (X,Y), and LEDG is
  !    the boundary edge of triangle LTRI to the left of the leftmost boundary
  !    edge visible from (X,Y).  1 <= LEDG <= 3.
  !
  !    Input/output, integer ( kind = 4 ) RTRI.  On input, the index of the
  !    boundary triangle to begin the search at.  On output, the index of the
  !    rightmost boundary triangle visible from (X,Y).
  !
  !    Input/output, integer ( kind = 4 ) REDG, the edge of triangle RTRI that
  !    is visible from (X,Y).  1 <= REDG <= 3.
  !
    implicit none
  
    integer ( kind = 4 ), parameter :: dim_num = 2
    integer ( kind = 4 ) node_num
    integer ( kind = 4 ) triangle_num
  
    integer ( kind = 4 ) a
    integer ( kind = 4 ) b
    integer ( kind = 4 ) e
    integer ( kind = 4 ) i4_wrap
    integer ( kind = 4 ) l
    logical ldone
    integer ( kind = 4 ) ledg
    integer ( kind = 4 ) lr
    integer ( kind = 4 ) lrline
    integer ( kind = 4 ) ltri
    real ( kind = 8 ) node_xy(2,node_num)
    integer ( kind = 4 ) redg
    integer ( kind = 4 ) rtri
    integer ( kind = 4 ) t
    integer ( kind = 4 ) triangle_neighbor(3,triangle_num)
    integer ( kind = 4 ) triangle_node(3,triangle_num)
    real ( kind = 8 ) x
    real ( kind = 8 ) y
  !
  !  Find the rightmost visible boundary edge using links, then possibly
  !  leftmost visible boundary edge using triangle neighbor information.
  !
    if ( ltri == 0 ) then
      ldone = .false.
      ltri = rtri
      ledg = redg
    else
      ldone = .true.
    end if
  
    do
  
      l = -triangle_neighbor(redg,rtri)
      t = l / 3
      e = mod ( l, 3 ) + 1
      a = triangle_node(e,t)
  
      if ( e <= 2 ) then
        b = triangle_node(e+1,t)
      else
        b = triangle_node(1,t)
      end if
  
      lr = lrline ( x, y, node_xy(1,a), node_xy(2,a), node_xy(1,b), &
        node_xy(2,b), 0.0D+00 )
  
      if ( lr <= 0 ) then
        exit
      end if
  
      rtri = t
      redg = e
  
    end do
  
    if ( ldone ) then
      return
    end if
  
    t = ltri
    e = ledg
  
    do
  
      b = triangle_node(e,t)
      e = i4_wrap ( e-1, 1, 3 )
  
      do while ( 0 < triangle_neighbor(e,t) )
  
        t = triangle_neighbor(e,t)
  
        if ( triangle_node(1,t) == b ) then
          e = 3
        else if ( triangle_node(2,t) == b ) then
          e = 1
        else
          e = 2
        end if
  
      end do
  
      a = triangle_node(e,t)
  
      lr = lrline ( x, y, node_xy(1,a), node_xy(2,a), node_xy(1,b), &
        node_xy(2,b), 0.0D+00 )
  
      if ( lr <= 0 ) then
        exit
      end if
  
    end do
  
    ltri = t
    ledg = e
  
    return
end