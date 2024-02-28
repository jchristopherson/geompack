subroutine swapec ( i, top, btri, bedg, node_num, node_xy, triangle_num, &
    triangle_node, triangle_neighbor, stack, ierr )
  
  !*****************************************************************************80
  !
  !! SWAPEC swaps diagonal edges until all triangles are Delaunay.
  !
  !  Discussion:
  !
  !    The routine swaps diagonal edges in a 2D triangulation, based on
  !    the empty circumcircle criterion, until all triangles are Delaunay,
  !    given that I is the index of the new vertex added to the triangulation.
  !
  !  Modified:
  !
  !    14 July 2001
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
  !    Input, integer ( kind = 4 ) I, the index of the new vertex.
  !
  !    Input/output, integer ( kind = 4 ) TOP, the index of the top of the stack.
  !    On output, TOP is zero.
  !
  !    Input/output, integer ( kind = 4 ) BTRI, BEDG; on input, if positive, are
  !    the triangle and edge indices of a boundary edge whose updated indices
  !    must be recorded.  On output, these may be updated because of swaps.
  !
  !    Input, integer ( kind = 4 ) NODE_NUM, the number of points.
  !
  !    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of
  !    the points.
  !
  !    Input, integer ( kind = 4 ) TRIANGLE_NUM, the number of triangles.
  !
  !    Input/output, integer ( kind = 4 ) TRIANGLE_NODE(3,TRIANGLE_NUM), the 
  !    triangle incidence list.  May be updated on output because of swaps.
  !
  !    Input/output, integer ( kind = 4 ) TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM),
  !    the triangle neighbor list; negative values are used for links of the
  !    counter-clockwise linked list of boundary edges;  May be updated on output
  !    because of swaps.
  !    LINK = -(3*I + J-1) where I, J = triangle, edge index.
  !
  !    Workspace, integer ( kind = 4 ) STACK(MAXST); on input, entries 1 through 
  !    TOP contain the indices of initial triangles (involving vertex I)
  !    put in stack; the edges opposite I should be in interior;  entries
  !    TOP+1 through MAXST are used as a stack.
  !
  !    Output, integer ( kind = 4 ) IERR is set to 8 for abnormal return.
  !
    implicit none
  
    integer ( kind = 4 ) node_num
    integer ( kind = 4 ) triangle_num
  
    integer ( kind = 4 ) a
    integer ( kind = 4 ) b
    integer ( kind = 4 ) bedg
    integer ( kind = 4 ) btri
    integer ( kind = 4 ) c
    integer ( kind = 4 ) diaedg
    integer ( kind = 4 ) e
    integer ( kind = 4 ) ee
    integer ( kind = 4 ) em1
    integer ( kind = 4 ) ep1
    integer ( kind = 4 ) f
    integer ( kind = 4 ) fm1
    integer ( kind = 4 ) fp1
    integer ( kind = 4 ) i
    integer ( kind = 4 ) ierr
    integer ( kind = 4 ) i4_wrap
    integer ( kind = 4 ) l
    real ( kind = 8 ) node_xy(2,node_num)
    integer ( kind = 4 ) r
    integer ( kind = 4 ) s
    integer ( kind = 4 ) stack(node_num)
    integer ( kind = 4 ) swap
    integer ( kind = 4 ) t
    integer ( kind = 4 ) top
    integer ( kind = 4 ) triangle_neighbor(3,triangle_num)
    integer ( kind = 4 ) triangle_node(3,triangle_num)
    integer ( kind = 4 ) tt
    integer ( kind = 4 ) u
    real ( kind = 8 ) x
    real ( kind = 8 ) y
  !
  !  Determine whether triangles in stack are Delaunay, and swap
  !  diagonal edge of convex quadrilateral if not.
  !
    x = node_xy(1,i)
    y = node_xy(2,i)
  
    do
  
      if ( top <= 0 ) then
        exit
      end if
  
      t = stack(top)
      top = top - 1
  
      if ( triangle_node(1,t) == i ) then
        e = 2
        b = triangle_node(3,t)
      else if ( triangle_node(2,t) == i ) then
        e = 3
        b = triangle_node(1,t)
      else
        e = 1
        b = triangle_node(2,t)
      end if
  
      a = triangle_node(e,t)
      u = triangle_neighbor(e,t)
  
      if ( triangle_neighbor(1,u) == t ) then
        f = 1
        c = triangle_node(3,u)
      else if ( triangle_neighbor(2,u) == t ) then
        f = 2
        c = triangle_node(1,u)
      else
        f = 3
        c = triangle_node(2,u)
      end if
  
      swap = diaedg ( x, y, node_xy(1,a), node_xy(2,a), node_xy(1,c), &
        node_xy(2,c), node_xy(1,b), node_xy(2,b) )
  
      if ( swap == 1 ) then
  
        em1 = i4_wrap ( e - 1, 1, 3 )
        ep1 = i4_wrap ( e + 1, 1, 3 )
        fm1 = i4_wrap ( f - 1, 1, 3 )
        fp1 = i4_wrap ( f + 1, 1, 3 )
  
        triangle_node(ep1,t) = c
        triangle_node(fp1,u) = i
        r = triangle_neighbor(ep1,t)
        s = triangle_neighbor(fp1,u)
        triangle_neighbor(ep1,t) = u
        triangle_neighbor(fp1,u) = t
        triangle_neighbor(e,t) = s
        triangle_neighbor(f,u) = r
  
        if ( 0 < triangle_neighbor(fm1,u) ) then
          top = top + 1
          stack(top) = u
        end if
  
        if ( 0 < s ) then
  
          if ( triangle_neighbor(1,s) == u ) then
            triangle_neighbor(1,s) = t
          else if ( triangle_neighbor(2,s) == u ) then
            triangle_neighbor(2,s) = t
          else
            triangle_neighbor(3,s) = t
          end if
  
          top = top + 1
  
          if ( node_num < top ) then
            ierr = 8
            return
          end if
  
          stack(top) = t
  
        else
  
          if ( u == btri .and. fp1 == bedg ) then
            btri = t
            bedg = e
          end if
  
          l = - ( 3 * t + e - 1 )
          tt = t
          ee = em1
  
          do while ( 0 < triangle_neighbor(ee,tt) )
  
            tt = triangle_neighbor(ee,tt)
  
            if ( triangle_node(1,tt) == a ) then
              ee = 3
            else if ( triangle_node(2,tt) == a ) then
              ee = 1
            else
              ee = 2
            end if
  
          end do
  
          triangle_neighbor(ee,tt) = l
  
        end if
  
        if ( 0 < r ) then
  
          if ( triangle_neighbor(1,r) == t ) then
            triangle_neighbor(1,r) = u
          else if ( triangle_neighbor(2,r) == t ) then
            triangle_neighbor(2,r) = u
          else
            triangle_neighbor(3,r) = u
          end if
  
        else
  
          if ( t == btri .and. ep1 == bedg ) then
            btri = u
            bedg = f
          end if
  
          l = - ( 3 * u + f - 1 )
          tt = u
          ee = fm1
  
          do while ( 0 < triangle_neighbor(ee,tt) )
  
            tt = triangle_neighbor(ee,tt)
  
            if ( triangle_node(1,tt) == b ) then
              ee = 3
            else if ( triangle_node(2,tt) == b ) then
              ee = 1
            else
              ee = 2
            end if
  
          end do
  
          triangle_neighbor(ee,tt) = l
  
        end if
  
      end if
  
    end do
  
    return
end