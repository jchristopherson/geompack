subroutine r8tris2 ( node_num, node_xy, triangle_num, triangle_node, &
    triangle_neighbor )
  
  !*****************************************************************************80
  !
  !! R8TRIS2 constructs a Delaunay triangulation of 2D vertices.
  !
  !  Discussion:
  !
  !    The routine constructs the Delaunay triangulation of a set of 2D vertices
  !    using an incremental approach and diagonal edge swaps.  Vertices are
  !    first sorted in lexicographically increasing (X,Y) order, and
  !    then are inserted one at a time from outside the convex hull.
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
  !    Input, integer ( kind = 4 ) NODE_NUM, the number of vertices.
  !
  !    Input/output, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates
  !    of the vertices.  On output, the vertices have been sorted into
  !    dictionary order.
  !
  !    Output, integer ( kind = 4 ) TRIANGLE_NUM, the number of triangles in
  !    the triangulation;  TRIANGLE_NUM is equal to 2*NODE_NUM - NB - 2, where
  !    NB is the number of boundary vertices.
  !
  !    Output, integer ( kind = 4 ) TRIANGLE_NODE(3,TRIANGLE_NUM), the nodes
  !    that make up each triangle.  The elements are indices of P.  The vertices
  !    of the triangles are in counter clockwise order.
  !
  !    Output, integer ( kind = 4 ) TRIANGLE_NEIGHBOR(3,TRIANGLE_NUM),
  !    the triangle neighbor list.  Positive elements are indices of TIL;
  !    negative elements are used for links of a counter clockwise linked list
  !    of boundary edges; LINK = -(3*I + J-1) where I, J = triangle, edge index;
  !    TRIANGLE_NEIGHBOR(J,I) refers to the neighbor along edge from vertex J
  !    to J+1 (mod 3).
  !
    implicit none
  
    integer ( kind = 4 ) node_num
  
    real ( kind = 8 ) cmax
    integer ( kind = 4 ) e
    integer ( kind = 4 ) i
    integer ( kind = 4 ) ierr
    integer ( kind = 4 ) indx(node_num)
    integer ( kind = 4 ) j
    integer ( kind = 4 ) k
    integer ( kind = 4 ) l
    integer ( kind = 4 ) ledg
    integer ( kind = 4 ) lr
    integer ( kind = 4 ) lrline
    integer ( kind = 4 ) ltri
    integer ( kind = 4 ) m
    integer ( kind = 4 ) m1
    integer ( kind = 4 ) m2
    integer ( kind = 4 ) n
    real ( kind = 8 ) node_xy(2,node_num)
    integer ( kind = 4 ) redg
    integer ( kind = 4 ) rtri
    integer ( kind = 4 ) stack(node_num)
    integer ( kind = 4 ) t
    real ( kind = 8 ) tol
    integer ( kind = 4 ) top
    integer ( kind = 4 ) triangle_neighbor(3,node_num*2)
    integer ( kind = 4 ) triangle_num
    integer ( kind = 4 ) triangle_node(3,node_num*2)
  
    tol = 100.0D+00 * epsilon ( tol )
  
    ierr = 0
  !
  !  Sort the vertices by increasing (x,y).
  !
    call r82vec_sort_heap_index_a ( node_num, node_xy, indx )
  
    call r82vec_permute ( node_num, node_xy, indx )
  !
  !  Make sure that the data points are "reasonably" distinct.
  !
    m1 = 1
  
    do i = 2, node_num
  
      m = m1
      m1 = i
  
      k = 0
  
      do j = 1, 2
  
        cmax = max ( abs ( node_xy(j,m) ), abs ( node_xy(j,m1) ) )
  
        if ( tol * ( cmax + 1.0D+00 ) &
             < abs ( node_xy(j,m) - node_xy(j,m1) ) ) then
          k = j
          exit
        end if
  
      end do
  
      if ( k == 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8TRIS2 - Fatal error!'
        write ( *, '(a,i8)' ) '  Fails for point number I = ', i
        write ( *, '(a,i8)' ) '  M = ', m
        write ( *, '(a,i8)' ) '  M1 = ', m1
        write ( *, '(a,2g14.6)' ) '  X,Y(M)  = ', node_xy(1:2,m)
        write ( *, '(a,2g14.6)' ) '  X,Y(M1) = ', node_xy(1:2,m1)
        ierr = 224
        return
      end if
  
    end do
  !
  !  Starting from points M1 and M2, search for a third point M that
  !  makes a "healthy" triangle (M1,M2,M)
  !
    m1 = 1
    m2 = 2
    j = 3
  
    do
  
      if ( node_num < j ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8TRIS2 - Fatal error!'
        ierr = 225
        return
      end if
  
      m = j
  
      lr = lrline ( node_xy(1,m), node_xy(2,m), node_xy(1,m1), &
        node_xy(2,m1), node_xy(1,m2), node_xy(2,m2), 0.0D+00 )
  
      if ( lr /= 0 ) then
        exit
      end if
  
      j = j + 1
  
    end do
  !
  !  Set up the triangle information for (M1,M2,M), and for any other
  !  triangles you created because points were collinear with M1, M2.
  !
    triangle_num = j - 2
  
    if ( lr == -1 ) then
  
      triangle_node(1,1) = m1
      triangle_node(2,1) = m2
      triangle_node(3,1) = m
      triangle_neighbor(3,1) = -3
  
      do i = 2, triangle_num
  
        m1 = m2
        m2 = i+1
        triangle_node(1,i) = m1
        triangle_node(2,i) = m2
        triangle_node(3,i) = m
        triangle_neighbor(1,i-1) = -3 * i
        triangle_neighbor(2,i-1) = i
        triangle_neighbor(3,i) = i - 1
  
      end do
  
      triangle_neighbor(1,triangle_num) = -3 * triangle_num - 1
      triangle_neighbor(2,triangle_num) = -5
      ledg = 2
      ltri = triangle_num
  
    else
  
      triangle_node(1,1) = m2
      triangle_node(2,1) = m1
      triangle_node(3,1) = m
      triangle_neighbor(1,1) = -4
  
      do i = 2, triangle_num
        m1 = m2
        m2 = i+1
        triangle_node(1,i) = m2
        triangle_node(2,i) = m1
        triangle_node(3,i) = m
        triangle_neighbor(3,i-1) = i
        triangle_neighbor(1,i) = -3 * i - 3
        triangle_neighbor(2,i) = i - 1
      end do
  
      triangle_neighbor(3,triangle_num) = -3 * triangle_num
      triangle_neighbor(2,1) = -3 * triangle_num - 2
      ledg = 2
      ltri = 1
  
    end if
  !
  !  Insert the vertices one at a time from outside the convex hull,
  !  determine visible boundary edges, and apply diagonal edge swaps until
  !  Delaunay triangulation of vertices (so far) is obtained.
  !
    top = 0
  
    do i = j+1, node_num
  
      m = i
      m1 = triangle_node(ledg,ltri)
  
      if ( ledg <= 2 ) then
        m2 = triangle_node(ledg+1,ltri)
      else
        m2 = triangle_node(1,ltri)
      end if
  
      lr = lrline ( node_xy(1,m), node_xy(2,m), node_xy(1,m1), &
        node_xy(2,m1), node_xy(1,m2), node_xy(2,m2), 0.0D+00 )
  
      if ( 0 < lr ) then
        rtri = ltri
        redg = ledg
        ltri = 0
      else
        l = -triangle_neighbor(ledg,ltri)
        rtri = l / 3
        redg = mod(l,3) + 1
      end if
  
      call vbedg ( node_xy(1,m), node_xy(2,m), node_num, node_xy, triangle_num, &
        triangle_node, triangle_neighbor, ltri, ledg, rtri, redg )
  
      n = triangle_num + 1
      l = -triangle_neighbor(ledg,ltri)
  
      do
  
        t = l / 3
        e = mod ( l, 3 ) + 1
        l = -triangle_neighbor(e,t)
        m2 = triangle_node(e,t)
  
        if ( e <= 2 ) then
          m1 = triangle_node(e+1,t)
        else
          m1 = triangle_node(1,t)
        end if
  
        triangle_num = triangle_num + 1
        triangle_neighbor(e,t) = triangle_num
        triangle_node(1,triangle_num) = m1
        triangle_node(2,triangle_num) = m2
        triangle_node(3,triangle_num) = m
        triangle_neighbor(1,triangle_num) = t
        triangle_neighbor(2,triangle_num) = triangle_num - 1
        triangle_neighbor(3,triangle_num) = triangle_num + 1
        top = top + 1
  
        if ( node_num < top ) then
          ierr = 8
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8TRIS2 - Fatal error!'
          write ( *, '(a)' ) '  Stack overflow.'
          return
        end if
  
        stack(top) = triangle_num
  
        if ( t == rtri .and. e == redg ) then
          exit
        end if
  
      end do
  
      triangle_neighbor(ledg,ltri) = -3 * n - 1
      triangle_neighbor(2,n) = -3 * triangle_num - 2
      triangle_neighbor(3,triangle_num) = -l
      ltri = n
      ledg = 2
  
      call swapec ( m, top, ltri, ledg, node_num, node_xy, triangle_num, &
        triangle_node, triangle_neighbor, stack, ierr )
  
      if ( ierr /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8TRIS2 - Fatal error!'
        write ( *, '(a)' ) '  Error return from SWAPEC.'
        return
      end if
  
    end do
  !
  !  Now account for the sorting that we did.
  !
    do i = 1, 3
      do j = 1, triangle_num
        triangle_node(i,j) = indx ( triangle_node(i,j) )
      end do
    end do
  
    call perm_inverse ( node_num, indx )
  
    call r82vec_permute ( node_num, node_xy, indx )
  
    return
end