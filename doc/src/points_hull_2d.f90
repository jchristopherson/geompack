subroutine points_hull_2d ( node_num, node_xy, hull_num, hull )
  
    !*****************************************************************************80
    !
    !! POINTS_HULL_2D computes the convex hull of 2D points.
    !
    !  Discussion:
    !
    !    The work involved is N*log(H), where N is the number of points, and H is
    !    the number of points that are on the hull.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    12 June 2006
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
    !
    !    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
    !
    !    Output, integer ( kind = 4 ) HULL_NUM, the number of nodes that lie on
    !    the convex hull.
    !
    !    Output, integer ( kind = 4 ) HULL(NODE_NUM).  Entries 1 through HULL_NUM
    !    contain the indices of the nodes that form the convex hull, in order.
    !
      implicit none
    
      integer ( kind = 4 ) node_num
    
      real ( kind = 8 ) angle
      real ( kind = 8 ) angle_max
      real ( kind = 8 ) angle_rad_2d
      real ( kind = 8 ) di
      real ( kind = 8 ) dr
      integer ( kind = 4 ) first
      integer ( kind = 4 ) hull(node_num)
      integer ( kind = 4 ) hull_num
      integer ( kind = 4 ) i
      real ( kind = 8 ) node_xy(2,node_num)
      real ( kind = 8 ) p_xy(2)
      integer ( kind = 4 ) q
      real ( kind = 8 ) q_xy(2)
      integer ( kind = 4 ) r
      real ( kind = 8 ) r_xy(2)
    
      if ( node_num < 1 ) then
        hull_num = 0
        return
      end if
    !
    !  If NODE_NUM = 1, the hull is the point.
    !
      if ( node_num == 1 ) then
        hull_num = 1
        hull(1) = 1
        return
      end if
    !
    !  If NODE_NUM = 2, then the convex hull is either the two distinct points,
    !  or possibly a single (repeated) point.
    !
      if ( node_num == 2 ) then
    
        if ( node_xy(1,1) /= node_xy(1,2) .or. node_xy(2,1) /= node_xy(2,2) ) then
          hull_num = 2
          hull(1) = 1
          hull(2) = 2
        else
          hull_num = 1
          hull(1) = 1
        end if
    
        return
    
      end if
    !
    !  Find the leftmost point and call it "Q".
    !  In case of ties, take the bottom-most.
    !
      q = 1
      do i = 2, node_num
        if ( node_xy(1,i) < node_xy(1,q) .or. &
           ( node_xy(1,i) == node_xy(1,q) .and. node_xy(2,i) < node_xy(2,q) ) ) then
          q = i
        end if
      end do
    
      q_xy(1:2) = node_xy(1:2,q)
    !
    !  Remember the starting point, so we know when to stop!
    !
      first = q
      hull_num = 1
      hull(1) = q
    !
    !  For the first point, make a dummy previous point, 1 unit south,
    !  and call it "P".
    !
      p_xy(1) = q_xy(1)
      p_xy(2) = q_xy(2) - 1.0D+00
    !
    !  Now, having old point P, and current point Q, find the new point R
    !  so the angle PQR is maximal.
    !
    !  Watch out for the possibility that the two nodes are identical.
    !
      do
    
        r = 0
        angle_max = 0.0D+00
    
        do i = 1, node_num
    
          if ( i /= q .and. &
               ( node_xy(1,i) /= q_xy(1) .or. node_xy(2,i) /= q_xy(2) ) ) then
    
            angle = angle_rad_2d ( p_xy, q_xy, node_xy(1:2,i) )
    
            if ( r == 0 .or. angle_max < angle ) then
    
              r = i
              r_xy(1:2) = node_xy(1:2,r)
              angle_max = angle
    !
    !  In case of ties, choose the nearer point.
    !
            else if ( r /= 0 .and. angle == angle_max ) then
    
              di = ( node_xy(1,i) - q_xy(1) )**2 + ( node_xy(2,i) - q_xy(2) )**2
              dr = ( r_xy(1)      - q_xy(1) )**2 + ( r_xy(2)      - q_xy(2) )**2
    
              if ( di < dr ) then
                r = i
                r_xy(1:2) = node_xy(1:2,r)
                angle_max = angle
              end if
    
            end if
    
          end if
    
        end do
    !
    !  We are done when we have returned to the first point on the convex hull.
    !
        if ( r == first ) then
          exit
        end if
    
        hull_num = hull_num + 1
    
        if ( node_num < hull_num ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'POINTS_HULL_2D - Fatal error!'
          write ( *, '(a)' ) '  The algorithm has failed.'
          stop
        end if
    !
    !  Add point R to convex hull.
    !
        hull(hull_num) = r
    !
    !  Set P := Q, Q := R, and prepare to search for next point R.
    !
        q = r
    
        p_xy(1:2) = q_xy(1:2)
        q_xy(1:2) = r_xy(1:2)
    
      end do
    
      return
end