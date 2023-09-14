subroutine quad_convex_random ( seed, xy )
  
    !*****************************************************************************80
    !
    !! QUAD_CONVEX_RANDOM returns a random convex quadrilateral.
    !
    !  Description:
    !
    !    The quadrilateral is constrained in that the vertices must all lie
    !    with the unit square.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    26 June 2009
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
    !    generator.
    !
    !    Output, real ( kind = 8 ) XY(2,NODE_NUM), the coordinates of the
    !    nodes of the quadrilateral, given in counterclockwise order.
    !
      implicit none
    
      integer ( kind = 4 ), parameter :: node_num = 4
    
      integer ( kind = 4 ) hull(node_num)
      integer ( kind = 4 ) hull_num
      integer ( kind = 4 ) j
      integer ( kind = 4 ) seed
      real ( kind = 8 ) xy(2,node_num)
      real ( kind = 8 ) xy_random(2,node_num)
    
      do
    !
    !  Generate 4 random points.
    !
        call r8mat_uniform_01 ( 2, node_num, seed, xy_random )
    !
    !  Determine the convex hull.
    !
        call points_hull_2d ( node_num, xy_random, hull_num, hull )
    !
    !  If HULL_NUM < NODE_NUM, then our convex hull is a triangle.
    !  Try again.
    !
        if ( hull_num == node_num ) then
          exit
        end if
    
      end do
    !
    !  Make an ordered copy of the random points.
    !
      do j = 1, node_num
        xy(1:2,j) = xy_random(1:2,hull(j))
      end do
    
      return
end