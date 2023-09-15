function angle_rad_2d ( p1, p2, p3 )
  
    !*****************************************************************************80
    !
    !! ANGLE_RAD_2D returns the angle swept out between two rays in 2D.
    !
    !  Discussion:
    !
    !    Except for the zero angle case, it should be true that
    !
    !      ANGLE_RAD_2D ( P1, P2, P3 ) + ANGLE_RAD_2D ( P3, P2, P1 ) = 2 * PI
    !
    !        P1
    !        /
    !       /
    !      /
    !     /
    !    P2--------->P3
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    15 January 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( kind = 8 ) P1(2), P2(2), P3(2), define the rays
    !    P1 - P2 and P3 - P2 which define the angle.
    !
    !    Output, real ( kind = 8 ) ANGLE_RAD_2D, the angle swept out by the rays,
    !    in radians.  0 <= ANGLE_RAD_2D < 2 * PI.  If either ray has zero
    !    length, then ANGLE_RAD_2D is set to 0.
    !
      implicit none
    
      integer ( kind = 4 ), parameter :: dim_num = 2
    
      real ( kind = 8 ) angle_rad_2d
      real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
      real ( kind = 8 ) p(dim_num)
      real ( kind = 8 ) p1(dim_num)
      real ( kind = 8 ) p2(dim_num)
      real ( kind = 8 ) p3(dim_num)
    
      p(1) = ( p3(1) - p2(1) ) * ( p1(1) - p2(1) ) &
           + ( p3(2) - p2(2) ) * ( p1(2) - p2(2) )
    
    
      p(2) = ( p3(1) - p2(1) ) * ( p1(2) - p2(2) ) &
           - ( p3(2) - p2(2) ) * ( p1(1) - p2(1) )
    
      if ( p(1) == 0.0D+00 .and. p(2) == 0.0D+00 ) then
        angle_rad_2d = 0.0D+00
        return
      end if
    
      angle_rad_2d = atan2 ( p(2), p(1) )
    
      if ( angle_rad_2d < 0.0D+00 ) then
        angle_rad_2d = angle_rad_2d + 2.0D+00 * pi
      end if
    
      return
  end