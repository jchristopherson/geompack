subroutine alpha_measure ( n, z, triangle_order, triangle_num, triangle_node, &
    alpha_min, alpha_ave, alpha_area )
  
  !*****************************************************************************80
  !
  !! ALPHA_MEASURE determines the triangulated pointset quality measure ALPHA.
  !
  !  Discusion:
  !
  !    The ALPHA measure evaluates the uniformity of the shapes of the triangles
  !    defined by a triangulated pointset.
  !
  !    We compute the minimum angle among all the triangles in the triangulated
  !    dataset and divide by the maximum possible value (which, in degrees,
  !    is 60).  The best possible value is 1, and the worst 0.  A good
  !    triangulation should have an ALPHA score close to 1.
  !
  !    The code has been modified to 'allow' 6-node triangulations.
  !    However, no effort is made to actually process the midside nodes.
  !    Only information from the vertices is used.
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license.
  !
  !  Modified:
  !
  !    21 June 2009
  !
  !  Author:
  !
  !    John Burkardt
  !
  !  Parameters:
  !
  !    Input, integer ( kind = 4 ) N, the number of points.
  !
  !    Input, real ( kind = 8 ) Z(2,N), the points.
  !
  !    Input, integer ( kind = 4 ) TRIANGLE_ORDER, the order of the triangles.
  !
  !    Input, integer ( kind = 4 ) TRIANGLE_NUM, the number of triangles.
  !
  !    Input, integer ( kind = 4 ) TRIANGLE_NODE(TRIANGLE_ORDER,TRIANGLE_NUM),
  !    the triangulation.
  !
  !    Output, real ( kind = 8 ) ALPHA_MIN, the minimum value of ALPHA over all
  !    triangles.
  !
  !    Output, real ( kind = 8 ) ALPHA_AVE, the value of ALPHA averaged over
  !    all triangles.
  !
  !    Output, real ( kind = 8 ) ALPHA_AREA, the value of ALPHA averaged over
  !    all triangles and weighted by area.
  !
    implicit none
  
    integer ( kind = 4 ) n
    integer ( kind = 4 ) triangle_num
    integer ( kind = 4 ) triangle_order
  
    real ( kind = 8 ) a_angle
    integer ( kind = 4 ) a_index
    real ( kind = 8 ) a_x
    real ( kind = 8 ) a_y
    real ( kind = 8 ) ab_len
    real ( kind = 8 ) alpha
    real ( kind = 8 ) alpha_area
    real ( kind = 8 ) alpha_ave
    real ( kind = 8 ) alpha_min
    real ( kind = 8 ) acos
    real ( kind = 8 ) area
    real ( kind = 8 ) area_total
    real ( kind = 8 ) b_angle
    integer ( kind = 4 ) b_index
    real ( kind = 8 ) b_x
    real ( kind = 8 ) b_y
    real ( kind = 8 ) bc_len
    real ( kind = 8 ) c_angle
    integer ( kind = 4 ) c_index
    real ( kind = 8 ) c_x
    real ( kind = 8 ) c_y
    real ( kind = 8 ) ca_len
    real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
    integer ( kind = 4 ) triangle
    integer ( kind = 4 ) triangle_node(triangle_order,triangle_num)
    real ( kind = 8 ) z(2,n)
  
    alpha_min = huge ( alpha )
    alpha_ave = 0.0D+00
    alpha_area = 0.0D+00
    area_total = 0.0D+00
  
    do triangle = 1, triangle_num
  
      a_index = triangle_node(1,triangle)
      b_index = triangle_node(2,triangle)
      c_index = triangle_node(3,triangle)
  
      a_x = z(1,a_index)
      a_y = z(2,a_index)
      b_x = z(1,b_index)
      b_y = z(2,b_index)
      c_x = z(1,c_index)
      c_y = z(2,c_index)
  
      area = 0.5D+00 * abs ( a_x * ( b_y - c_y ) &
                           + b_x * ( c_y - a_y ) &
                           + c_x * ( a_y - b_y ) )
  
      ab_len = sqrt ( ( a_x - b_x )**2 + ( a_y - b_y )**2 )
      bc_len = sqrt ( ( b_x - c_x )**2 + ( b_y - c_y )**2 )
      ca_len = sqrt ( ( c_x - a_x )**2 + ( c_y - a_y )**2 )
  !
  !  Take care of a ridiculous special case.
  !
      if ( ab_len == 0.0D+00 .and. &
           bc_len == 0.0D+00 .and. &
           ca_len == 0.0D+00 ) then
  
        a_angle = 2.0D+00 * pi / 3.0D+00
        b_angle = 2.0D+00 * pi / 3.0D+00
        c_angle = 2.0D+00 * pi / 3.0D+00
  
      else
  
        if ( ca_len == 0.0D+00 .or. ab_len == 0.0D+00 ) then
          a_angle = pi
        else
          a_angle = acos ( ( ca_len**2 + ab_len**2 - bc_len**2 ) &
            / ( 2.0D+00 * ca_len * ab_len ) )
        end if
  
        if ( ab_len == 0.0D+00 .or. bc_len == 0.0D+00 ) then
          b_angle = pi
        else
          b_angle = acos ( ( ab_len**2 + bc_len**2 - ca_len**2 ) &
            / ( 2.0D+00 * ab_len * bc_len ) )
        end if
  
        if ( bc_len == 0.0D+00 .or. ca_len == 0.0D+00 ) then
          c_angle = pi
        else
          c_angle = acos ( ( bc_len**2 + ca_len**2 - ab_len**2 ) &
            / ( 2.0D+00 * bc_len * ca_len ) )
        end if
  
      end if
  
      alpha_min = min ( alpha_min, a_angle )
      alpha_min = min ( alpha_min, b_angle )
      alpha_min = min ( alpha_min, c_angle )
  
      alpha_ave = alpha_ave + alpha_min
  
      alpha_area = alpha_area + area * alpha_min
  
      area_total = area_total + area
  
    end do
  
    alpha_ave = alpha_ave / real ( triangle_num, kind = 8 )
    alpha_area = alpha_area / area_total
  !
  !  Normalize angles from [0,pi/3] degrees into qualities in [0,1].
  !
    alpha_min = alpha_min * 3.0D+00 / pi
    alpha_ave = alpha_ave * 3.0D+00 / pi
    alpha_area = alpha_area * 3.0D+00 / pi
  
    return
end