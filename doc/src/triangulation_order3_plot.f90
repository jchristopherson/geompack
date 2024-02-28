subroutine triangulation_order3_plot ( file_name, node_num, node_xy, &
    triangle_num, triangle_node, node_show, triangle_show )
  
  !*****************************************************************************80
  !
  !! TRIANGULATION_ORDER3_PLOT plots a 3-node triangulation of a set of nodes.
  !
  !  Discussion:
  !
  !    The triangulation is most usually a Delaunay triangulation,
  !    but this is not necessary.
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license.
  !
  !  Modified:
  !
  !    16 March 2005
  !
  !  Author:
  !
  !    John Burkardt
  !
  !  Parameters:
  !
  !    Input, character ( len = * ) FILE_NAME, the name of the output file.
  !
  !    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
  !
  !    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
  !
  !    Input, integer ( kind = 4 ) TRIANGLE_NUM, the number of triangles.
  !
  !    Input, integer ( kind = 4 ) TRIANGLE_NODE(3,TRIANGLE_NUM), lists, for each
  !    triangle, the indices of the nodes that form the vertices of the triangle.
  !
  !    Input, integer ( kind = 4 ) NODE_SHOW,
  !    0, do not show nodes;
  !    1, show nodes;
  !    2, show nodes and label them.
  !
  !    Input, integer ( kind = 4 ) TRIANGLE_SHOW,
  !    0, do not show triangles;
  !    1, show triangles;
  !    2, show triangles and label them.
  !
  !  Local parameters:
  !
  !    Local, integer ( kind = 4 ) CIRCLE_SIZE, controls the size of the circles
  !    depicting the nodes.  Currently set to 5.  3 is pretty small, and 1 is
  !    barely visible.
  !
    implicit none
  
    integer ( kind = 4 ) node_num
    integer ( kind = 4 ) triangle_num
  
    real ( kind = 8 ) ave_x
    real ( kind = 8 ) ave_y
    integer ( kind = 4 ), parameter :: circle_size = 5
    integer ( kind = 4 ) delta
    integer ( kind = 4 ) e
    character ( len = * ) file_name
    integer ( kind = 4 ) file_unit
    integer ( kind = 4 ) i
    integer ( kind = 4 ) i4_wrap
    integer ( kind = 4 ) ios
    integer ( kind = 4 ) node
    integer ( kind = 4 ) node_show
    real ( kind = 8 ) node_xy(2,node_num)
    character ( len = 40 ) string
    integer ( kind = 4 ) triangle
    integer ( kind = 4 ) triangle_node(3,triangle_num)
    integer ( kind = 4 ) triangle_show
    real ( kind = 8 ) x_max
    real ( kind = 8 ) x_min
    integer ( kind = 4 ) x_ps
    integer ( kind = 4 ) :: x_ps_max = 576
    integer ( kind = 4 ) :: x_ps_max_clip = 594
    integer ( kind = 4 ) :: x_ps_min = 36
    integer ( kind = 4 ) :: x_ps_min_clip = 18
    real ( kind = 8 ) x_scale
    real ( kind = 8 ) y_max
    real ( kind = 8 ) y_min
    integer ( kind = 4 ) y_ps
    integer ( kind = 4 ) :: y_ps_max = 666
    integer ( kind = 4 ) :: y_ps_max_clip = 684
    integer ( kind = 4 ) :: y_ps_min = 126
    integer ( kind = 4 ) :: y_ps_min_clip = 108
    real ( kind = 8 ) y_scale
  !
  !  We need to do some figuring here, so that we can determine
  !  the range of the data, and hence the height and width
  !  of the piece of paper.
  !
    x_max = maxval ( node_xy(1,1:node_num) )
    x_min = minval ( node_xy(1,1:node_num) )
    x_scale = x_max - x_min
  
    x_max = x_max + 0.05D+00 * x_scale
    x_min = x_min - 0.05D+00 * x_scale
    x_scale = x_max - x_min
  
    y_max = maxval ( node_xy(2,1:node_num) )
    y_min = minval ( node_xy(2,1:node_num) )
    y_scale = y_max - y_min
  
    y_max = y_max + 0.05D+00 * y_scale
    y_min = y_min - 0.05D+00 * y_scale
    y_scale = y_max - y_min
  
    if ( x_scale < y_scale ) then
  
      delta = nint ( real ( x_ps_max - x_ps_min, kind = 8 ) &
        * ( y_scale - x_scale ) / ( 2.0D+00 * y_scale ) )
  
      x_ps_max = x_ps_max - delta
      x_ps_min = x_ps_min + delta
  
      x_ps_max_clip = x_ps_max_clip - delta
      x_ps_min_clip = x_ps_min_clip + delta
  
      x_scale = y_scale
  
    else if ( y_scale < x_scale ) then
  
      delta = nint ( real ( y_ps_max - y_ps_min, kind = 8 ) &
        * ( x_scale - y_scale ) / ( 2.0D+00 * x_scale ) )
  
      y_ps_max      = y_ps_max - delta
      y_ps_min      = y_ps_min + delta
  
      y_ps_max_clip = y_ps_max_clip - delta
      y_ps_min_clip = y_ps_min_clip + delta
  
      y_scale = x_scale
  
    end if
  
    call get_unit ( file_unit )
  
    open ( unit = file_unit, file = file_name, status = 'replace', &
      iostat = ios )
  
    if ( ios /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGULATION_ORDER3_PLOT - Fatal error!'
      write ( *, '(a)' ) '  Can not open output file "', trim ( file_name ), '".'
      return
    end if
  
    write ( file_unit, '(a)' ) '%!PS-Adobe-3.0 EPSF-3.0'
    write ( file_unit, '(a)' ) '%%Creator: triangulation_order3_plot.f90'
    write ( file_unit, '(a)' ) '%%Title: ' // trim ( file_name )
    write ( file_unit, '(a)' ) '%%Pages: 1'
    write ( file_unit, '(a,i3,2x,i3,2x,i3,2x,i3)' ) '%%BoundingBox: ', &
      x_ps_min, y_ps_min, x_ps_max, y_ps_max
    write ( file_unit, '(a)' ) '%%Document-Fonts: Times-Roman'
    write ( file_unit, '(a)' ) '%%LanguageLevel: 1'
    write ( file_unit, '(a)' ) '%%EndComments'
    write ( file_unit, '(a)' ) '%%BeginProlog'
    write ( file_unit, '(a)' ) '/inch {72 mul} def'
    write ( file_unit, '(a)' ) '%%EndProlog'
    write ( file_unit, '(a)' ) '%%Page: 1 1'
    write ( file_unit, '(a)' ) 'save'
    write ( file_unit, '(a)' ) '%'
    write ( file_unit, '(a)' ) '%  Set the RGB line color to very light gray.'
    write ( file_unit, '(a)' ) '%'
    write ( file_unit, '(a)' ) '0.900  0.900  0.900 setrgbcolor'
    write ( file_unit, '(a)' ) '%'
    write ( file_unit, '(a)' ) '%  Draw a gray border around the page.'
    write ( file_unit, '(a)' ) '%'
    write ( file_unit, '(a)' ) 'newpath'
    write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', x_ps_min, y_ps_min, ' moveto'
    write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', x_ps_max, y_ps_min, ' lineto'
    write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', x_ps_max, y_ps_max, ' lineto'
    write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', x_ps_min, y_ps_max, ' lineto'
    write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', x_ps_min, y_ps_min, ' lineto'
    write ( file_unit, '(a)' ) 'stroke'
    write ( file_unit, '(a)' ) '%'
    write ( file_unit, '(a)' ) '%  Set the RGB color to black.'
    write ( file_unit, '(a)' ) '%'
    write ( file_unit, '(a)' ) '0.000  0.000  0.000 setrgbcolor'
    write ( file_unit, '(a)' ) '%'
    write ( file_unit, '(a)' ) '%  Set the font and its size.'
    write ( file_unit, '(a)' ) '%'
    write ( file_unit, '(a)' ) '/Times-Roman findfont'
    write ( file_unit, '(a)' ) '0.50 inch scalefont'
    write ( file_unit, '(a)' ) 'setfont'
    write ( file_unit, '(a)' ) '%'
    write ( file_unit, '(a)' ) '%  Print a title.'
    write ( file_unit, '(a)' ) '%'
    write ( file_unit, '(a)' ) '%  210  702  moveto'
    write ( file_unit, '(a)' ) '%  (Triangulation)  show'
    write ( file_unit, '(a)' ) '%'
    write ( file_unit, '(a)' ) '%  Define a clipping polygon.'
    write ( file_unit, '(a)' ) '%'
    write ( file_unit, '(a)' ) 'newpath'
    write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', &
      x_ps_min_clip, y_ps_min_clip, ' moveto'
    write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', &
      x_ps_max_clip, y_ps_min_clip, ' lineto'
    write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', &
      x_ps_max_clip, y_ps_max_clip, ' lineto'
    write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', &
      x_ps_min_clip, y_ps_max_clip, ' lineto'
    write ( file_unit, '(a,i3,2x,i3,2x,a)' ) '  ', &
      x_ps_min_clip, y_ps_min_clip, ' lineto'
    write ( file_unit, '(a)' ) 'clip newpath'
  !
  !  Draw the nodes.
  !
    if ( 1 <= node_show ) then
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Draw filled dots at the nodes.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Set the RGB color to blue.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '0.000  0.150  0.750 setrgbcolor'
      write ( file_unit, '(a)' ) '%'
  
      do node = 1, node_num
  
        x_ps = int ( &
          ( ( x_max - node_xy(1,node)         ) * real ( x_ps_min, kind = 8 )   &
          + (         node_xy(1,node) - x_min ) * real ( x_ps_max, kind = 8 ) ) &
          / ( x_max                   - x_min ) )
  
        y_ps = int ( &
          ( ( y_max - node_xy(2,node)         ) * real ( y_ps_min, kind = 8 )   &
          + (         node_xy(2,node) - y_min ) * real ( y_ps_max, kind = 8 ) ) &
          / ( y_max                   - y_min ) )
  
        write ( file_unit, '(a,i4,2x,i4,2x,i4,2x,a)' ) 'newpath ', x_ps, y_ps, &
          circle_size, '0 360 arc closepath fill'
  
      end do
  
    end if
  !
  !  Label the nodes.
  !
    if ( 2 <= node_show ) then
  
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Label the nodes:'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Set the RGB color to darker blue.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '0.000  0.250  0.850 setrgbcolor'
      write ( file_unit, '(a)' ) '/Times-Roman findfont'
      write ( file_unit, '(a)' ) '0.20 inch scalefont'
      write ( file_unit, '(a)' ) 'setfont'
      write ( file_unit, '(a)' ) '%'
  
      do node = 1, node_num
  
        x_ps = int ( &
          ( ( x_max - node_xy(1,node)         ) * real ( x_ps_min, kind = 8 )   &
          + (       + node_xy(1,node) - x_min ) * real ( x_ps_max, kind = 8 ) ) &
          / ( x_max                   - x_min ) )
  
        y_ps = int ( &
          ( ( y_max - node_xy(2,node)         ) * real ( y_ps_min, kind = 8 )   &
          + (         node_xy(2,node) - y_min ) * real ( y_ps_max, kind = 8 ) ) &
          / ( y_max                   - y_min ) )
  
        write ( string, '(i4)' ) node
        string = adjustl ( string )
  
        write ( file_unit, '(i4,2x,i4,a)' ) x_ps, y_ps+5, &
          ' moveto (' // trim ( string ) // ') show'
  
      end do
  
    end if
  !
  !  Draw the triangles.
  !
    if ( 1 <= triangle_show ) then
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Set the RGB color to red.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '0.900  0.200  0.100 setrgbcolor'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Draw the triangles.'
      write ( file_unit, '(a)' ) '%'
  
      do triangle = 1, triangle_num
  
        write ( file_unit, '(a)' ) 'newpath'
  
        do i = 1, 4
  
          e = i4_wrap ( i, 1, 3 )
  
          node = triangle_node(e,triangle)
  
          x_ps = int ( &
            ( ( x_max - node_xy(1,node)         ) &
            * real ( x_ps_min, kind = 8 )   &
            + (         node_xy(1,node) - x_min ) &
            * real ( x_ps_max, kind = 8 ) ) &
            / ( x_max                   - x_min ) )
  
          y_ps = int ( &
            ( ( y_max - node_xy(2,node)         ) &
            * real ( y_ps_min, kind = 8 )   &
            + (         node_xy(2,node) - y_min ) &
            * real ( y_ps_max, kind = 8 ) ) &
            / ( y_max                   - y_min ) )
  
          if ( i == 1 ) then
            write ( file_unit, '(i3,2x,i3,2x,a)' ) x_ps, y_ps, ' moveto'
          else
            write ( file_unit, '(i3,2x,i3,2x,a)' ) x_ps, y_ps, ' lineto'
          end if
  
        end do
  
        write ( file_unit, '(a)' ) 'stroke'
  
      end do
  
    end if
  !
  !  Label the triangles.
  !
    if ( 2 <= triangle_show ) then
  
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Label the triangles:'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '%  Set the RGB color to darker red.'
      write ( file_unit, '(a)' ) '%'
      write ( file_unit, '(a)' ) '0.950  0.250  0.150 setrgbcolor'
      write ( file_unit, '(a)' ) '/Times-Roman findfont'
      write ( file_unit, '(a)' ) '0.20 inch scalefont'
      write ( file_unit, '(a)' ) 'setfont'
      write ( file_unit, '(a)' ) '%'
  
      do triangle = 1, triangle_num
  
        ave_x = 0.0D+00
        ave_y = 0.0D+00
  
        do i = 1, 3
  
          node = triangle_node(i,triangle)
  
          ave_x = ave_x + node_xy(1,node)
          ave_y = ave_y + node_xy(2,node)
  
        end do
  
        ave_x = ave_x / 3.0D+00
        ave_y = ave_y / 3.0D+00
  
        x_ps = int ( &
          ( ( x_max - ave_x         ) * real ( x_ps_min, kind = 8 )   &
          + (       + ave_x - x_min ) * real ( x_ps_max, kind = 8 ) ) &
          / ( x_max         - x_min ) )
  
        y_ps = int ( &
          ( ( y_max - ave_y         ) * real ( y_ps_min, kind = 8 )   &
          + (         ave_y - y_min ) * real ( y_ps_max, kind = 8 ) ) &
          / ( y_max         - y_min ) )
  
        write ( string, '(i4)' ) triangle
        string = adjustl ( string )
  
        write ( file_unit, '(i4,2x,i4,a)' ) x_ps, y_ps, ' moveto (' &
          // trim ( string ) // ') show'
  
      end do
  
    end if
  
    write ( file_unit, '(a)' ) '%'
    write ( file_unit, '(a)' ) 'restore  showpage'
    write ( file_unit, '(a)' ) '%'
    write ( file_unit, '(a)' ) '%  End of page.'
    write ( file_unit, '(a)' ) '%'
    write ( file_unit, '(a)' ) '%%Trailer'
    write ( file_unit, '(a)' ) '%%EOF'
    close ( unit = file_unit )
  
    return
end