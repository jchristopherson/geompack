function lrline ( xu, yu, xv1, yv1, xv2, yv2, dv )
  
    !*****************************************************************************80
    !
    !! LRLINE determines if a point is left of, right or, or on a directed line.
    !
    !  Discussion:
    !
    !    The directed line is parallel to, and at a signed distance DV from
    !    a directed base line from (XV1,YV1) to (XV2,YV2).
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
    !    Input, real ( kind = 8 ) XU, YU, the coordinates of the point whose
    !    position relative to the directed line is to be determined.
    !
    !    Input, real ( kind = 8 ) XV1, YV1, XV2, YV2, the coordinates of two points
    !    that determine the directed base line.
    !
    !    Input, real ( kind = 8 ) DV, the signed distance of the directed line
    !    from the directed base line through the points (XV1,YV1) and (XV2,YV2).
    !    DV is positive for a line to the left of the base line.
    !
    !    Output, integer ( kind = 4 ) LRLINE, the result:
    !    +1, the point is to the right of the directed line;
    !     0, the point is on the directed line;
    !    -1, the point is to the left of the directed line.
    !
      implicit none
    
      real ( kind = 8 ) dv
      real ( kind = 8 ) dx
      real ( kind = 8 ) dxu
      real ( kind = 8 ) dy
      real ( kind = 8 ) dyu
      integer ( kind = 4 ) lrline
      real ( kind = 8 ) t
      real ( kind = 8 ) tol
      real ( kind = 8 ) tolabs
      real ( kind = 8 ) xu
      real ( kind = 8 ) xv1
      real ( kind = 8 ) xv2
      real ( kind = 8 ) yu
      real ( kind = 8 ) yv1
      real ( kind = 8 ) yv2
    
      tol = 100.0D+00 * epsilon ( tol )
    
      dx = xv2 - xv1
      dy = yv2 - yv1
      dxu = xu - xv1
      dyu = yu - yv1
    
      tolabs = tol * max ( abs ( dx ), abs ( dy ), abs ( dxu ), &
        abs ( dyu ), abs ( dv ) )
    
      t = dy * dxu - dx * dyu + dv * sqrt ( dx * dx + dy * dy )
    
      if ( tolabs < t ) then
        lrline = 1
      else if ( -tolabs <= t ) then
        lrline = 0
      else
        lrline = -1
      end if
    
      return
end