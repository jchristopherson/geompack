function diaedg ( x0, y0, x1, y1, x2, y2, x3, y3 )
  
    !*****************************************************************************80
    !
    !! DIAEDG chooses a diagonal edge.
    !
    !  Discussion:
    !
    !    The routine determines whether 0--2 or 1--3 is the diagonal edge
    !    that should be chosen, based on the circumcircle criterion, where
    !    (X0,Y0), (X1,Y1), (X2,Y2), (X3,Y3) are the vertices of a simple
    !    quadrilateral in counterclockwise order.
    !
    !  Modified:
    !
    !    19 February 2001
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
    !    Input, real ( kind = 8 ) X0, Y0, X1, Y1, X2, Y2, X3, Y3, the
    !    coordinates of the vertices of a quadrilateral, given in
    !    counter clockwise order.
    !
    !    Output, integer ( kind = 4 ) DIAEDG, chooses a diagonal:
    !    +1, if diagonal edge 02 is chosen;
    !    -1, if diagonal edge 13 is chosen;
    !     0, if the four vertices are cocircular.
    !
      implicit none
    
      real ( kind = 8 ) ca
      real ( kind = 8 ) cb
      integer ( kind = 4 ) diaedg
      real ( kind = 8 ) dx10
      real ( kind = 8 ) dx12
      real ( kind = 8 ) dx30
      real ( kind = 8 ) dx32
      real ( kind = 8 ) dy10
      real ( kind = 8 ) dy12
      real ( kind = 8 ) dy30
      real ( kind = 8 ) dy32
      real ( kind = 8 ) s
      real ( kind = 8 ) tol
      real ( kind = 8 ) tola
      real ( kind = 8 ) tolb
      real ( kind = 8 ) x0
      real ( kind = 8 ) x1
      real ( kind = 8 ) x2
      real ( kind = 8 ) x3
      real ( kind = 8 ) y0
      real ( kind = 8 ) y1
      real ( kind = 8 ) y2
      real ( kind = 8 ) y3
    
      tol = 100.0D+00 * epsilon ( tol )
    
      dx10 = x1 - x0
      dy10 = y1 - y0
      dx12 = x1 - x2
      dy12 = y1 - y2
      dx30 = x3 - x0
      dy30 = y3 - y0
      dx32 = x3 - x2
      dy32 = y3 - y2
    
      tola = tol * max ( abs ( dx10 ), abs ( dy10 ), abs ( dx30 ), abs ( dy30 ) )
      tolb = tol * max ( abs ( dx12 ), abs ( dy12 ), abs ( dx32 ), abs ( dy32 ) )
    
      ca = dx10 * dx30 + dy10 * dy30
      cb = dx12 * dx32 + dy12 * dy32
    
      if ( tola < ca .and. tolb < cb ) then
    
        diaedg = -1
    
      else if ( ca < -tola .and. cb < -tolb ) then
    
        diaedg = 1
    
      else
    
        tola = max ( tola, tolb )
        s = ( dx10 * dy30 - dx30 * dy10 ) * cb + ( dx32 * dy12 - dx12 * dy32 ) * ca
    
        if ( tola < s ) then
          diaedg = -1
        else if ( s < -tola ) then
          diaedg = 1
        else
          diaedg = 0
        end if
    
      end if
    
      return
end