function r8_acos ( c )
  
    !*****************************************************************************80
    !
    !! R8_ACOS computes the arc cosine function, with argument truncation.
    !
    !  Discussion:
    !
    !    If you call your system ACOS routine with an input argument that is
    !    even slightly outside the range [-1.0, 1.0 ], you may get an unpleasant
    !    surprise (I did).
    !
    !    This routine simply truncates arguments outside the range.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    19 October 2012
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( kind = 8 ) C, the argument.
    !
    !    Output, real ( kind = 8 ) R8_ACOS, an angle whose cosine is C.
    !
      implicit none
    
      real ( kind = 8 ) c
      real ( kind = 8 ) c2
      real ( kind = 8 ) r8_acos
    
      c2 = c
      c2 = max ( c2, -1.0D+00 )
      c2 = min ( c2, +1.0D+00 )
    
      r8_acos = acos ( c2 )
    
      return
end