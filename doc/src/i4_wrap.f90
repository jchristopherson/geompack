function i4_wrap ( ival, ilo, ihi )
  
    !*****************************************************************************80
    !
    !! I4_WRAP forces an I4 to lie between given limits by wrapping.
    !
    !  Example:
    !
    !    ILO = 4, IHI = 8
    !
    !    I  I4_WRAP
    !
    !    -2     8
    !    -1     4
    !     0     5
    !     1     6
    !     2     7
    !     3     8
    !     4     4
    !     5     5
    !     6     6
    !     7     7
    !     8     8
    !     9     4
    !    10     5
    !    11     6
    !    12     7
    !    13     8
    !    14     4
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    19 August 2003
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) IVAL, a value.
    !
    !    Input, integer ( kind = 4 ) ILO, IHI, the desired bounds for the value.
    !
    !    Output, integer ( kind = 4 ) I4_WRAP, a "wrapped" version of the value.
    !
      implicit none
    
      integer ( kind = 4 ) i4_modp
      integer ( kind = 4 ) i4_wrap
      integer ( kind = 4 ) ihi
      integer ( kind = 4 ) ilo
      integer ( kind = 4 ) ival
      integer ( kind = 4 ) jhi
      integer ( kind = 4 ) jlo
      integer ( kind = 4 ) wide
    
      jlo = min ( ilo, ihi )
      jhi = max ( ilo, ihi )
    
      wide = jhi - jlo + 1
    
      if ( wide == 1 ) then
        i4_wrap = jlo
      else
        i4_wrap = jlo + i4_modp ( ival - jlo, wide )
      end if
    
      return
end