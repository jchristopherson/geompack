subroutine i4_swap ( i, j )
  
    !*****************************************************************************80
    !
    !! I4_SWAP swaps two I4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    30 November 1998
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input/output, integer ( kind = 4 ) I, J.  On output, the values of I and
    !    J have been interchanged.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      integer ( kind = 4 ) k
    
      k = i
      i = j
      j = k
    
      return
end