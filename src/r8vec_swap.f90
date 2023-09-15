subroutine r8vec_swap ( n, a1, a2 )
  
    !*****************************************************************************80
    !
    !! R8VEC_SWAP swaps the entries of two R8VEC's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    04 December 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the number of entries in the arrays.
    !
    !    Input/output, real ( kind = 8 ) A1(N), A2(N), the vectors to swap.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a1(n)
      real ( kind = 8 ) a2(n)
      real ( kind = 8 ) a3(n)
    
      a3(1:n) = a1(1:n)
      a1(1:n) = a2(1:n)
      a2(1:n) = a3(1:n)
    
      return
end