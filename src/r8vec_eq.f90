function r8vec_eq ( n, a1, a2 )
  
    !*****************************************************************************80
    !
    !! R8VEC_EQ is true if two R8VEC's are equal.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    05 December 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
    !
    !    Input, real ( kind = 8 ) A1(N), A2(N), two vectors to compare.
    !
    !    Output, logical R8VEC_EQ, is TRUE if every pair of elements A1(I)
    !    and A2(I) are equal, and FALSE otherwise.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a1(n)
      real ( kind = 8 ) a2(n)
      logical r8vec_eq
    
      r8vec_eq = ( all ( a1(1:n) == a2(1:n) ) )
    
      return
end