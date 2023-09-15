function r8vec_lt ( n, a1, a2 )
  
    !*****************************************************************************80
    !
    !! R8VEC_LT == ( A1 < A2 ) for R8VEC's.
    !
    !  Discussion:
    !
    !    The comparison is lexicographic.
    !
    !    A1 < A2  <=>                              A1(1) < A2(1) or
    !                 ( A1(1)     == A2(1)     and A1(2) < A2(2) ) or
    !                 ...
    !                 ( A1(1:N-1) == A2(1:N-1) and A1(N) < A2(N)
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
    !    Input, integer ( kind = 4 ) N, the dimension of the vectors.
    !
    !    Input, real ( kind = 8 ) A1(N), A2(N), the vectors to be compared.
    !
    !    Output, logical R8VEC_LT, is TRUE if and only if A1 < A2.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a1(n)
      real ( kind = 8 ) a2(n)
      integer ( kind = 4 ) i
      logical r8vec_lt
    
      r8vec_lt = .false.
    
      do i = 1, n
    
        if ( a1(i) < a2(i) ) then
          r8vec_lt = .true.
          exit
        else if ( a2(i) < a1(i) ) then
          r8vec_lt = .false.
          exit
        end if
    
      end do
    
      return
end