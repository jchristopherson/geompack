subroutine r82vec_permute ( n, a, p )
  
    !*****************************************************************************80
    !
    !! R82VEC_PERMUTE permutes an R82VEC in place.
    !
    !  Discussion:
    !
    !    This routine permutes an array of real "objects", but the same
    !    logic can be used to permute an array of objects of any arithmetic
    !    type, or an array of objects of any complexity.  The only temporary
    !    storage required is enough to store a single object.  The number
    !    of data movements made is N + the number of cycles of order 2 or more,
    !    which is never more than N + N/2.
    !
    !  Example:
    !
    !    Input:
    !
    !      N = 5
    !      P = (   2,    4,    5,    1,    3 )
    !      A = ( 1.0,  2.0,  3.0,  4.0,  5.0 )
    !          (11.0, 22.0, 33.0, 44.0, 55.0 )
    !
    !    Output:
    !
    !      A    = (  2.0,  4.0,  5.0,  1.0,  3.0 )
    !             ( 22.0, 44.0, 55.0, 11.0, 33.0 ).
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    08 December 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the number of objects.
    !
    !    Input/output, real ( kind = 8 ) A(2,N), the array to be permuted.
    !
    !    Input, integer ( kind = 4 ) P(N), the permutation.  P(I) = J means
    !    that the I-th element of the output array should be the J-th
    !    element of the input array.  P must be a legal permutation
    !    of the integers from 1 to N, otherwise the algorithm will
    !    fail catastrophically.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a(2,n)
      real ( kind = 8 ) a_temp(2)
      integer ( kind = 4 ) ierror
      integer ( kind = 4 ) iget
      integer ( kind = 4 ) iput
      integer ( kind = 4 ) istart
      integer ( kind = 4 ) p(n)
    
      call perm_check ( n, p, ierror )
    
      if ( ierror /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R82VEC_PERMUTE - Fatal error!'
        write ( *, '(a)' ) '  The input array does not represent'
        write ( *, '(a)' ) '  a proper permutation.  In particular, the'
        write ( *, '(a,i8)' ) '  array is missing the value ', ierror
        stop
      end if
    !
    !  Search for the next element of the permutation that has not been used.
    !
      do istart = 1, n
    
        if ( p(istart) < 0 ) then
    
          cycle
    
        else if ( p(istart) == istart ) then
    
          p(istart) = -p(istart)
          cycle
    
        else
    
          a_temp(1:2) = a(1:2,istart)
          iget = istart
    !
    !  Copy the new value into the vacated entry.
    !
          do
    
            iput = iget
            iget = p(iget)
    
            p(iput) = -p(iput)
    
            if ( iget < 1 .or. n < iget ) then
              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'R82VEC_PERMUTE - Fatal error!'
              stop
            end if
    
            if ( iget == istart ) then
              a(1:2,iput) = a_temp(1:2)
              exit
            end if
    
            a(1:2,iput) = a(1:2,iget)
    
          end do
    
        end if
    
      end do
    !
    !  Restore the signs of the entries.
    !
      p(1:n) = -p(1:n)
    
      return
end