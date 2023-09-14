subroutine i4vec_sorted_unique ( n, a, nuniq )
  
    !*****************************************************************************80
    !
    !! I4VEC_SORTED_UNIQUE finds the unique elements in a sorted I4VEC.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    09 July 2000
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the number of elements in A.
    !
    !    Input/output, integer ( kind = 4 ) A(N).  On input, the sorted
    !    integer ( kind = 4 ) array.  On output, the unique elements in A.
    !
    !    Output, integer ( kind = 4 ) NUNIQ, the number of unique elements in A.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(n)
      integer ( kind = 4 ) itest
      integer ( kind = 4 ) nuniq
    
      nuniq = 0
    
      if ( n <= 0 ) then
        return
      end if
    
      nuniq = 1
    
      do itest = 2, n
    
        if ( a(itest) /= a(nuniq) ) then
          nuniq = nuniq + 1
          a(nuniq) = a(itest)
        end if
    
      end do
    
      return
end