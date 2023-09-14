subroutine i4vec_sort_heap_a ( n, a )
  
    !*****************************************************************************80
    !
    !! I4VEC_SORT_HEAP_A ascending sorts an I4VEC using heap sort.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    15 April 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    A Nijenhuis and H Wilf,
    !    Combinatorial Algorithms,
    !    Academic Press, 1978, second edition,
    !    ISBN 0-12-519260-6.
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the number of entries in the array.
    !
    !    Input/output, integer ( kind = 4 ) A(N).
    !    On input, the array to be sorted;
    !    On output, the array has been sorted.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(n)
      integer ( kind = 4 ) n1
    
      if ( n <= 1 ) then
        return
      end if
    !
    !  1: Put A into descending heap form.
    !
      call i4vec_heap_d ( n, a )
    !
    !  2: Sort A.
    !
    !  The largest object in the heap is in A(1).
    !  Move it to position A(N).
    !
      call i4_swap ( a(1), a(n) )
    !
    !  Consider the diminished heap of size N1.
    !
      do n1 = n-1, 2, -1
    !
    !  Restore the heap structure of A(1) through A(N1).
    !
        call i4vec_heap_d ( n1, a )
    !
    !  Take the largest object from A(1) and move it to A(N1).
    !
        call i4_swap ( a(1), a(n1) )
    
      end do
    
      return
end