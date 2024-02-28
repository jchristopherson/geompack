subroutine r82vec_sort_heap_index_a ( n, a, indx )
  
    !*****************************************************************************80
    !
    !! R82VEC_SORT_HEAP_INDEX_A does an indexed heap ascending sort of an R82VEC.
    !
    !  Discussion:
    !
    !    The sorting is not actually carried out.  Rather an index array is
    !    created which defines the sorting.  This array may be used to sort
    !    or index the array, or to sort or index related arrays keyed on the
    !    original array.
    !
    !    Once the index array is computed, the sorting can be carried out
    !    "implicitly:
    !
    !      A(1:2,INDX(I)), I = 1 to N is sorted,
    !
    !    or explicitly, by the call
    !
    !      call R82VEC_PERMUTE ( N, A, INDX )
    !
    !    after which A(1:2,I), I = 1 to N is sorted.
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
    !    Input, integer ( kind = 4 ) N, the number of entries in the array.
    !
    !    Input, real ( kind = 8 ) A(2,N), an array to be index-sorted.
    !
    !    Output, integer ( kind = 4 ) INDX(N), the sort index.  The
    !    I-th element of the sorted array is A(1:2,INDX(I)).
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a(2,n)
      real ( kind = 8 ) aval(2)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) indx(n)
      integer ( kind = 4 ) indxt
      integer ( kind = 4 ) ir
      integer ( kind = 4 ) j
      integer ( kind = 4 ) l
    
      if ( n < 1 ) then
        return
      end if
    
      do i = 1, n
        indx(i) = i
      end do
    
      if ( n == 1 ) then
        return
      end if
    
      l = n / 2 + 1
      ir = n
    
      do
    
        if ( 1 < l ) then
    
          l = l - 1
          indxt = indx(l)
          aval(1:2) = a(1:2,indxt)
    
        else
    
          indxt = indx(ir)
          aval(1:2) = a(1:2,indxt)
          indx(ir) = indx(1)
          ir = ir - 1
    
          if ( ir == 1 ) then
            indx(1) = indxt
            exit
          end if
    
        end if
    
        i = l
        j = l + l
    
        do while ( j <= ir )
    
          if ( j < ir ) then
            if (   a(1,indx(j)) <  a(1,indx(j+1)) .or. &
                 ( a(1,indx(j)) == a(1,indx(j+1)) .and. &
                   a(2,indx(j)) <  a(2,indx(j+1)) ) ) then
              j = j + 1
            end if
          end if
    
          if (   aval(1) <  a(1,indx(j)) .or. &
               ( aval(1) == a(1,indx(j)) .and. &
                 aval(2) <  a(2,indx(j)) ) ) then
            indx(i) = indx(j)
            i = j
            j = j + j
          else
            j = ir + 1
          end if
    
        end do
    
        indx(i) = indxt
    
      end do
    
      return
end