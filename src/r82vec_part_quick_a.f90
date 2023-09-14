subroutine r82vec_part_quick_a ( n, a, l, r )
  
    !*****************************************************************************80
    !
    !! R82VEC_PART_QUICK_A reorders an R82VEC as part of a quick sort.
    !
    !  Discussion:
    !
    !    The routine reorders the entries of A.  Using A(1:2,1) as a
    !    key, all entries of A that are less than or equal to the key will
    !    precede the key, which precedes all entries that are greater than the key.
    !
    !  Example:
    !
    !    Input:
    !
    !      N = 8
    !
    !      A = ( (2,4), (8,8), (6,2), (0,2), (10,6), (10,0), (0,6), (4,8) )
    !
    !    Output:
    !
    !      L = 2, R = 4
    !
    !      A = ( (0,2), (0,6), (2,4), (8,8), (6,2), (10,6), (10,0), (4,8) )
    !             -----------          ----------------------------------
    !             LEFT          KEY    RIGHT
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
    !    Input, integer ( kind = 4 ) N, the number of entries of A.
    !
    !    Input/output, real ( kind = 8 ) A(2,N).  On input, the array to be checked.
    !    On output, A has been reordered as described above.
    !
    !    Output, integer ( kind = 4 ) L, R, the indices of A that define
    !    the three segments.
    !    Let KEY = the input value of A(1:2,1).  Then
    !    I <= L                 A(1:2,I) < KEY;
    !         L < I < R         A(1:2,I) = KEY;
    !                 R <= I    KEY < A(1:2,I).
    !
      implicit none
    
      integer ( kind = 4 ) n
      integer ( kind = 4 ), parameter :: dim_num = 2
    
      real ( kind = 8 ) a(dim_num,n)
      integer ( kind = 4 ) i
      real ( kind = 8 ) key(dim_num)
      integer ( kind = 4 ) l
      integer ( kind = 4 ) m
      integer ( kind = 4 ) r
      logical r8vec_eq
      logical r8vec_gt
      logical r8vec_lt
    
      if ( n < 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R82VEC_PART_QUICK_A - Fatal error!'
        write ( *, '(a)' ) '  N < 1.'
        stop
      else if ( n == 1 ) then
        l = 0
        r = 2
        return
      end if
    
      key(1:dim_num) = a(1:dim_num,1)
      m = 1
    !
    !  The elements of unknown size have indices between L+1 and R-1.
    !
      l = 1
      r = n + 1
    
      do i = 2, n
    
        if ( r8vec_gt ( dim_num, a(1:dim_num,l+1), key(1:dim_num) ) ) then
          r = r - 1
          call r8vec_swap ( dim_num, a(1:dim_num,r), a(1:dim_num,l+1) )
        else if ( r8vec_eq ( dim_num, a(1:dim_num,l+1), key(1:dim_num) ) ) then
          m = m + 1
          call r8vec_swap ( dim_num, a(1:dim_num,m), a(1:dim_num,l+1) )
          l = l + 1
        else if ( r8vec_lt ( dim_num, a(1:dim_num,l+1), key(1:dim_num) ) ) then
          l = l + 1
        end if
    
      end do
    !
    !  Now shift small elements to the left, and KEY elements to center.
    !
      do i = 1, l - m
        a(1:dim_num,i) = a(1:dim_num,i+m)
      end do
    
      l = l - m
    
      do i = 1, dim_num
        a(i,l+1:l+m) = key(i)
      end do
    
      return
end