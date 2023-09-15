subroutine r82vec_sort_quick_a ( n, a )
  
    !*****************************************************************************80
    !
    !! R82VEC_SORT_QUICK_A ascending sorts an R82VEC using quick sort.
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
    !    Input/output, real ( kind = 8 ) A(2,N).
    !    On input, the array to be sorted.
    !    On output, the array has been sorted.
    !
      implicit none
    
      integer ( kind = 4 ), parameter :: level_max = 25
      integer ( kind = 4 ) n
      integer ( kind = 4 ), parameter :: dim_num = 2
    
      real ( kind = 8 ) a(dim_num,n)
      integer ( kind = 4 ) base
      integer ( kind = 4 ) l_segment
      integer ( kind = 4 ) level
      integer ( kind = 4 ) n_segment
      integer ( kind = 4 ) rsave(level_max)
      integer ( kind = 4 ) r_segment
    
      if ( n < 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R82VEC_SORT_QUICK_A - Fatal error!'
        write ( *, '(a)' ) '  N < 1.'
        stop
      else if ( n == 1 ) then
        return
      end if
    
      level = 1
      rsave(level) = n + 1
      base = 1
      n_segment = n
    
      do
    !
    !  Partition the segment.
    !
        call r82vec_part_quick_a ( n_segment, a(1,base), l_segment, r_segment )
    !
    !  If the left segment has more than one element, we need to partition it.
    !
        if ( 1 < l_segment ) then
    
          if ( level_max < level ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'R82VEC_SORT_QUICK_A - Fatal error!'
            write ( *, '(a,i8)' ) '  Exceeding recursion maximum of ', level_max
            stop
          end if
    
          level = level + 1
          n_segment = l_segment
          rsave(level) = r_segment + base - 1
    !
    !  The left segment and the middle segment are sorted.
    !  Must the right segment be partitioned?
    !
        else if ( r_segment < n_segment ) then
    
          n_segment = n_segment + 1 - r_segment
          base = base + r_segment - 1
    !
    !  Otherwise, we back up a level if there is an earlier one.
    !
        else
    
          do
    
            if ( level <= 1 ) then
              return
            end if
    
            base = rsave(level)
            n_segment = rsave(level-1) - rsave(level)
            level = level - 1
    
            if ( 0 < n_segment ) then
              exit
            end if
    
          end do
    
        end if
    
      end do
    
      return
end