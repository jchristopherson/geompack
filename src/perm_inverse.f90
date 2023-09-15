subroutine perm_inverse ( n, p )
  
    !*****************************************************************************80
    !
    !! PERM_INVERSE inverts a permutation "in place".
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    25 July 2000
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the number of objects being permuted.
    !
    !    Input/output, integer ( kind = 4 ) P(N), the permutation, in standard
    !    index form.  On output, P describes the inverse permutation
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i0
      integer ( kind = 4 ) i1
      integer ( kind = 4 ) i2
      integer ( kind = 4 ) ierror
      integer ( kind = 4 ) is
      integer ( kind = 4 ) p(n)
    
      if ( n <= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PERM_INV - Fatal error!'
        write ( *, '(a,i8)' ) '  Input value of N = ', n
        stop
      end if
    
      call perm_check ( n, p, ierror )
    
      if ( ierror /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PERM_INV - Fatal error!'
        write ( *, '(a)' ) '  The input array does not represent'
        write ( *, '(a)' ) '  a proper permutation.  In particular, the'
        write ( *, '(a,i8)' ) '  array is missing the value ', ierror
        stop
      end if
    
      is = 1
    
      do i = 1, n
    
        i1 = p(i)
    
        do while ( i < i1 )
          i2 = p(i1)
          p(i1) = -i2
          i1 = i2
        end do
    
        is = -sign ( 1, p(i) )
        p(i) = sign ( p(i), is )
    
      end do
    
      do i = 1, n
    
        i1 = -p(i)
    
        if ( 0 <= i1 ) then
    
          i0 = i
    
          do
    
            i2 = p(i1)
            p(i1) = i0
    
            if ( i2 < 0 ) then
              exit
            end if
    
            i0 = i1
            i1 = i2
    
          end do
    
        end if
    
      end do
    
      return
end