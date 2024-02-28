subroutine perm_check ( n, p, ierror )
  
    !*****************************************************************************80
    !
    !! PERM_CHECK checks that a vector represents a permutation.
    !
    !  Discussion:
    !
    !    The routine verifies that each of the values from 1
    !    to N occurs among the N entries of the permutation.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    01 February 2001
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the number of entries.
    !
    !    Input, integer ( kind = 4 ) P(N), the array to check.
    !
    !    Output, integer ( kind = 4 ) IERROR, error flag.
    !    0, the array represents a permutation.
    !    nonzero, the array does not represent a permutation.  The smallest
    !    missing value is equal to IERROR.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) ierror
      integer ( kind = 4 ) ifind
      integer ( kind = 4 ) iseek
      integer ( kind = 4 ) p(n)
    
      ierror = 0
    
      do iseek = 1, n
    
        ierror = iseek
    
        do ifind = 1, n
          if ( p(ifind) == iseek ) then
            ierror = 0
            exit
          end if
        end do
    
        if ( ierror /= 0 ) then
          return
        end if
    
      end do
    
      return
end