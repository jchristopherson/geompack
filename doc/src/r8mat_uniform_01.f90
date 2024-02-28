subroutine r8mat_uniform_01 ( m, n, seed, r )
  
    !*****************************************************************************80
    !
    !! R8MAT_UNIFORM_01 returns a unit pseudorandom R8MAT.
    !
    !  Discussion:
    !
    !    An R8MAT is an array of R8's.
    !
    !    For now, the input quantity SEED is an integer variable.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    31 May 2007
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    Paul Bratley, Bennett Fox, Linus Schrage,
    !    A Guide to Simulation,
    !    Second Edition,
    !    Springer, 1987,
    !    ISBN: 0387964673,
    !    LC: QA76.9.C65.B73.
    !
    !    Bennett Fox,
    !    Algorithm 647:
    !    Implementation and Relative Efficiency of Quasirandom
    !    Sequence Generators,
    !    ACM Transactions on Mathematical Software,
    !    Volume 12, Number 4, December 1986, pages 362-376.
    !
    !    Pierre L'Ecuyer,
    !    Random Number Generation,
    !    in Handbook of Simulation,
    !    edited by Jerry Banks,
    !    Wiley, 1998,
    !    ISBN: 0471134031,
    !    LC: T57.62.H37.
    !
    !    Peter Lewis, Allen Goodman, James Miller,
    !    A Pseudo-Random Number Generator for the System/360,
    !    IBM Systems Journal,
    !    Volume 8, Number 2, 1969, pages 136-143.
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns
    !    in the array.
    !
    !    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
    !    should NOT be 0.  On output, SEED has been updated.
    !
    !    Output, real ( kind = 8 ) R(M,N), the array of pseudorandom values.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ), parameter :: i4_huge = 2147483647
      integer ( kind = 4 ) j
      integer ( kind = 4 ) k
      integer ( kind = 4 ) seed
      real ( kind = 8 ) r(m,n)
    
      if ( seed == 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if
    
      do j = 1, n
    
        do i = 1, m
    
          k = seed / 127773
    
          seed = 16807 * ( seed - k * 127773 ) - k * 2836
    
          if ( seed < 0 ) then
            seed = seed + i4_huge
          end if
    
          r(i,j) = real ( seed, kind = 8 ) * 4.656612875D-10
    
        end do
      end do
    
      return
end