subroutine i4mat_transpose_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )
  
    !*****************************************************************************80
    !
    !! I4MAT_TRANSPOSE_PRINT_SOME prints some of the transpose of an I4MAT.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    09 February 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, integer ( kind = 4 ) A(M,N), an M by N matrix to be printed.
    !
    !    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
    !
    !    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
    !
    !    Input, character ( len = * ) TITLE, an optional title.
    !
      implicit none
    
      integer ( kind = 4 ), parameter :: incx = 10
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      character ( len = 7 ) ctemp(incx)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i2
      integer ( kind = 4 ) i2hi
      integer ( kind = 4 ) i2lo
      integer ( kind = 4 ) ihi
      integer ( kind = 4 ) ilo
      integer ( kind = 4 ) inc
      integer ( kind = 4 ) j
      integer ( kind = 4 ) j2hi
      integer ( kind = 4 ) j2lo
      integer ( kind = 4 ) jhi
      integer ( kind = 4 ) jlo
      character ( len = * ) title
    
      if ( 0 < len_trim ( title ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) trim ( title )
      end if
    
      do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx
    
        i2hi = i2lo + incx - 1
        i2hi = min ( i2hi, m )
        i2hi = min ( i2hi, ihi )
    
        inc = i2hi + 1 - i2lo
    
        write ( *, '(a)' ) ' '
    
        do i = i2lo, i2hi
          i2 = i + 1 - i2lo
          write ( ctemp(i2), '(i7)') i
        end do
    
        write ( *, '(''  Row '',10a7)' ) ctemp(1:inc)
        write ( *, '(a)' ) '  Col'
        write ( *, '(a)' ) ' '
    
        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )
    
        do j = j2lo, j2hi
    
          do i2 = 1, inc
    
            i = i2lo - 1 + i2
    
            write ( ctemp(i2), '(i7)' ) a(i,j)
    
          end do
    
          write ( *, '(i5,1x,10a7)' ) j, ( ctemp(i), i = 1, inc )
    
        end do
    
      end do
    
      return
end