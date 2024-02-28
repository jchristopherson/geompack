subroutine i4mat_transpose_print ( m, n, a, title )
  
    !*****************************************************************************80
    !
    !! I4MAT_TRANSPOSE_PRINT prints an I4MAT, transposed.
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
    !    Input, character ( len = * ) TITLE, an optional title.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      character ( len = * ) title
    
      call i4mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )
    
      return
end