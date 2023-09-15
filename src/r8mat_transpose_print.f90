subroutine r8mat_transpose_print ( m, n, a, title )
  
    !*****************************************************************************80
    !
    !! R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    14 June 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
    !
    !    Input, character ( len = * ) TITLE, an optional title.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a(m,n)
      character ( len = * ) title
    
      call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )
    
      return
end