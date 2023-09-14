subroutine i4vec_heap_d ( n, a )
  
    !*****************************************************************************80
    !
    !! I4VEC_HEAP_D reorders an I4VEC into an descending heap.
    !
    !  Discussion:
    !
    !    A descending heap is an array A with the property that, for every index J,
    !    A(J) >= A(2*J) and A(J) >= A(2*J+1), (as long as the indices
    !    2*J and 2*J+1 are legal).
    !
    !                  A(1)
    !                /      \
    !            A(2)         A(3)
    !          /     \        /  \
    !      A(4)       A(5)  A(6) A(7)
    !      /  \       /   \
    !    A(8) A(9) A(10) A(11)
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
    !    Input, integer ( kind = 4 ) N, the size of the input array.
    !
    !    Input/output, integer ( kind = 4 ) A(N).
    !    On input, an unsorted array.
    !    On output, the array has been reordered into a heap.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) ifree
      integer ( kind = 4 ) key
      integer ( kind = 4 ) m
    !
    !  Only nodes N/2 down to 1 can be "parent" nodes.
    !
      do i = n/2, 1, -1
    !
    !  Copy the value out of the parent node.
    !  Position IFREE is now "open".
    !
        key = a(i)
        ifree = i
    
        do
    !
    !  Positions 2*IFREE and 2*IFREE + 1 are the descendants of position
    !  IFREE.  (One or both may not exist because they exceed N.)
    !
          m = 2 * ifree
    !
    !  Does the first position exist?
    !
          if ( n < m ) then
            exit
          end if
    !
    !  Does the second position exist?
    !
          if ( m + 1 <= n ) then
    !
    !  If both positions exist, take the larger of the two values,
    !  and update M if necessary.
    !
            if ( a(m) < a(m+1) ) then
              m = m + 1
            end if
    
          end if
    !
    !  If the large descendant is larger than KEY, move it up,
    !  and update IFREE, the location of the free position, and
    !  consider the descendants of THIS position.
    !
          if ( a(m) <= key ) then
            exit
          end if
    
          a(ifree) = a(m)
          ifree = m
    
        end do
    !
    !  Once there is no more shifting to do, KEY moves into the free spot IFREE.
    !
        a(ifree) = key
    
      end do
    
      return
end