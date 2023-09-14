module geompack
    use iso_fortran_env
    implicit none

!  Interfaces
interface
    subroutine alpha_measure(n, z, triangle_order, triangle_num, &
        triangle_node, alpha_min, alpha_ave, alpha_area)
        !! Determines the triangulated pointset quality measure ALPHA.
        !!
        !! The ALPHA measure evaluates the uniformity of the shapes of the triangles
        !! defined by a triangulated pointset.
        !!
        !! We compute the minimum angle among all the triangles in the triangulated
        !! dataset and divide by the maximum possible value (which, in degrees,
        !! is 60).  The best possible value is 1, and the worst 0.  A good
        !! triangulation should have an ALPHA score close to 1.
        !!
        !! The code has been modified to 'allow' 6-node triangulations.
        !! However, no effort is made to actually process the midside nodes.
        !! Only information from the vertices is used.
        use iso_fortran_env, only : int32, real64
        
        ! Arguments
        integer(int32), intent(in) :: n
            !! The number of points.
        real(real64), intent(in) :: z(2, n)
            !! The points.
        integer(int32), intent(in) :: triangle_order
            !! The order of the triangles.
        integer(int32), intent(in) :: triangle_num
            !! The number of triangles.
        integer(int32), intent(in) :: triangle_node(triangle_order, triangle_num)
            !! The triangulation.
        real(real64), intent(out) :: alpha_min
            !! The minimum value of alpha over all triangles.
        real(real64), intent(out) :: alpha_ave
            !! The value of alpha averaged over all triangles.
        real(real64), intent(out) :: alpha_area
            !! The value of alpha averaged over all triangles weighted by area.
    end subroutine

    function angle_rad_2d(p1, p2, p3) result(rst)
        !! Returns the angle swept out between two rays in 2D.
        use iso_fortran_env, only : real64
        
        ! Arguments
        real(real64), intent(in) :: p1(2)
            !! Point 1.
        real(real64), intent(in) :: p2(2)
            !! Point 2.
        real(real64), intent(in) :: p3(2)
            !! Point 3.
        real(real64) :: rst
            !! The angle swept out by the rays, in radians.  If either ray has
            !! zero length, then the angle is set to 0.
    end function

    function diaedg(x0, y0, x1, y1, x2, y2, x3, y3) result(rst)
        !! Chooses a diagonal edge.
        !!
        !! The routine determines whether 0--2 or 1--3 is the diagonal edge
        !! that should be chosen, based on the circumcircle criterion, where
        !! (X0,Y0), (X1,Y1), (X2,Y2), (X3,Y3) are the vertices of a simple
        !! quadrilateral in counterclockwise order.
        use iso_fortran_env, only : int32, real64
        
        ! Arguments
        real(real64), intent(in) :: x0
            !! The x-coordinate of vertex 0.
        real(real64), intent(in) :: y0
            !! The y-coordinate of vertex 0.
        real(real64), intent(in) :: x1
            !! The x-coordinate of vertex 1.
        real(real64), intent(in) :: y1
            !! The y-coordinate of vertex 1.
        real(real64), intent(in) :: x2
            !! The x-coordinate of vertex 2.
        real(real64), intent(in) :: y2
            !! The y-coordinate of vertex 2.
        real(real64), intent(in) :: x3
            !! The x-coordinate of vertex 3.
        real(real64), intent(in) :: y3
            !! The y-coordinate of vertex 3.
        integer(int32) :: rst
            !! The edge.
    end function

    function lrline(xu, yu, xv1, yv1, xv2, yv2, dv) result(rst)
        !! Determines if a point is left of, right of, or on a directed line.
        !!
        !! The directed line is parallel to, and at a signed distance DV from
        !! a directed base line from (XV1,YV1) to (XV2,YV2).
        use iso_fortran_env, only : int32, real64

        ! Arguments
        real(real64), intent(in) :: xu
            !! The x-coordinate of the point whose position relative to the 
            !! directed line is to be determined.
        real(real64), intent(in) :: yu
            !! The y-coordinate of the point whose position relative to the 
            !! directed line is to be determined.
        real(real64), intent(in) :: xv1
            !! The x-coordinate of the first point that defines the base line.
        real(real64), intent(in) :: yv1
            !! The y-coordinate of the first point that defines the base line.
        real(real64), intent(in) :: xv2
            !! The x-coordinate of the second point that defines the base line.
        real(real64), intent(in) :: yv2
            !! The y-coordinate of the second point that defines the base line.
        real(real64), intent(in) :: dv
            !! The signed distance of the directed line from the directed base
            !! line through the points (XV1,YV1) and (XV2,YV2).  DV is positive
            !! for a line to the left of the base line.
        integer(int32) :: rst
            !! +1, the point is to the right of the directed line;
            !! 0, the point is on the directed line;
            !! -1, the point is to the left of the directed line.
    end function

    subroutine perm_check(n, p, ierror)
        !! Checks that a vector represents a permutation.
        !!
        !! The routine verifies that each of the values from 1
        !! to N occurs among the N entries of the permutation.
        use iso_fortran_env, only : int32

        ! Arguments
        integer(int32), intent(in) :: n
            !! The number of entries.
        integer(int32), intent(in) :: p(n)
            !! The array to check.
        integer(int32), intent(out) :: ierror
            !! 0, the array represents a permutation.
            !! nonzero, the array does not represent a permutation.  The 
            !! smallest missing value is equal to IERROR.
    end subroutine

    subroutine perm_inverse(n, p)
        !! Inverts a permutation in-place.
        use iso_fortran_env, only : int32

        ! Arguments
        integer(int32), intent(in) :: n
            !! The number of entries.
        integer(int32), intent(inout) :: p(n)
            !! The permutation, in standard index form.  On output, the
            !! inverse permutation.
    end subroutine

    subroutine points_delaunay_naive_2d(node_num, node_xy, maxtri, &
        triangle_num, triangle_node)
        !! A naive Delaunay triangulation scheme.
        !!
        !! This routine is only suitable as a demonstration code for small
        !! problems.  Its running time is of order NODE_NUM**4.  Much faster
        !! algorithms are available.
        !!
        !! Given a set of nodes in the plane, a triangulation is set of
        !! triples of distinct nodes, forming triangles, so that every
        !! point within the convex hull of the set of nodes is either
        !! one of the nodes, or lies on an edge of one or more triangles,
        !! or lies within exactly one triangle.
        !!
        !! A Delaunay triangulation is a triangulation with additional
        !! properties.
        !!
        !! NODE_NUM must be at least 3.
        use iso_fortran_env, only : int32, real64

        ! Arguments
        integer(int32), intent(in) :: node_num
            !! The number of nodes.
        real(real64), intent(in) :: node_xy(2, node_num)
            !! The coordinates of the nodes.
        integer(int32), intent(in) :: maxtri
            !! The maximum number of triangles.
        integer(int32), intent(out) :: triangle_num
            !! The number of triangles in the triangulation.
        integer(int32), intent(out) :: triangle_node(3, maxtri)
            !! The indices of the triangle nodes.
    end subroutine

    subroutine points_hull_2d(node_num, node_xy, hull_num, hull)
        !! Computes the convex hull of 2D points.
        !!
        !! The work involved is N*log(H), where N is the number of points, and H
        !! is the number of points that are on the hull.
        use iso_fortran_env, only : int32, real64

        ! Arguments
        integer(int32), intent(in) :: node_num
            !! The number of nodes.
        real(real64), intent(in) :: node_xy(2, node_num)
            !! The coordinates of the nodes.
        integer(int32), intent(out) :: hull_num
            !! The number of nodes that lie on the convex hull.
        integer(int32), intent(out) :: hull(node_num)
            !! Entries 1 through HULL_NUM contain the indices of the nodes that 
            !! form the convex hull, in order.
    end subroutine

    subroutine quad_convex_random(seed, xy)
        !! Returns a random convex quadrilateral.
        !!
        !! The quadrilateral is constrained in that the vertices must all lie
        !! with the unit square.
        use iso_fortran_env, only : int32, real64
    
        ! Parameters
        integer(int32), parameter :: node_num = 4
    
        ! Arguments
        integer(int32), intent(inout) :: seed
            !! A seed for the random number generator.
        real(real64), intent(out) :: xy(2, node_num)
            !! The coordinates of the nodes of the quadrilateral given in 
            !! counterclockwise order.
    end subroutine

    subroutine r8tris2(node_num, node_xy, triangle_num, triangle_node, &
        triangle_neighbor)
        !! Constructs a Delaunay triangulation of 2D vertices.
        !!
        !! The routine constructs the Delaunay triangulation of a set of 2D vertices
        !! using an incremental approach and diagonal edge swaps.  Vertices are
        !! first sorted in lexicographically increasing (X,Y) order, and
        !! then are inserted one at a time from outside the convex hull.
        use iso_fortran_env, only : int32, real64

        ! Arguments
        integer(int32), intent(in) :: node_num
            !! The number of vertices.
        real(real64), intent(inout) :: node_xy(2, node_num)
            !! The coordinates of the vertices.  On output, the vertices have 
            !! been sorted into dictionary order.
        integer(int32), intent(out) :: triangle_num
            !! The number of triangles in the triangulation;  TRIANGLE_NUM is 
            !! equal to 2*NODE_NUM - NB - 2, where NB is the number of boundary 
            !! vertices.
        integer(int32), intent(out) :: triangle_node(3, 2*node_num)
            !! The nodes that make up each triangle.  The elements are indices 
            !! of P.  The vertices of the triangles are in counter clockwise 
            !! order.
        integer(int32), intent(out) :: triangle_neighbor(3, 2*node_num)
            !! The triangle neighbor list.  Positive elements are indices of 
            !! TIL; negative elements are used for links of a counter clockwise 
            !! linked list of boundary edges; LINK = -(3*I + J-1) where I, 
            !! J = triangle, edge index; TRIANGLE_NEIGHBOR(J,I) refers to the 
            !! neighbor along edge from vertex J to J+1 (mod 3).
    end subroutine

    subroutine triangle_circumcenter_2d(t, center)
        !! Computes the circumcenter of a triangle in 2D.
        !!
        !! The circumcenter of a triangle is the center of the circumcircle, the
        !! circle that passes through the three vertices of the triangle.
        !!
        !! The circumcircle contains the triangle, but it is not necessarily the
        !! smallest triangle to do so.
        !!
        !! If all angles of the triangle are no greater than 90 degrees, then
        !! the center of the circumscribed circle will lie inside the triangle.
        !! Otherwise, the center will lie outside the circle.
        !!
        !! The circumcenter is the intersection of the perpendicular bisectors
        !! of the sides of the triangle.
        !!
        !! In geometry, the circumcenter of a triangle is often symbolized by "O".
        use iso_fortran_env, only : real64

        ! Arguments
        real(real64), intent(in) :: t(2, 3)
        real(real64), intent(out) :: center(2)
    end subroutine
end interface
end module