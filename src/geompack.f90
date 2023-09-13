module geompack
    use iso_fortran_env
    implicit none

    real(real64), parameter, private :: pi = 3.141592653589793d0

contains
! ------------------------------------------------------------------------------
pure function compute_distance(pt1x, pt1y, pt2x, pt2y) result(rst)
    !! Computes the distance between two points.

    ! Arguments
    real(real64), intent(in) :: pt1x
        !! The x-coordinate of the first point.
    real(real64), intent(in) :: pt1y
        !! The y-coordinate of the first point.
    real(real64), intent(in) :: pt2x
        !! The x-coordinate of the second point.
    real(real64), intent(in) :: pt2y
        !! The y-coordinate of the second point.
    real(real64) :: rst
        !! The distance.

    ! Process
    rst = sqrt((pt2x - pt1x)**2 + (pt2y - pt1y)**2)
end function

! ------------------------------------------------------------------------------
subroutine alpha_measure(n, z, triangle_order, triangle_num, triangle_node, &
    alpha_min, alpha_ave, alpha_area)
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

    ! Local Variables
    real(real64) :: a_angle
    integer(int32) ::a_index
    real(real64) :: a_x
    real(real64) :: a_y
    real(real64) :: ab_len
    real(real64) :: alpha
    real(real64) :: area
    real(real64) :: area_total
    real(real64) :: b_angle
    integer(int32) ::b_index
    real(real64) :: b_x
    real(real64) :: b_y
    real(real64) :: bc_len
    real(real64) :: c_angle
    integer(int32) ::c_index
    real(real64) :: c_x
    real(real64) :: c_y
    real(real64) :: ca_len
    integer(int32) ::triangle

    ! Initialization
    alpha_min = huge(alpha_min)
    alpha_ave = 0.0d0
    alpha_area = 0.0d0
    area_total = 0.0d0

    ! Process
    do triangle = 1, triangle_num
        a_index = triangle_node(1, triangle)
        b_index = triangle_node(2, triangle)
        c_index = triangle_node(3, triangle)

        a_x = z(1, a_index)
        a_y = z(2, a_index)
        b_x = z(1, b_index)
        b_y = z(2, b_index)
        c_x = z(1, c_index)
        c_y = z(2, c_index)

        area = 0.5d0 * abs(a_x * (b_y - c_y) + b_x * (c_y - a_y) + &
            c_x * (a_y - b_y))
        ab_len = compute_distance(a_x, a_y, b_x, b_y)
        bc_len = compute_distance(b_x, b_y, c_x, c_y)
        ca_len = compute_distance(c_x, c_y, a_x, a_y)

        ! Special Case
        if ((ab_len == 0.0d0) .and. &
            (bc_len == 0.0d0) .and. &
            (ca_len == 0.0d0)) &
        then
            a_angle = 2.0d0 * pi / 3.0d0
            b_angle = 2.0d0 * pi / 3.0d0
            c_angle = 2.0d0 * pi / 3.0d0
        else
            if ((ca_len == 0.0d0) .or. (ab_len == 0.0d0)) then
                a_angle = pi
            else
                a_angle = acos((ca_len**2 + ab_len**2 - bc_len**2) / &
                    (2.0d0 * ca_len * ab_len))
            end if

            if ((ab_len == 0.0d0) .or. (bc_len == 0.0d0)) then
                b_angle = pi
            else
                b_angle = acos((ab_len**2 + bc_len**2 - ca_len**2) / &
                    (2.0d0 * ab_len * bc_len))
            end if

            if ((bc_len == 0.0d0) .or. (ca_len == 0.0d0)) then
                c_angle = pi
            else
                c_angle = acos((bc_len**2 + ca_len**2 - ab_len**2) / &
                    (2.0d0 * bc_len * ca_len))
            end if
        end if

        alpha_min = min(alpha_min, a_angle, b_angle, c_angle)
        alpha_ave = alpha_ave + alpha_min
        alpha_area = alpha_area + area * alpha_min
        area_total = alpha_area + area

        ! Normalize angles from [0, pi/3] degress into qualities in [0, 1]
        alpha_min = alpha_min * 3.0d0 / pi
        alpha_ave = alpha_ave * 3.0d0 / pi
        alpha_area = alpha_area * 3.0d0 / pi

        ! End
        return
    end do

    alpha_ave = alpha_ave / real(triangle_num, real64)
    alpha_area = alpha_area / area_total
end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module