module geompack_test_routines
    use iso_fortran_env
    use geompack
    use fortran_test_helper
    implicit none
contains
! ------------------------------------------------------------------------------
function test_r8tris2() result(rst)
    ! Arguments
    logical :: rst

    ! Parameters
    integer(int32), parameter :: npts = 5
    integer(int32), parameter :: ntri_ans = 4

    ! Local Variables
    integer(int32) :: ntri, tris(3, 2 * npts), tneigh(3, 2 * npts), &
        tri_ans(3, ntri_ans)
    real(real64) :: x(npts), y(npts), nodes(2, npts)

    ! Initialization
    rst = .true.
    x = [-1.0d0, 1.0d0, 1.0d0, -1.0d0, 0.0d0]
    y = [-1.0d0, -1.0d0, 1.0d0, 1.0d0, 0.0d0]
    nodes(1,:) = x
    nodes(2,:) = y
    tri_ans = reshape([4, 1, 5, 5, 1, 2, 5, 2, 3, 4, 5, 3], [3, ntri_ans])

    ! Process
    tris = 0
    tneigh = 0
    call r8tris2(npts, nodes, ntri, tris, tneigh)

    ! Check the output
    if (.not.assert(ntri, ntri_ans)) then
        rst = .false.
    end if
    if (.not.assert(tris(:,:ntri_ans), tri_ans)) then
        rst = .false.
    end if
end function

! ------------------------------------------------------------------------------
end module