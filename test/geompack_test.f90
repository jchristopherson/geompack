program main
    use geompack_test_routines
    implicit none

    logical :: check

    ! Tests
    check = test_r8tris2()
    if (.not. check) stop 1
end program