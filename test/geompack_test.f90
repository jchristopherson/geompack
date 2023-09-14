program main
    use geompack_test_routines
    implicit none

    logical :: check

    ! Tests
    check = test_alpha_measure()
    if (.not. check) stop 1
end program