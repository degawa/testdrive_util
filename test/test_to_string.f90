program test_to_string
    use, intrinsic :: iso_fortran_env, only: error_unit, int32
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use :: unittests_to_string, only:collect_test_to_string
    implicit none

    integer(int32) :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("to_string", &
                               collect_test_to_string) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program test_to_string
