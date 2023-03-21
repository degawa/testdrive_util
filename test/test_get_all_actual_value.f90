program test_read_actual_value_to_eof
    use, intrinsic :: iso_fortran_env, only: error_unit, int32
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use :: unittests_get_actual_value_to_eof
    implicit none

    integer(int32) :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("get_actual_value_to_eof", &
                               collect_test_get_actual_value_to_eof) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program test_read_actual_value_to_eof
