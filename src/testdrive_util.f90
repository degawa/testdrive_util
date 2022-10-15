module testdrive_util
    use :: testdrive, only:run_testsuite, testsuite_type, error_type
    implicit none
    private
    public :: occurred
    public :: run_test

    character(len=*), private, parameter :: fmt = '("#", *(1x, a))'

contains
    !> utility function allowing to write `if(occurred(error))`
    !> instead of `if(allocated(error))`
    logical function occurred(error)
        implicit none

        type(error_type), allocatable :: error

        occurred = allocated(error)
    end function occurred

    !> a subroutine running tests
    subroutine run_test(testsuites)
        use, intrinsic :: iso_fortran_env, only: error_unit
        implicit none
        type(testsuite_type) :: testsuites(:)
        integer :: stat, is

        stat = 0

        do is = 1, size(testsuites)
            write (error_unit, fmt) "Testing:", testsuites(is)%name
            call run_testsuite(testsuites(is)%collect, error_unit, stat)
        end do

        if (stat > 0) then
            write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
            error stop
        end if
    end subroutine run_test
end module testdrive_util
