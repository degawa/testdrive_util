module testdrive_util
    use :: testdrive, only:run_testsuite, testsuite_type, error_type
    implicit none
    private
    public :: occurred
    public :: run_test
    public :: to_string

    character(len=*), private, parameter :: fmt = '("#", *(1x, a))'

    interface to_string
        procedure :: to_string_l
    end interface

contains
    !> returns `.true.` when `error` is allocated.
    !>
    !> this function allows to write `if(occurred(error))`
    !> instead of `if(allocated(error))`
    logical function occurred(error)
        implicit none

        type(error_type), allocatable :: error

        occurred = allocated(error)
    end function occurred

    !> runs test suites.
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

    !> returns "T"/"F" when l is `.true.`/`.false.`
    function to_string_l(l) result(string)
        implicit none
        logical, intent(in) :: l
        character(1) :: string

        if (l) then
            string = "T"
        else
            string = "F"
        end if
    end function to_string_l
end module testdrive_util
