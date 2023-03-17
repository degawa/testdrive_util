module testdrive_util
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:run_testsuite, testsuite_type, error_type, check, to_string
    implicit none
    private
    public :: occurred
    public :: run_test
    public :: to_string
    public :: get_actual_value

    character(len=*), private, parameter :: fmt = '("#", *(1x, a))'

    integer(int32), private, parameter :: default_buffer_length = 256

    interface to_string
        procedure :: to_string_l
    end interface

    interface get_actual_value
        procedure :: get_actual_value_from_unit_str
    end interface

contains
    !> returns `.true.` when `error` is allocated.
    !>
    !> this function allows to write `if(occurred(error))`
    !> instead of `if(allocated(error))`
    logical function occurred(error)
        implicit none
        type(error_type), allocatable, intent(in) :: error

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

    !>get string from unit specified by `unit_number`
    !>and stores it in `string`.
    !>
    !>allocate `error` if an error occurs.
    !>
    !>The unit specified by `unit_number` must be open.
    subroutine get_actual_value_from_unit_str(error, unit_number, string)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        integer(int32), intent(in) :: unit_number
            !! opened unit number containing actual value
        character(:), allocatable, intent(inout) :: string

        logical :: opened
        integer(int32) :: iostat
        character(default_buffer_length) :: buffer

        inquire (unit=unit_number, opened=opened, iostat=iostat)
        call check(error, opened, &
                   "unit "//to_string(unit_number)//" is not opened.")
        if (occurred(error)) return

        rewind (unit=unit_number, iostat=iostat)
        call check(error, iostat == 0, &
                   "io error "//to_string(iostat)// &
                   " occured while rewinding the unit.")
        if (occurred(error)) return

        read (unit_number, '(A)', iostat=iostat) buffer
        if (is_iostat_end(iostat)) then
            buffer = ""
        else
            call check(error, iostat == 0, &
                       "io error "//to_string(iostat)// &
                       " occured while reading actual value from the unit.")
            if (occurred(error)) return
        end if

        string = trim(buffer)
    end subroutine get_actual_value_from_unit_str
end module testdrive_util
