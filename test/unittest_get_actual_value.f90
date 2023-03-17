module unittests_get_actual_value
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type, error_type, check
    use :: testdrive_util, only:get_actual_value
    implicit none
    private
    public :: collect_test_get_actual_value

contains
    subroutine collect_test_get_actual_value(testsuite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [new_unittest("it reads string from unit specified by `unit_number`", &
                                  get_actual_value_should_read_string_from_unit) &
                     , new_unittest("it reads zero length string when file is empty", &
                                    get_actual_value_should_read_0_len_string_from_empty_file) &
                     , new_unittest("it allocate `error` when an error occured", &
                                    get_actual_value_should_allocate_error_when_error_occured) &
                     ]
    end subroutine collect_test_get_actual_value

    subroutine get_actual_value_should_read_string_from_unit(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        type(error_type), allocatable :: err
        integer(int32) :: unit_number
        character(:), allocatable :: string, test_name

        call setup(unit_number, test_name)

        call get_actual_value(err, unit_number, string)

        call check(error,.not. allocated(err), &
                   "expected that `error` have not been allocated, but allocated.")
        if (allocated(error)) return

        call check(error, trim(string) == "read_actual_value() should read string from unit specified by `unit_number`", &
                   "expected "//test_name//", but got "//trim(string))

        call teardown(unit_number, test_name)
    contains
        subroutine setup(scratch_unit_number, str)
            implicit none
            integer(int32), intent(inout) :: scratch_unit_number
            character(:), allocatable, intent(inout) :: str

            str = "read_actual_value() should read string from unit specified by `unit_number`"
            open (newunit=scratch_unit_number, status="scratch")
            write (scratch_unit_number, '(A)') str
        end subroutine setup

        subroutine teardown(scratch_unit_number, str)
            implicit none
            integer(int32), intent(in) :: scratch_unit_number
            character(:), allocatable, intent(inout) :: str

            close (scratch_unit_number)
            if (allocated(str)) deallocate (str)
        end subroutine teardown
    end subroutine get_actual_value_should_read_string_from_unit

    subroutine get_actual_value_should_read_0_len_string_from_empty_file(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        type(error_type), allocatable :: err
        integer(int32) :: unit_number
        character(:), allocatable :: string, test_name

        call setup(unit_number, test_name)

        call get_actual_value(err, unit_number, string)

        call check(error,.not. allocated(err), &
                   "expected that `error` have not been allocated, but allocated.")
        if (allocated(error)) return

        call check(error, allocated(string), &
                   "expected that `string` have been allocated, but not allocated.")
        if (allocated(error)) return

        call check(error, trim(string) == "", &
                   "expected "//'""'//", but got "//'"'//trim(string)//'"')
        if (allocated(error)) return

        call teardown(unit_number, test_name)
    contains
        subroutine setup(scratch_unit_number, str)
            implicit none
            integer(int32), intent(inout) :: scratch_unit_number
            character(:), allocatable, intent(inout) :: str

            str = "read_actual_value() should read zero length string when unit is empty."
            open (newunit=scratch_unit_number, status="scratch")
        end subroutine setup

        subroutine teardown(scratch_unit_number, str)
            implicit none
            integer(int32), intent(in) :: scratch_unit_number
            character(:), allocatable, intent(inout) :: str

            close (scratch_unit_number)
            if (allocated(str)) deallocate (str)
        end subroutine teardown
    end subroutine get_actual_value_should_read_0_len_string_from_empty_file

    subroutine get_actual_value_should_allocate_error_when_error_occured(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        type(error_type), allocatable :: err
        integer(int32) :: unit_number
        character(:), allocatable :: string, test_name

        call setup(unit_number, test_name)

        call get_actual_value(err, unit_number, string)

        call check(error, allocated(err), &
                   "expected that `error` have been allocated, but not allocated.")
        if (allocated(error)) return

        call teardown(test_name, err)
    contains
        subroutine setup(scratch_unit_number, str)
            implicit none
            integer(int32), intent(inout) :: scratch_unit_number
            character(:), allocatable, intent(inout) :: str

            str = "read_actual_value() should allocate `error` when an error occured."
            scratch_unit_number = 100
        end subroutine setup

        subroutine teardown(str, err)
            implicit none
            character(:), allocatable, intent(inout) :: str
            type(error_type), allocatable, intent(inout) :: err

            if (allocated(str)) deallocate (str)
            if (allocated(err)) then
                err%stat = 0
                deallocate (err)
            end if
        end subroutine teardown
    end subroutine get_actual_value_should_allocate_error_when_error_occured

end module unittests_get_actual_value
