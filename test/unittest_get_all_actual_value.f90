module unittests_get_actual_value_to_eof
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type, error_type, check, to_string
    use :: testdrive_util, only:get_all_actual_value
    implicit none
    private
    public :: collect_test_get_actual_value_to_eof

contains
    subroutine collect_test_get_actual_value_to_eof(testsuite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [new_unittest("it reads string from unit specified by `unit_number`", &
                                  get_actual_value_to_eof_should_read_string_from_unit) &
                     , new_unittest("it reads zero length string when file is empty", &
                                    get_actual_value_to_eof_should_read_0_len_str_from_empty_file) &
                     , new_unittest("it does not read tailing spaces", &
                                    get_actual_value_to_eof_should_not_read_tailing_space) &
                     ]
    end subroutine collect_test_get_actual_value_to_eof

    subroutine get_actual_value_to_eof_should_read_string_from_unit(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        type(error_type), allocatable :: err
        integer(int32) :: unit_number
        character(:), allocatable :: string, expected

        call setup(unit_number, expected)

        call get_all_actual_value(err, unit_number, string)

        call check(error,.not. allocated(err), &
                   "expected that `error` have not been allocated, but allocated.")
        if (allocated(error)) return

        call check(error, trim(string) == trim(expected), &
                   "expected "//expected//", but got "//trim(string))

        call teardown(unit_number, expected)
    contains
        subroutine setup(scratch_unit_number, str)
            implicit none
            integer(int32), intent(inout) :: scratch_unit_number
            character(:), allocatable, intent(inout) :: str

            str = "get_actual_value_from_beginning_to_eof_as_str_from_unit() should read string"//new_line("a")// &
                  "from beginning to EOF"//new_line("a")// &
                  "from unit specified by `unit_number`"
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
    end subroutine get_actual_value_to_eof_should_read_string_from_unit

    subroutine get_actual_value_to_eof_should_read_0_len_str_from_empty_file(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        type(error_type), allocatable :: err
        integer(int32) :: unit_number
        character(:), allocatable :: string, test_name

        call setup(unit_number, test_name)

        call get_all_actual_value(err, unit_number, string)

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
    end subroutine get_actual_value_to_eof_should_read_0_len_str_from_empty_file

    subroutine get_actual_value_to_eof_should_not_read_tailing_space(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        type(error_type), allocatable :: err
        integer(int32) :: unit_number
        character(:), allocatable :: string, expected
        integer(int32) :: len_expected, len_actual

        call setup(unit_number, expected, len_expected)

        call get_all_actual_value(err, unit_number, string)

        call check(error,.not. allocated(err), &
                   "expected that `error` have not been allocated, but allocated.")
        if (allocated(error)) return

        len_actual = len(string)
        call check(error, len_actual == len_expected - 3, &
                   "expected "//to_string(len_expected - 3)//", but got "//to_string(len_actual))

        call teardown(unit_number, expected)
    contains
        subroutine setup(scratch_unit_number, str, len_str)
            implicit none
            integer(int32), intent(inout) :: scratch_unit_number
            character(:), allocatable, intent(inout) :: str
            integer(int32), intent(out) :: len_str

            str = "this string "//new_line("a")// &
                  "has some "//new_line("a")// &
                  "tailing space "
            open (newunit=scratch_unit_number, status="scratch")
            write (scratch_unit_number, '(A)') str
            len_str = len(str)
        end subroutine setup

        subroutine teardown(scratch_unit_number, str)
            implicit none
            integer(int32), intent(in) :: scratch_unit_number
            character(:), allocatable, intent(inout) :: str

            close (scratch_unit_number)
            if (allocated(str)) deallocate (str)
        end subroutine teardown
    end subroutine get_actual_value_to_eof_should_not_read_tailing_space
end module unittests_get_actual_value_to_eof
