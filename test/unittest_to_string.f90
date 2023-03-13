module unittests_to_string
    use :: testdrive, only:new_unittest, unittest_type, error_type, check
    use :: testdrive_util, only:to_string
    implicit none
    private
    public :: collect_test_to_string

contains
    subroutine collect_test_to_string(testsuite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [new_unittest("it returns 'T' when input `.true.`", &
                                  to_string_should_return_T_in_string_when_input_true), &
                     new_unittest("it returns 'F' when input `.false.`", &
                                  to_string_should_return_F_in_string_when_input_false) &
                     ]
    end subroutine collect_test_to_string

    subroutine to_string_should_return_T_in_string_when_input_true(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        call check(error, actual=to_string(.true.), expected="T")
    end subroutine to_string_should_return_T_in_string_when_input_true

    subroutine to_string_should_return_F_in_string_when_input_false(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        call check(error, actual=to_string(.false.), expected="F")
    end subroutine to_string_should_return_F_in_string_when_input_false
end module unittests_to_string
