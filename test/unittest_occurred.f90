module unittests_occurred
    use :: testdrive, only:new_unittest, unittest_type, error_type, check
    use :: testdrive_util, only:occurred
    implicit none
    private
    public :: collect_test_occurred

contains
    subroutine collect_test_occurred(testsuite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [new_unittest("it returns true when argument `error` is allocated", &
                                  occurred_should_return_true_when_error_is_allocated), &
                     new_unittest("it returns false when argument `error` is not allocated", &
                                  occurred_should_return_false_when_error_is_not_allocated) &
                     ]
    end subroutine collect_test_occurred

    subroutine occurred_should_return_true_when_error_is_allocated(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(error_type), allocatable :: err_allocated

        allocate (err_allocated)
        call check(error, actual=occurred(err_allocated), expected=.true.)
    end subroutine occurred_should_return_true_when_error_is_allocated

    subroutine occurred_should_return_false_when_error_is_not_allocated(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        type(error_type), allocatable :: err_not_allocated

        if (allocated(err_not_allocated)) deallocate (err_not_allocated)
        call check(error, actual=occurred(err_not_allocated), expected=.false.)
    end subroutine occurred_should_return_false_when_error_is_not_allocated
end module unittests_occurred
