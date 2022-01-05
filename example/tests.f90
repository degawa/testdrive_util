module tests
    use :: testdrive, only:new_unittest, unittest_type, error_type, check
    implicit none
    private
    public :: collect_test_runtest_pass, collect_test_runtest_fail

contains
    subroutine collect_test_runtest_pass(testsuite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [new_unittest("this test is always passed", &
                                  test_always_pass) &
                     ]
    end subroutine collect_test_runtest_pass

    subroutine collect_test_runtest_fail(testsuite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [new_unittest("this test is always failed", &
                                  test_always_fail, should_fail=.true.) &
                     ]
    end subroutine collect_test_runtest_fail

    subroutine test_always_pass(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        call check(error, .true.)
    end subroutine test_always_pass

    subroutine test_always_fail(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        call check(error, .false., message="the test successfully failed")
    end subroutine test_always_fail
end module tests
