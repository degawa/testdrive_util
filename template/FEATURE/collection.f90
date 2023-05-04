module test_FEATURE_collection
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_FEATURE_unitTests_FUNCTION
    implicit none
    private
    public :: collect_FEATURE

contains
    subroutine collect_FEATURE(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("test name.", &
                                  FUNCTION_test_condition) &
                     ]
    end subroutine collect_FEATURE
end module test_FEATURE_collection
