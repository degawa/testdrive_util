module test_FEATURE_unitTests_FUNCTION
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    implicit none
    private
    public :: FUNCTION_test_condition

contains
    subroutine FUNCTION_test_condition(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        ! execut FUNCTION

        call check(error, cond, "")
    end subroutine FUNCTION_test_condition
end module test_FEATURE_unitTests_FUNCTION
