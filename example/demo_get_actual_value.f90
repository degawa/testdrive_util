
program demo_get_actual_value
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:get_actual_value, occurred
    use :: stdlib_logger
    implicit none

    integer(int32) :: scratch_unit
    type(error_type), allocatable :: error
    character(:), allocatable :: actual

    open (newunit=scratch_unit, status="scratch")
    call global_logger%add_log_unit(scratch_unit)
    call global_logger%configure(time_stamp=.false.)

    call global_logger%log_information("calling global_logger%log_inoformation")
    call global_logger%remove_log_unit(scratch_unit, close_unit=.false.)

    call get_actual_value(error, scratch_unit, actual)
    call check(error, actual == "INFO: "//"calling global_logger%log_inoformation")
    if (occurred(error)) then
        error stop
    else
        print *, "no error occurred"
    end if

end program demo_get_actual_value
