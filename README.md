# testdrive_util
This repository provides procedures for me to improve the convenience of [test-drive](https://github.com/fortran-lang/test-drive), a testing framework for Fortran.

## features
### occurred
`occurred` function returns `.true.` if the argument `error` is allocated.
This function is introduced with a purpose of writing `if(occurred(error))` instead of `if(allocated(error))` to improve readability of codes.

```Fortran
call check(error, 1 + 2 == 3)
if (occurred(error)) return
```

When I started using test-drive, I confused because I couldn't catch the intention of `if (allocated (error))`. I found that `error` is allocated only when a test fails. I think `occurrd` (there may be other more appropriate names) is better than `allocated` for expressing the behavior.

### run_test
`run_test` subroutine executes the typical expression when executing testsuites shown below:

```Fortran
type(testsuite_type) :: testsuites(:)
integer :: stat, is

do is = 1, size(testsuites)
    write (error_unit, fmt) "Testing:", testsuites(is)%name
    call run_testsuite(testsuites(is)%collect, error_unit, stat)
end do

if (stat > 0) then
    write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
    error stop
end if
```

### to_string
`to_string` returns `"T"`/`"F"` when the argument is `.true.`/`.false.`

```Fortran
print *, "expected "//to_string(.true.)//", but got "//to_string(.false.)
! expected T, but got F
```

### get_actual_value
`get_actual_value` is a helper subroutine to somewhat simplify testing of procedures with output to a unit.

`get_actual_value` rewinds the opened unit and reads contents at the first line as a string.

The following code is a `get_actual_value` usage example for testing a log message output of the logger provided by stdlib.

```Fortran
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:get_actual_value, occurred
    use :: stdlib_logger
    implicit none

    type(error_type), allocatable :: error

    integer(int32) :: scratch_unit
    character(:), allocatable :: actual

    open (newunit=scratch_unit, status="scratch")
    call global_logger%add_log_unit(scratch_unit)
    call global_logger%configure(time_stamp=.false.)

    call global_logger%log_information("calling global_logger%log_inoformation")
    call global_logger%remove_log_unit(scratch_unit, close_unit=.false.)

    ! read string from scratch file for testing string output.
    call get_actual_value(error, scratch_unit, actual)

    ! stdlib logger_type%log_information should output log message with prefix "INFO: "
    call check(error, actual == "INFO: "//"calling global_logger%log_inoformation")
    if (occurred(error)) then
        error stop
    else
        print *, "no error occurred"
    end if
```

## Related projects
- [test-drive](https://github.com/fortran-lang/test-drive) - a community-made, lightweight, procedural unit testing framework for Fortran

## Acknowledgments
I would like to express my gratitude to fortran-lang community for their activities and creating some great softwares.
