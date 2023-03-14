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

## Related projects
- [test-drive](https://github.com/fortran-lang/test-drive) - a community-made, lightweight, procedural unit testing framework for Fortran

## Acknowledgments
I would like to express my gratitude to fortran-lang community for their activities and creating some great softwares.
