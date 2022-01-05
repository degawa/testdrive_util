
program demo_runtest
    use testdrive, only: new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    use :: tests, only:collect_test_runtest_pass, collect_test_runtest_fail
    implicit none

    type(testsuite_type), allocatable :: testsuites(:)

    testsuites = [ &
                 new_testsuite("test suite that alway passes", &
                               collect_test_runtest_pass), &
                 new_testsuite("test suite that alway failds", &
                               collect_test_runtest_fail) &
                 ]
    call run_test(testsuites)
end program demo_runtest
