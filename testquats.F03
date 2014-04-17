subroutine calcNormSqDev(resultArray, expectedArray, datlen, sqdev, sqdevsum, counter)
    implicit none
    integer                             ::  datlen, counter
    double precision, intent(in)        ::  resultArray(datlen), expectedArray(datlen)
    double precision                    ::  sqdev, sqdevsum

    double precision                    ::  normh
    integer                             ::  it

    sqdev = 0.d0

    normh = 1.d0 / dot_product(expectedArray, expectedArray)
    sqdev = sqdev + normh * dot_product(expectedArray - resultArray, expectedArray - resultArray)

    !    write(6,*)  '[calcSqDev] Error: Provided arrays are of unequal length!'

    sqdevsum = sqdevsum + sqdev
    counter  = counter + 1

    write(6,*) 'results', resultArray
    write(6,*) 'expected', expectedArray
    write(6,*) 'sqdev', sqdev
end subroutine

subroutine sqDevTest(sqdev, tolerance, datlen, testpassed, allpassed)
    implicit none
    double precision, intent(in)            ::  sqdev, tolerance
    integer, intent(in)                     ::  datlen
    logical                                 ::  allpassed, testpassed

    testpassed = .false.
    if (sqdev < tolerance*datlen**2) then
        testpassed = .true.
    end if
    allpassed = allpassed .and. testpassed
    write(6,*) 'passed', testpassed
end subroutine

program testquaternions
    use quaternions
    implicit none

    type(quaternion)                    ::  testquat, testquat2
    double precision                    ::  sqdev, sqdevsum = 0.d0
    integer                             ::  counter = 0

    double precision                    ::  expectedArray(4), tolerance = 1.d-30
    logical                             ::  testpassed, allpassed = .true.
    call testquat%init()
    call testquat2%init()

    write(6,*) 'Running tests...'
    write(6,*)
    write(6,*)  'Test "set from axis-angle"'
    call testquat%set((/ 1.d0, 1.d0, 1.d0 /), 0.75d0*dacos(-1.d0))
    expectedArray = (/ 0.38268343236508984d0, 0.533402096794177d0, 0.533402096794177d0, 0.533402096794177d0 /)
    call calcNormSqDev(testquat%array, expectedArray, 4, sqdev, sqdevsum, counter)
    call sqDevTest(sqdev, tolerance, 4, testpassed, allpassed)
    write(6,*)

    write(6,*)  'Test "overloaded operators with scalars"'
    testquat = testquat + 1.d0
    testquat = testquat - 1.d0
    testquat = (-2.d0) * testquat
    testquat = testquat / (-2.d0)
    expectedArray = (/ 0.38268343236508984d0, 0.533402096794177d0, 0.533402096794177d0, 0.533402096794177d0 /)
    call calcNormSqDev(testquat%array, expectedArray, 4, sqdev, sqdevsum, counter)
    call sqDevTest(sqdev, tolerance, 4, testpassed, allpassed)
    write(6,*)

    write(6,*)  'Test "normalize"'
    call testquat%normalize()
        expectedArray = (/ 0.38268343236508984d0, 0.533402096794177d0, 0.533402096794177d0, 0.533402096794177d0 /)
    call calcNormSqDev(testquat%array, expectedArray, 4, sqdev, sqdevsum, counter)
    call sqDevTest(sqdev, tolerance, 4, testpassed, allpassed)
    write(6,*)

    write(6,*)  'Test "conjugate"'
    call testquat%conjugate()
        expectedArray = (/ 0.38268343236508984d0, -0.533402096794177d0, -0.533402096794177d0, -0.533402096794177d0 /)
    call calcNormSqDev(testquat%array, expectedArray, 4, sqdev, sqdevsum, counter)
    call sqDevTest(sqdev, tolerance, 4, testpassed, allpassed)
    write(6,*)

    write(6,*)  'Test "multiply by scalar"'
    call testquat%multiply(-2.d0)
        expectedArray = -2.d0 * (/ 0.38268343236508984d0, -0.533402096794177d0, -0.533402096794177d0, -0.533402096794177d0 /)
    call calcNormSqDev(testquat%array, expectedArray, 4, sqdev, sqdevsum, counter)
    call sqDevTest(sqdev, tolerance, 4, testpassed, allpassed)
    write(6,*)

    write(6,*)  'Test "invert"'
    call testquat2%copy(testquat)
    call testquat%invert()
        expectedArray = -0.5d0*(/ 0.38268343236508984d0, 0.533402096794177d0, 0.533402096794177d0, 0.533402096794177d0 /)
    call calcNormSqDev(testquat%array, expectedArray, 4, sqdev, sqdevsum, counter)
    call sqDevTest(sqdev, tolerance, 4, testpassed, allpassed)
    write(6,*)

    write(6,*)  'Test "multiply (with the pre-invert version of testquat)"'
    call testquat%multiply(testquat2)
        expectedArray = (/ 1.d0, 0.d0, 0.d0, 0.d0 /)
    call calcNormSqDev(testquat%array, expectedArray, 4, sqdev, sqdevsum, counter)
    call sqDevTest(sqdev, tolerance, 4, testpassed, allpassed)
    write(6,*)

    !write(6,*) testquat%array, testquat%w, testquat%x, testquat%y, testquat%z, testquat%scalar, testquat%vector
    !write(6,*) testquat%norm()
    !call testquat%normalize()
    !write(6,*) testquat%norm()
    !call testquat%conjugate()
    !write(6,*) testquat%norm()
    !call testquat%invert()
    !write(6,*) testquat%norm()

    call testquat2%copy(testquat)
    testquat=addQuaternions(testquat, testquat2)
    write(6,*) testquat%array, testquat%w, testquat%x, testquat%y, testquat%z, testquat%scalar, testquat%vector

    if (allpassed) then
        write(6,*) 'Yay, all tests passed!'
    end if

    write(6,*) 'Example program output:'
    call execute_command_line("./quatexample")
end program
