program testquaternions
    use quaternions
    implicit none

    type(quaternion) :: testquat, testquat2
    call testquat%init()
    call testquat2%init()

    call testquat%set(1.d0, 1.d0, 1.d0, 1.d0)
    call testquat%add(1.d0)
    call testquat%multiply(-1.d0)

    write(6,*) testquat%array, testquat%w, testquat%x, testquat%y, testquat%z, testquat%scalar, testquat%vector
    write(6,*) testquat%norm()
    call testquat%normalize()
    write(6,*) testquat%norm()
    call testquat%conjugate()
    write(6,*) testquat%norm()
    call testquat%invert()
    write(6,*) testquat%norm()

    call testquat2%copy(testquat)
    testquat=addQuaternions(testquat, testquat2)
    write(6,*) testquat%array, testquat%w, testquat%x, testquat%y, testquat%z, testquat%scalar, testquat%vector

    write(6,*) 'Example program output:'
    call execute_command_line("./quatexample")
end program
