program quatexample
  use quaternions
  implicit none
	
  double precision  :: testvec(3) = (/ 1.d0, 0.d0, 0.d0 /)  
  type (quaternion) :: testquat, testquat2

  !---- THIS IS NOT OPTIONAL  ----!
  call testquat%init() ! or e.g.  call testquat%init(1.d0, 0.d0, 0.d0, 1.d0) 
  !----                       ----!

  call testquat%set(1.0d0, 0.d0, 0.d0, 1.0d0)
  write(6,*) 'testquat', testquat%array

  call testquat%add(testquat)   ! or e.g. testquat = testquat + testquat
  write(6,*) 'testquat', testquat%scalar, testquat%vector

  testquat = testquat * 0.5d0   ! or e.g. call testquat%multiply(0.5d0)
  write(6,*) 'testquat', testquat%w, testquat%x, testquat%y, testquat%z

  call testquat%normalize()
  write(6,*) 'testquat', testquat%array

  write(6,*) 'testvec', testvec
  write(6,*) 'testvec', rotatedVectorQ(testvec, testquat)

  testquat = testquat * testquat%inverse()
  write(6,*) 'testquat', testquat%array

end program
