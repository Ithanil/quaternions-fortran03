program quatexample
  use quaternions
  implicit none
	
  double precision  :: testvec = (/ 1.d0, 0.d0, 0.d0 /)  
  type (quaternion) :: testquat

  !---- THIS IS NOT OPTIONAL  ----!
  call testquat%init() ! or e.g.  call testquat%init(1.d0, 0.d0, 0.d0, 1.d0) 
  !----                       ----!

  call testquat%set(1.d0, 2.d0, 3.d0, 4.d0)
  write(6,*) 'testquat', testquat%array

  testquat = testquat + testquat
  write(6,*) 'testquat', testquat%scalar, testquat%vector

  testquat = testquat * 0.5d0
  write(6,*) 'testquat', testquat%w, testquat%x, testquat%y, testquat%z

  call testquat%normalize()
  write(6,*) 'testquat', testquat%array

  write(6,*) 'testvec', testvec
  write(6,*) 'testvec', rotatedVectorQ(testvec, testquat)  

  call testquat%multiply(testquat%inverse())
  write(6,*) 'testquat', testquat%array 

end program
