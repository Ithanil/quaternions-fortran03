quaternions-fortran03
=====================

##### Short Description:
A Fortran 2003 module, which provides a quaternion type with common operations as type-bound procedures and several functions on type quaternion (including overloading of + / - / * / =)

### Basic Usage Example:

```FORTRAN

program testQuaternion
  implicit none

  type (quaternion) :: testquat

  !---- THIS IS NOT OPTIONAL  ----!
  call testquat%init() ! or e.g.  call testquat%init(1.d0, 0.d0, 0.d0, 1.d0) 
  !----                       ----!

  testquat%set(1.d0, 2.d0, 3.d0, 4.d0)
  write(6,*) testquat%array

  testquat = testquat + testquat
  write(6,*) testquat%scalar, testquat%vector

  testquat = testquat * 0.5d0
  write(6,*) testquat%array

  testquat%multiply(testquat%inverse())
  write(6,*) testquat%w, testquat%x, testquat%y, testquat%z 

end program
```
