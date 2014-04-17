! ============================================================================
! Name        : quaternions.F03
! Author      : Jan Kessler
! Version     :
! Description : Module implementing Quaternion type
! License     : GNU LGPLv3
!
!    Copyright (C) 2014  Jan Kessler
!
!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU Lesser General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU Lesser General Public License for more details.
!
!    You should have received a copy of the GNU Lesser General Public License
!    along with this program.  If not, see <http://www.gnu.org/licenses/>.
! ============================================================================

module quaternions
    implicit none

    type quaternion

        double precision                ::  array(4)
        !double precision, pointer               ::  w=>array(1), x=>array(2), y=>array(3), z=>array(4)
        double precision, pointer       ::  w, x, y, z
        double precision, pointer       ::  scalar
        double precision, pointer,dimension(:)  ::  vector
        !double precision, pointer      ::  vector=>array(2:4)

    contains

        procedure       ::  initQPointers
        procedure       ::  initQuaternion
        procedure       ::  initQuaternionfromQ
        procedure       ::  initQuaternionfromArr
        procedure       ::  initQuaternionfromAxisAngle
        generic         ::  init => initQPointers, initQuaternion, initQuaternionfromQ, &
                                    initQuaternionfromArr, initQuaternionfromAxisAngle

        procedure       ::  quatSet
        procedure       ::  quatSetfromArr
        procedure       ::  quatSetfromAxisAngle
        generic         ::  set => quatSet, quatSetfromArr, quatSetfromAxisAngle

        procedure       ::  quatCopy
        generic         ::  copy => quatCopy

        procedure       ::  quatMultiply
        procedure       ::  quatMultiplyScalar
        generic         ::  multiply => quatMultiply, quatMultiplyScalar

        procedure       ::  quatAdd
        procedure       ::  quatAddScalar
        generic         ::  add => quatAdd, quatAddScalar

        procedure       ::  quatSub
        generic         ::  sub => quatSub

        procedure       ::  quatNorm
        generic         ::  norm => quatNorm

        procedure       ::  quatNormalize
        generic         ::  normalize => quatNormalize

        procedure       ::  quatConjugated
        generic         ::  conjugated => quatConjugated

        procedure       ::  quatConjugate
        generic         ::  conjugate => quatConjugate

        procedure       ::  quatInverse
        generic         ::  inverse => quatInverse

        procedure       ::  quatInvert
        generic         ::  invert => quatInvert
        
        procedure       ::  quatAngle
        generic         ::  angle => quatAngle
        
        procedure       ::  quatAxis
        generic         ::  axis => quatAxis
        
        procedure       ::  quatMatrix
        generic         ::  matrix => quatMatrix

    end type quaternion

    interface operator(.o.)
        module procedure dotVectors
        module procedure dotQuaternions
    end interface

    interface operator(.x.)
        module procedure crossVectors
        module procedure crossQuaternions
    end interface

    interface assignment(=)
        module procedure quatCopy
    end interface

    interface operator(+)
        module procedure addQuaternions
        module procedure addQuaternionScalar
        module procedure addScalarQuaternion
    end interface


    interface operator(-)
        module procedure subQuaternions
        module procedure subQuaternionScalar
        module procedure subScalarQuaternion
    end interface

    interface operator(*)
        module procedure multiplyQuaternions
        module procedure multiplyQuaternionScalar
        module procedure multiplyScalarQuaternion
    end interface

    interface operator(/)
        module procedure divideQuaternionsRS
        module procedure divideQuaternionScalar
    end interface

    interface operator(//)
        module procedure divideQuaternionsLS
    end interface

contains

       !-----             "CONSTRUCTOR"                -----!

    function newDefaultQuaternion()
        ! default constructor

        implicit none
        type (quaternion)               ::  newDefaultQuaternion

        call newDefaultQuaternion%init()
    end function

    !-----             INITIALIZERS                -----!

    subroutine initQPointers(this)
        ! default initializer

        implicit none
        class (quaternion), target      ::  this

        this%scalar => this%array(1)
        this%vector => this%array(2:4)

        this%w => this%scalar
        this%x => this%vector(1)
        this%y => this%vector(2)
        this%z => this%vector(3)
    end subroutine


    subroutine initQuaternion(this, w, x, y, z)
        ! 'from wxyz'-initializer

        implicit none
        double precision, intent(in)    ::  w, x, y, z
        class (quaternion)              ::  this

        call this%init()
        call this%set(w, x, y, z)
    end subroutine


    subroutine initQuaternionfromQ(this, quat)
        ! 'from quat'-initializer

        implicit none
        class (quaternion), intent(in)  ::  quat
        class (quaternion)              ::  this

        call this%init()
        call this%copy(quat)
    end subroutine


    subroutine initQuaternionfromArr(this, array)
        ! 'from array'-initializer, expects array with values (w,x,y,z)

        implicit none
        double precision, intent(in)    ::  array(4)
        class (quaternion)              ::  this

        call this%init()
        call this%set(array)
    end subroutine


    subroutine initQuaternionfromAxisAngle(this, axis, angle)
        ! rotational quaternion initializer for given axis and angle

        implicit none
        double precision, intent(in)    ::  axis(3), angle
        class (quaternion)              ::  this
       
        call this%init()
        call this%set(axis, angle)
    end subroutine
    
    
    !-----          TYPE-BOUND PROCEDURES           -----!


    subroutine quatSet(this, w, x, y, z)
        ! sets the components of the quaternion

        implicit none
        double precision, intent(in)    ::  w, x, y, z
        class (quaternion)              ::  this

        this%w = w
        this%x = x
        this%y = y
        this%z = z
    end subroutine


    subroutine quatSetfromArr(this, array)
        ! sets the quaternion from array with values (w,x,y,z)

        implicit none
        double precision, intent(in)    ::  array(4)
        class (quaternion)              ::  this

        this%array = array
    end subroutine
    
    
    subroutine quatSetfromAxisAngle(this, axis, angle)
        ! sets the quaternion as rotational quaternion for axis and angle

        implicit none
        double precision, intent(in)    ::  axis(3), angle
        class (quaternion)              ::  this
        double precision                ::  normfac, hangle
        hangle = 0.5d0*angle
        normfac = sin(hangle)/norm2(axis)
          
        this%scalar = cos(hangle)           
        this%vector = axis*normfac
    end subroutine


    subroutine quatCopy(this, quat)
        ! copy the quaternion quat to this

        implicit none
        class (quaternion), intent(in)      ::  quat
        class (quaternion), intent(inout)   ::  this

        this%array = quat%array
    end subroutine


    subroutine quatMultiply(this, quat)
        ! multiply quaternion quat from left with this: this = quat*this

        implicit none
        class (quaternion), intent(in)  ::  quat
        class (quaternion)              ::  this
        double precision                ::  newscalar, newvector(3)

        newscalar =  quat%scalar * this%scalar - (quat%vector.o.this%vector)
        newvector =  quat%scalar * this%vector + this%scalar * quat%vector + (quat%vector.x.this%vector)

        this%scalar = newscalar
        this%vector = newvector
    end subroutine


    subroutine quatMultiplyScalar(this, scalar)
        ! multiply scalar with this: this = scalar*this

        implicit none
        double precision, intent(in)    ::  scalar
        class (quaternion)              ::  this

        this%array = scalar * this%array
    end subroutine


    subroutine quatAdd(this, quat)
        ! add quaternion quat: this = quat + this

        implicit none
        class (quaternion), intent(in)  ::  quat
        class (quaternion)              ::  this

        this%array = quat%array + this%array
    end subroutine


    subroutine quatAddScalar(this, scalar)
        ! add scalar: Re(this) = scalar + Re(quat)

        implicit none
        double precision, intent(in)    ::  scalar
        class (quaternion)              ::  this

        this%scalar = scalar + this%scalar
    end subroutine


    subroutine quatSub(this, quat)
        ! sub quaternion quat: this = - quat + this

        implicit none
        class (quaternion), intent(in)  ::  quat
        class (quaternion)              ::  this

        this%array = this%array - quat%array
    end subroutine


    function quatNorm(this)
        ! return norm of quaternion this

        implicit none
        class (quaternion), intent(in)  ::  this
        double precision                ::  quatNorm

        quatNorm = norm2(this%array)
    end function


    subroutine quatNormalize(this)
        ! normalize quaternion this

        implicit none
        class (quaternion)              ::  this

        this%array = this%array / norm2(this%array)
    end subroutine


    function quatConjugated(this)
        ! return a conjugated copy of quaternion this

        implicit none
        class (quaternion), intent(in)  ::  this
        type (quaternion)               ::  quatConjugated
        call quatConjugated%init()

        call quatConjugated%copy(this)
        quatConjugated%vector = -1.d0 * quatConjugated%vector
    end function


    subroutine quatConjugate(this)
        ! conjugate quaternion this

        implicit none
        class (quaternion)              ::  this

        this%vector = -1.d0 * this%vector
    end subroutine


    function quatInverse(this)
        ! return an inverted copy of quaternion this

        implicit none
        class (quaternion), intent(in)  ::  this
        type (quaternion)               ::  quatInverse
        double precision                ::  normh
        call quatInverse%init()
        
        normh = this%norm()
        normh = 1.d0 / (normh * normh)
        call quatInverse%copy(this%conjugated())
        call quatInverse%multiply(normh)
    end function


    subroutine quatInvert(this)
        ! invert quaternion this

        implicit none
        class (quaternion)              ::  this
        double precision                ::  normh
        
        normh = this%norm()
        normh = 1.d0 / (normh * normh)
        call this%conjugate()
        call this%multiply(normh)
    end subroutine

    function quatAngle(this)
         ! return the angle if quaternion this is interpreted as rotation (assuming normalized quaternion)
        implicit none
        class(quaternion), intent(in)        :: this
        double precision                    :: quatAngle

        quatAngle = 2.d0*acos(this%w)
    end function
    

    function quatAxis(this)
        ! return the axis if quaternion this is interpreted as rotation (assuming normalized quaternion)
    
        implicit none
        class(quaternion), intent(in)        :: this
        double precision                    :: quatAxis(3), normfac
    
        normfac = norm2(this%vector)
        quatAxis = this%vector / normfac
    end function
    
    function quatMatrix(this)
        ! return the rotational matrix if quaternion this is interpreted as rotation (assuming normalized quaternion)
    
        implicit none
        class(quaternion), intent(in)        :: this
        double precision                     :: quatMatrix(3,3)
        double precision                     :: thelp, sthelp, axhelp(3)
        
        thelp = norm2(this%vector)
        axhelp = this%vector / thelp
        sthelp = this%scalar*this%scalar - thelp*thelp
        quatMatrix = reshape((/ sthelp, 0.d0, 0.d0, 0.d0, sthelp, 0.d0, 0.d0, 0.d0, sthelp /), shape(quatMatrix))
        quatMatrix = quatMatrix + (2.d0 * thelp*thelp) * dyadVectors(axhelp, axhelp) - (2.d0 * this%scalar*thelp) * &
            crossMatrix(axhelp)
    end function


    !-----          NON-TYPE-BOUND PROCEDURES           -----!


    function dotVectors(veca, vecb)
        implicit none
        double precision, intent(in)    ::  veca(3), vecb(3)
        double precision                ::  dotVectors

        dotVectors = veca(1)*vecb(1) + veca(2)*vecb(2) + veca(3)*vecb(3)
    end function


    function dotQuaternions(quata, quatb)
        implicit none
        class (quaternion), intent(in)  ::  quata, quatb
        double precision                ::  dotQuaternions

        dotQuaternions = dot_product(quata%array,quatb%array)
    end function


    function crossVectors(veca, vecb)
        implicit none
        double precision, intent(in)    ::  veca(3), vecb(3)
        double precision                ::  crossVectors(3)

        crossVectors(1) = veca(2) * vecb(3) - veca(3) * vecb(2)
        crossVectors(2) = veca(3) * vecb(1) - veca(1) * vecb(3)
        crossVectors(3) = veca(1) * vecb(2) - veca(2) * vecb(1)
    end function

    function crossMatrix(vec)
        implicit none
        double precision, intent(in)    ::  vec(3)
        double precision                ::  crossMatrix(3,3)

        crossMatrix = reshape((/ 0.d0, -vec(3), vec(2), vec(3), 0.d0, -vec(1), vec(2), vec(1), 0.d0 /), shape(crossMatrix))
    end function

    function crossQuaternions(quata, quatb)
        implicit none
        class (quaternion), intent(in)  ::  quata, quatb
        type (quaternion)               ::  crossQuaternions
        call crossQuaternions%init()

        crossQuaternions%scalar = 0.d0
        crossQuaternions%vector = quata%vector.x.quatb%vector
    end function


    function dyadVectors(veca, vecb)
        implicit none
        double precision, intent(in)    ::  veca(3), vecb(3)
        double precision                ::  dyadVectors(3,3)
        
        dyadVectors(1,:) = veca(1) * vecb
        dyadVectors(2,:) = veca(2) * vecb
        dyadVectors(3,:) = veca(3) * vecb       
    end function
        
        
    function addQuaternions(quata, quatb)
        implicit none
        class (quaternion), intent(in)  ::  quata, quatb
        type (quaternion)               ::  addQuaternions
        call addQuaternions%init()

        call addQuaternions%copy(quatb)
        call addQuaternions%add(quata)
    end function

    function addQuaternionScalar(quat, scalar)
        implicit none
        class (quaternion), intent(in)  ::  quat
        double precision, intent(in)    ::  scalar
        type (quaternion)               ::  addQuaternionScalar
        call addQuaternionScalar%init()

        call addQuaternionScalar%copy(quat)
        call addQuaternionScalar%add(scalar)
    end function

    function addScalarQuaternion(scalar, quat)
        implicit none
        class (quaternion), intent(in)  ::  quat
        double precision, intent(in)    ::  scalar
        type (quaternion)               ::  addScalarQuaternion
        call addScalarQuaternion%init()

        call addScalarQuaternion%copy(quat)
        call addScalarQuaternion%add(scalar)
    end function

    function subQuaternions(quata, quatb)
        implicit none
        class (quaternion), intent(in)  ::  quata, quatb
        type (quaternion)               ::  subQuaternions
        call subQuaternions%init()

        call subQuaternions%copy(quatb)
        call subQuaternions%multiply(-1.d0)
        call subQuaternions%add(quata)
    end function

    function subQuaternionScalar(quat, scalar)
        implicit none
        class (quaternion), intent(in)  ::  quat
        double precision, intent(in)    ::  scalar
        type (quaternion)               ::  subQuaternionScalar
        call subQuaternionScalar%init()

        call subQuaternionScalar%copy(quat)
        call subQuaternionScalar%add(-scalar)
    end function

    function subScalarQuaternion(scalar, quat)
        implicit none
        class (quaternion), intent(in)  ::  quat
        double precision, intent(in)    ::  scalar
        type (quaternion)               ::  subScalarQuaternion
        call subScalarQuaternion%init()

        call subScalarQuaternion%copy(quat)
        call subScalarQuaternion%multiply(-1.d0)
        call subScalarQuaternion%add(scalar)
    end function


    function multiplyQuaternions(quata, quatb)
        implicit none
        class (quaternion), intent(in)  ::  quata, quatb
        type (quaternion)               ::  multiplyQuaternions
        call multiplyQuaternions%init()

        call multiplyQuaternions%copy(quatb)
        call multiplyQuaternions%multiply(quata)
    end function

    function multiplyQuaternionScalar(quat, scalar)
        implicit none
        class (quaternion), intent(in)  ::  quat
        double precision, intent(in)    ::  scalar
        type (quaternion)               ::  multiplyQuaternionScalar
        call multiplyQuaternionScalar%init()

        call multiplyQuaternionScalar%copy(quat)
        call multiplyQuaternionScalar%multiply(scalar)
    end function

    function multiplyScalarQuaternion(scalar, quat)
        implicit none
        class (quaternion), intent(in)  ::  quat
        double precision, intent(in)    ::  scalar
        type (quaternion)               ::  multiplyScalarQuaternion
        call multiplyScalarQuaternion%init()

        call multiplyScalarQuaternion%copy(quat)
        call multiplyScalarQuaternion%multiply(scalar)
    end function

    function divideQuaternionsRS(quata, quatb)
        implicit none
        class (quaternion), intent(in)  ::  quata, quatb
        type (quaternion)               ::  divideQuaternionsRS
        call divideQuaternionsRS%init()

        call divideQuaternionsRS%copy(quatb%inverse())
        call divideQuaternionsRS%multiply(quata)
    end function

    function divideQuaternionScalar(quat, scalar)
        implicit none
        class (quaternion), intent(in)  ::  quat
        double precision, intent(in)    ::  scalar
        type (quaternion)               ::  divideQuaternionScalar
        call divideQuaternionScalar%init()

        call divideQuaternionScalar%copy(quat)
        call divideQuaternionScalar%multiply(1.d0/scalar)
    end function

    function divideQuaternionsLS(quata, quatb)
        implicit none
        class (quaternion), intent(in)  ::  quata, quatb
        type (quaternion)               ::  divideQuaternionsLS
        call divideQuaternionsLS%init()

        call divideQuaternionsLS%copy(quatb)
        call divideQuaternionsLS%multiply(quata%inverse())
    end function
    
    function rotatedVectorQ(vector, quat)
        implicit none
        double precision  , intent(in)  ::  vector(3)
        class (quaternion), intent(in)  ::  quat
        double precision                ::  rotatedVectorQ(3)
        
        rotatedVectorQ = matmul(quat%matrix(), vector)
    end function
    
    subroutine rotateVectorQ(vector, quat)
        implicit none
        double precision                ::  vector(3)
        class (quaternion), intent(in)  ::  quat
        
        
        vector = rotatedVectorQ(vector, quat)
    end subroutine
end module quaternions
