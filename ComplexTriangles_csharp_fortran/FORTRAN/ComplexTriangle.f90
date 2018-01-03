! Author: Brady Coles
! Module that can determine area, side lengths, and angles of a triangle in
! the Complex Plane.
MODULE complex_triangle
    IMPLICIT NONE
    private
    public :: triangle ! triangle is the only public subroutine
CONTAINS
    ! calculates the distance between complex points
    ! arguments are points in the complex plane
    FUNCTION distance(z1, z2) 
        COMPLEX(KIND=8), INTENT(IN) :: z1, z2
        REAL(kind=8) distance
        distance = CDABS(z1 - z2)
    END FUNCTION distance

    ! calculates an angle of a triangle
    ! arguments are sidelengths.
    FUNCTION angle (opp, adj1, adj2) 
        REAL(kind=8), INTENT(IN) :: opp, adj1, adj2
        REAL(kind =8) angle
        REAL, PARAMETER :: PI = 4.0 * ATAN(1.0)
        angle = DACOS((adj1**2 + adj2**2 - opp**2) / (2 * adj1 * adj2))
        angle = angle * (360.0 / (2 * PI))
    END FUNCTION angle

    ! calculates the area of a triangle with Herons formula
    ! a, b, c are sidelengths.
    FUNCTION area(a, b, c) 
        REAL(kind=8), INTENT(IN) :: a, b, c
        REAL(kind=8) :: area, s
        s = (a + b + c) / 2.0d+0
        area = DSQRT(s*(s-a)*(s-b)*(s-c))
    END FUNCTION area

    ! prints out info about triangle
    SUBROUTINE triangle(z1, z2, z3) 
        COMPLEX(KIND=8), INTENT(IN) :: z1, z2, z3 ! points of a triangle
        REAL(kind=8) :: z1z2, z2z3, z3z1, angz1, angz2, angz3
        CHARACTER(20) :: fmt ='(F0.4,SP,F0.4,"i")'
        
        ! Check if points are distinct
        IF (z1 == z2 .or. z2 == z3 .or. z3 == z1) THEN
            PRINT *, 'Not a valid triangle. Points must be distinct.'
            RETURN
        END IF
        
        z1z2 = distance(z1, z2)
        z2z3 = distance(z2, z3)
        z3z1 = distance(z3, z1)
        
        ! Check if points are collinear by getting area of triangle
        IF (area(z1z2, z2z3, z3z1) == 0) THEN      ss ! Fails in extreme cases
            PRINT *, 'Not a valid triangle. Points must not be collinear.'
            RETURN
        END IF

        PRINT *, 'TRIANGLE'
        PRINT '("  ",A,": ",F0.4,SP,F0.4,"i")', 'Z1', z1, 'Z2', z2, 'Z3', z3 
        PRINT *, 'SIDE LENGTHS'
        PRINT *,  ' Z1 Z2 : ', z1z2
        PRINT *,  ' Z2 Z3 : ', z2z3
        PRINT *,  ' Z3 Z1 : ', z3z1
        PRINT *, 'ANGLES (degrees)'
        PRINT *, ' Z1 : ', angle(z2z3, z1z2, z3z1)
        PRINT *, ' Z2 : ', angle(z3z1, z1z2, z2z3)
        PRINT *, ' Z3 : ', angle(z1z2, z2z3, z3z1)
    END SUBROUTINE triangle
END MODULE complex_triangle

! A test program to try the module
PROGRAM test
USE complex_triangle
COMPLEX(kind=8) :: z1, z2, z3
z1 = COMPLEX(-2.1d+0, -0.0001d+0)
z2 = COMPLEX(-2098.7d+0, 15.1d+0)
z3 = COMPLEX(30d+0, 0.0001d+0)
CALL triangle(z1, z2, z3)
END PROGRAM 