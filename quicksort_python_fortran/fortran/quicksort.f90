! Author: Brady Coles
! Quicksort Program

! This module has a sort subroutine, and a function to check if an array is
! sorted. Only works on Double Precision floats.
MODULE quicksort
    IMPLICIT NONE
CONTAINS
    ! Sorts array a with a quicksort, between (and including) elements s and e
    RECURSIVE SUBROUTINE sort(a, s, e)
        DOUBLE PRECISION, DIMENSION (:), INTENT(inout) :: a
        INTEGER, INTENT(in) :: s, e
        INTEGER :: split, i
        DOUBLE PRECISION :: temp
        
        ! Base case, 1 or 0 elements to sort
        IF (e - s < 1) THEN
            RETURN
        END IF
        
        split = s
        DO i = s, e - 1
            IF (a(i) <= a(e)) THEN
                 temp = a(i)
                 a(i) = a(split)
                 a(split) = temp
                 split = split + 1
            END IF
        END DO
        temp = a(split)
        a(split) = a(e)
        a(e) = temp
        ! Recursive call
        call sort(a, s, split-1)
        call sort(a, split+1, e)
    END SUBROUTINE sort
    ! Checks if elements in array a between (and including) elements s and e
    ! are sorted in increasing order.
    FUNCTION check(a, s, e)
        DOUBLE PRECISION, DIMENSION (:), INTENT(in) :: a
        INTEGER, INTENT(in) :: s, e
        LOGICAL :: check
        INTEGER :: i
        
        check = .TRUE.
        DO i = s + 1, e
            IF (a(i) < a(i-1)) THEN
                PRINT *, 'Array not sorted.'
                check = .FALSE.
                RETURN
            END IF
        END DO
        PRINT *, 'Array is sorted.'
    END FUNCTION check
END MODULE quicksort

! This module can read a file to create an array and make a file from an
! array. Only works on Double Precision floats.
MODULE sortIO
    IMPLICIT NONE
CONTAINS
    ! Reades a file named fn and puts each line in an array.
    FUNCTION readfile(fn)
        CHARACTER(len=*), INTENT(in) :: fn
        DOUBLE PRECISION :: x
        INTEGER :: arrsize, i, ios
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: readfile
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: temp
        LOGICAL :: file_exists
        
        ! Check that file exists
        INQUIRE(file=fn, exist=file_exists)
        IF (.NOT. file_exists) THEN
            PRINT *, 'File not found: ', fn
            STOP 1
        END IF

        i = 1
        arrsize = 1; allocate(readfile(1))
        
        OPEN (1, file=fn)
        DO
            READ(1,*,IOSTAT=ios) x
            IF (ios > 0) THEN
                PRINT *, 'Input was unreadable on a line. Skipped.'
            ELSE IF (ios < 0) THEN
                ! End of file
                EXIT
            ELSE
                IF (i > SIZE(readfile)) THEN
                    ! Dynamically resize readfile when it gets full
                    arrsize = SIZE(readfile)
                    arrsize = arrsize + arrsize
                    ALLOCATE(temp(arrsize))
                    temp(:SIZE(readfile)) = readfile
                    CALL MOVE_ALLOC(temp, readfile)
                END IF
                readfile(i) = x
                i = i + 1
            END IF
        END DO
        CLOSE(1)
        readfile = readfile(:i-1) ! Resize to only include filled elements
        PRINT *, 'Array read from file'
    END FUNCTION readfile
    ! Creates a file with filename fn and puts array a into it, one element
    ! per line.
    SUBROUTINE writefile(a, fn)
        DOUBLE PRECISION, DIMENSION (:), INTENT(in) :: a
        CHARACTER(len=*), INTENT(in) :: fn
        INTEGER :: i
        OPEN (2, file=fn, status='REPLACE')
        DO i=1, SIZE(a)
            WRITE(2,*) a(i)
        END DO
        CLOSE(2)
        PRINT *, 'Write finished.'
    END SUBROUTINE writefile
END MODULE sortIO

! Command line program
PROGRAM test
USE quicksort
USE sortIO
DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: arr
CHARACTER(len=100) :: infn, outfn
IF (COMMAND_ARGUMENT_COUNT() == 2) THEN
    call GET_COMMAND_ARGUMENT(1, infn)
    call GET_COMMAND_ARGUMENT(2, outfn)
    arr = readfile(infn)
    call sort(arr, 1, SIZE(arr))
    IF (.NOT. check(arr, 1, SIZE(arr))) print *, 'Program failed.'
    call writefile(arr, outfn)
ELSE
    PRINT *, 'Invalid arguments. Enter an input and output file:'
    PRINT *, 'lab2.out <inFile> <outFile>'
END IF
END PROGRAM
        
            
