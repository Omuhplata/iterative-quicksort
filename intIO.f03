module intIO
    implicit none

contains

    subroutine readUnsorted(arr)
        integer, dimension(:), allocatable, intent(out) :: arr
        character(len = 50) :: filename
        integer :: i, num, numElements, eof
        logical :: lexist

        write(*, *) "Enter filename to be loaded:"
        read(*, '(A50)') filename

        ! Check if file exists and open
        inquire(file = filename, exist = lexist)
        if(lexist) then
            open(unit = 10, file = filename, action = 'read')
        else
            write(*, *) "Error: File does not exist"
            return
        end if

        ! Read num of elements in file
        numElements = 0
        do 
            read(10, *, iostat = eof) num
            if(eof < 0) exit
            numElements = numElements + 1
        end do

        ! Allocate space for array 
        allocate(arr(numElements))

        ! Rewind file to beginning 
        rewind(10)

        ! Read integer values into array
        do i = 1, numElements
            read(10, *) arr(i)
        end do

        close(10)

    end subroutine readUnsorted


    subroutine writeSorted(arr)
        integer, dimension(:), intent(in) :: arr
        character(len = 20) :: filename = "sortedNUM.txt"
        integer :: i
        character(len = 1) :: response
        logical :: lexist

        ! Check if file already exists 
        inquire(file = filename, exist = lexist)
        if(.not. lexist) then
            open(unit = 9, file = filename, status = 'new', action = 'write')
        else
            ! prompt user if file already exists
            write(*, *) "Output file already exists - overwrite? (y/n)"
            read(*, '(A1)') response
            if(response /= 'y') then
                write(*, *) "Action aborted"
                return
            end if

            ! if reached user opts to overwrite the file
            open(unit = 9, file = filename, status = 'old', action = 'write')
        end if

        ! Write sorted array to output file
        do i = 1, size(arr)
            write(9, *) arr(i)
        end do

        ! Close output file
        close(9, status = 'keep')

    end subroutine writeSorted



end module intIO