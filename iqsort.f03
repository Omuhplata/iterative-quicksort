module iterativeSort
    use stackADT
    implicit none

contains

    subroutine iterativeQsort(arr, size)
        integer, dimension(:), intent(inout) :: arr
        integer, intent(in) :: size
        type(StackType) :: stack
        integer :: left, right, pivot, i, j

        ! Create the stack and push the initial array bounds
        call createStack(stack, size)
        call push(stack, 1)
        call push(stack, size)

        do while(.not. isEmpty(stack))
            call pop(stack, right)
            call pop(stack, left)

            do while(left < right)
            ! partition arr(left) to arr(right)
                pivot = arr((left+right)/2)
                i = left
                j = right

                do while(i <= j)
                    do while(arr(i) < pivot)
                        i = i + 1
                    end do
                    do while(arr(j) > pivot)
                        j = j - 1
                    end do
                    if(i <= j) then
                        call swap(arr(i), arr(j))
                        i = i + 1
                        j = j - 1
                    end if
                end do

                if((j - left) < (right - i)) then
                    if(i < right) then
                    ! push right partition to stack
                        call push(stack, i)
                        call push(stack, right)
                    end if
                    ! continue sorting left
                    right = j
                else
                    if(left < j) then
                    !  push left partition to stack
                        call push(stack, left)
                        call push(stack, j)
                    end if
                    ! continue sorting right
                    left = i
                end if
            end do
        end do

        call clear(stack)
    end subroutine iterativeQsort


    subroutine swap(a, b)
    ! Takes two integers and swaps their values 
        integer, intent(inout) :: a, b
        integer :: temp

        temp = a
        a = b
        b = temp
    end subroutine swap

end module iterativeSort


! Main Program !

program Main
    use iterativeSort
    use intIO
    implicit none

    integer :: n = 0
    integer, dimension(:), allocatable :: arr
    ! integer :: i 
    real :: startTime, endTime, timeTaken
    
    ! Prompt user for file to read data
    call readUnsorted(arr)
    n = size(arr)

    ! Proceed if array loaded correctly (size(arr) > 0)
    if(n > 0) then
        ! ! To view array before sorting !!
        ! write(*, *) "Array before sorting: "
        ! do i = 1, n
        !     write(*, *) arr(i)
        ! end do

        ! Call quicksort and calculate time taken
        call cpu_time(startTime)
        call iterativeQsort(arr, n)
        call cpu_time(endTime)

        timeTaken = endTime - startTime

        ! ! To view array after sorting !!
        ! write(*, *) "Array after sorting: "
        ! do i = 1, n
        !     write(*, *) arr(i)
        ! end do

        ! Output time taken and write data to file
        write(*, *) "Average time over 10 runs: ", timeTaken
        
        call writeSorted(arr)

    end if

end program Main

