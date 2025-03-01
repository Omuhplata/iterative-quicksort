module recursiveSort
    implicit none

contains

! Calls quicksort recursively
    recursive subroutine recursiveQsort(arr, left, right)
        integer, dimension(:), intent(inout) :: arr
        integer, intent(in) :: left, right
        integer :: pivot

        if(left < right) then
            call partition(arr, left, right, pivot)
            call recursiveQsort(arr, left, pivot - 1)
            call recursiveQsort(arr, pivot + 1, right)
        end if

    end subroutine recursiveQsort

! Sets pivot and partitions array according to pivot value
    subroutine partition(arr, left, right, pivot)
        integer, dimension(:), intent(inout) :: arr
        integer, intent(in) :: left, right
        integer :: i, j, pivot

        ! set pivot to last element
        pivot = arr(right)
        i = left - 1

        ! Loop j through array and swap when required
        do j = left, right - 1
            if(arr(j) <= pivot) then
                i = i + 1
                call swap(arr(i), arr(j))
            end if
        end do

        
        call swap(arr(i + 1), arr(right))
        pivot = i + 1

    end subroutine partition


! Takes two integers and swaps their values 
    subroutine swap(a, b)
        integer, intent(inout) :: a, b
        integer :: temp

        temp = a
        a = b
        b = temp
    end subroutine swap


end module recursiveSort



! Main program !
program Main
    use recursiveSort
    use intIO
    implicit none 

    integer :: n = 0
    integer, dimension(:), allocatable :: arr
    ! integer :: i
    real startTime, endTime, timeTaken

    ! Prompt user for file to read data
    call readUnsorted(arr)
    n = size(arr)

     ! Proceed if array loaded correctly (size(arr) > 0)
    if(n > 0) then
        ! ! To view array before sorting
        ! write(*, *) "Array before sorting: "
        ! do i = 1, n
        !     write(*, *) arr(i)
        ! end do

        ! call quicksort and calculate time taken
        call cpu_time(startTime)
        call recursiveQsort(arr, 1, n)
        call cpu_time(endTime)

        timeTaken = endTime - startTime

        ! ! To view array after sorting
        ! write(*, *) "Array after sorting: "
        ! do i = 1, n
        !     write(*, *) arr(i)
        ! end do


        ! Output time taken and write data to file
        write(*, *) "Time elapsed: ", timeTaken

        call writeSorted(arr)

    end if

end program Main