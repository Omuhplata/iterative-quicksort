module stackADT
    implicit none

    ! define new data type to implement stack
    type :: StackType
        integer, dimension(:), allocatable :: elements
        integer :: top
    end type StackType

contains

    subroutine createStack(stack, maxSize)
        type(StackType), intent(inout) :: stack
        integer, intent(in) :: maxSize
        
        allocate(stack%elements(maxSize))
        stack%top = 0
    end subroutine createStack


    subroutine push(stack, value)
        type(StackType), intent(inout) :: stack
        integer, intent(in) :: value
        
        ! increment and set new value to top of stack
        stack%top = stack%top + 1
        stack%elements(stack%top) = value
    end subroutine push


    subroutine pop(stack, value)
        type(StackType), intent(inout) :: stack
        integer, intent(out) :: value

        ! decrement and remove value from top of stack
        value = stack%elements(stack%top)
        stack%top = stack%top - 1
    end subroutine pop


    subroutine clear(stack)
        type(StackType), intent(inout) :: stack
        
        ! free all elements of stack and set top to zero
        deallocate(stack%elements)
        stack%top = 0
    end subroutine clear


    logical function isEmpty(stack)
        type(StackType), intent(in) :: stack
        isEmpty = (stack%top == 0)
    end function isEmpty

end module stackADT





