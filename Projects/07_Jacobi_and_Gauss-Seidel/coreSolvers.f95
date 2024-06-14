module StateTools
    integer, parameter, public ::dp = kind(1.0d0)

    contains

    
    function addState(state1, mul1, state2, mul2) result(resultState)
        real(kind=dp), intent(in), dimension(:):: state1
        real(kind=dp), intent(in), dimension(size(state1))::  state2
        real(kind=dp), intent(in)::mul1, mul2 
        real(kind=dp), dimension(size(state1)):: resultState
        integer::i = 0
        do i=1,size(state1)
            resultState(i) = state1(i)*mul1 + state2(i)*mul2
        end do
    end function addState

    function mul(n, state, mull) result(resultState)
        integer, intent(in) :: n 
        real(kind=dp), intent(in), dimension(n)::state
        real(kind=dp), intent(in)::mull
        real(kind=dp), dimension(n):: resultState
        integer::i = 0
        do i=1,n 
            resultState(i) = state(i)*mull
        end do
    end function mul


    
    function norm(state, l) result(normVal)
        ! l2 is euclidean norm while l(-1) will be interpreted at l_infty norm
        !Beware of overflow
        real(kind=dp), dimension(:)::state
        integer:: l 
        real(kind=dp):: normVal
        integer::i
        normVal = 0.0d0
        if ( l==(-1) ) then
            do i=1, size(state)
                if(state(i)>normVal) then
                    normVal = state(i)
                end if
            end do
        else
            do i=1, size(state)
                normVal = normVal + ((abs(state(i)))**(1.0d0*l))
            end do
            normVal = normVal**(1.0d0/l)
        end if
    end function



    subroutine copyState(targetState, originalState)
        real(kind=dp),dimension(:), intent(in):: originalState
        real(kind=dp),dimension(size(originalState)), intent(out)::targetState
        integer::i =  1
        do i=1, size(originalState)
            targetState(i) = originalState(i)
        end do

    end subroutine

    
end module StateTools



module Solvers
    use StateTools

    contains
    subroutine jacobiSolver(state, max_iterations)
        integer, intent(in)::max_iterations
        real(kind=dp), dimension(:), intent(inout)::state
        integer::i 
        real(kind=dp), dimension(size(state)):: tempState
        call copyState(tempState, state)
        do i = 1,max_iterations
            !Matrix is diagonally dominant. Therefore convergence is guarenteed
            tempState(1) = (3.0+state(3) + state(2))/4.0d0
            tempState(2) = (9.0 - state(3) + (2*state(1)))/6.0d0
            tempState(3) = (-6.0 - state(2) +state(1))/7.0d0

            if ( norm(addState(state, 1.0d0, tempState, -1.0d0), -1)<0.00001 ) then
                
                exit
            end if

            call copyState(state, tempState)
        end do
        call copyState(state, tempState)
        
    end subroutine

    subroutine gaussSeidelSolver(state, max_iterations)
        integer, intent(in):: max_iterations
        real(kind=dp), dimension(:), intent(inout)::state
        integer::i 
        real(kind=dp), dimension(size(state)):: prevState
        
        do i = 1,max_iterations
            call copyState(prevState, state)
            !Matrix is diagonally dominant. Therefore convergence is guarenteed
            state(1) = (3.0+state(3) + state(2))/4.0d0
            state(2) = (9.0 - state(3) + (2*state(1)))/6.0d0
            state(3) = (-6.0 - state(2) +state(1))/7.0d0

            if ( norm(addState(state, 1.0d0, prevState, -1.0d0), -1)<0.00001 ) then
                exit
            end if

            ! call copyState(state, tempState)
        end do

    end subroutine
end module


program main

    real(kind=dp) :: x = 0
    real(kind=dp):: y = 5
    real(kind=dp):: z = 0
    real(kind=dp):: t = 1
    real(kind=dp):: dt = 0.001
    real(kind=dp), dimension(4) :: state 
    integer:: i= 0
    state(1) = x
    state(2) = y
    state(3) = z
    state(4) = t


    open(1, file='solutionValues.txt', status='old')

    do i=1, 100000
        call updateValues(4, state, dt)
        ! print*, state(3), state(1)
        write(1,*) state(1), state(2), state(3)
    end do
    close(1)
    call execute_command_line('gnuplot -p '//'plot3D.plt')




end program main