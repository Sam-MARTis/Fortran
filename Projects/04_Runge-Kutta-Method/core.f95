module DE
    integer, parameter, public ::dp = kind(1.0d0)
    contains
    function f(state) result(stateP)
        implicit none
        ! integer, parameter :: dp = kind(1.0d0)
        ! real(kind=dp), intent(in):: t
        real(kind=dp), dimension(3), intent(in):: state
        ! real(kind=dp)::ydp
        real(kind=dp), dimension(3):: stateP

        stateP(1) = state(2)
        stateP(2) = 10*sin(state(3)) - 6*state(1) - 5*state(2)
        stateP(3) = 1

        

    end function f

    function addState(n, state1, mul1, state2, mul2) result(resultState)
        integer, intent(in) :: n 
        real(kind=dp), intent(in), dimension(n):: state1, state2
        real(kind=dp), intent(in)::mul1, mul2 
        real(kind=dp), dimension(n):: resultState
        integer::i = 0
        do i=1,n 
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
end module DE

module RK4
    use DE, only: f, dp, addState, mul
        ! integer, parameter, public ::dp = kind(1.0d0)
    contains
    subroutine updateValues(state, dt)
        ! yp is y-prime, ydp is y-double-prime
        implicit none
        ! integer, parameter :: dp = kind(1.0d0)
        ! real(kind=dp):: getddy
        real(kind=dp), intent(in):: dt
        ! real(kind=dp), intent(inout) :: t, y, yp
        real(kind=dp), dimension(4, 3) :: K_arr
        real(kind=dp), dimension(3), intent(inout) :: state
        real(kind=dp), dimension(3):: tempState
        ! state(1) = y 
        ! state(2) = yp
        ! state(3) = t

        K_arr(1, :) = f(state)
        K_arr(2, :) = f(addState(3, state, 1.0d0, K_arr(1, :), dt/2))
        K_arr(3, :) = f(addState(3, state, 1.0d0, K_arr(2, :), dt/2))
        K_arr(4, :) = f(addState(3, state, 1.0d0, K_arr(3, :), dt))
        ! print *, K_arr
        tempState = addState(3, addState(3, K_arr(1, :), 1.0d0, K_arr(2, :), 2.0d0), 1.0d0, K_arr(3, :), 2.0d0)
        state = addState(3, state, 1.0d0, addState(3, tempState, 1.0d0, K_arr(4, :), 1.0d0), dt/6)
        ! state = mul(3, state, 1d0/6)



        
        ! real(kind=dp), intent(out) ::  

        
    end subroutine updateValues
end module RK4

program main
    ! integer, parameter ::wp = kind(1.0d0)
    use RK4, only: updateValues, dp

    real(kind=dp) :: y = 0
    real(kind=dp):: dy = 5
    ! real(kind=dp):: ddy
    real(kind=dp):: t = 0
    real(kind=dp):: dt = 0.1
    real(kind=dp), dimension(3) :: state 
    integer:: i= 0
    state(1) = y 
    state(2) = dy
    state(3) = t 


    open(1, file='solutionValues.txt', status='old')

    do i=1, 100
        call updateValues(state, dt)
        print*, state(3), state(1)
        write(1,*) state(3), state(1)
    end do
    close(1)
    call execute_command_line('gnuplot -p '//'plot2D.plt')




end program main