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

    subroutine matrixBackward(operation_matrix)
        real(kind=dp), dimension(:,:), intent(inout)::operation_matrix
        integer::i,j
        real(kind=dp) :: mulFactor = 1.0d0
        do i=1,size(operation_matrix,1)
            mulFactor = operation_matrix(i,i)
            do j=1,size(operation_matrix,2)
                operation_matrix(i,j) = -operation_matrix(i,j)/mulFactor
            end do
            operation_matrix(i,i) = 0
        end do

    end subroutine

    subroutine scaleAnswerVectorForBackward(operation_matrix, rhs)
        real(kind=dp), dimension(:,:), intent(in)::operation_matrix
        real(kind=dp), dimension(:), intent(inout)::rhs
        integer::i
        do i=1,size(operation_matrix,1)
            rhs(i) = rhs(i)/operation_matrix(i,i)
        end do

    end subroutine

    subroutine prepareBackwardForSolver(operation_matrix, rhs)
        real(kind=dp), dimension(:,:), intent(inout)::operation_matrix
        real(kind=dp), dimension(:), intent(inout)::rhs
        call matrixBackward(operation_matrix)
        call scaleAnswerVectorForBackward(operation_matrix, rhs)
    end subroutine

    subroutine matrixMul(matrix, vector)
        real(kind=dp), dimension(:,:), intent(in)::matrix
        real(kind=dp), dimension(:), intent(inout)::vector
        real(kind=dp), dimension(size(vector)):: tempVector
        integer::i,j
        do i=1,size(matrix,1)
            tempVector(i) = 0
            do j=1,size(matrix,2)
                tempVector(i) = tempVector(i) + matrix(i,j)*vector(j)
            end do
        end do
        call copyState(vector, tempVector)
    end subroutine

    
end module StateTools



module Solvers
    use StateTools

    contains

    ! subroutine
    subroutine jacobiSolver(state, operator_matrix, rhs, max_iterations)
        integer, intent(in)::max_iterations
        real(kind=dp), dimension(:), intent(inout)::state
        real(kind=dp), dimension(:), intent(in)::rhs
        real(kind=dp), dimension(size(rhs)):: reverse_rhs

        real(kind=dp), dimension(size(state)*size(rhs), size(rhs)*size(rhs)), intent(in)::operator_matrix
        real(kind=dp), dimension(size(rhs)*size(state), size(state)*size(rhs)):: reverse_matrix
        ! real
        integer::i 
        real(kind=dp), dimension(size(state)):: tempState
        call copyState(tempState, state)

        reverse_matrix(:,:) = operator_matrix(:,:)
        reverse_rhs(:) = rhs(:)
        call prepareBackwardForSolver(reverse_matrix, reverse_rhs)




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
    use Solvers

    real(kind=dp), dimension(3):: jacobiSol, gaussSeidelSol
    jacobiSol = [0,0,0]
    gaussSeidelSol = [0,0,0]
    ! call jacobiSolver(jacobiSol, 100)
    call gaussSeidelSolver(gaussSeidelSol, 100)
    print *, "Jacobi solver gave answer: "
    print *, jacobiSol

    print *, "Gauss-Seidel solver gave answer: "
    print *, gaussSeidelSol


    

end program main