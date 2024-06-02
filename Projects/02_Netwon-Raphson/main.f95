
module derivative_calculator_mod
    implicit none
    integer, parameter, public:: wp = kind(1.0d0)
    
    contains
    function OVD(f, x) result(derivative)
        interface
            pure function target_func(t) result(y)
                import wp
                implicit none
                real(kind=wp), intent(in)::t
                real(kind=wp)::y
            end function target_func
        end interface

        procedure(target_func)::f 
        real(kind=wp), intent(in) :: x
        real(kind=wp):: derivative
        real(kind=wp) :: dx = 1e-10
        derivative = real(f(x+dx) - f(x))/dx
    end function OVD


end module derivative_calculator_mod
module customFunctions
    use derivative_calculator_mod, only: wp
    implicit none
    
    
    contains
    pure function f1(x) result(fx)
        real(kind=wp), intent(in)::x
        real(kind=wp)::fx
        fx = cos(x) - 0.226*x
    end function f1

    pure function find_E(kl) result(E)
        real(kind=wp), intent(in):: kl
        real(kind=wp)::E
        real(kind=wp)::K 

        K = kl/(2e-10)
        E = (K**2)* ((6.58e-16) **2) /(2*(9.1e-31))
    end function find_E

end module customFunctions

module Newton_Raphson
    use derivative_calculator_mod, only: wp, OVD
    ! use customFunctions, only: f1, find_E
    implicit none
    contains
    function findZero(f, x) result(x0)
        interface
            pure function target_func(t) result(y)
                import wp
                implicit none
                real(kind=wp), intent(in)::t
                real(kind=wp)::y
            end function target_func
        end interface

        procedure(target_func)::f 
        real(kind=wp), intent(in) :: x
        real(kind=wp):: x0 
        integer:: i
        x0 = x

        do i=1, 10000
            x0 = x0 - 0.1*f(x0)/OVD(f, x0)
        end do
        ! real(kind=wp) :: dx = 1e-10
        ! x0 = x+1

        ! derivative = real(f(x+dx) - f(x))/dx
    end function findZero
    



end module Newton_Raphson

program main
    use customFunctions, only: find_E, f1, wp
    use Newton_Raphson, only: findZero

    
    real(kind=wp):: sol
    ! real(kind=wp):: f1
    real(kind=wp):: initalGuess
    real(kind=wp):: E
    initalGuess = 1.4
    print *, f1(initalGuess)

    sol = findZero(f1, initalGuess)
    print *, sol
    E = find_E(sol)
    print *, E

    
    

end program main