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


module functions_mod
    use derivative_calculator_mod, only: wp
    implicit none


    contains
    
    pure function test_func(x) result(val)
        ! integer, parameter::wp = kind(1.0d0)
        ! import wp
        real(kind=wp), intent(in):: x
        real(kind=wp) :: val

        val = 3*(x**3) + 2*(x**2) + x
    end function test_func
end module functions_mod

program testing
    use derivative_calculator_mod, only: wp, OVD
    use functions_mod, only: test_func
    implicit none
    ! real(kind=wp):: test_func
    ! real(kind=wp):: OVD
    real(kind=wp)::x
    read *, x
    print *, test_func(x)
    print *, OVD(test_func, x)

    
end program testing