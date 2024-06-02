real function f1(x) result(x2)
    real, intent(in) :: x
    x2 = x**2
end function

real function f2(x) result(x3)
    real, intent(in) :: x
    x3 = x**3
end function

program main
    implicit none
    abstract interface
        function func (z)
        real :: func
        real, intent (in) :: z
        end function func
    end interface


    real:: x
    
    real:: f1, f2
    procedure (func), pointer :: f1_ptr => null(), f2_ptr => null ()
    f1_ptr => f1
    read *, x
    if(x>4) then
        f1_ptr => f2
    end if

    
    print *, f1(x)
    print *, f2(x)
    print *, f1_ptr(x)
    
end program main