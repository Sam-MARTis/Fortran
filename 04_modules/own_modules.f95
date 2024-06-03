

module circle_functions
    implicit none

    public pi, circumference_calc, area_calc
    real:: pi = 3.14159265358979323846
 
    
contains
function circumference_calc(r) result(circ)
    real, intent(in)::r
    real:: circ
    circ = 2*pi*r 
end function circumference_calc

function area_calc(r) result(area)
    real, intent(in)::r 
    real:: area
    area = pi* (r**2)
end function area_calc

    
end module circle_functions


program main
    
    use circle_functions
    implicit none
    ! real:: area_calc
    ! real:: circumference_calc
    real:: radius
    print *, "Input radius: "
    read *, radius
    print *, "Area is: ", area_calc(radius)
    print *, "Circumference is ", circumference_calc(radius)



end program main