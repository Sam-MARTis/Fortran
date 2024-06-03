module universal_constants
implicit none
real :: pi = 3.14159265358979323846
real :: hbar = 1.054571726e-34
real :: c = 2.99792458e8
real :: e = 1.602176565e-19

    

end module universal_constants

program main
implicit none
real :: radius
block
    use universal_constants, only: pi
    real:: circumference
    print *, "Enter radius of circle: "
    read *, radius
    circumference = 2.0*pi*radius
    print *, "The circumference of the circle is ", circumference
    
end block


end program main
