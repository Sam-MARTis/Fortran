program float_precision
 use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
    implicit none
    real(sp) :: x
    real(dp) :: y
    x = 1.0_sp
    y = 1.0_dp
    print *, x
    print *, y
    print *, x == y
    print *, x + y
    print *, x - y

end program float_precision