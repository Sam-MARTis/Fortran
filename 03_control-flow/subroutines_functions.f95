subroutine sumSubRoutine(a,  b, c) 
    integer, intent(in) :: a
    integer, intent(in) :: b
    integer, intent(out):: c
    c = a+b
    
end subroutine sumSubRoutine


function sumFunc(i, j) result(norm)
    integer, intent(in):: i, j
    integer:: norm
    norm = i+j

end function sumFunc


program functions_subroutines
    implicit none
    integer:: i
    integer:: j
    integer:: s 
    integer:: sumFunc
    
    i=10
    j=20
    s= 3
    call sumSubRoutine(i, j, s)
    print *, "Subroutine"
    print *, s
    print *, "Function"
    print *, "Sum of ", i, " and ",j," is: ",sumFunc(i, j)
    



    
end program functions_subroutines