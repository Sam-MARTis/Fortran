program arrays_2
implicit none
integer, dimension(10):: arr1
! integer :: i 
! integer :: j
integer :: arr(4,4)
integer:: i, j
real:: arr2(10, 10, 10)
integer:: arr3(0: 5) ! Inclusive of 0 and 5
! integer:: arr(5, 5)

arr3(:) = 1
print*, arr3
! arr = reshape([(i, i = 0, 4, j=0, 4)], [5,5])
! integer :: arr(4,4)

arr = reshape([((i*j, i=1,4), j=1,4)], shape(arr))
print*, arr


end program arrays_2