program arrays_2
implicit none
integer, dimension(10):: arr1
integer:: i, j
real:: arr2(10, 10, 10)
integer:: arr3(0: 5) ! Inclusive of 0 and 5
integer:: arr4(5, 5)

arr3(:) = 1
print*, arr3
arr4 = [(i, i=1,4), :]
print*, arr4


end program arrays_2