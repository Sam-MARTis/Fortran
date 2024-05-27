program mem_and_string
    implicit none
    
    integer, allocatable:: intArr(:, :)


    character(len=4) :: firstName
    character(len=5):: secondName
    character(10):: fullNameOne
    character(:), allocatable :: thirdName
    ! character(len=, kind=), attributes :: name
    character(:), allocatable :: fourthName

    allocate(character(6):: fourthName)
    fourthName = "carter"
    thirdName = "William"



    ! character(:):: fourthName = "carter"

    allocate(intArr(10, 10))
    intArr(:,:) = 1
    

    

end program mem_and_string

