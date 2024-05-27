program loops_and_ifs

    implicit none
    integer:: i 
    do i=1,10
        print *, i
    end do

    if(3>2 .and. 3<9) then 
        print *, "Yup"
    end if



    i=0
    do while(i<10)
        print *, i
        i = i+1
    end do
    


    

end program loops_and_ifs
