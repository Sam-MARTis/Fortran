program adv_loops
    implicit none
    integer:: i, j
    integer:: power_two(10)
    i = 0
    do while (i<10)
        i = i+1
        if (i==4) then
            cycle
        elseif (i==8) then
            exit
        else
            print *, i
        end if
    end do

    i=0
    j=0

    outer_loop: do i=0,10,2
        inner_loop: do j=3,20,3
            if ((i+j)>13) then
                cycle outer_loop
            endif
        end do inner_loop
    end do outer_loop




    print *, "Concurrent loop: "



    ! concurrent loop
    do concurrent (i=1:10)
        power_two(i) = 4**(10-i)
        ! print *, 4**(10-i)
    end do
    print *, power_two(:)
    ! Faster


            






    

end program adv_loops
