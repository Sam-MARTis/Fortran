program main
    integer, dimension(3, 3):: a, b
    a = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 4, 6], shape(b))
    print *, a(2, :)
    b(:, :) = a(:, :)
    a(2, :) = 0
    print *, b(2, :)
end program main