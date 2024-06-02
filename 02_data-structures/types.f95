module customTypes
    implicit none
    public
    type :: Organism
    integer :: age
    logical :: isAlive
    integer :: complexity
    end type


    type, extends(Organism):: species
    character(len=:), allocatable:: name
    integer:: AliveCount
    end type

    type, extends(species):: Civilization
    integer:: population
    end type

end module


program main
    use customTypes
    implicit none
    

    type(Civilization) :: humans
    
    humans%population = 20e8
    humans%AliveCount = humans%population
    humans%complexity = 8
    humans%isAlive = .true.
    humans%age = 10e5

    
    print *, "Hello world"
end program main