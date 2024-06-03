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

    contains
    procedure :: fractionAlive  ! procedure declaration
  end type
    contains

    real function fractionAlive(self) result(frac)
    class(Civilization), intent(in) :: self
    frac = real(self%AliveCount) / self%population
    end function

    ! real function fractionAlive(self) result(frac)
    !     type(Civilization), intent(in) :: self
    !     frac = real(self%AliveCount) / self%population
        ! real:: frac
        ! frac 
        

    ! end function

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