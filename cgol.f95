!> =============================================================================
!> 
!> cgol.f95 - Conway's game of life.
!> 
!> =============================================================================
!> 
!>  Copyright (C) 2020  Pablo Edronkin (pablo.edronkin at yahoo.com)
!> 
!>    This program is free software: you can redistribute it and/or modify
!>    it under the terms of the GNU Lesser General Public License as published
!>    by the Free Software Foundation, either version 3 of the License, or
!>    (at your option) any later version.
!> 
!>    This program is distributed in the hope that it will be useful,
!>    but WITHOUT ANY WARRANTY; without even the implied warranty of
!>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!>    GNU Lesser General Public License for more details.
!> 
!>    You should have received a copy of the GNU Lesser General Public License
!>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
!> 
!> =============================================================================


!> Main program.
!>
!> Sources:
!> - Es.wikipedia.org. 2020. Autómata Celular. [online] Available at:
!>   https://es.wikipedia.org/wiki/Aut%C3%B3mata_celular [Accessed 18 June 2020].
!> - En.wikipedia.org. 2020. Conway's Game Of Life. [online] Available at:
!>   https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life [Accessed 18 June 2020].
!> - Gcc.gnu.org. 2020. Top (The GNU Fortran Compiler). [online] Available at:
!>   https://gcc.gnu.org/onlinedocs/gfortran/index.html#SEC_Contents [Accessed 21 June 2020].
!> - Pl.wikipedia.org. 2020. Gra W Życie. [online] Available at:
!>   https://pl.wikipedia.org/wiki/Gra_w_%C5%BCycie [Accessed 24 June 2020].
!> - En.wikipedia.org. 2020. Gun (Cellular Automaton). [online] Available at:
!>   https://en.wikipedia.org/wiki/Gun_(cellular_automaton) [Accessed 26 June 2020].
!>
program cgol

  use omp_lib
  use cgol1
  use cgol3
  
  implicit none
  integer :: o1
  integer, dimension(:,:), allocatable :: w1
  integer, dimension(9,1) :: c1
  
  ! Initial configuration.
  c1 = 0
  o1 = 1  
  call cgol_config_init(c1)
    
  ! Main menu cycle.
  do while (o1 > 0)
     call system("clear")
     call cgol_comment("Select your option:")
     call cgol_comment("0 - Exit.")
     call cgol_comment("1 - Configure")
     call cgol_comment("2 - Start.")
     read(*,*) o1

     select case (o1)
     case (1)
        call cgol_config_new(c1)
     case (2)
        allocate(w1(c1(1,1),c1(2,1)))
        call cgol_life(w1, c1)
        deallocate (w1)
     case default
        o1 = 0
        call system("clear")
        call cgol_comment("Bye!")        
     end select

  end do
  
end program cgol


