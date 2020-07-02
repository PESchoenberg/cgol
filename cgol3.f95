!> =============================================================================
!> 
!> cgol3.f95 - cgol configuration.
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


module cgol3

  use cgol1
  use cgol2
  implicit none

contains

  
  !> cgol_config_init - Sets the initial configuration.
  !>
  !> Arguments:
  !> - p_c1: configuration matrix.
  !>
  subroutine cgol_config_init(p_c1)

    implicit none
    integer, dimension( : , :) :: p_c1
    
    ! m1 - w1 rows.
    p_c1(1,1) = 30

    ! n1 - w1 cols.
    p_c1(2,1) = 135

    ! o2 - Neighborhood type.
    !      - 0: Von Neumann.
    !      - 1: Moore.
    p_c1(3,1) = 1

    ! t1 - Max number of iterations or ticks.
    p_c1(4,1) = 2000

    ! f1 - Initial percentage of cells alive.
    p_c1(5,1) = 75

    ! r1 - Rule.
    p_c1(6,1) = 1

    ! t3 - Stop on iter.
    !      - 0: Don't stop.
    !      - 1: stop (requires the user to press [enter] after each iteration.
    p_c1(7,1) = 0

    ! t4 - Wait on iter. Number of seconds that the simulation will wait after each iter.
    p_c1(8,1) = 0

    ! t6 - Initial world map. TODO.
    !      - 0: Random.
    !      - > 0: map number.
    p_c1(9,1) = 0

  end subroutine cgol_config_init


  !> cgol_config_custom - Sets an user - defined configuration.
  !>
  !> Arguments:
  !> - p_c1: configuration matrix.
  !>   
  subroutine cgol_config_custom(p_c1)

    implicit none
    integer :: n1, o1
    integer, dimension( : , : ) :: p_c1 

    o1 = 1
    
    ! Menu cycle.
    do while (o1 > 0)
       call system("clear")
       call cgol_comment("Select your option:")
       call cgol_comment("0 - Exit.")
       call cgol_comment("1 - Rows.")
       call cgol_comment("2 - Columns.")       
       call cgol_comment("3 - Neighborhood type.")
       call cgol_comment("4 - Number of iterations.")
       call cgol_comment("5 - Initial percentage of cells alive.")
       call cgol_comment("6 - Rule.")
       call cgol_comment("7 - Stop on iteration.")
       call cgol_comment("8 - Wait on iteration.")
       call cgol_comment("9 - Initial world map.")
       read(*,*) o1

       call system("clear")
       select case (o1)
       case (1)
          call cgol_comment("1 - Rows [30]?")
          read (*,*) n1
          if (n1.gt.10) then
             p_c1(1,1) = n1
          end if
       case (2)
          call cgol_comment("2 - Columns [135]?")
          read (*,*) n1
          if (n1.gt.10) then
             p_c1(2,1) = n1
          end if          
       case (3)
          call cgol_comment("3 - Neighborhood type [1] (0 = Von Neumann, 1 = Moore)?")
          read (*,*) n1
          if ((n1.eq.0).or.(n1.eq.1)) then
             p_c1(3,1) = n1
          end if
       case (4)
          call cgol_comment("4 - Number of iterations [2000]?")
          read (*,*) n1
          if (n1.gt.0) then
             p_c1(4,1) = n1
          end if
       case (5)
          call cgol_comment("5 - Initial percentage of cells alive [75]?")
          read (*,*) n1
          if ((n1.ge.0).and.(n1.le.100)) then
             p_c1(5,1) = n1
          end if
       case (6)
          call cgol_comment("6 - Rule [75] (1 = b3s23, 2 = b3s234)?")
          read (*,*) n1
          if ((n1.ge.1).and.(n1.le.2)) then
             p_c1(6,1) = n1
          end if
       case (7)
          call cgol_comment("7 - Stop on iter [0] (0 = no, 1 = yes)?")
          read (*,*) n1
          if ((n1.ge.0).and.(n1.le.1)) then
             p_c1(7,1) = n1
          end if
       case (8)          
          call cgol_comment("8 - Wait on iter [0]?")
          read (*,*) n1
          if (n1.ge.0) then
             p_c1(8,1) = n1
          end if
       case (9)          
          call cgol_comment("9 - Initial world map [0] (0 =  random, > 0 = user-specified)?")
          read (*,*) n1
          if (n1.ge.0) then
             p_c1(9,1) = n1
          else
             p_c1(9,1) = 0
          endif          
       case default
          o1 = 0          
       end select

    end do

  end subroutine cgol_config_custom
  

  !> cgol_config_new - Config change options menu.
  !>
  !> Arguments:
  !> - p_c1: configuration matrix.
  !>  
  subroutine cgol_config_new(p_c1)

    implicit none
    integer :: o1
    integer, dimension( : , : ) :: p_c1    

    o1 = 1
    
    ! Menu cycle.
    do while (o1 > 0)
       call system("clear")
       call cgol_comment("Select your option:")
       call cgol_comment("0 - Exit.")
       call cgol_comment("1 - Use initial config.")
       call cgol_comment("2 - Use custom config.")
       read(*,*) o1

       select case (o1)
       case (1)
          call cgol_config_init(p_c1)
       case (2)
          call cgol_config_custom(p_c1)
       case default
          o1 = 0
          call system("clear")
       end select

    end do
    
  end subroutine cgol_config_new
  
end module cgol3
