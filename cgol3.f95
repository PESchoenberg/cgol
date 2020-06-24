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
    
    ! m1 - Rows.
    p_c1(1,1) = 30

    ! n1 - Cols.
    p_c1(2,1) = 135

    ! o2 -Neighborhood option.
    p_c1(3,1) = 1

    ! t1 - Max number of iterations.
    p_c1(4,1) = 2000

    ! f1 = Growth factor %.
    p_c1(5,1) = 75

    !> r1 - Rule.
    p_c1(6,1) = 1

    !> t3 - Stop on iter.
    p_c1(7,1) = 0

    !> t4 - Wait on iter.
    p_c1(8,1) = 0

    !> t5 - Stop on equilibrium reached.
    p_c1(9,1) = 0

  end subroutine cgol_config_init


  !> cgol_config_new - Sets an user - defined configuration.
  !>
  !> Arguments:
  !> - p_c1: configuration matrix.
  !>  
  subroutine cgol_config_new(p_c1)

    implicit none
    integer, dimension( : , :) :: p_c1    

    !cgol_config_new = p_c1
    
  end subroutine cgol_config_new
  
end module cgol3
