!> =============================================================================
!> 
!> cgol2.f95 - cgol rules.
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


module cgol2

  implicit none

contains


  !> cgol_rule_b3s23 - Applies rule b3s23.
  !>
  !> Arguments:
  !> - p_e1: value of the element being evaluated.
  !> - p_n1: neightborhood sum.
  !>
  integer function cgol_rule_b3s23(p_e1, p_n1)

    implicit none
    integer :: p_e1, p_n1, res

    res = 0
    
    if (p_e1.ne.1) then
       if (p_n1.eq.3) then
          res = 1
       else
          res = 0
       end if
    else if (p_e1.eq.1) then
       if (p_n1.eq.2) then
          res = 1
       else if (p_n1.eq.3) then
          res = 1
       else
          res = 0
       end if
    end if

    cgol_rule_b3s23 = res

  end function cgol_rule_b3s23


  !> cgol_rule_b3s234 - Applies rule b3s23.
  !>
  !> Arguments:
  !> - p_e1: value of the element being evaluated.
  !> - p_n1: neightborhood sum.
  !>
  integer function cgol_rule_b3s234(p_e1, p_n1)

    implicit none
    integer :: p_e1, p_n1, res

    res = 0
    
    if (p_e1.ne.1) then
       if (p_n1.eq.3) then
          res = 1
       else
          res = 0
       end if
    else if (p_e1.eq.1) then
       if (p_n1.eq.2) then
          res = 1
       else if (p_n1.eq.3) then
          res = 1
       else if (p_n1.eq.4) then
          res = 1          
       else
          res = 0
       end if
    end if

    cgol_rule_b3s234 = res

  end function cgol_rule_b3s234


  !> cgol_apply_rule - Applies scpecified rule.
  !>
  !> Arguments:
  !> - p_r1: rule specification.
  !>   - 1: applies rule b3s23.
  !>   - Default: applies rule b3s23.
  !>
  integer function cgol_apply_rule(p_r1, p_e1, p_n1)

    integer :: p_r1, p_e1, p_n1, res

    res = 0
    
    if (p_r1.eq.1) then
       res = cgol_rule_b3s23(p_e1, p_n1)
    else if (p_r1.eq.2) then
       res = cgol_rule_b3s234(p_e1, p_n1)       
    end if
    
    cgol_apply_rule = res
    
  end function cgol_apply_rule
  
  
end module cgol2

