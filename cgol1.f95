!> =============================================================================
!> 
!> cgol1.f95 - cgol basic functions.
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


module cgol1

  use cgol2
  implicit none

contains

  !> cgol_comment - Writes a comment on console.
  !>
  !> Arguments:
  !> - p_t: text to be written.
  !>
  subroutine cgol_comment( p_t )

    implicit none
    character(len = *) :: p_t

    write(*,*) p_t
    write(*,*)

  end subroutine cgol_comment


  !> cgol_eval_neighborhood_moore - Returns the number of
  !> elements alive in the Moore neighborhood of an element.
  !>
  !> Arguments:
  !> - p_w1: world matrix.
  !> - p_i1: i value.
  !> - p_j1: j value.
  !>
  !> Sources:
  !> - https://en.wikipedia.org/wiki/Moore_neighborhood
  !>
  integer function cgol_eval_neighborhood_moore(p_w1, p_i1, p_j1)

    implicit none
    integer, intent(in) :: p_i1, p_j1
    integer :: res, hm1, hn1, lm2, hm2, ln2, hn2
    integer, intent(in), dimension( : , : ) :: p_w1
    integer, dimension(3,3) :: w2
    
    res = 0
    
    !> Obtain matrix info.
    hm1 = size(p_w1, 1) ! Rows.
    hn1 = size(p_w1, 2) ! Columns.

    !> Copy neighbouring values into w2
    !> Periodic border.
    if (p_i1.eq.1) then
       lm2 = hm1
       hm2 = p_i1 + 1
    else if (p_i1.eq.hm1) then
       lm2 = p_i1 - 1
       hm2 = 1
    else
       lm2 = p_i1 - 1
       hm2 = p_i1 + 1
    end if
    if (p_j1.eq.1) then
       ln2 = hn1
       hn2 = p_j1 + 1
    else if (p_j1.eq.hn1) then
       ln2 = p_j1 - 1
       hn2 = 1
    else
       ln2 = p_j1 - 1
       hn2 = p_j1 + 1
    end if

    ! Assign values to secondary matrix.
    w2(1,1) = p_w1(lm2, ln2)
    w2(1,2) = p_w1(lm2, p_j1)
    w2(1,3) = p_w1(lm2, hn2)
    w2(2,1) = p_w1(p_i1, ln2)
    w2(2,2) = p_w1(p_i1, p_j1)
    w2(2,3) = p_w1(p_i1, hn2)
    w2(3,1) = p_w1(hm2, ln2)
    w2(3,2) = p_w1(hm2, p_j1)
    w2(3,3) = p_w1(hm2, hn2)

    ! Calculate proximity value.
    cgol_eval_neighborhood_moore = sum(W2) - w2(2,2)
    
  end function cgol_eval_neighborhood_moore


  !> cgol_eval_neighborhood_von_neumann - Returns the number
  !> of elements alive in the Von Neumann neighborhood of
  !> an element.
  !>
  !> Arguments:
  !> - p_w1: world matrix.
  !> - p_i1: i value.
  !> - p_j1: j value.
  !>
  !> Sources:
  !> - https://en.wikipedia.org/wiki/Von_Neumann_neighborhood
  !>  
  integer function cgol_eval_neighborhood_von_neumann(p_w1, p_i1, p_j1)

    implicit none
    integer, intent(in) :: p_i1, p_j1
    integer :: res, hm1, hn1, lm2, hm2, ln2, hn2
    integer, intent(in), dimension( : , : ) :: p_w1
    integer, dimension(3,3) :: w2
    
    res = 0
    
    !> Obtain matrix info.
    hm1 = size(p_w1, 1) ! Rows.
    hn1 = size(p_w1, 2) ! Columns.

    !> Copy neighbouring values into w2
    !> Periodic border.
    if (p_i1.eq.1) then
       lm2 = hm1
       hm2 = p_i1 + 1
    else if (p_i1.eq.hm1) then
       lm2 = p_i1 - 1
       hm2 = 1
    else
       lm2 = p_i1 - 1
       hm2 = p_i1 + 1
    end if
    if (p_j1.eq.1) then
       ln2 = hn1
       hn2 = p_j1 + 1
    else if (p_j1.eq.hn1) then
       ln2 = p_j1 - 1
       hn2 = 1
    else
       ln2 = p_j1 - 1
       hn2 = p_j1 + 1
    end if

    ! Assign values to secondary matrix.
    w2(1,1) = 0
    w2(1,2) = p_w1(lm2, p_j1)
    w2(1,3) = 0
    w2(2,1) = p_w1(p_i1, ln2)
    w2(2,2) = p_w1(p_i1, p_j1)
    w2(2,3) = p_w1(p_i1, hn2)
    w2(3,1) = 0
    w2(3,2) = p_w1(hm2, p_j1)
    w2(3,3) = 0

    ! Calculate proximity value.
    cgol_eval_neighborhood_von_neumann = sum(W2) - w2(2,2)

  end function cgol_eval_neighborhood_von_neumann


  !> cgol_eval_neighborhood - Returns the number of elements
  !> alive in neighborhood of an element.
  !>
  !> Arguments:
  !> - p_o2: option:
  !>   - 2: Von Neumann neighborhood.
  !>   - <> 2: Moore neighborhood.
  !> - p_w1: world matrix.
  !> - p_i1: i value.
  !> - p_j1: j value.
  !>  
  integer function cgol_eval_neighborhood(p_o2, p_w1, p_i1, p_j1)

    implicit none
    integer, intent(in) :: p_i1, p_j1, p_o2
    integer, intent(in), dimension( : , : ) :: p_w1

    if (p_o2.eq.2) then
       cgol_eval_neighborhood = cgol_eval_neighborhood_von_neumann(p_w1, p_i1, p_j1)
    else
       cgol_eval_neighborhood = cgol_eval_neighborhood_moore(p_w1, p_i1, p_j1)
    end if
        
  end function cgol_eval_neighborhood
  
  
  !> cgol_randomize_world - Creates initial, random
  !> conditions.
  !>
  !> Arguments:
  !> - p_w1: world matrix.
  !> - p_f1: initial population.
  !>
  subroutine cgol_randomize_world(p_w1, p_f1)

    implicit none
    integer, dimension( : , : ) :: p_w1
    integer :: hm1, hn1, i1, j1, s1
    real :: p_f1, r1
    integer, allocatable :: seed(:)

    
    !> Obtain matrix info.
    hm1 = size(p_w1, 1) ! Rows.
    hn1 = size(p_w1, 2) ! Columns.

    ! Set random number generation.
    call random_seed() ! initialize with system generated seed
    call random_seed(size=s1) ! find out size of seed
    allocate(seed(s1))
    call random_seed(put=seed)
    call random_seed(get=seed)

    ! Generate random 1 values in w1.
    do j1 = 1, hn1
       do i1 = 1, hm1
          call random_number(r1)
          if (r1.gt.p_f1) then
             p_w1(i1,j1) = 1
          else
             p_w1(i1,j1) = 0
          endif
       end do
    end do

    ! Clean-up.
    deallocate(seed)
    
  end subroutine cgol_randomize_world
  

  !> cgol_display_world - Displays a simulation world
  !> instance on the console.
  !>
  !> Arguments:
  !> - p_w1: world matrix.
  !>
  subroutine cgol_display_world(p_w1, p_t1, p_t12, p_t3, p_t4)

    implicit none
    integer, dimension( : , : ) :: p_w1
    integer :: i1, j1, hm, hn, p_t1, p_t12, p_t3, p_t4
    character(1):: c1    

    !> Obtain matrix info.
    hm = size(p_w1, 1) ! Rows.
    hn = size(p_w1, 2) ! Columns.

    !> Clear console.
    call system("clear")

    !> Cycle and show.
    do i1 = 1, hm 
       do j1 = 1, hn
          if (p_w1(i1,j1).eq.1) then
             c1 = "@"
          else
             c1 = " "
          end if
          write(*,fmt="(A)", advance='no') c1
       end do
       write(*,*)
    end do

    !> Display info.   
    print "(/, 1a10, 1i6, 1a3, 1i6)", "Iter: ", p_t1, " / ", p_t12
    if (p_t4.gt.0) then
       call sleep(p_t4)
    else if (p_t3.eq.1) then
       read(*,*)
    end if
    
  end subroutine cgol_display_world


  !> cgol_calculate_iteration - Calculates the state of
  !> each automata per iteration.
  !>
  !> Arguments:
  !> - p_o2: neighborhood eval method.
  !> - p_w1: world matrix.
  !> - p_t5: integer.
  !>
  subroutine cgol_calculate_iteration(p_o2, p_w1, p_r1)

    implicit none
    integer, dimension( : , : ) :: p_w1
    integer :: i1, j1, hm, hn, p_o2, p_r1        
    integer, dimension(size(p_w1, 1), size(p_w1, 2)) :: w2
    
    !> Obtain matrix info.
    hm = size(p_w1, 1) ! Rows.
    hn = size(p_w1, 2) ! Columns.

    w2 = p_w1
    
    do i1 = 1, hm 
       do j1 = 1, hn
          w2(i1, j1) = cgol_apply_rule(p_r1, p_w1(i1, j1), cgol_eval_neighborhood(p_o2, p_w1, i1, j1))  
       end do
    end do

    p_w1 = w2
    
  end subroutine cgol_calculate_iteration


  !> cgol_save_world_file - Save a copy of the current p_w1 matrix.
  !>
  !> Arguments: 
  !> - p_w1: world matrix.
  !> - p_s1: file name to save.
  !>   - If equal to "na" then it will ask for a name for the file
  !>
  subroutine cgol_save_world_file(p_w1, p_s1)

    implicit none
    integer, dimension( : , : ) :: p_w1
    character(len = *) :: p_s1
    character(254) :: s1
    character(1) :: s2
    
    if (trim(p_s1).eq."na") then
       call cgol_comment("Save current world file (y/n)?")
       read(*,*) s2
       if (s2.eq."y") then
          call cgol_comment("Name of the current world file to be saved?")
          read(*,*) s1
          open(11, file = trim(s1))
          write(11,*) p_w1
          close(11)
       end if
    else
       open(11, file = trim(p_s1))
       write(11,*) p_w1
       close(11)
    end if
    
  end subroutine cgol_save_world_file


  !> cgol_find_and replace_string - If substring p_s1 is found in p_s2
  !> it s replaced with p_s3.
  !>
  !>
  !>
  function cgol_find_and_replace_string(p_s1, p_s2, p_s3) result(res)

    implicit none
    character(len = *) :: p_s1, p_s2, p_s3
    character(len(p_s2)+len(p_s3)) :: res
    integer :: n1

    n1 = index(p_s2, p_s1)

    ! Only do this if p_s1 is found in s2
    if (n1.gt.1) then
       res = trim(p_s2(1:(n1-1))//p_s3//p_s2(n1+len(p_s1):))
    else
       res = trim(p_s2)
    end if

  end function cgol_find_and_replace_string
  
  
  !> cgol_create_fname - Creates an experiment file name.
  !>
  !> Arguments:
  !> - p_e1: prepend text.
  !>
  subroutine cgol_create_fname(p_e1)

    character(len = *) :: p_e1
    character(8)  :: date
    character(10) :: time
    character(5)  :: zone
    !character(len(p_e1)+len(date)+len(time)+len(zone)) :: e2
    integer, dimension(8) :: values
    
    call date_and_time(date, time, zone, values)
    call date_and_time(DATE=date, ZONE=zone)
    call date_and_time(TIME=time)
    !e2 = p_e1//date//time
    !print '(a)', e2
    print '(a)', cgol_find_and_replace_string(".", p_e1//date//time, "_")
    !read(*,*)
  end subroutine cgol_create_fname
  
  
  !> cgol_life - Start an instance of a game.
  !>
  !> Arguments:
  !> - p_o2: neighborhood eval method.  
  !> - p_w1: world matrix.
  !> - p_t1: iterations.
  !> - p_f1: initial population.
  !> - p_r1: rule to apply.
  !>
  subroutine cgol_life(p_w1, p_c1)

    implicit none
    integer :: i1, j1, t1
    integer, dimension( : , : ) :: p_w1, p_c1
    character(254) :: s1
    
    i1 = 0
    j1 = 0

    ! Generate file name if so configured.
    ! https://gcc.gnu.org/onlinedocs/gfortran/DATE_005fAND_005fTIME.html
    call cgol_create_fname("exp_")
    
    !> Generation of the initial world conditions.
    if (p_c1(9,1).eq.0) then
       call cgol_randomize_world(p_w1, (p_c1(5,1)/100.00))
       call cgol_save_world_file(p_w1, "na")
    else
       call cgol_comment("Name of the world file to be loaded?")
       read(*,*) s1
       open(11, file = trim(s1))
       read(11,*) p_w1
       close(11)
    end if
    
    !> Repeat this p_t1 times.
    do t1 = 1, p_c1(4,1)

       !$OMP SECTIONS

       !$OMP SECTION
       call cgol_display_world(p_w1, t1, p_c1(4,1), p_c1(7,1), p_c1(8,1))
       !$OMP END SECTION

       !$OMP SECTION
       call cgol_calculate_iteration(p_c1(3,1), p_w1, p_c1(6,1))
       !$OMP END SECTION

       !$OMP BARRIER
       !$OMP END SECTIONS
       
    end do

    call cgol_save_world_file(p_w1, "na")
    
  end subroutine cgol_life


end module cgol1
