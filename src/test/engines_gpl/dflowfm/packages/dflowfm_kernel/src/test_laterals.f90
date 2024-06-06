!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module test_lateral
   use ftnunit
   use precision, only: hp
   use stdlib_kinds, only: dp
   use dfm_error, only: DFM_NOERR, DFM_GENERICERROR
   use m_lateral

   implicit none

   real(hp), parameter :: tolerance = 1.0e-10_hp

   contains
!
!
!==============================================================================
!   
subroutine tests_lateral
   call test( test_get_lateral_volume_per_layer, 'Test computation of water volume per layer in laterals.')
   call test(test_distribute_lateral_discharge_per_layer_per_cell, 'Test the distribution of lateral discharge per layer,' // &
                                                                   ' which is retrieved from BMI, to per layer per cell.')
end subroutine tests_lateral
!
!==============================================================================
!> Test computation of water volume per layer for laterals.
!> This test assumes a model of dimension (nx,ny,nz) = (3,3,3).
!> In the last node, the model is shallow meaning it has only 2 active layers.
!> The model contains 2 laterals. 
subroutine test_get_lateral_volume_per_layer
   use m_flow, only: vol1, kbot, ktop, kmxn, kmx
   
   real(kind=dp), allocatable, dimension(:,:)   :: lateral_volume_per_layer  !< Water volume per layer in laterals, dimension = (number_of_layer,number_of_lateral) = (kmx,numlatsg)
   
   integer :: ierr                     !< error flag
   integer :: ndx, ndkx
   integer :: i_cell
   
   ! specify number of computational cells, ndx
   ndx = 9
   
   ! initialize number of active layers for each node
   allocate(kmxn(ndx),stat=ierr)
   kmxn = (/3,3,3,3,3,3,3,3,2/) ! the last cell is assumed shallow and contains only two layers

   ! initialize water volume per cell, vol1
   kmx = 3
   ndkx = ndx * (kmx + 1) - 1 ! one cell is shallow and contains only two layers
   allocate(vol1(ndkx),stat=ierr)
   vol1(ndx+1:) = 1d0 ! only volume per cell, per layer is needed; the first ndx elements contain 2D volume (i.e. total volume over all layers) and are not needed for the function tested here, hence not set.
   
   ! initialize kbot and ktop
   allocate(kbot(ndx),stat=ierr)
   allocate(ktop(ndx),stat=ierr)
   kbot(1) = ndx + 1
   ktop(1) = kbot(1) + kmxn(1) - 1
   do i_cell = 2,ndx
      kbot(i_cell) = ktop(i_cell-1) + 1
      ktop(i_cell) = kbot(i_cell) + kmxn(i_cell) - 1
   enddo
   
   ! initialize laterals administration
   numlatsg = 2
   allocate(n1latsg(numlatsg),stat=ierr)
   allocate(n2latsg(numlatsg),stat=ierr)
   allocate(nnlat(nlatnd),stat=ierr)
   nnlat = (/1,2,8,9/)
   n1latsg = (/1,3/)
   n2latsg = (/2,4/)
   
   allocate(lateral_volume_per_layer(kmx, numlatsg),stat=ierr)
   call get_lateral_volume_per_layer(lateral_volume_per_layer)
   call assert_comparable(lateral_volume_per_layer(1,1), 2d0, tolerance, "get_lateral_volume_per_layer(1,1) output lateral_volume_per_layer is not correct" )
   call assert_comparable(lateral_volume_per_layer(2,1), 2d0, tolerance, "get_lateral_volume_per_layer(2,1) output lateral_volume_per_layer is not correct" )
   call assert_comparable(lateral_volume_per_layer(3,1), 2d0, tolerance, "get_lateral_volume_per_layer(3,1) output lateral_volume_per_layer is not correct" )
   call assert_comparable(lateral_volume_per_layer(1,2), 1d0, tolerance, "get_lateral_volume_per_layer(1,2) output lateral_volume_per_layer is not correct" )
   call assert_comparable(lateral_volume_per_layer(2,2), 2d0, tolerance, "get_lateral_volume_per_layer(2,2) output lateral_volume_per_layer is not correct" )
   call assert_comparable(lateral_volume_per_layer(3,2), 2d0, tolerance, "get_lateral_volume_per_layer(3,2) output lateral_volume_per_layer is not correct" )

   deallocate(kmxn)
   deallocate(vol1)
   deallocate(kbot)
   deallocate(ktop)
   deallocate(n1latsg)
   deallocate(n2latsg)
   deallocate(nnlat)
   deallocate(lateral_volume_per_layer)

end subroutine test_get_lateral_volume_per_layer

!==============================================================================
!> Test computation of distribution of lateral discharge per layer, which is retrived from BMI,
!! to discharge per layer per cell.
!! This test assumes a model of dimension (nx,ny,nz) = (4,2,3), i.e. 3 layers.
!! In the last node, the model is shallow meaning it has only 2 active layers.
!! The model contains 2 laterals.
subroutine test_distribute_lateral_discharge_per_layer_per_cell
   use m_flow,          only: vol1, kbot, ktop, kmxn, kmx
   use m_alloc,         only: realloc

   real(kind=dp), allocatable, dimension(:,:) :: lateral_discharge_per_layer_per_cell !< Discharge per layer per cell,
                                                                                      !! dimension=(number_of_layer,number_of_node)
                                                                                      !!          =(kmx,ndkx)

   integer :: ierr !< error flag
   integer :: ndx, ndkx
   integer :: i_node, i_layer
   real(kind=dp), allocatable, dimension(:,:) :: provided_lateral_discharge_per_layer

   ! Specify number of computational cells, ndx
   ndx = 8

   ! Initialize number of active layers for each cell
   call realloc(kmxn, ndx, stat=ierr, keepExisting=.false., fill=0)
   call check_allocation_error_write_message(ierr, 'kmxn', 'test_distribute_lateral_discharge_per_layer_per_cell')
   kmxn = [3,3,3,3,3,3,3,2] ! The last cell is assumed shallow and contains only two layers

   ! Initialize water volume per cell, vol1
   kmx = 3
   ndkx = ndx * (kmx + 1) - 1 ! one cell is shallow and contains only two layers
   call realloc(vol1, ndkx, stat=ierr, keepExisting=.false., fill=0d0)
   call check_allocation_error_write_message(ierr, 'vol1', 'test_distribute_lateral_discharge_per_layer_per_cell')

   do i_node = ndx+1, ndkx
      vol1(i_node) = 10 + i_node ! only volume per cell, per layer is needed; the first ndx elements contain 2D volume
                                 ! (i.e. total volume over all layers) and are not needed for the function tested here,
                                 ! hence not set.
   end do

   ! Initialize kbot and ktop
   call realloc(kbot, ndx, stat=ierr, keepExisting=.false., fill=0)
   call check_allocation_error_write_message(ierr, 'kbot', 'test_distribute_lateral_discharge_per_layer_per_cell')

   call realloc(ktop, ndx, stat=ierr, keepExisting=.false., fill=0)
   call check_allocation_error_write_message(ierr, 'ktop', 'test_distribute_lateral_discharge_per_layer_per_cell')

   kbot(1) = ndx + 1
   ktop(1) = kbot(1) + kmxn(1) - 1
   do i_node = 2,ndx
      kbot(i_node) = ktop(i_node-1) + 1
      ktop(i_node) = kbot(i_node) + kmxn(i_node) - 1
   enddo

   ! Initialize laterals administration
   numlatsg = 2
   call realloc(n1latsg, numlatsg, stat=ierr, keepExisting=.false., fill=0)
   call check_allocation_error_write_message(ierr, 'n1latsg', 'test_distribute_lateral_discharge_per_layer_per_cell')

   call realloc(n2latsg, numlatsg, stat=ierr, keepExisting=.false., fill=0)
   call check_allocation_error_write_message(ierr, 'n2latsg', 'test_distribute_lateral_discharge_per_layer_per_cell')

   call realloc(nnlat,   nlatnd,   stat=ierr, keepExisting=.false., fill=0)
   call check_allocation_error_write_message(ierr, 'nnlat', 'test_distribute_lateral_discharge_per_layer_per_cell')


   nnlat   = [1,2,7,8]
   n1latsg = [1,3]
   n2latsg = [2,4]

   call realloc(lateral_volume_per_layer, (/ kmx, numlatsg /), stat=ierr, keepExisting=.false., fill=0d0)
   call check_allocation_error_write_message(ierr, 'lateral_volume_per_layer', &
                                             'test_distribute_lateral_discharge_per_layer_per_cell')
   call realloc(provided_lateral_discharge_per_layer, (/ kmx, numlatsg /), stat=ierr, keepExisting=.false., fill=0d0)
   call check_allocation_error_write_message(ierr, 'provided_lateral_discharge_per_layer', &
                                             'test_distribute_lateral_discharge_per_layer_per_cell')
   call realloc(lateral_discharge_per_layer_per_cell, (/ kmx, ndkx/), stat=ierr, keepExisting=.false., fill=0d0)
   call check_allocation_error_write_message(ierr, 'lateral_discharge_per_layer_per_cell', &
                                             'test_distribute_lateral_discharge_per_layer_per_cell')

   lateral_volume_per_layer(1:kmx,1) = [100, 300, 250]
   lateral_volume_per_layer(1:kmx,2) = [200, 250, 330]
   provided_lateral_discharge_per_layer(1:kmx,1) = [1000, 1500, 2800]
   provided_lateral_discharge_per_layer(1:kmx,2) = [2000, 3000, 2500]

   ! Distribute the lateral discharge
   call distribute_lateral_discharge_per_layer_per_cell(provided_lateral_discharge_per_layer, lateral_discharge_per_layer_per_cell)

   ! Compare results with reference results
   call assert_comparable(lateral_discharge_per_layer_per_cell(1,9),  190.0d0, tolerance, &
                          "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(1,9)" // &
                          " is not correct.")
   call assert_comparable(lateral_discharge_per_layer_per_cell(2,10), 100.0d0, tolerance, &
                          "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(2,10)" // &
                          " is not correct.")
   call assert_comparable(lateral_discharge_per_layer_per_cell(3,11), 235.2d0, tolerance, &
                          "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(3,11)" // &
                          " is not correct.")
   call assert_comparable(lateral_discharge_per_layer_per_cell(1,12), 220.0d0, tolerance, &
                          "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(1,12)" // &
                          " is not correct.")
   call assert_comparable(lateral_discharge_per_layer_per_cell(2,13), 115.0d0, tolerance, &
                          "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(2,13)" // &
                          " is not correct.")
   call assert_comparable(lateral_discharge_per_layer_per_cell(3,14), 268.8d0, tolerance, &
                          "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(3,14)" // &
                          " is not correct.")
   call assert_comparable(lateral_discharge_per_layer_per_cell(1,27), 370.0d0, tolerance, &
                          "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(1,27)" // &
                          " is not correct.")
   call assert_comparable(lateral_discharge_per_layer_per_cell(2,28), 456.0d0, tolerance, &
                          "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(2,13)" // &
                          " is not correct.")
   call assert_comparable(lateral_discharge_per_layer_per_cell(3,29), 295.454545454545d0, tolerance, &
                          "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(3,29)" // &
                          " is not correct.")
   call assert_comparable(lateral_discharge_per_layer_per_cell(2,30), 480.0d0, tolerance, &
                          "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(2,30)" // &
                          " is not correct.")
   call assert_comparable(lateral_discharge_per_layer_per_cell(3,31), 310.606060606061d0, tolerance, &
                          "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(3,31)" // &
                          " is not correct.")

   ! Deallocate
   if (allocated(kmxn)) then
      deallocate(kmxn)
   end if
   if (allocated(vol1)) then
      deallocate(vol1)
   end if
   if (allocated(kbot)) then
      deallocate(kbot)
   end if
   if (allocated(ktop)) then
      deallocate(ktop)
   end if
   if (allocated(n1latsg)) then
      deallocate(n1latsg)
   end if
   if (allocated(n2latsg)) then
      deallocate(n2latsg)
   end if
   if (allocated(nnlat)) then
      deallocate(nnlat)
   end if
   if (allocated(lateral_volume_per_layer)) then
      deallocate(lateral_volume_per_layer)
   end if
   if (allocated(provided_lateral_discharge_per_layer)) then
      deallocate(provided_lateral_discharge_per_layer)
   end if
   if (allocated(lateral_discharge_per_layer_per_cell)) then
      deallocate(lateral_discharge_per_layer_per_cell)
   end if

end subroutine test_distribute_lateral_discharge_per_layer_per_cell

!> Checks the allocation error.
!! If an error occures, then write an error message.
subroutine check_allocation_error_write_message(ierr, array_name, subroutine_name)
   use MessageHandling, only: LEVEL_ERROR, mess
   use dfm_error,       only: DFM_NOERR

   integer,          intent(in) :: ierr            !< Error flag.
   character(len=*), intent(in) :: array_name      !< Name of the array that was to be allocated.
   character(len=*), intent(in) :: subroutine_name !< Name of the subroutine where the allocation was to be done.

   if (ierr /= DFM_NOERR) then
      call mess(LEVEL_ERROR, &
               'Error occured when allocating array '''//trim(array_name)//''' in subroutine'''//trim(subroutine_name)//'''.')
   end if
end subroutine check_allocation_error_write_message

end module test_lateral
