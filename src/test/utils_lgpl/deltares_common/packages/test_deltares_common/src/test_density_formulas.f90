!!  Copyright (C)  Stichting Deltares, 2025-2025.
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
module test_density_formulas
   use ftnunit
   use precision, only: dp
   use m_density_formulas

   implicit none
   private

   public :: tests_density_formulas

   real(dp), parameter :: tolerance = 1.0e-10_dp

contains

   subroutine tests_density_formulas
      call test(test_unesco_83, 'Test computation of water density using UNESCO 83 description.')
   end subroutine tests_density_formulas

   !> tests computation of varying air density
   subroutine test_unesco_83
      use precision, only: dp
      real(kind=dp) :: sal, tem, pres, dum0, dum1, dum2, rho_u

      write (*, *) 'rhounesco83 at 0 m and 10 km depth '

      sal = 30.0_dp; tem = 30.0_dp; pres = 0.0_dp * 1d5
      dum0 = density_unesco83(sal, tem, pres)

      sal = 30.0_dp; tem = 30.0_dp; pres = 1.0_dp * 1d5
      dum1 = density_unesco83(sal, tem, pres)

      sal = 8.0_dp; tem = 10.0_dp; pres = 10.0_dp * 1d5
      dum2 = density_unesco83(sal, tem, pres)

      sal = 0.0_dp; tem = 0.0_dp; pres = 0.0_dp * 1d5; rho_u = density_unesco83(sal, tem, pres)
      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
      sal = 0.0_dp; tem = 0.0_dp; pres = 1000.0_dp * 1d5; rho_u = density_unesco83(sal, tem, pres)
      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u

      sal = 40.0_dp; tem = 0.0_dp; pres = 000.0_dp * 1d5; rho_u = density_unesco83(sal, tem, pres)
      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
      sal = 40.0_dp; tem = 0.0_dp; pres = 1000.0_dp * 1d5; rho_u = density_unesco83(sal, tem, pres)
      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u

      sal = 00.0_dp; tem = 40.0_dp; pres = 000.0_dp * 1d5; rho_u = density_unesco83(sal, tem, pres)
      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
      sal = 00.0_dp; tem = 40.0_dp; pres = 1000.0_dp * 1d5; rho_u = density_unesco83(sal, tem, pres)
      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u

      sal = 40.0_dp; tem = 40.0_dp; pres = 000.0_dp * 1d5; rho_u = density_unesco83(sal, tem, pres)
      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
      sal = 40.0_dp; tem = 40.0_dp; pres = 1000.0_dp * 1d5; rho_u = density_unesco83(sal, tem, pres)
      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
   end subroutine test_unesco_83
end module test_density_formulas
