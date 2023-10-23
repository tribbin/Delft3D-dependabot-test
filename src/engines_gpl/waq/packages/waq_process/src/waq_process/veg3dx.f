!!  Copyright (C)  Stichting Deltares, 2012-2023.
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
      module m_veg3dx
      use m_waq_type_definitions


      implicit none

      contains


      subroutine veg3dx     ( pmsa   , fl     , ipoint , increm, noseg ,
     +                        noflux , iexpnt , iknmrk , noq1  , noq2  ,
     +                        noq3   , noq4   )

      use m_evaluate_waq_attribute
      use layered_sediment

      ! function distribute multiple vegetation fluxes over the vertical

      implicit none

!     arguments            i/o description

      real(kind=sp) ::pmsa(*)     !i/o process manager system array, window of routine to process library
      real(kind=sp) ::fl(*)       ! o  array of fluxes made by this process in mass/volume/time
      integer(kind=int_32) ::ipoint(*)   ! i  array of pointers in pmsa to get and store the data
      integer(kind=int_32) ::increm(*)   ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer(kind=int_32) ::noseg       ! i  number of computational elements in the whole model schematisation
      integer(kind=int_32) ::noflux      ! i  number of fluxes, increment in the fl array
      integer(kind=int_32) ::iexpnt(4,*) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
      integer(kind=int_32) ::iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
      integer(kind=int_32) ::noq1        ! i  nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      integer(kind=int_32) ::noq2        ! i  nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer(kind=int_32) ::noq3        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer(kind=int_32) ::noq4        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)

      ! from pmsa array

      real(kind=sp) ::depth       ! i  depth of segment                               (m)
      real(kind=sp) ::totaldepth  ! i  total depth water column                       (m)
      real(kind=sp) ::localdepth  ! i  depth from water surface to bottom of segment  (m)
      real(kind=sp) ::swmacdis    ! i  switch gr. distr.vb   (1)cont. (2)lin. (3)exp. (-)
      real(kind=sp) ::hmax        ! i  maxmimum lenght macrophytes                    (m)
      real(kind=sp) ::ffac        ! i  form factor macropyhyte                        (m)
      integer(kind=int_32) ::nvbxx       ! i  number of vb fractions to be distributed       (-)
      real(kind=sp) ::vb          ! i  macrophyte submerged                          (gc)
      real(kind=sp) ::delt        ! i  time step                                      (d)
      real(kind=sp) ::frbmlay     ! o  fraction bm per layer                          (-)
      real(kind=sp) ::bmlayvb     ! o  biomass layer vb                              (gc)

      ! local declarations

      integer(kind=int_32) ::iseg        !    local loop counter for computational element loop
      real(kind=sp) ::z2          !    height bottom segment from bottom              (m)
      real(kind=sp) ::z1          !    height top segment from bottom                 (m)
      integer(kind=int_32) ::ikmrk1
      integer(kind=int_32) ::ikmrk2
      real(kind=sp) ::zm          !    watersurface to top macropyte                  (-)
      real(kind=sp) ::a           !    lineair factor a (ax + b)                      (-)
      real(kind=sp) ::b           !    lineair factor b (ax + b)                      (-)
      integer(kind=int_32) ::iq          !    loop counter
      integer(kind=int_32) ::ifrom       !    from segment
      integer(kind=int_32) ::ito         !    from segment
      integer(kind=int_32) ::iflux       !    index in the fl array

      integer(kind=int_32), parameter            ::nipfix =  9         ! first number of entries in pmsa independent of number of parameters
      integer(kind=int_32), parameter            ::nopfix =  1         ! first output entries in pmsa independent of number of parameters
      integer(kind=int_32), parameter            ::nivar  =  1         ! number of variable inputs per nvbxx
      integer(kind=int_32), parameter            ::novar  =  1         ! number of variable outputs per nvbxx
      integer(kind=int_32) ::npnt                ! number of pointers
      integer(kind=int_32) ::ivbxx               ! loop counter nvbxx
      integer(kind=int_32), allocatable          ::ipnt(:)             ! local work array for the pointering
      integer(kind=int_32) ::ibotseg             ! bottom segment for macrophyte

      logical                      :: alt_delwaqg         ! Use the classical layered sediment approach (.false.) or the
                                                          ! new one (process DelwaqG, .true.)
      integer(kind=int_32) ::ilay                ! Layer index
      integer(kind=int_32) ::isx                 ! Index into sedconc
      integer(kind=int_32), dimension(16), save  ::isidx =             ! List of indices into the sedconc array, mirrors the fluxes
     &     [is_POC1, is_POC2, is_POC3, is_PON1, is_PON2, is_PON3,
     &      is_POP1, is_POP2, is_POP3, is_POS1, is_POS2, is_POS3,
     &      is_POC4, is_PON4, is_POP4, is_POS4]           ! Note: using POC4 instead of POC5 - omission in DelwaqG?

      alt_delwaqg = allocated(sedconc)
      delt        = pmsa(ipoint(8))

      nvbxx = nint(pmsa(ipoint(9)))
      npnt  = nipfix + nivar*nvbxx + nopfix + novar*nvbxx
      allocate(ipnt(npnt))
      ipnt  = ipoint(1:npnt)
      iflux = 0
      do iseg = 1 , noseg

         depth       = pmsa(ipnt(1))
         totaldepth  = pmsa(ipnt(2))
         localdepth  = pmsa(ipnt(3))
         ibotseg     = NINT(pmsa(ipnt(4)))
         swmacdis    = pmsa(ipnt(5))
         hmax        = pmsa(ipnt(6))
         ffac        = pmsa(ipnt(7))

         call evaluate_waq_attribute(1,iknmrk(iseg),ikmrk1)
         call evaluate_waq_attribute(2,iknmrk(iseg),ikmrk2)
         if (ikmrk1.lt.3) then ! also when dry!

            ! active water segment

            if ( hmax .gt. 0.0 ) then

               ! distribution over the water segments

               hmax = min(hmax,totaldepth)
               zm = totaldepth - hmax
               z1 = localdepth - depth
               z2 = localdepth

               ! switch = 1:  constant biomass distribution

               if (swmacdis .eq. 1 ) then
                  ffac = 1
               endif

               a = (2. - (2. * ffac)) / (totaldepth - zm) / hmax
               b = (ffac * (zm + totaldepth) - 2. * zm) / (totaldepth - zm) / hmax

               if (zm .gt. z2) then
                  ! macrophyte is not in segment:
                  frbmlay = 0
               elseif (zm .lt. z1 ) then
                  ! macropyhte is partialy in segment:
                  frbmlay = (a/2)  * (z2*z2 - z1*z1) + b * (z2 - z1)
               else
                  ! macropyhte is completely in segment:
                  frbmlay = (a/2)  * (z2*z2 - zm*zm) + b * (z2 - zm)
               endif

            else

               ! distribution over the bottom, no values for water segment

               frbmlay = 0.0

            endif

         endif

         if (ikmrk1.eq.3 .or. ( ikmrk2 == 0 .or. ikmrk2 == 3 ) ) then

            ! delwaq-g segment or alternative layered sediment approach

            if ( ikmrk1.eq.3 .and. hmax .ge. 0.0 ) then

               ! distribution over the water column, no values for bottom segment

               frbmlay = 0.0

            else

               ! distribution over the bottom segments (or the segments adjacent to the bottom)

               hmax = abs(hmax)
               hmax = min(hmax,totaldepth)
               zm = totaldepth - hmax
               z1 = totaldepth - localdepth
               z2 = z1 + depth

               ! switch = 1:  constant biomass distribution

               if (swmacdis .eq. 1 ) then
                  ffac = 1
               endif

               a = (2. - (2. * ffac)) / (totaldepth - zm) / hmax
               b = (ffac * (zm + totaldepth) - 2. * zm) / (totaldepth - zm) / hmax

               if (zm .gt. z2) then
                  ! macrophyte is not in segment:
                  frbmlay = 0
               elseif (zm .lt. z1 ) then
                  ! macropyhte is partialy in segment:
                  frbmlay = (a/2)  * (z2*z2 - z1*z1) + b * (z2 - z1)
               else
                  ! macropyhte is completely in segment:
                  frbmlay = (a/2)  * (z2*z2 - zm*zm) + b * (z2 - zm)
               endif

            endif

         endif

         pmsa(ipnt(nipfix+nivar*nvbxx+1)) = frbmlay

         if ( .not. alt_delwaqg ) then
            !
            ! Use the classic approach - all sediment layers are separate segments and
            ! there is no difference between POC etc. in the water phase or the sediment
            !
            do ivbxx = 1, nvbxx
! alway calculate the fluxes, even in dry cells...
               vb      = pmsa(ipoint(nipfix+ivbxx)+(ibotseg-1)*increm(nipfix+ivbxx))
               bmlayvb = frbmlay * vb
               pmsa(ipnt(nipfix+nivar*nvbxx+1+ivbxx)) = bmlayvb
               if (depth.gt.0.0) then
                  fl(ivbxx+iflux) =  bmlayvb/depth
               else
                  fl(ivbxx+iflux) =  0.0
               end if
            enddo
         else
            !
            ! The alternative approach - sediment layers are represented in a different data structure
            ! Less flexible, so we need to make sure the process definition is in sync with this code!
            !
            do ivbxx = 1, nvbxx
               vb      = pmsa(ipoint(nipfix+ivbxx)+(ibotseg-1)*increm(nipfix+ivbxx))
               bmlayvb = frbmlay * vb
               pmsa(ipnt(nipfix+nivar*nvbxx+1+ivbxx)) = bmlayvb
               fl(ivbxx+iflux) =  0.0
            enddo
            do ivbxx = 1,size(isidx)
               isx     = isidx(ivbxx)
               vb      = pmsa(ipoint(nipfix+ivbxx)+(ibotseg-1)*increm(nipfix+ivbxx))
               bmlayvb = frbmlay * vb

               do ilay = 1,nolay
                  !sedconc(ilay,isx,iseg) = sedconc(ilay,isx,iseg) + bmlayvb * dl(ilay) / bd(nolay) * delt
                  sedconc(ilay,isx,iseg) = sedconc(ilay,isx,iseg) + vb * delt

                  if ( iseg == 4 .and. ilay == 1 ) then
                      write(88,*) isx, sedconc(ilay,isx,iseg), vb
                  endif
               enddo
            enddo
         endif

         ipnt  = ipnt  + increm(1:npnt)
         iflux = iflux + noflux

      enddo

      deallocate(ipnt)

      return
      end

      end module m_veg3dx
