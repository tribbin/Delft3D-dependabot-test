!!  Copyright(C) Stichting Deltares, 2012-2023.
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
      module m_mpbllm
      use m_waq_type_definitions


      implicit none

      contains


      SUBROUTINE MPBLLM ( PMSA   , FL     , IPOINT , INCREM , NOSEG  ,
     +                    NOFLUX , IEXPNT , IKNMRK , NOQ1   , NOQ2   ,
     +                    NOQ3   , NOQ4   )
C***********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     +----------------------------------------+
C***********************************************************************
C
C     Function     : Calculation of the light limitation function
C
C     Project      : Implementatie pilot GEM (T2087)
C     Formulations : NIOO-CEMO Yerseke
C     Programmer   : M. Bokhorst
C     Date         : 09-04-97           Version : 1.0
C
C     History :
C
C     Date    Programmer      Description
C     ------  --------------  ------------------------------------------
C     090497  M. Bokhorst     First version
C     040399  A. Blauw        Formulation completed (see TRM)
C     050399  J. vGils        Optional S1 mode implemented
C                             Explicit declaration
C     110399  J. vGils        Error in ICLIM computation corrected
c                             (note: GEM documentation is not correct!!)
C     110399  J. vGils        C-limitation removed from loop over Z
C     110399  J. vGils        Error in time integration removed
C     111103  Jan van Beek    2003 implementation
C***********************************************************************

      use m_evaluate_waq_attribute
      use m_write_error_message

      IMPLICIT NONE

C     arguments

      REAL(kind=sp) ::PMSA(*)            ! in/out input-output array space to be adressed with IPOINT/INCREM
      REAL(kind=sp) ::FL(*)              ! in/out flux array
      INTEGER(kind=int_32) ::IPOINT(*)          ! in     start index input-output parameters in the PMSA array (segment or exchange number 1)
      INTEGER(kind=int_32) ::INCREM(*)          ! in     increment for each segment-exchange for the input-output parameters in the PMSA array
      INTEGER(kind=int_32) ::NOSEG              ! in     number of segments
      INTEGER(kind=int_32) ::NOFLUX             ! in     total number of fluxes (increment in FL array)
      INTEGER(kind=int_32) ::IEXPNT(4,*)        ! in     exchange pointer table
      INTEGER(kind=int_32) ::IKNMRK(*)          ! in     segment features array
      INTEGER(kind=int_32) ::NOQ1               ! in     number of exchanges in first direction
      INTEGER(kind=int_32) ::NOQ2               ! in     number of exchanges in second direction
      INTEGER(kind=int_32) ::NOQ3               ! in     number of exchanges in third direction
      INTEGER(kind=int_32) ::NOQ4               ! in     number of exchanges in fourth direction

C     from PMSA array

      REAL(kind=sp) ::RADSURF            !  1 in  , irradiation at the water surface            (W/m2)
      REAL(kind=sp) ::RADTOP             !  2 in  , irradiation at the segment upper-boundary   (W/m2)
      REAL(kind=sp) ::EXTVL              !  3 in  , VL extinction coefficient                    (1/m)
      REAL(kind=sp) ::A_ENH              !  4 in  , enhancement factor in radiation calculation    (-)
      REAL(kind=sp) ::FPAR               !  5 in  , fraction Photosynthetic Active Radiance        (-)
      REAL(kind=sp) ::PM                 !  6 in  , MPB maximum photosynthesis           (gC/(gChl)/d)
      REAL(kind=sp) ::RADSAT             !  7 in  , MPB saturation radiation                    (W/m2)
      INTEGER(kind=int_32) ::SWEMERSION         !  8 in  , switch indicating submersion(0) or emersion(1) (-)
      REAL(kind=sp) ::MIGRDEPTH1         !  9 in  , MPB migration depth 1                          (m)
      REAL(kind=sp) ::MIGRDEPTH2         ! 10 in  , MPB migration depth 2                          (m)
      REAL(kind=sp) ::DEPTH              ! 11 in  , depth of segment                               (m)
      REAL(kind=sp) ::LOCSEDDEPT         ! 12 in  , Sediment layer depth to bottom of segment      (m)
      INTEGER(kind=int_32) ::I_NRDZ             ! 13 in  , Nr. of integration intervals over depth        (-)
      INTEGER(kind=int_32) ::ITIME              ! 14 in  , DELWAQ time                                  (scu)
      INTEGER(kind=int_32) ::IDT                ! 15 in  , DELWAQ timestep                              (scu)
      INTEGER(kind=int_32) ::ITSTRT             ! 16 in  , DELWAQ start time                            (scu)
      INTEGER(kind=int_32) ::AUXSYS             ! 17 in  , ratio between days and system clock        (scu/d)
      LOGICAL            :: S1_BOTTOM          ! 18 in  , switch for S1 bottom approach (.true.) or DELWAQ-G approach (.false.)
      REAL(kind=sp) ::RADBOT             ! 19 in  , irradiation at the segment lower-boundary   (W/m2)
      REAL(kind=sp) ::EXTVLS1            ! 20 in  , VL extinction coefficient in the sediment    (1/m)
      REAL(kind=sp) ::ZSED               ! 21 in  , Depth of microfytobenthos layer                (m)
      REAL(kind=sp) ::WS1                ! 22 i/o , Workspace array 1                              (-)
      REAL(kind=sp) ::WS2                ! 23 i/o , Workspace array 2                              (-)
      REAL(kind=sp) ::WS3                ! 24 i/o , Workspace array 3                              (-)
      REAL(kind=sp) ::WS4                ! 25 i/o , Workspace array 4                              (-)
      REAL(kind=sp) ::FLT                ! 26 out , MPB light limitation                           (-)
      REAL(kind=sp) ::FLTS1              ! 27 out , MPB light limitation in sediment layer 1       (-)

C     local

      REAL(kind=sp), PARAMETER     ::PI     = 3.1415927 ! pi
      INTEGER(kind=int_32) ::ISEG               ! loop counter segment loop
      INTEGER(kind=int_32) ::IZ                 ! loop counter integration layers
      INTEGER(kind=int_32) ::IKMRK1             ! first feature inactive(0)-active(1)-bottom(2) segment
      INTEGER(kind=int_32) ::IKMRK2             ! second feature 2D(0)-surface(1)-middle(2)-bottom(3) segment
      INTEGER(kind=int_32), parameter  ::NO_POINTER = 30    ! number of input output variables in PMSA array
      INTEGER(kind=int_32) ::IP(NO_POINTER)     ! index pointer in PMSA array updated for each segment
      REAL(kind=sp) ::ACTDEP             ! actual depth
      REAL(kind=sp) ::ACTLIM             ! limitation at actual radiance
      REAL(kind=sp) ::ACTRAD             ! radiance at actual depth
      REAL(kind=sp) ::CUMLIM             ! cummulative limitation
      REAL(kind=sp) ::DZ                 ! depth of integration layers
      REAL(kind=sp) ::FRACSURF           ! fraction of migrating MPB to reach surface
      REAL(kind=sp) ::LIMSURF            ! limitation with RADSURF
      REAL(kind=sp) ::RELZ               ! relative Z in migration dpeth
      REAL(kind=sp) ::Z                  ! Z in total sediment layer

      INTEGER(kind=int_32) ::ISTEP
      REAL(kind=sp) ::RTIME
      REAL(kind=sp) ::RDT
      REAL(kind=sp) ::RTSTRT

C     initialise pointers for PMSA and FL array

      IP = IPOINT(1:NO_POINTER)

C     loop over the segments

      DO 1000 ISEG = 1 , NOSEG

         CALL evaluate_waq_attribute(1,IKNMRK(ISEG),IKMRK1)
         CALL evaluate_waq_attribute(2,IKNMRK(ISEG),IKMRK2)

         RADSURF    = PMSA(IP(1))
         RADTOP     = PMSA(IP(2))
         EXTVL      = PMSA(IP(3))
         A_ENH      = PMSA(IP(4))
         FPAR       = PMSA(IP(5))
         RADSAT     = PMSA(IP(6))
         SWEMERSION = NINT(PMSA(IP(7)))
         MIGRDEPTH1 = PMSA(IP(8))
         MIGRDEPTH2 = PMSA(IP(9))
         DEPTH      = PMSA(IP(10))
         LOCSEDDEPT = PMSA(IP(11))
         I_NRDZ     = NINT(PMSA(IP(12)))
         RTIME      = PMSA(IP(13))
         RDT        = PMSA(IP(14))
         RTSTRT     = PMSA(IP(15))
         AUXSYS     = NINT(PMSA(IP(16)))
         S1_BOTTOM  = NINT(PMSA(IP(17))) .EQ. 1
         RADBOT     = PMSA(IP(18))
         EXTVLS1    = PMSA(IP(19))
         ZSED       = PMSA(IP(20))
         WS1        = PMSA(IP(21))
         WS2        = PMSA(IP(22))
         WS3        = PMSA(IP(23))
         WS4        = PMSA(IP(24))

         ISTEP      = NINT((RTIME-RTSTRT)/RDT)
         IDT        = NINT(RDT)
         ITSTRT     = NINT(RTSTRT)
         ITIME      = ITSTRT + ISTEP*IDT

C        check proces parameters

         IF (I_NRDZ.LE.0) CALL write_error_message_with_values('I_NRDZ'   ,FLOAT(I_NRDZ),ISEG,'MPBLLM')

C        scale all radiance to PAR, radsurf with enhancement since it is used as top of sediment layer radiation

         RADSURF    = RADSURF*FPAR*A_ENH
         RADTOP     = RADTOP*FPAR
         RADBOT     = RADBOT*FPAR

C        Active water segments and bottom segments

C         IF ( IKMRK1.EQ.1 .OR. IKMRK1.EQ.2 ) THEN

C           for top layer thicker then euphotic depth all production in euphotic zone, so intergate only over ZSED

            IF ( IKMRK1 .EQ. 2 .AND. ABS(DEPTH - LOCSEDDEPT) .LT. 1.E-20 .AND. DEPTH .GT. ZSED ) THEN
               DZ   = ZSED  / I_NRDZ
            ELSE
               DZ   = DEPTH / I_NRDZ
            ENDIF

            CUMLIM  = 0.0

C           Bereken totale lichthoeveelheid en lichtlimitatie per laagje

            LIMSURF = 1.0 - EXP(- RADSURF / RADSAT)
            DO IZ = 1 , I_NRDZ
               IF (IZ .EQ. 1) THEN
                  ACTDEP = 0.5 * DZ
               ELSE
                  ACTDEP = ACTDEP + DZ
               ENDIF

C              bereken de fractie algen die naar het sediment oppervlak zijn gemigreerd

               IF ( IKMRK1 .EQ. 2 .AND. SWEMERSION .EQ. 1 ) THEN
                  IF ( MIGRDEPTH2 .LE. 1E-20 ) THEN
                     FRACSURF = 0.0
                  ELSE
                     Z        = LOCSEDDEPT - DEPTH + ACTDEP
                     RELZ     = MIN(1.0,(MAX(0.0,(Z-MIGRDEPTH1)/(MIGRDEPTH2-MIGRDEPTH1))))
                     FRACSURF = 0.5*COS(PI*RELZ)+0.5
                  ENDIF
               ELSE
                  FRACSURF = 0.0
               ENDIF

               ACTRAD = RADTOP * EXP ( -EXTVL * ACTDEP )
               ACTLIM = 1.0 - EXP(- ACTRAD / RADSAT)

               CUMLIM = CUMLIM + FRACSURF*LIMSURF + (1.0-FRACSURF)*ACTLIM

            ENDDO

C           gemiddelde lichtlimitatie

            CUMLIM = CUMLIM / I_NRDZ

C           Integratie over de dag

            IF   ( MOD(ITIME-ITSTRT,AUXSYS) .LT. IDT )   THEN
               IF ( ITIME .EQ. ITSTRT ) THEN
                  WS2       = CUMLIM
               ELSE
                  WS2       = WS1 / AUXSYS
               ENDIF
               WS1          = 0.0
               PMSA(IP(23)) = WS2
            ENDIF

            FLT          = WS2
            WS1          = WS1 + CUMLIM * IDT

            PMSA(IP(21)) = WS1
            PMSA(IP(25)) = FLT

C         ENDIF

C        S1_BOTTOM

         IF ( S1_BOTTOM .AND. ( IKMRK2 .EQ. 0 .OR. IKMRK2 .EQ. 3 ) ) THEN

            DZ   = ZSED / I_NRDZ
            CUMLIM  = 0.0

C           Bereken totale lichthoeveelheid en lichtlimitatie per laagje

            LIMSURF = 1.0 - EXP(- RADBOT / RADSAT)
            DO IZ = 1 , I_NRDZ
               IF (IZ .EQ. 1) THEN
                  ACTDEP = 0.5 * DZ
               ELSE
                  ACTDEP = ACTDEP + DZ
               ENDIF

C              bereken de fractie algen die naar het sediment oppervlak zijn gemigreerd

               IF ( SWEMERSION .EQ. 1 ) THEN
                  IF ( MIGRDEPTH2 .LE. 1E-20 ) THEN
                     FRACSURF = 0.0
                  ELSE
                     RELZ     = MIN(1.0,(MAX(0.0,(ACTDEP-MIGRDEPTH1)/(MIGRDEPTH2-MIGRDEPTH1))))
                     FRACSURF = 0.5*COS(PI*RELZ)+0.5
                  ENDIF
               ELSE
                  FRACSURF = 0.0
               ENDIF

               ACTRAD = RADBOT * EXP ( -EXTVLS1 * ACTDEP )
               ACTLIM = 1.0 - EXP(- ACTRAD / RADSAT)

               CUMLIM = CUMLIM + FRACSURF*LIMSURF + (1.0-FRACSURF)*ACTLIM
            ENDDO

C           gemiddelde lichtlimitatie

            CUMLIM = CUMLIM / I_NRDZ

C           Integratie over de dag

            IF   ( MOD(ITIME-ITSTRT,AUXSYS) .LT. IDT )   THEN
               IF ( ITIME .EQ. ITSTRT ) THEN
                  WS4       = CUMLIM
               ELSE
                  WS4       = WS3 / AUXSYS
               ENDIF
               WS3          = 0.0
               PMSA(IP(24)) = WS4
            ENDIF

            FLTS1        = WS4
            WS3          = WS3 + CUMLIM * IDT

            PMSA(IP(23)) = WS3
            PMSA(IP(26)) = FLTS1

         ENDIF

C        update pointering in PMSA array

         IP    = IP    + INCREM(1:NO_POINTER)

 1000 CONTINUE

      RETURN
      END

      end module m_mpbllm
