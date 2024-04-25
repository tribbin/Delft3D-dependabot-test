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
module m_dlwqtr
    use m_waq_precision
    use m_string_utils

    implicit none

    contains


    !> reads SURFACE from coupling
    !! Sets dispersion length in vertical
    subroutine dlwqtr(   notot  , nosys  , noseg  , noq    , noq1   , & 
                         noq2   , noq3   , nopa   , nosfun , nodisp , & 
                         novelo , ipoint , volume , area   , flow   , & 
                         aleng  , conc   , disp   , cons   , param  , & 
                         func   , segfun , disper , velo   , itime  , & 
                         idt    , syname , nocons , nofun  , coname , & 
                         paname , funame , sfname , updatr , ilflag )
        use m_srstop
        use m_monsys

        SAVE
        integer(kind=int_wp), intent(in) :: notot           !< Total number of substances
        integer(kind=int_wp), intent(in) :: nosys           !< number of active substances
        integer(kind=int_wp), intent(in) :: noseg           !< Nr. of computational elements
        integer(kind=int_wp), intent(in) :: noq             !< Total number of exchanges
        integer(kind=int_wp), intent(in) :: noq1            !< Nr. of exchanges direction 1
        integer(kind=int_wp), intent(in) :: noq2            !< Nr. of exchanges direction 2
        integer(kind=int_wp), intent(in) :: noq3            !< Nr. of exchanges direction 3
        integer(kind=int_wp), intent(in) :: nopa            !< Number of parameters
        integer(kind=int_wp), intent(in) :: nosfun          !< Number of segment functions
        integer(kind=int_wp), intent(in) :: nodisp          !< Number of user-dispersions
        integer(kind=int_wp), intent(in) :: novelo          !< Number of user-flows
        integer(kind=int_wp), intent(in) :: ipoint(4, noq)   !< 1= "From"   segment pointers
                                                            !< 2= "To"     segment pointers
                                                            !< 3= "From-1" segment pointers
                                                            !< 4= "To+1"   segment pointers
        real(kind=real_wp), intent(in) :: VOLUME(NOSEG) !< Segment volumes
        real(kind=real_wp), intent(in) :: AREA(NOQ) !< Exchange surfaces
        real(kind=real_wp), intent(in) :: FLOW(NOQ) !< Flows
        real(kind=real_wp), intent(inout) :: ALENG(2, NOQ) !< 1= Length to "From" surface
                                                        !< 2= Length to "To"   surface
                                                        !< 3 lengths in the grid
        real(kind=real_wp), intent(in) :: CONC(NOTOT, NOSEG) !< Model concentrations
        real(kind=real_wp), intent(inout) :: DISP(3) !< Dispersion in 3 directions
        real(kind=real_wp), intent(inout) :: CONS(*) !< Model constants
        real(kind=real_wp), intent(inout) :: PARAM(nopa, noseg) !< Model parameters
        real(kind=real_wp), intent(inout) :: FUNC(*) !< Model functions at ITIME
        real(kind=real_wp), intent(inout) :: SEGFUN(noseg, *) !< Segment functions at ITIME
        real(kind=real_wp), intent(out)   :: DISPER(*) !< User defined dispersion
        real(kind=real_wp), intent(out)   :: VELO(*) !< User defined flows
        integer(kind=int_wp), intent(in) :: ITIME !< Time in system clock units
        integer(kind=int_wp), intent(in) :: IDT   !< Time step system clock units
        character(len=20), intent(in) :: SYNAME(NOTOT) !< names of systems
        integer(kind=int_wp), intent(in) :: NOCONS !< Number of constants used
        integer(kind=int_wp), intent(in) :: NOFUN !< Number of functions ( user )
        character(len=20), intent(in) :: CONAME(*) !< Constant names
        character(len=20), intent(in) :: PANAME(*) !< Parameter names
        character(len=20), intent(in) :: FUNAME(*) !< Function names
        character(len=20), intent(in) :: SFNAME(*) !< Segment function names
        logical, intent(inout) :: UPDATR   !< Flag indicating if the transport
                                           !< matrix is changed. The user should
                                           !< set this flag to .T. if he alters
                                           !< part of the matrix and uses integratio
                                           !< option 10.xx .
        integer(kind=int_wp), intent(in) :: ILFLAG !< if 0 then 3 length values

        ! Local variables
        INTEGER(kind=int_wp) ::LCCCO, ier, ierr, ier2, lunrep, isurf, & 
                nmaxa, mmaxa, nma, idummy, nmt, k, iseg, & 
                ilay, iq, ipos, ifrom, ito, layt
        LOGICAL    FIRST ,  LINIT , LEXI
        DATA       FIRST / .TRUE. /
        DATA       LINIT / .FALSE. /
!
!          check usage w.r.t. parallel computing
!
!          AM:
!          I removed this check, as all the computations set up using
!          the Delft3D user-interface have the SURF parameter.
!          Even if not, then the file should be available on all
!          nodes, as they share the directory.
!
!          check number of parameters
!
!     Initialisation set index pointers, read surface areas
!
      IF ( FIRST ) THEN
         FIRST = .FALSE.
         IER   = 0
         CALL GETMLU(LUNREP)
         WRITE(LUNREP,*)
         WRITE(LUNREP,2000)
!
!        Set pointers in param array
!
         ISURF = index_in_array( 'SURF      ', PANAME (:NOPA))
!
!          read surface areas
!
         IF ( ISURF > 0 ) THEN
            IF ( ILFLAG == 1 .AND. NOQ3 > 0 ) THEN
               LINIT = .TRUE.
               WRITE(LUNREP,2040)
            ENDIF
            INQUIRE  ( FILE='areachar.dat', EXIST = LEXI )
            IF ( .NOT. LEXI ) THEN
!
!
!              It is assumed the SURF parameter has been set in the input
!
            ELSE
               OPEN ( NEWUNIT = LCCCO, FILE='areachar.dat', FORM  ='UNFORMATTED', & 
                                      STATUS='OLD'       , IOSTAT=IER2         )
               IF ( IER2 /= 0 ) THEN
                  WRITE (LUNREP,2010)
                  WRITE ( *    ,2010)
                  IER = IER + 1
               ELSE
                  WRITE(LUNREP,2030)
                  READ ( LCCCO ) NMAXA, MMAXA, NMA, NMA, NMA, IDUMMY
                  LAYT = NOSEG/NMA
                  NMT = NMA*LAYT
                  IF ( NMT /= NOSEG ) THEN
                     WRITE (LUNREP,2050) NMA,LAYT,NMT,NOSEG
                     WRITE (  *   ,2050) NMA,LAYT,NMT,NOSEG
                     IER = IER + 1
                  ENDIF
                  IF ( IER == 0 ) THEN
                     READ ( LCCCO ) (PARAM(ISURF,K),K=1,NMA)
                     DO ILAY = 2, LAYT
                        DO ISEG = 1, NMA
                           IPOS = (ILAY-1)*NMA + ISEG
                           PARAM(ISURF,IPOS) = PARAM(ISURF,ISEG)
                         end do
                     end do
                  ENDIF
                  CLOSE ( LCCCO )
               ENDIF
            ENDIF
!
            IF ( IER /= 0 ) THEN
               CALL SRSTOP(1)
            ENDIF
         ENDIF
!
         WRITE(LUNREP,2070)
!
      ENDIF
!
!     adapt the length for the third direction
!
      IF ( LINIT ) THEN
         DO IQ = NOQ1 + NOQ2 + 1, NOQ
              IFROM = IPOINT(1,IQ)
              ITO   = IPOINT(2,IQ)
              IF ( IFROM > 0 ) THEN
                 IF ( PARAM(ISURF,IFROM) > 1.0E-15 ) THEN
                      ALENG(1,IQ) = VOLUME(IFROM)/PARAM(ISURF,IFROM)/2.
                 ENDIF
              ENDIF
              IF ( ITO   > 0 ) THEN
                 IF ( PARAM(ISURF,IFROM) > 1.0E-15 ) THEN
                      ALENG(2,IQ) = VOLUME(ITO)/PARAM(ISURF,IFROM)/2.
                 ENDIF
              ENDIF
          end do
      ENDIF
!
!     end of the subroutine
!
      RETURN
!
!     Output formats
!
 2000 FORMAT (' Extra functionality DLWQTR')
 2010 FORMAT (' ERROR: opening file <areachar.dat> !')
 2030 FORMAT (' Surface area''s will be read from file <areachar.dat>')
 2040 FORMAT (' Dispersion length in third dir. will be calculated')
 2050 FORMAT (' ERROR: File areachar.dat does not match.', & 
             ' NMA = ',I8,' LAYT= ',I8,' NMT = ',I8,' NOSEG=',I8)
 2070 FORMAT (' End extra functionality DLWQTR')
!
      END

      end module m_dlwqtr
