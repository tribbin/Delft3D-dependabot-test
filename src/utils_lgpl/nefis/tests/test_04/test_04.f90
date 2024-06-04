!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!
program test4
   integer * 4 fds
   integer ::&
   &Clsdat,&
   &Clsdef,&
   &Credat,&
   &Defcel,&
   &Defelm,&
   &Defgrp,&
   &Getnfv,&
   &Opndat,&
   &Opndef,&
   &Putelt,&
   &Neferr
   integer Getelt
   integer error,&
   &idum,&
   &i,&
   &imax,&
   &start,&
   &UINDEX(3, 1)
   real buffer,&
   &cpu1,&
   &cpu2
   character coding * 1
   character * 1024 errstr
   character * 255 version
!
   cpu1 = 0.0
   cpu2 = 0.0
   idum = 0
   coding = 'N'
   imax = 1000
   start = 1
!
   call clock(cpu1)
   error = getnfv(version)
   write (*, *)
   write (*, *) trim(version(5:))
   write (*, *)

   error = Opndef(fds, 'nefis_ex.def', coding)
   if (error /= 0) goto 9999
!
   error = Defelm(fds, 'ELEM_R_4', 'REAL', 4,&
   &'GROOTHEID 1', 'eenheid 1', 'Beschrijving 1',&
   &0, idum)
   if (error /= 0) goto 9999

   error = Defelm(fds, 'ELEM_STR', 'CHARACTE', 20,&
   &'GROOTHEID 2', 'eenheid 2', 'Beschrijving 2',&
   &0, idum)
   if (error /= 0) goto 9999
!
   error = Defcel(fds, 'CEL_TEST_1', 1, 'ELEM_R_4')
   if (error /= 0) goto 9999

   error = Defcel(fds, 'CEL_TEST_2', 1, 'ELEM_STR')
   if (error /= 0) goto 9999
!
   error = Defgrp(fds, 'GRP_TEST_1', 'CEL_TEST_1', 1, imax, 1)
   if (error /= 0) goto 9999
!
   error = Defgrp(fds, 'GRP_TEST_2', 'CEL_TEST_2', 1, imax, 1)
   if (error /= 0) goto 9999
!==========================================================
   error = Defgrp(fds, 'GRP_TEMP', 'CEL_TEST_1', 1, 1, 1)
   if (error /= 0) goto 9999
!==========================================================
!
   error = Opndat(fds, 'nefis_ex.dat', coding)
   if (error /= 0) goto 9999
!
   error = Credat(fds, 'DATAGRP_TEST_1A', 'GRP_TEST_1')
   if (error /= 0) goto 9999
!
   error = Credat(fds, 'DATAGRP_TEST_1B', 'GRP_TEST_1')
   if (error /= 0) goto 9999
   call clock(cpu2)
   write (*, '(''Initialisation NEFIS files [sec]'',1PE13.5)')&
   &cpu2 - cpu1
!
   write (*, *)
   write (*, '(''Schrijf elementen'')')
   write (*, *)
!
   call clock(cpu1)
   UINDEX(3, 1) = 1
   do 10 i = 1, imax
      UINDEX(1, 1) = i
      UINDEX(2, 1) = i
      error = Putelt(fds, 'DATAGRP_TEST_1A', '*',&
      &UINDEX, 1, real(i))
      if (error /= 0) goto 9999
10    continue
      call clock(cpu2)
      write (*, '(''DATAGRP_TEST_1A written in [sec]'',1PE13.5)')&
      &cpu2 - cpu1

      call clock(cpu1)
      do 20 i = imax, 1, -1
         UINDEX(1, 1) = i
         UINDEX(2, 1) = i
         error = Putelt(fds, 'DATAGRP_TEST_1B', '*',&
         &UINDEX, 1, -1.*real(i))
         if (error /= 0) goto 9999
20       continue
         call clock(cpu2)
         write (*, '(''DATAGRP_TEST_1B written in [sec]'',1PE13.5)')&
         &cpu2 - cpu1

         call clock(cpu1)
         do 21 i = imax, 1, -1
            UINDEX(1, 1) = i
            UINDEX(2, 1) = i
            error = Putelt(fds, 'DATAGRP_TEST_1C', '*',&
            &UINDEX, 1, 'ABCDEFGHIJKLMNOPQRST')
            if (error /= 0) goto 9999
21          continue

            call clock(cpu2)
            write (*, '(''DATAGRP_TEST_1C written in [sec]'',1PE13.5)')&
            &cpu2 - cpu1

!
!=====================================================================
            write (*, *)
            write (*, '(''Lees elementen'')')
            write (*, *)
!
            call clock(cpu1)
            do 30 i = imax, 1, -1
               UINDEX(1, 1) = i
               UINDEX(2, 1) = i
               error = Getelt(fds, 'DATAGRP_TEST_1A', '*',&
               &UINDEX, 1, 4, buffer)
               if (error /= 0) goto 9999
               if (nint(buffer) /= i) print *, 'error, i= ', i, buffer&
               &, nint(buffer)
30             continue
               call clock(cpu2)
               write (*, '(''DATAGRP_TEST_1A read    in [sec]'',1PE13.5)')&
               &cpu2 - cpu1
!
               call clock(cpu1)
               do 40 i = 1, imax
                  UINDEX(1, 1) = i
                  UINDEX(2, 1) = i
                  error = Getelt(fds, 'DATAGRP_TEST_1B', '*',&
                  &UINDEX, 1, 4, buffer)
                  if (error /= 0) goto 9999
                  if (nint(buffer) /= -1 * i) print *, 'error, i= ', i, buffer&
                  &, nint(buffer)
40                continue
                  call clock(cpu2)
                  write (*, '(''DATAGRP_TEST_1B read    in [sec]'',1PE13.5)')&
                  &cpu2 - cpu1
                  write (*, *)
!
                  error = Clsdat(fds)
                  error = Clsdef(fds)
!
9999              continue
!
                  error = Neferr(0, errstr)
                  write (*, '(a)') trim(errstr)
!
               end
!====================================================================
!     Convert clock time to seconds
!
               subroutine clock(cpu)

                  integer ihr, imin, isec, i100th
                  real cpu

                  ihr = 0
                  imin = 0
                  isec = 0
                  i100th = 0
                  cpu = 0.
!      CALL Gettim(ihr, imin, isec, i100th)
!      cpu = ihr*3600.0 + imin*60.0 + isec + i100th/100.0
!      call system_clock(ihr,imin)
!      cpu = ihr/real(imin)

               end
