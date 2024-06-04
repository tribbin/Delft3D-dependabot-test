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
program Test12

! In deze test wordt gecontroleerd of meerdere(3) nefis files
! tegelijk open kunnen zijn.

   implicit none

   integer * 4 fds_a,&
   &fds_b,&
   &fds_c
   integer clsdat,&
   &clsdef,&
   &getnfv,&
   &NEFERR
   integer error
   character ERRSTR * 1024
   character * 255 version

   error = getnfv(version)
   write (*, *)
   write (*, *) trim(version(5:))
   write (*, *)

   call WriteFile('data_c12a', fds_a, 33)
   call WriteFile('data_c12b', fds_b, 39)
   call WriteFile('data_c12c', fds_c, 78)

   call ReadFile(fds_a, 33)
   call ReadFile(fds_b, 39)
   call ReadFile(fds_c, 78)

   error = Clsdat(fds_a)
   if (error /= 0) then
      error = neferr(0, errstr)
      write (*, *) trim(errstr)
   end if

   error = Clsdat(fds_b)
   if (error /= 0) then
      error = neferr(0, errstr)
      write (*, *) trim(errstr)
   end if

   error = Clsdat(fds_c)
   if (error /= 0) then
      error = neferr(0, errstr)
      write (*, *) trim(errstr)
   end if

   error = Clsdef(fds_a)
   if (error /= 0) then
      error = neferr(0, errstr)
      write (*, *) trim(errstr)
   end if

   error = Clsdef(fds_b)
   if (error /= 0) then
      error = neferr(0, errstr)
      write (*, *) trim(errstr)
   end if

   error = Clsdef(fds_c)
   if (error /= 0) then
      error = neferr(0, errstr)
      write (*, *) trim(errstr)
   end if

   if (error == 0) then
      error = neferr(0, errstr)
      write (*, *)
      write (*, '(a)') trim(errstr)
   end if

end
!
!
subroutine WriteFile(fName, fds, bias)
   implicit none
   character * (*) fName
   integer * 4 fds,&
   &bias

   integer NTIMES, BUFSIZ
   parameter(NTIMES=40, BUFSIZ=10000)

   integer Credat,&
   &Defelm,&
   &Defcel,&
   &Defgrp
   integer Opndat,&
   &Opndef,&
   &Putelt,&
   &NEFERR
   integer error,&
   &i, j,&
   &grpdms,&
   &grpord,&
   &usrord,&
   &UINDEX(3)
   real * 8 buffer(BUFSIZ)
   character names * 14, coding * 1
   character ERRSTR * 1024

   coding = 'B'
   error = Opndef(fds, fName//'.def', coding)
   if (error /= 0) then
      error = neferr(0, errstr)
      write (*, *) trim(errstr)
   end if

   error = Opndat(fds, fName//'.dat', coding)
   if (error /= 0) then
      error = neferr(0, errstr)
      write (*, *) trim(errstr)
   end if

   error = Defelm(fds, 'ELEM_R_8_DIM_1', 'REAL8', 8,&
   &'GROOTHEID 2', 'eenheid 2', 'Beschrijving 2',&
   &1, BUFSIZ)
   if (error /= 0) then
      error = neferr(0, errstr)
      write (*, *) trim(errstr)
   end if

   names = 'ELEM_R_8_DIM_1'
   error = Defcel(fds, 'CEL_TEST_3', 1, names)
   if (error /= 0) then
      error = neferr(0, errstr)
      write (*, *) trim(errstr)
   end if

   grpdms = 0
   grpord = 1
   error = Defgrp(fds, 'GRP_TEST_3D', 'CEL_TEST_3', 1,&
   &grpdms, grpord)
   if (error /= 0) then
      error = neferr(0, errstr)
      write (*, *) trim(errstr)
   end if
!---------------------------------------------------------------------
   error = Credat(fds, 'DATAGRP_TEST_3D', 'GRP_TEST_3D')
   if (error /= 0) then
      error = neferr(0, errstr)
      write (*, *) trim(errstr)
   end if
!---------------------------------------------------------------------

   usrord = 1

   UINDEX(3) = 1

   write (*,&
   &'(I5,'' schrijfopdrachten van '',I9,'' bytes'')') NTIMES, BUFSIZ * 8
   do 20 j = 1, NTIMES
      do 10 i = 1, BUFSIZ
         buffer(i) = dble(i) * dble(j) * dble(bias)
10       continue
         write (*, '(''Opdracht '', I3)') j
         UINDEX(1) = j
         UINDEX(2) = j
         error = Putelt(fds, 'DATAGRP_TEST_3D',&
         &'ELEM_R_8_DIM_1', UINDEX, usrord, buffer)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (*, *) trim(errstr)
         end if
20       continue

      end

!
!
      subroutine ReadFile(fds, bias)
         implicit none
         integer * 4 fds,&
         &bias

         integer NTIMES, BUFSIZ
         parameter(NTIMES=40, BUFSIZ=10000)

         integer error,&
         &i, j,&
         &usrord,&
         &UINDEX(3),&
         &NEFERR,&
         &Getelt
         character ERRSTR * 1024
         real * 8 buffer(BUFSIZ)

         UINDEX(3) = 1
         usrord = 1

         write (*,&
         &'(''Lees '', I5, '' keer '',I9,'' bytes'')') NTIMES, BUFSIZ * 8
         do 40 j = 1, NTIMES
            write (*, '(''Opdracht '', I3)') j
            UINDEX(1) = j
            UINDEX(2) = j
            error = Getelt(fds, 'DATAGRP_TEST_3D',&
            &'ELEM_R_8_DIM_1', UINDEX, usrord, BUFSIZ * 8,&
            &buffer)
            if (error /= 0) then
               error = neferr(0, errstr)
               write (*, *) trim(errstr)
               exit
            end if
            do 30 i = 1, BUFSIZ
               if (int(buffer(i) - dble(i) * dble(j) * dble(bias)) /= 0) then
                  write (*, *) 'error, i= ', i
                  exit
               end if
30             continue
40             continue

            end

