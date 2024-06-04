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
program DEMO2
!
!     Company name                    : Deltares
!                                       P.O.Box 177
!                                       2600 MH Delft
!                                       The Netherlands
!--------------------------------------------------------------------------------
!     System: NEFIS
!
!     $Header: /delft3d/libraries/nefis/demo/demo_02/demo_02.f 2     10/03/06 9:56 Mooiman $
!--------------------------------------------------------------------------------
!     Programmer                      : A. Hoekstra
!     Project                         : NEutral FIle Structure
!--------------------------------------------------------------------------------
!      * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
!
!     - This demo-program demonstrates the use of NEFIS store-
!       and retrieval functions. Special is the use of a
!       datagroup with a variable dimension.
!
!       This program performs the following tasks:
!       - create an element, cel and a 3-d group defintion
!       - create a data group
!       - store data in this group
!       - retrieve data from this group, using a
!         different view
!       - retrieve data using a filter
!
!     Note: the error-return code from the NEFIS-functions is
!           not checked
!--------------------------------------------------------------------------------
!     ..
!     .. Scalars
   character * 1024 ERRSTR
!                 .. character string to catch the NEFIS error message
   character CODING * 1
!                 .. indicates Y/N neutral representation of data
   integer ERROR
!                 .. contains return-value of NEFIS-functions
!     ..
!     .. Arrays
   integer FDS
!                 .. nefis file descriptor
!     ..
!     .. Declarations of NEFIS-functions
   integer CLSDAT&
   &, CLSDEF&
   &, CREDAT&
   &, FLSDAT&
   &, OPNDAT&
   &, OPNDEF&
   &, NEFERR
!
   external CLSDAT&
   &, CLSDEF&
   &, CREDAT&
   &, FLSDAT&
   &, OPNDAT&
   &, OPNDEF&
   &, NEFERR
!     ..
!     .. Executable statements
!
!                 ..
!                 .. Open a definition file
   CODING = 'N'
   ERROR = OPNDEF(FDS, 'data_d02.def', CODING)
   if (ERROR /= 0) goto 9999
!                 ..
!                 .. Define element, cel, and group-definition
   call DEFINE(FDS)
!                 ..
!                 .. Open a data file
   CODING = 'N'
   ERROR = OPNDAT(FDS, 'data_d02.dat', CODING)
   if (ERROR /= 0) goto 9999
!                 ..
!                 .. Create space for data
   ERROR = CREDAT(FDS, 'GrpNaam', 'Groep')
   if (ERROR /= 0) goto 9999
!
   ERROR = FLSDAT(FDS)
   if (ERROR /= 0) goto 9999
!                 ..
!                 .. Write data to file
   call PUTDAT(FDS)
!                 ..
!                 .. Retrieve data, using a different view
   call DTVIEW(FDS)
!                 ..
!                 .. Retrieve a part of the data
   call FILTER(FDS)
!                 ..
!                 .. Close the files
9999 continue

   if (error == 0) ERROR = CLSDEF(FDS)
   if (error == 0) ERROR = CLSDAT(FDS)

   ERROR = NEFERR(1, ERRSTR)
!
end
!================================================================================
subroutine DEFINE(FDS)
!
   integer FDS
!
   integer ERROR
   character * 134 ERRSTR

   integer GRPDMS(5)&
   &, GRPORD(5)
!
   integer DEFCEL&
   &, DEFELM&
   &, DEFGRP&
   &, FLSDEF&
   &, NEFERR
   external DEFCEL&
   &, DEFELM&
   &, DEFGRP&
   &, FLSDEF&
   &, NEFERR
!     ..
!     .. Executable statements
!
!                 ..
!                 .. Define a simple element, type Real*4
   ERROR = DEFELM(FDS, 'ElmName', 'Integer', 4,&
   &'ElmQuantity', 'ElmUnity', 'ElmDescription',&
   &1, 1)
   if (ERROR /= 0) goto 9999
!                 ..
!                 .. Define a cel with only one real value
   ERROR = DEFCEL(FDS, 'Cell', 1, 'ElmName')
   if (ERROR /= 0) goto 9999
!                 ..
!                 .. Define a 3-d group of dimension (3,5,0),
!                 .. so a group with a variable dimension
   GRPDMS(1) = 3
   GRPDMS(2) = 5
   GRPDMS(3) = 0
   GRPORD(1) = 1
   GRPORD(2) = 3
   GRPORD(3) = 2
   ERROR = DEFGRP(FDS, 'Groep', 'Cell', 3, GRPDMS, GRPORD)
   if (ERROR /= 0) goto 9999
!                 ..
!                 .. Flush buffers to file
   ERROR = FLSDEF(FDS)
   if (ERROR /= 0) goto 9999
!
9999 continue
   ERROR = NEFERR(1, ERRSTR)
end
!================================================================================
subroutine PUTDAT(FDS)
!
   character * 1024 ERRSTR
!
   integer FDS
!
   integer START, stop, INCR
   parameter(START=1, stop=2, INCR=3)
   equivalence(AARRAY, ARRAY)
!
   character SPACE * 7
   integer COL&
   &, ERROR&
   &, PLANE&
   &, ROW
   integer UINDEX(3, 5)&
   &, USRORD(5)
   integer ARRAY(3, 5, 7)
   integer AARRAY(105)
!
   integer FLSDAT&
   &, PUTELT&
   &, NEFERR
   external FLSDAT&
   &, PUTELT&
   &, NEFERR
!     ..
!     .. Executable statements
!
   SPACE = '       '
!                 ..
!                 .. Set view to (3,5,*)
   USRORD(1) = 1
   USRORD(2) = 2
   USRORD(3) = 3
!                 ..
!                 .. Define indices for each dimension
   UINDEX(START, 1) = 1
   UINDEX(stop, 1) = 3
   UINDEX(INCR, 1) = 1
   UINDEX(START, 2) = 1
   UINDEX(stop, 2) = 5
   UINDEX(INCR, 2) = 1
   UINDEX(START, 3) = 1
   UINDEX(stop, 3) = 7
   UINDEX(INCR, 3) = 1
!                 ..
!                 .. Fill array with values
   do 30 PLANE = 1, 7
      do 20 COL = 1, 5
         do 10 ROW = 1, 3
            ARRAY(ROW, COL, PLANE) = ROW * 1000 + COL * 100 + PLANE
10          continue
20          continue
30          continue
!                 ..
!                 .. Write data to file
            ERROR = PUTELT(FDS, 'GrpNaam', '*'& !     ERROR = PUTELT (FDS, 'GrpNaam', 'ElmName'
            &, UINDEX, USRORD, ARRAY)
            if (ERROR /= 0) goto 9999
!                 ..
!                 .. Flush the buffers
            ERROR = FLSDAT(FDS)
            if (ERROR /= 0) goto 9999
!                 ..
!                 .. Output data to screen
!     write(*,'('' ARRAY(105) written to file:'')')
!     DO 11 PLANE = 1,105
!       WRITE (*,'(  I10)') AARRAY(PLANE)
!  11 CONTINUE
            write (*, '('' ARRAY(3,5,7) written to file:'')')
            do 50 PLANE = 1, 7
               do 40 COL = 1, 5
!           WRITE (*,'(  3F10.2)')
                  write (*, '(  3I10)')&
                  &(ARRAY(ROW, COL, PLANE), ROW=1, 3)
40                continue
                  write (*, *)
50                continue
!
9999              continue
                  ERROR = NEFERR(1, ERRSTR)
               end
!================================================================================
               subroutine DTVIEW(FDS)
!
                  character * 1024 ERRSTR
!
                  integer FDS
!
                  integer START, stop, INCR
                  parameter(START=1, stop=2, INCR=3)
!
                  character SPACE * 7
                  integer COL&
                  &, ERROR&
                  &, PLANE&
                  &, ROW
                  integer UINDEX(3, 5)&
                  &, USRORD(3)
                  integer ARRAY(7, 3, 5)
!
                  integer GETELT&
                  &, NEFERR
                  external GETELT&
                  &, NEFERR
!     ..
!     .. Executable statements
!
                  SPACE = '       '
!                 ..
!                 .. Change view to (*,3,5)
                  USRORD(1) = 3
                  USRORD(2) = 1
                  USRORD(3) = 2
!                 ..
!                 .. Define indices for each dimension
                  UINDEX(START, 1) = 1
                  UINDEX(stop, 1) = 7
                  UINDEX(INCR, 1) = 1
                  UINDEX(START, 2) = 1
                  UINDEX(stop, 2) = 3
                  UINDEX(INCR, 2) = 1
                  UINDEX(START, 3) = 1
                  UINDEX(stop, 3) = 5
                  UINDEX(INCR, 3) = 1
!                 ..
!                 .. Retrieve data
                  ERROR = GETELT(FDS, 'GrpNaam', '*'&
                  &, UINDEX, USRORD, 7 * 3 * 5 * 4, ARRAY)
                  if (ERROR /= 0) goto 9999
!                 ..
!                 .. Output data to screen
                  write (*, '('' Same values now retrieved in ARRAY(7,3,5)'')')
                  do 20 PLANE = 1, 5
                     do 10 COL = 1, 3
!           WRITE (*,'(  7F10.2)')
                        write (*, '(  7I10  )')&
                        &(ARRAY(ROW, COL, PLANE), ROW=1, 7)
10                      continue
                        write (*, *)
20                      continue
!
9999                    continue
                        ERROR = NEFERR(1, ERRSTR)
                     end
!================================================================================
                     subroutine FILTER(FDS)
!
                        character * 1024 ERRSTR
!
                        integer FDS
!
                        integer START, stop, INCR
                        parameter(START=1, stop=2, INCR=3)
!
                        character SPACE * 7
                        integer COL&
                        &, ERROR&
                        &, PLANE&
                        &, ROW
                        integer UINDEX(3, 5)&
                        &, USRORD(3)
                        integer ARRAY(4, 2, 3)
!
                        integer GETELT&
                        &, NEFERR
                        external GETELT&
                        &, NEFERR
!     ..
!     .. Executable statements
!
                        SPACE = '       '
!                 ..
!                 .. Change view to (*,3,5)
                        USRORD(1) = 3
                        USRORD(2) = 1
                        USRORD(3) = 2
!                 ..
!                 .. Define indices and step for each dimension
!                 .. The stepsize of 2 creates a filter
                        UINDEX(START, 1) = 1
                        UINDEX(stop, 1) = 7
                        UINDEX(INCR, 1) = 2
                        UINDEX(START, 2) = 1
                        UINDEX(stop, 2) = 3
                        UINDEX(INCR, 2) = 2
                        UINDEX(START, 3) = 1
                        UINDEX(stop, 3) = 5
                        UINDEX(INCR, 3) = 2
!                 ..
!                 .. Retrieve data
                        ERROR = GETELT(FDS, 'GrpNaam', '*'&
                        &, UINDEX, USRORD, 4 * 2 * 3 * 4, ARRAY)
                        if (ERROR /= 0) goto 9999
!                 ..
!                 .. Output data to screen
                        write (*, '('' Every other value retrieved in ARRAY(4,2,3)'')')
                        do 20 PLANE = 1, 3
                           do 10 COL = 1, 2
!           WRITE (*,'(  4F10.2)')
                              write (*, '(  4I10  )')&
                              &(ARRAY(ROW, COL, PLANE), ROW=1, 4)
10                            continue
                              write (*, *)
20                            continue
!
9999                          continue
                              ERROR = NEFERR(0, ERRSTR)
                              write (*, '(a)') trim(errstr)
                           end
!
