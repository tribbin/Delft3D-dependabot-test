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
module m_getloc
    use m_waq_precision

    implicit none

contains


    subroutine getloc (file_name, itype, locdef, maxdef, iprdep, &
            itmdep, maxlst, loclst, loctyp, locnr, &
            nrlst, ierror, option)
        ! ODS GETLOC routine for DELWAQ HIS-files
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ---------------------------------------------------------
        !     file_name   CHAR*256   3        IN/LOC  Complete file name
        !     ITYPE   INTEGER    1        INPUT   File type
        !     LOCDEF  CHAR*20  MAXDEF     INPUT   List with wanted locations
        !     MAXDEF  INTEGER    1        INPUT   Length of LOCDEF
        !     IPRDEP  INTEGER    1        INPUT   Par code for dimensions
        !     ITMDEP  INTEGER    1        INPUT   Time code for dimensions
        !     MAXLST  INTEGER    1        INPUT   Dimension of the output arrays
        !     LOCLST  CHAR*20  MAXLST     OUTPUT  List of locations found
        !     LOCTYP  INTEGER  MAXLST     OUTPUT  List of location types
        !     LOCNR   INTEGER  MAXLST     OUTPUT  List of index nr. locations
        !     NRLST   INTEGER    1        OUTPUT  Nr of parameters found
        !     IERROR  INTEGER    1        OUTPUT  Error code
        !     OPTION  CHAR*256   1        IN/OUT  For future use

        use m_string_manipulation, only : upper_case
        use m_open_waq_files
        use m_file_path_utils, only : extract_file_extension

        character*256 file_name(3), option
        character*20  locdef(maxdef), loclst(maxlst)
        dimension     loctyp(maxlst), locnr (maxlst)
        logical       setall
        character*256 :: ext     ! file extension
        integer(kind = int_wp) :: extpos   ! position of extension
        integer(kind = int_wp) :: extlen   ! length of file extension
        logical :: mapfil  ! true if map file extension
        integer(kind = int_wp) :: lun
        integer(kind = int_wp) :: k, i1, i2, i3, notot, ierror, nodump, idummy
        integer(kind = int_wp) :: itype, itmdep, iprdep, nrlst, maxk, nbase
        integer(kind = int_wp) :: locnr, loctyp, maxdef, maxlst

        ! open the delwaq .his file
        lun = 10
        call open_waq_files (lun, file_name(1), 24, 2, ierror)
        if (ierror /= 0) return

        ! map or his
        call extract_file_extension(file_name(1), ext, extpos, extlen)
        call upper_case(ext, ext, extlen)
        if (ext == 'map') then
            mapfil = .true.
        else
            mapfil = .false.
        endif

        ! read primary system characteristics
        read (lun, err = 100) file_name(3)(1:160)
        read (lun, err = 110) notot, nodump
        read (lun, err = 120) (file_name(3)(181:200), k = 1, notot)

        ! read parameter names and try to find the wanted subset
        nrlst = 0
        setall = .false.
        if (locdef(1) == '*') setall = .true.

        do i1 = 1, nodump, maxlst
            maxk = min(nodump, i1 + maxlst - nrlst - 1) - i1 + 1
            if (.not. mapfil) then
                read (lun, err = 130) (idummy, loclst(k), k = nrlst + 1, nrlst + maxk)
            else
                do k = nrlst + 1, nrlst + maxk
                    write(loclst(k), '(''segment '',i8)') k
                enddo
            endif
            nbase = nrlst
            do i2 = 1, maxk
                do i3 = 1, maxdef
                    if (loclst(nbase + i2) == locdef(i3) .or. setall) then
                        nrlst = nrlst + 1
                        if (nrlst > maxlst) then
                            ierror = -nodump
                            goto 50
                        endif
                        loclst(nrlst) = loclst(nbase + i2)
                        locnr (nrlst) = i1 + i2 - 1
                        goto 30
                    endif
                end do
                30    continue
            end do
        end do

        ! supply the desired statistics
        50 do i1 = 1, nrlst
            loctyp(i1) = 2
        end do
        goto 200

        ! supply the desired statistics
        100 ierror = 10
        goto 200
        110 ierror = 11
        goto 200
        120 ierror = 12
        goto 200
        130 ierror = 13

        200 close (lun)
        return
    end subroutine getloc

end module m_getloc
