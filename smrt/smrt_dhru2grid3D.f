       subroutine smrt_dhru2grid3D (smrtVar, mfVar3, mult_TF)
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ Purpose ~ ~ ~
!!    This subroutine converts SMRT-based disaggregated HRUs (dhrus) to 3D MODFLOW-based grids.
!!    Additionally, this subroutine multiplies a MODFLOW 3D variable by the grid area of each
!!    MODFLOW grid if mult_TF is 1, divides a MODFLOW 3D variable by the grid area of each
!!    MODFLOW grid if mult_TF is 2, or does nothing additional if mult_TF is 0.
!!
!!    ~ ~ ~ Variables Used ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    d2g_id      |none          |Array per MODFLOW grid of the IDs of the
!!                |              |dhrus which contribute to this grid
!!    d2g_area    |none          |Array per MODFLOW grid of the percent area of
!!                |              |the dhrus which contribute to this grid
!!    d2g_size    |none          |the maximum number of dhrus which contribute
!!                |              |to a single grid, used for looping and
!!                |              |dimensioning variables
!!    smrtVar     |unknown       |smrt variable (list per dhru)
!!    mfVar3      |unknown       |3D MODFLOW variable (NCOL, NROW, NLAY) to be
!!                |              |populated with the contents of the SMRT variable
!!    mult_TF     |n/a           |integer, if "1" will multiply array
!!                |              |by the grid area, if "2" will divide
!!                |              |array by the grid area, if "0" will not
!!                |              |convert the variable based on MODFLOW area
!!    DELR        |LENUNI        |MODFLOW variable, the thickness of the row per column
!!    DELC        |LENUNI        |MODFLOW variable, the thickness of the column per row
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Variables Modified ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mfVar3      |unknown       |3D MODFLOW variable (NCOL, NROW) now populated
!!                |              |with the contents of the SMRT variable
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Local Definitions ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    newVar3     |unknown       |3D MODFLOW variable (NCOL,NROW,NLAY) that is
!!                |              |temporarily populated with the contents of 
!!                |              |the SM variable before checking for remaining
!!                |              |grid area not contributed to by a dhru
!!    areaUsed    |%             |3D MODFLOW variable (NCOL,NROW,NLAY) that is
!!                |              |used to track how much of the grid area has
!!                |              |been contributed to by the dhrus so that any
!!                |              |remaining grid area not contributed to by a dhru
!!                |              |uses the original MODFLOW value     
!!    h           |n/a           |MODFLOW layer loop index
!!    i           |n/a           |MODFLOW row loop index
!!    j           |n/a           |MODFLOW column loop index
!!    ctr         |n/a           |MODFLOW grid id index
!!    k           |n/a           |d2g_size loop index
!!    cellUsed    |n/a           |a true/false for whether the MODFLOW grid
!!                |              |has interacted with a SMRT dhru and should
!!                |              |therefore be converted for area/unit reasons
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!     Import variables
      use GLOBAL, only: NCOL,NROW,NLAY,DELR,DELC !MODFLOW
      use smrt_parm, only: dhru,d2g_size,d2g_id,d2g_area !smrt linkage
      implicit none
      
!     Define local variables
      real smrtVar(dhru)
      real mfVar3(NCOL,NROW,NLAY)
      real newVar3(NCOL,NROW,NLAY)
      real areaRemain(NCOL,NROW,NLAY)
      integer mult_TF, h, i, j, k, ctr, dhruID
      logical cellUsed
      
!     Initialize local variables
      newVar3 = 0.
      areaRemain = 1.
      ctr = 1
      cellUsed = .false.
      
!     Convert SMRT dhru variables into MODFLOW-grid variables by multiplying each
!     contributing dhru variable by its percent area contributing to each grid
      do h=1, NLAY
        do j=1, NROW
          do i=1, NCOL
            do k=1, d2g_size
              dhruID = d2g_id(ctr,k)
              if(dhruID.ne.0)then
                !Convert the dhru information into grid information
                newVar3(i,j,h) = newVar3(i,j,h)+smrtVar(dhruID)*
     &                             d2g_area(ctr,k)
                !Track how much of the area of the grid has been contributed to (%)
                areaRemain(i,j,h) = areaRemain(i,j,h) - d2g_area(ctr,k)
                cellUsed = .true.
              endif
            enddo
          
            !If the grid cell was contributed to by dhrus, and
            !the units need to convert the area, do so here
            if(cellUsed .and. mult_TF.eq.1)then
              !Multiply variable by cell area
              newVar3(i,j,h) = newVar3(i,j,h)*DELR(i)*DELC(j)
            else if(cellUsed .and. mult_TF.eq.2)then
              !Divide variable by cell area
              newVar3(i,j,h) = newVar3(i,j,h)/(DELR(i)*DELC(j))
            endif
            
            !Store the converted dhru results into the MODFLOW variable
            if(areaRemain(i,j,h) > 0)then
              !If the dhrus do not completely cover the grid cell, retain a portion of the original grid data
              mfVar3(i,j,h) = newVar3(i,j,h) + mfVar3(i,j,h)*
     &                          areaRemain(i,j,h)
            else
              mfVar3(i,j,h) = newVar3(i,j,h)
            endif
            
            !reset the counters for the loop
            cellUsed = .false.
            ctr = ctr + 1
          enddo
        enddo
        ctr = 1
      enddo
        

      return
      end