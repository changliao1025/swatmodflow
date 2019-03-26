      subroutine smrt_dhru2rtgrid4D (smrtVar, rt4dVar, mult_TF, dim)
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ Purpose ~ ~ ~
!!    This subroutine converts SMRT-based disaggregated hrus (dhru) to 4D UZF-RT3D-based variables
!!    where the index of the 4th dimension being modified(number of chemical species) is defined by "dim"
!!    This subroutine converts SMRT-based disaggregated HRUs (dhrus) to 2D MODFLOW-based grids.
!!    Additionally, this subroutine multiplies a UZF-RT3D 4D-variable by the grid area of each
!!    corresponding MODFLOW grid if mult_TF is 1, divides a UZF-RT3D 4D-variable by the grid area 
!!    of each corresponding MODFLOW grid if mult_TF is 2, or does nothing additional if mult_TF is 0.
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
!!    rt4dVar     |unknown       |4D UZF-RT3D variable (NCOL,NROW,NLAY,ncomp) to be
!!                |              |populated with the contents of the smrtVar
!!    dim         |none          |the location in the 4th dimension of the UZF-RT3D grid
!!                |              |in which to store the converted data from smrtVar
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
!!    rt4dVar     |unknown       |4D UZF-RT3D variable (NCOL,NROW,NLAY,ncomp) to be
!!                |              |populated with the contents of the SMRT variable
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Local Definitions ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
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
      use rt_global, only: ncomp !UZF-RT3D
      use smrt_parm, only: dhru,d2g_size,d2g_id,d2g_area !smrt linkage
      implicit none
      
!     Define local variables
      real smrtVar(dhru)
      real rt4dVar(NCOL,NROW,NLAY,ncomp)
      integer mult_TF, h, i, j, k, l, ctr, dim, dhruID
      logical cellUsed
      
!     Initialize local variables
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
              
                if(cellUsed)then
                  !if this is the second or more time referencing this cell, add to its contents
                  rt4dVar(i,j,h,dim)=rt4dVar(i,j,h,dim)+smrtVar(dhruID)*
     &                          d2g_area(ctr,k)
                else
                  !if this is the first time referencing this cell, overwrite its contents
                  rt4dVar(i,j,h,dim) = smrtVar(dhruID)*d2g_area(ctr,k)
                  cellUsed = .true.
                endif
              
              endif
            enddo
          
            !If the grid cell was contributed to by dhrus, and
            !the units need to convert the area, do so here
            if(cellUsed .and. mult_TF.eq.1)then
              !Multiply variable by cell area
              rt4dVar(i,j,h,dim) = rt4dVar(i,j,h,dim)*DELR(i)*DELC(j)
            else if(cellUsed .and. mult_TF.eq.2)then
              !Divide variable by cell area
              rt4dVar(i,j,h,dim) = rt4dVar(i,j,h,dim)/(DELR(i)*DELC(j))
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