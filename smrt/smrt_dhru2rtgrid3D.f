      subroutine smrt_dhru2rtgrid3D (smrtVar, rt3dVar, dim)
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ Purpose ~ ~ ~
!!    This subroutine converts SMRT-based disaggregated hrus (dhru) to 3D UZF-RT3D-based grids
!!    where the index of the 3rd dimension being modified (number of chemical species) is defined by "dim"
!!
!!    ~ ~ ~ Variables Used ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dhru2grid   |none          |Array of percent areas of SMRT-dhrus
!!                |              |contributing to each MODFLOW-grid per watershed
!!                |              |format: (MODFLOW grid ID, hru)
!!    smrtVar     |unknown       |SMRT variable (dhru)
!!    dim         |none          |the location in the 4th dimension of the UZF-RT3D grid
!!                |              |in which to store the converted data from smrtVar
!!    rt3dVar     |unkwon        |3D UZF-RT3D variable (NCOL,NROW,ncomp) to be
!!                |              |populated with the contents of the SMRT variable
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Variables Modified ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rt3dVar     |unkwon        |3D UZF-RT3D variable (NCOL,NROW,ncomp) to be
!!                |              |populated with the contents of the SMRT variable
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Local Definitions ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i           |none          |MODFLOW row loop index
!!    j           |none          |MODFLOW column loop index
!!    l           |none          |SMRT dhru loop index
!!    ctr         |none          |index for what total grid ID should be used
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!     Import variables
      use GLOBAL, only: NCOL,NROW,DELR,DELC !MODFLOW
      use rt_global, only: ncomp !UZF-RT3D
      use smrt_parm, only: dhru,d2g_size,d2g_id,d2g_area,cell_dhrus !smrt linkage
      implicit none
      
!     Define local variables
      real smrtVar(dhru)
      real rt3dVar(NCOL,NROW,ncomp)
      integer mult_TF, i, j, k, l, ctr, dim, dhruID
      logical cellUsed
      
!     Initialize variables
      ctr = 1
      cellUsed = .false.
      rt3dVar = 0.

!     Convert SMRT-dhru variables into UZF-RT3D grid variables by multiplying each
!     contributing dhru variable by its percent area contributing to each grid
!     Note: data is only placed in the 3rd dimension of the array specified by the variable "dim"
!     contributing dhru variable by its percent area contributing to each grid
      do j=1,NROW
        do i=1,NCOL
          
          !loop through the dhrus that contribute to the grid cell
          do k=1,cell_dhrus(ctr)
            dhruID = d2g_id(ctr,k)
            if(dhruID.ne.0)then
            
              if(cellUsed)then
                !if this is the second or more time referencing this cell, add to its contents
                rt3dVar(i,j,dim) = rt3dVar(i,j,dim)+smrtVar(dhruID)*
     &                             d2g_area(ctr,k)
              else
                !if this is the first time referencing this cell, overwrite its contents
                rt3dVar(i,j,dim) = smrtVar(dhruID)*d2g_area(ctr,k)
                cellUsed = .true.
              endif
            
            endif
          enddo
        
          !If the grid cell was contributed to by dhrus, and
          !the units need to convert the area, do so here
          if(cellUsed .and. mult_TF.eq.1)then
            !Multiply variable by cell area
            rt3dVar(i,j,dim) = rt3dVar(i,j,dim)*DELR(i)*DELC(j)
          else if(cellUsed .and. mult_TF.eq.2)then
            !Divide variable by cell area
            rt3dVar(i,j,dim) = rt3dVar(i,j,dim)/(DELR(i)*DELC(j))
          endif
        
          !reset the counters for the loop
          cellUsed = .false.
          ctr = ctr + 1

        enddo !go to next cell
      enddo

      return
      end