      subroutine smrt_read_grid2dhru
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ Purpose ~ ~ ~
!!    This subroutine reads in the file containing the information to convert 
!!    MODFLOW-based grid variables to SMRT-based disaggregated HRU (dhru) variables
!!
!!    ~ ~ ~ Variables Used ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dhru         |none          |the total number of disaggregated HRUs in the 
!!                 |              |entire watershed (not just the current subbasin)
!!    NCOL         |none          |the current number of columns of MODFLOW grids
!!    NROW         |none          |the current number of rowss of MODFLOW grids
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Variables Modified ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    g2d_r        |none          |Array per SMRT dhru of the row IDs of the grids
!!                 |              |which contribute to this dhru
!!    g2d_c        |none          |Array per SMRT dhru of the column IDs of the grids
!!                 |              |which contribute to this dhru
!!    g2d_area     |none          |Array per SMRT dhru of the percent area of
!!                 |              |the grids which contribute to this dhru
!!    g2d_size     |none          |the maximum number of grids which contribute
!!                 |              |to a single dhru, used for looping and
!!                 |              |dimensioning variables
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Local Definitions ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i            |none         |counter index for the number of SMRT dhru
!!    j            |none         |counter index for reading in contributing
!!                 |             |areas and looping through all grids
!!    dhruID       |none         |index of current SMRT dhru
!!    ngrid_current|none         |index of the number of contributing areas
!!                 |             |to loop through
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!     Import variables
      use GLOBAL, only: NCOL, NROW, NLAY !MODFLOW
      use smrt_parm, only: dhru, g2d_size, g2d_r, g2d_c, g2d_area !smrt linkage
      implicit none
      
!     Initialize local variables
      integer i, j, dhruID, ngrid_current
      real, dimension (:,:), allocatable :: gridlist
      
!     Read in the ID and percent area of each MODFLOW grid contributing to each SMRT dhru
      open (6004,file="swatmf_grid2dhru.txt")
      print *, 'Reading Grid to DHRU mapping...'

!     The first line of this file is the total number of disaggregated hrus in the watershed (aka, how many will be read in)
      read(6004,*) dhru, g2d_size
      
!     Initialize variables
      allocate(g2d_r(dhru, g2d_size))
      allocate(g2d_c(dhru, g2d_size))
      allocate(g2d_area(dhru, g2d_size))
      g2d_r = 0.
      g2d_c = 0.
      g2d_area = 0.
      
      do i=1, dhru
        read(6004,*) dhruID, ngrid_current ! the dhru's global ID within the watershed, the number of grids contributing to this dhru
        
        if(ngrid_current.gt.0)then
          read(6004,*) (g2d_r(dhruID,j),j=1,ngrid_current) ! list of grid row ID numbers which contribute to this dhru
          read(6004,*) (g2d_c(dhruID,j),j=1,ngrid_current) ! list of grid column ID numbers which contribute to this dhru
          read(6004,*) (g2d_area(dhruID,j),j=1,ngrid_current) ! list of % areas of that grid contributing to this dhru
        else
          read(6004,*)
          read(6004,*)
          read(6004,*)
        endif
        
      enddo
      close(6004)
      
      return
      end