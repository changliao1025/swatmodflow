      subroutine smrt_read_dhru2grid
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ Purpose ~ ~ ~
!!    This subroutine reads in the file containing the information to convert 
!!    SMRT-based disaggregated hrus (dhru) variables to MODFLOW-based grid variables
!!
!!    ~ ~ ~ Variables Used ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    NCOL         |none          |the current number of columns of MODFLOW grids
!!    NROW         |none          |the current number of rowss of MODFLOW grids
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Variables Modified ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    d2g_id       |none          |Array per MODFLOW grid of the IDs of the dhrus
!!                 |              |which contribute to this grid
!!    d2g_area     |none          |Array per MODFLOW grid of the percent area of
!!                 |              |the dhrus which contribute to this grid
!!    d2g_size     |none          |the maximum number of dhrus which contribute
!!                 |              |to a single grid, used for looping and
!!                 |              |dimensioning variables
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Local Definitions ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i            |none          |counter index for the number of MOFLOW grids
!!    j            |none          |counter index for reading in contributing
!!                 |              |areas and looping through all hrus
!!    temp         |none          |index of current MODFLOW grid
!!    nhru_current |none          |index of the number of contributing areas
!!                 |              |to loop through
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!     Import variables
      use GLOBAL, only: NCOL, NROW, NLAY !MODFLOW
      use smrt_parm, only: d2g_size, d2g_id, d2g_area, cell_dhrus !smrt linkage
      implicit none
      
!     Initialize local variables
      integer i, j, gridID, nhru_current, numGrid
      
!     The first line of this file is the total number of MODFLOW grids (active and inactive) = NROW * NCOL
      open (6002,file="swatmf_dhru2grid.txt")
      print *, 'Reading DHRU to Grid mapping...'
      read(6002,*) numGrid, d2g_size
      
!     Initialize variables
      allocate(d2g_id(NCOL*NROW, d2g_size))
      allocate(d2g_area(NCOL*NROW, d2g_size))
      allocate(cell_dhrus(NCOL*NROW))
      d2g_id = 0.
      d2g_area = 0.
      cell_dhrus = 0
      
      do i=1,numGrid
        read(6002,*) gridID,nhru_current ! grid # then the number of dhrus contributing to this grid cell
        cell_dhrus(i) = nhru_current !record for later use

        if(nhru_current.gt.0)then
          read(6002,*) (d2g_id(gridID,j),j=1,nhru_current) ! list of dhru ID numbers which contribute to this grid cell
          read(6002,*) (d2g_area(gridID,j),j=1,nhru_current) ! list of % areas of that dhru contributing to this grid cell
        endif
        
      enddo
      close(6002)
      
      return
      end