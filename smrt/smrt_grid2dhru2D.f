      subroutine smrt_grid2dhru2D (mfVar2, smrtVar)
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ Purpose ~ ~ ~
!!    This subroutine converts 2D MODFLOW-based grids to SMRT-based disaggregated HRUs (dhrus)
!!
!!    ~ ~ ~ Variables Used ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    g2d_r       |none          |Array per SMRT dhru of the row IDs of the grids
!!                |              |which contribute to this dhru
!!    g2d_c       |none          |Array per SMRT dhru of the column IDs of the grids
!!                |              |which contribute to this dhru
!!    g2d_area    |none          |Array per SMRT dhru of the percent area of
!!                |              |the grids which contribute to this dhru
!!    g2d_size    |none          |the maximum number of grids which contribute
!!                |              |to a single dhru, used for looping and
!!                |              |dimensioning variables
!!    mfVar2      |unknown       |2D MODFLOW variable (NCOL, NROW)
!!    smrtVar     |unknown       |SMRT variable (list of dhrus) to be populated
!!                |              |with the contents of the MODFLOW variable
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Variables Modified ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    smrtVar     |unknown       |SMRT variable (list of dhrus) now populated
!!                |              |with the contents of the MODFLOW variable
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Local Definitions ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i           |none          |SMRT dhru loop index
!!    j           |none          |g2d_size loop index
!!    row         |none          |row index for MODFLOW
!!    col         |none          |column index for MODFLOW
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!     Import variables
      use GLOBAL, only: NCOL,NROW !MODFLOW
      use smrt_parm, only: dhru, g2d_size, g2d_r, g2d_c, g2d_area !smrt linkage
      implicit none
      
!     Define local variables
      real mfVar2(NCOL,NROW)
      real smrtVar(dhru)
      integer i, j, row, col
      
!     Initialize variables
      smrtVar = 0.
      
!     Convert MODFLOW-grid variables into SMRT-dhru variables by multiplying each
!     contributing grid variable by its percent area contributing to each dhru
      do i=1, dhru
        do j=1, g2d_size
          row = g2d_r(i,j)
          col = g2d_c(i,j)
          if(col.ne.0 .and. row.ne.0)then
            smrtVar(i) = smrtVar(i) + mfVar2(col,row)*g2d_area(i,j)
          endif
        enddo
      enddo
      
      return
      end