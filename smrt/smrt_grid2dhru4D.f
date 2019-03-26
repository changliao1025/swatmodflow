      subroutine smrt_grid2dhru4D (rt4dVar, smrtVar, location, dim)
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ Purpose ~ ~ ~
!!    This subroutine converts 4D UZF-RT3D-based grids where the index of the 4th dimension being 
!!    modified (number of chemical species) is defined by "dim" to SMRT-based disaggregated hrus (dhru)
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
!!    dim         |none          |the location in the 4th dimension of the UZF-RT3D grid
!!                |              |in which to retrieve the data, see below
!!    rt4dVar     |unknown       |4D UZF-RT3D variable (NCOL,NROW,location,dim)
!!    smrtVar     |unknown       |SMRT variable (list of dhru) to be populated
!!                |              |with the contents of the MODFLOW variable
!!    location    |none          |an array(NCOL, NROW) with the value equal to which
!!                |              |layer(NLAY) the MODFLOW value is to be taken from
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
!!    lay         |none          |layer index for MODFLOW
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!     Import variables
      use GLOBAL, only: NCOL,NROW,NLAY !MODFLOW
      use rt_global, only: ncomp !UZF-RT3D
      use smrt_parm, only: dhru, g2d_size, g2d_r, g2d_c, g2d_area !smrt linkage
      implicit none
      
!     Define local variables
!     real sum
      double precision rt4dVar(NCOL,NROW,NLAY,ncomp)
      real smrtVar(dhru)
      real location(NCOL,NROW)
      integer i, j, row, col, lay, dim
      
!     Initialize variables
!     sum = 0
      smrtVar = 0.
      
!     Convert MODFLOW-grid variables into SMRT-dhru variables by multiplying each
!     contributing grid variable by its percent area contributing to each dhru
!     For 3D variable arrays taking specified layer value only
      do i=1, dhru
        do j=1, g2d_size
          row = g2d_r(i,j)
          col = g2d_c(i,j)
          if(col.ne.0 .and. row.ne.0)then
            lay = location(col,row)
            smrtVar(i) = smrtVar(i) +
     &          rt4dVar(col,row,lay,dim)*g2d_area(i,j)
          endif
        enddo
      enddo
!     For 3D variable arrays taking average of all layers, relic code below and not accurate
!     do l=1, dhru
!       do j=1, NROW
!         do i=1, NCOL
!           do k=1, NLAY
!             sum = sum + array(i,j,k)
!           enddo
!           sum = sum/NLAY
!           smrtVar(l) = smrtVar(l) + sum*grid2dhru(l,ctr)
!           ctr = ctr + 1
!           sum = 0
!         enddo
!       enddo
!       ctr = 1
!     enddo
      
      return
      end