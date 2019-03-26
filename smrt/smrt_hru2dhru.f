      subroutine smrt_hru2dhru (swatVar, smrtVar)
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ Purpose ~ ~ ~
!!    This subroutine converts a SWAT HRU-variable to a SMRT disaggregated HRUs (dhrus) variable
!!
!!    ~ ~ ~ Variables Used ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    d2h_id      |none          |Array per SWAT hru of the IDs of the dhrus
!!                |              |which contribute to this hru
!!    swatVar     |unknown       |SWAT variable (list of nhru)
!!    smrtVar     |unknown       |SMRT variable (list of dhrus) to be populated
!!                |              |with the contents of the SWAT variable
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Variables Modified ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    smrtVar     |unknown       |SMRT variable (list of dhrus) to be populated
!!                |              |with the contents of the SWAT variable
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Local Definitions ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i           |none          |SWAT HRU loop index
!!    j           |none          |SMRT dhru loop index
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!     Import variables
      use parm, only: nhru !SWAT
      use smrt_parm, only: dhru, d2h_size, d2h_id, d2h_area !smrt linkage
      implicit none
      
!     Define local variables
      real swatVar(nhru)
      real smrtVar(dhru)
      integer i, j
      
!     Initialize variables
      smrtVar = 0.
      
!     Convert SWAT-HRU variables to SMRT-dhru variables
!     The conversion does not involve a weighted average because the SWAT variables
!     are calculated such that the variable's value is the same within each DHRU
!     The Water Table in SWAT is a depth. The depth is the same everywhere in the HRU.
!     Thus, any dhru will also have the same value. Hence, do not apply a weighted average.
      do i=1,nhru
        do j=1,d2h_size
          if(d2h_id(i,j).ne.0)then
            smrtVar(d2h_id(i,j)) = swatVar(i) !each dhru gets the hru value
          endif
        enddo
      enddo
      
      return
      end