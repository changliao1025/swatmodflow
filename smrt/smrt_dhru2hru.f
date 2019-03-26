      subroutine smrt_dhru2hru (smrtVar, swatVar, mult_TF)
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ Purpose ~ ~ ~
!!    This subroutine converts SMRT-based disaggregated HRUs (dhru) to SWAT HRUs.
!!    Additionally, this subroutine multiplies a SWAT variable by the area, in
!!    km**2, of each SWAT hru if mult_TF is true 1, divides a SWAT variable by
!!    the area, in km**2, of each SWAT hru if mult_TF is false 2, or does nothing
!!    additional if mult_TF is 0.
!!
!!    ~ ~ ~ Variables Used ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    d2h_id      |none          |Array per SWAT hru of the IDs of the dhrus
!!                |              |which contribute to this hru
!!    d2h_area    |none          |Array per SWAT hru of the percent area of
!!                |              |the dhrus which contribute to this hru
!!    d2h_size    |none          |the maximum number of dhrus which contribute
!!                |              |to a single hru, used for looping and
!!                |              |dimensioning variables
!!    smrtVar     |unknown       |SMRT variable (list of dhrus)
!!    swatVar     |unknown       |SWAT variable (list of nhru) to be populated
!!                |              |with the contents of the SMRT variable
!!    hru_km(:)   |km**2         |area of HRU in square kilometers
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Variables Modified ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    swatVar     |unknown       |SWAT variable (list of nhru) to be populated
!!                |              |with the contents of the SMRT variable
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Local Definitions ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i           |none          |SWAT HRU loop index
!!    j           |none          |SMRT dhru loop index
!!    cellUsed    |n/a           |a true/false for whether the SWAT hru
!!                |              |has interacted with a SMRT dhru and should
!!                |              |therefore be converted for area/unit reasons
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!     Import variables
      use parm, only: nhru, hru_km !SWAT
      use smrt_parm, only: dhru, d2h_size, d2h_id, d2h_area !smrt linkage
      implicit none
      
!     Define local variables
      real smrtVar(dhru)
      real swatVar(nhru)
      integer mult_TF, i, j, dhruID
      logical cellUsed
      
!     Initialize local variables
      cellUsed = .false.
      
!     Convert SMRT-dhru variables into SWAT-HRU variables by multiplying each
!     contributing dhru variable by its percent area contributing to each HRU
      do i=1, nhru
        do j=1, d2h_size
          dhruID = d2h_id(i,j)
          if(dhruID.ne.0)then
            
            if(cellUsed)then
              !if this is the second or more time referencing this cell, add to its contents
              swatVar(i) = swatVar(i) + smrtVar(dhruID)*d2h_area(i,j)
            else
              !if this is the first time referencing this cell, overwrite its contents
              swatVar(i) = smrtVar(dhruID)*d2h_area(i,j)
              cellUsed = .true.
            endif
            
          endif
        enddo
        
        !If the hru was contributed to by dhrus, and !the units need to convert the area, do so here
        if(cellUsed .and. mult_TF.eq.1)then
          !Multiply variable by cell area, in km
          swatVar(i)=swatVar(i)*hru_km(i)
        else if(cellUsed .and. mult_TF.eq.2)then
          !Divide variable by cell area, in km
          swatVar(i)=swatVar(i)/(hru_km(i))
        endif
        
        !reset the counters for the loop
        cellUsed = .false.
      enddo
      
      return
      end