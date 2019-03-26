      subroutine smrt_read_dhru2hru
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ Purpose ~ ~ ~
!!    This subroutine reads in the file containing the information to convert 
!!    SMRT-based disaggregated HRU (dhru) variables to SWAT-based normal/aggregated HRU variables
!!
!!    ~ ~ ~ Variables Used ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    nhru         |none          |the current number of SWAT hrus
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Variables Modified ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    d2h_id       |none          |Array per SWAT hru of the IDs of the dhrus
!!                 |              |which contribute to this hru
!!    d2h_area     |none          |Array per SWAT hru of the percent area of
!!                 |              |the dhrus which contribute to this hru
!!    d2h_size     |none          |the maximum number of dhrus which contribute
!!                 |              |to a single hru, used for looping and
!!                 |              |dimensioning variables
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Local Definitions ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i            |none         |counter index for reading in contributing
!!                 |             |areas and looping through all grids
!!    j            |none         |counter index for the number of SMRT dhru
!!    hruID        |none         |index of current SMRT dhru
!!    dhru_current |none         |index of the number of contributing areas
!!                 |             |to loop through
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!     Import variables
      use smrt_parm, only: d2h_size, d2h_id, d2h_area !smrt linkage
      implicit none
      
!     Initialize local variables
      integer ahru,i,j,hruID,dhru_current,subID
      real, dimension (:,:), allocatable :: dhrulist
      real blah
      
!     Read in the ID and percent area of each SMRT dhru contributing to each SWAT HRU
      open (6003,file="swatmf_dhru2hru.txt")
      print *, 'Reading DHRU to HRU mapping...'

!     The first line of this file is the total number of HRUs in the watershed (ie, how many will be read in)
      read(6003,*) ahru, d2h_size !must equal SWAT's nhru
      
!     Initialize variables
      allocate(d2h_id(ahru, d2h_size))
      allocate(d2h_area(ahru, d2h_size))
      d2h_id = 0.
      d2h_area = 0.
      
      do i=1, ahru
        ! the HRU's global ID within the watershed, the number of dhrus contributing to this HRU, 
        ! and the subbasin number for this HRU.
        read(6003,*) hruID, dhru_current, subID
        
        if(dhru_current.gt.0)then
          read(6003,*) (d2h_id(hruID,j),j=1,dhru_current) ! list of dhru ID numbers which contribute to this hru
          read(6003,*) (d2h_area(hruID,j),j=1,dhru_current) ! list of % areas of that dhru contributing to this hru
        else
          read(6003,*)
          read(6003,*)
        endif
        
      enddo
      close(6003)
      
      return
      end