      subroutine smrt_conversion2mf
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine converts the necessary SWAT variables (per hru) into SMRT variables
!!    (per disaggregated hru, dhru) then converts these into the proper units for MODFLOW
!!
!!    ~ ~ ~ Variables Used~ ~ ~
!!    name            |units                 |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    LENUNI          |unit_in               |the MODFLOW variable for which length units are being used
!!    ITMUNI          |unit_out              |the MODFLOW variable for which time units are being used
!!    sepbtm(:)       |mm H2O                |percolation from bottom of soil profile for
!!                    |                      |the day in HRU
!!    etremain(:)     |mm H2O                |remaining et to be passed from SWAT to
!!                    |                      |MODFLOW-UZF, etremain(j) = pet_day-etday
!!    etremain_dis(:) |mm H2O           (in) |remaining et to be passed from SWAT to
!!                    |LENUNI**3/ITMUNI (out)|MODFLOW-UZF per disaggregated hru (dhru)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Variables Modified ~ ~ ~
!!    name            |units                 |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sepbtm_dhru(:)  |mm H2O           (in) |percolation from bottom of the soil profile
!!                    |                      |populated from sepbtm and smrt_hru2dhru conversion
!!                    |LENUNI**3/ITMUNI (out)|for the day in HRU, now in MODFLOW units
!!    etremain_dis(:) |mm H2O           (in) |remaining et to be passed from SWAT to
!!                    |LENUNI**3/ITMUNI (out)|MODFLOW-UZF per disaggregated hru (dhru)
!!                    |                      |populated from etremain and smrt_hru2dhru conversion
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Local Definitions ~ ~ ~
!!    name          |units                 |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mf_lengthUnit |MODFLOW length unit   |the modified inteteger to represent
!!                  |                      |the MODFLOW unit of length for units.f
!!    mf_timeUnit   |MODFLOW time unit     |the modified integer to represent
!!                  |                      |the MODFLOW unit of time for units.f
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
!     Import variables
      use parm, only:etremain,leapyr,sepbtm,nhru !SWAT
      use GLOBAL, only:LENUNI,ITMUNI !MODFLOW
      use smrt_parm !smrt linkage
      implicit none
      
!     Define local variables
      integer mf_lengthUnit,mf_timeUnit,i
      mf_lengthUnit = LENUNI + 10
      mf_timeUnit = ITMUNI
      
      !write out deep percolation values to file
      if(out_SWAT_recharge.eq.1) then
        write(30001,*) 'Deep perc. (mm) for each HRU for current day'
        do i=1,nhru
          write(30001,*) sepbtm(i)
        enddo
        write(30001,*)
      endif

!     Convert SWAT hru based variables into SMRT dis-aggregated hrus for more accurate surface/groundwater interaction
      call smrt_hru2dhru(sepbtm, sepbtm_dhru)
      call smrt_hru2dhru(etremain, etremain_dhru)
      
!     Convert SMRT variables into MODFLOW units
      call units(sepbtm_dhru, 14, mf_lengthUnit, 1, dhru, leapyr)! to convert units (mm/day to LENUNI/day)
      call units(sepbtm_dhru, mf_timeUnit, 4, 1, dhru, leapyr)! to convert time units (LENUNI/days to LENUNI/ITMUNI)
      call units(etremain_dhru, 14, mf_lengthUnit, 1, dhru, leapyr)! to convert length units (mm/day to LENUNI/day)
      call units(etremain_dhru, mf_timeUnit, 4, 1, dhru, leapyr)! to convert time units (LENUNI/days to LENUNI/ITMUNI)

      return
      end