      subroutine smrt_mf_read(eventdata)
!!    ~ ~ ~ Authors ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    Andre Dozier, PhD student
!!    Colorado State University 2012-2016
!!    Comment initials "aqd"
!!
!!    ~ ~ ~ Purpose ~ ~ ~
!!    This subroutine converts the provided array to different units based on 
!!    provided flags for the incoming and outgoing units (based on a modified 
!!    set of MODFLOW's unit flags listed below)
!!
!!    ~ ~ ~ Local Variables ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    leapyr      |none          |leap year flag for unit conversions
!!                |              |involving years
!!                |              |0  leap year
!!                |              |1  regular year
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
!       Initialize local variables
        use modevent
        use parm, only: leapyr !SWAT
        use GLOBAL, only: readMFinput, MF_leapyr !MODFLOW
        implicit none
        class (ieventdata), pointer :: eventdata

        !mf_read has already been called by SMRT linkage, so do not call it again
        readMFinput = .false.
        
        !make sure modflow's unit conversion for the time-step are based on years or leap years correctly
        MF_leapyr = leapyr

      end
