      subroutine smrt_normOut(eventdata)
!!    ~ ~ ~ Authors ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    Andre Dozier, PhD student
!!    Colorado State University 2012-2016
!!    Comment initials "aqd"
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine updates a flag in SWAT to not save 
!!    groundwater discharge variables (gw_q and gw_qdeep)
!!    as this is now taken care of in smrt_updateOutput.f
!!      
        use modevent
        use parm, only: normOut !SWAT
        implicit none 
        class (ieventdata), pointer :: eventdata
        
        !Because SWAT-MODFLOW will update groundwater's output summary variables itself, turn off the normal summary in sumv.f
        normOut = .false.

      end subroutine 
