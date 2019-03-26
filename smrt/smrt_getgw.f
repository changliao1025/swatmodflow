      module smrt_getgw
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
!!    This subroutine sets up events for the groundwater portion of the
!!    SWAT-MODFLOW linkage
!!
        use parm
        use modevent
        implicit none
      contains 

        subroutine smrt_getgwcontr(eventdata)
          class (ieventdata), pointer :: eventdata
          computegw = .false.
        end subroutine smrt_getgwcontr
        
        subroutine smrt_getrechloss(eventdata)
          class (ieventdata), pointer :: eventdata
          initializeRCHloss = .false.
        end subroutine smrt_getrechloss
        

      end module smrt_getgw


