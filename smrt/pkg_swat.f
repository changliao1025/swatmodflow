      subroutine pkg_swat
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    Andre Dozier, PhD student
!!    Colorado State University 2012-2016
!!    Comment initials "aqd"
!!
!!    Ryan Bailey, Post-Doc student (2012-2013), Assistant Professor (2013-)
!!    Colorado State University 2012-
!!    Comment initials "rtb"
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Adds the SMRT package to SWAT replacing the groundwater modeling
!!    component with MODFLOW
!!
        use modevent !event class
        use parm, only: OnInit,normOutUpdate,OnCommand19,!SWAT events
     &                  OnGroundWater,OnRouteInitialize
        use smrt_getgw !Groundwater Related events
        implicit none

        integer :: pkg_i

        interface
            subroutine smrt_init_mf(eventdata)
                import :: ieventdata
                class (ieventdata), pointer :: eventdata
            end subroutine
        end interface

        interface
            subroutine smrt_mf_run(eventdata)
                import :: ieventdata
                class (ieventdata), pointer :: eventdata
            end subroutine
        end interface
        
        interface
            subroutine smrt_normOut(eventdata)
                import :: ieventdata
                class (ieventdata), pointer :: eventdata
            end subroutine
        end interface
        
        interface
            subroutine smrt_modifySWATgw(eventdata)
                import :: ieventdata
                class (ieventdata), pointer :: eventdata
            end subroutine
        end interface

        ! Initialize MODFLOW
        pkg_i = OnInit%subscribe(smrt_init_mf)
        
        !Check if SWAT should update it's groundwater output variables
        pkg_i = normOutUpdate%subscribe(smrt_normOut)
        
        ! Call MODFLOW
        pkg_i = OnCommand19%subscribe(smrt_mf_run)
        
        !Calculate GW contribution from MODFLOW
        pkg_i = OnSoilTempCalc%subscribe(smrt_getgwcontr)
        
        !Zero out SWAT groundwater for hrus overlapping the MODFLOW model
        pkg_i = OnGroundWater%subscribe(smrt_modifySWATgw)
        
        !Determine if initializing the swat reach loss variablle is necessary !tcw
        pkg_i = OnRouteInitialize%subscribe(smrt_getrechloss)

      end subroutine pkg_swat
