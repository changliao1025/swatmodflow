      subroutine pkg_rt3d
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    Andre Dozier, PhD student
!!    Colorado State University 2012-2016
!!    Comment initials "aqd"
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Adds the SMRT package to UZF-RT3D
!!
        use modevent !event class
        use parm, only: OnInit !SWAT event
        use global, only: OnRT3Dinit,OnRT3D !MODFLOW event
        implicit none

        integer :: pkg_i
        
        interface
            subroutine smrt_init_rt3d(eventdata)
                import :: ieventdata
                class (ieventdata), pointer :: eventdata
            end subroutine
        end interface

        interface
            subroutine smrt_rt3d_stress(eventdata)
                import :: ieventdata
                class(ieventdata), pointer :: eventdata
            end subroutine
        end interface

        interface
            subroutine smrt_rt3d_run(eventdata)
                import :: ieventdata
                class(ieventdata), pointer :: eventdata
            end subroutine
        end interface
        
        ! Initialize UZF-RT3D as called from SWAT
        pkg_i = OnInit%subscribe(smrt_init_rt3d)
        
        ! Initialize UZF-RT3D's stress period
        pkg_i = OnRT3Dinit%subscribe(smrt_rt3d_stress)
        
        ! Call UZF-RT3D within MODFLOW
        pkg_i = OnRT3D%subscribe(smrt_rt3d_run)

      end subroutine pkg_rt3d
