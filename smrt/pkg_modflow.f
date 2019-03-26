      subroutine pkg_modflow
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
!!    Adds the SMRT package to MODFLOW
!!
        use modevent !event class
        use global, only: OnRivPkg,OnRechPkg,OnUZFPkg,MFrunTop !MODFLOW events
        implicit none

        integer :: pkg_i
        
        interface
            subroutine smrt_mfRiver(eventdata)
                import :: ieventdata
                class(ieventdata), pointer :: eventdata
            end subroutine
        end interface

        interface
            subroutine smrt_recharge(eventdata)
                import :: ieventdata
                class(ieventdata), pointer :: eventdata
            end subroutine
        end interface

        interface
            subroutine smrt_uzf(eventdata)
                import :: ieventdata
                class(ieventdata), pointer :: eventdata
            end subroutine
        end interface

        interface
            subroutine smrt_mf_read(eventdata)
                import :: ieventdata
                class(ieventdata), pointer :: eventdata
            end subroutine
        end interface

        ! River stage is provided by SWAT 
        pkg_i = OnRivPkg%subscribe(smrt_mfRiver)

        ! Recharge is provided by SWAT 
        pkg_i = OnRechPkg%subscribe(smrt_recharge)

        ! Infiltration and ET is provided by SWAT 
        pkg_i = OnUZFPkg%subscribe(smrt_uzf)
        
        ! If SMRT is used, de-activate mf_read and SLMT7PNT insided of mf_run because it has already been called by the linkage
        pkg_i = MFrunTop%subscribe(smrt_mf_read)

      end subroutine pkg_modflow
