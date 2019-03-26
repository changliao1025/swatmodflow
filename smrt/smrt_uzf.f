      subroutine smrt_uzf(eventdata)
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
!!    This subroutine converts the necessary SWAT variables for the MODFLOW
!!    unsaturated zone flow (UZF1) package's variables
!!      
        use modevent
        ! Inside of MODFLOW, infiltration and ET is 
        ! provided by SWAT
        use smrt_parm, only: sepbtm_dhru, etremain_dhru
        use GWFUZFMODULE, only: FINF,PETRATE
        implicit none 
        class (ieventdata), pointer :: eventdata

        call smrt_dhru2grid2D(sepbtm_dhru,FINF,1)
        call smrt_dhru2grid2D(etremain_dhru,PETRATE,1)

      end subroutine 
