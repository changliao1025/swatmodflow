      subroutine smrt_rt3d_run(eventdata)
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
!!    This subroutine calls conversions from SWAT to UZF-RT3D, then runs UZF-RT3D
!!    for one day, then the groundwater results are converted back from UZF-RT3D
!!    to SWAT in smrt_conversion2swat
!!      
        use modevent
        implicit none
        class (ieventdata), pointer :: eventdata
      
          !Convert SWAT variables units into UZF-RT3D variables and units
          call smrt_conversion2rt3d
          
          !Prep. UZF-RT3D
          call rtmf_prepare !rtb
          
          !Call UZF-RT3D
          call rt_run !rtb
          
          !There is no call to conversion back to SWAT here because this is handled
          !in smrt_converstion2swat which is called after this from smrt_mf_run       !tcw

      end subroutine smrt_rt3d_run
