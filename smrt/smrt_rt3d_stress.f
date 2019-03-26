      subroutine smrt_rt3d_stress(eventdata)
!!    ~ ~ ~ Authors ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!    
!!    Ryan Bailey, Post-Doc student (2012-2013), Assistant Professor (2013-)
!!    Colorado State University 2012-
!!    Comment initials "rtb"
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine calls UZF-RT3D's stress period reader
!!      
        use modevent
        use mf_rt_link, only: rt_kstp !MODFLOW-(UZF-RT3D) linkage
        implicit none
        class (ieventdata), pointer :: eventdata
          
        rt_kstp = 0
        call rt_stress

      end subroutine smrt_rt3d_stress
