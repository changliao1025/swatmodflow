      subroutine smrt_init_rt3d(eventdata)
!!    ~ ~ ~ Authors ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine initializes UZF-RT3D linking subroutins of
!!    SWAT-MODFLOW-(UZF-RT3D) (SMRT)
!!
        use modevent
        implicit none
        class (ieventdata), pointer :: eventdata

!     Set up UZF-RT3D data and allocate arrays
        print *, 'UZF-RT3D is being used'
        call rt_read !tcw
        
      end subroutine smrt_init_rt3d
