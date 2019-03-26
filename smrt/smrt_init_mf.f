      subroutine smrt_init_mf(eventdata)
!!    ~ ~ ~ Authors ~ ~ ~
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
!!    This subroutine initializes MODFLOW linking subroutins of
!!    SWAT-MODFLOW-(UZF-RT3D) (SMRT)
!!
        use modevent
        implicit none
        class (ieventdata), pointer :: eventdata

!     Set up MODFLOW data and allocate arrays
        print *, 'MODFLOW is being used' !rtb
        call mf_read !rtb
        call smrt_read_grid2dhru !tcw
        call smrt_read_dhru2grid !tcw
        call smrt_read_dhru2hru !tcw
        call smrt_read_river2grid !tcw
      end subroutine smrt_init_mf
