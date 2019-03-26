      program main
!!    ~ ~ ~ Authors ~ ~ ~
!!    Tyler Wible, MS student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    Andre Dozier, PhD student
!!    Colorado State University 2012-2016
!!    Comment initials "aqd"
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine links sets up the SWAT-MODFLOW-(UZF-RT3D) (SMRT) linking subroutines
!!    and "events" and then calls SWAT-MODFLOW through "events"
!!
        use mf_rt_link, only: rt_active !MODFLOW-(UZF-RT3D) Linkage
        use smrt_parm, only: mf_active !SMRT linkage
        implicit none
        
        ! Read the swat-modflow input files
        call smrt_read_link !tcw
        if (mf_active.eq.1) then
          ! Subscribe to events within modflow and swat for MODFLOW and SWAT
          call pkg_modflow
          call pkg_swat
        end if 
        
        if (rt_active.eq.1) then
          ! Subscribe to events within modflow and swat for RT3D
          call pkg_rt3d
        end if
 
        ! Run SWAT's main subroutine
        call swat_main

        ! Close swat-modflow (close files, deallocate variables, etc.)s
        call smrt_close
        call mf_close
        call rt_close
      end program main
