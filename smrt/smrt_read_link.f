      subroutine smrt_read_link
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ Purpose ~ ~ ~
!!    This subroutine reads in the file containing the information to linke
!!    SWAT and MODFLOW
!!
!!    ~ ~ ~ Variables Modified ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mf_active    |none          |index whether or not to use MODFLOW to
!!                 |              |calculate groundwater flow processes
!!                 |              |instead of SWAT's gwmod
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!     Import variables
      use GLOBAL, only: mf_interval,mf_ran !MODFLOW
      use mf_rt_link, only: rt_active !MODFLOW-(UZF-RT3D) Linkage
      use smrt_parm !SMRT linkage
      implicit none
      
!     Read in extra information for linking SWAT and MODFLOW
      open(6001,file="swatmf_link.txt")
      !read(6001, '(I20)') mf_active
      read(6001,*) mf_active
      mf_shallow_dynamics = 0
      read(6001,*) rt_active
      read(6001,*) mf_interval ! the number of days that MODFLOW simulates when called by SWAT
      mf_ran = .false.
      read(6001,*) mf_obs_flag

      !Optional output files (for SWAT-MODFLOW information)
        
      !SWAT Deep Percolation (by SWAT HRU)
      read(6001,*)
      read(6001,*) out_SWAT_recharge 
      if(out_SWAT_recharge.eq.1) then
        open(30001,file='swatmf_out_SWAT_recharge') !rtb
        write(30001,*) 'SWAT deep percolation (mm) (for each HRU)'
      endif

      !MODFLOW Recharge (by MODFLOW cell)
      read(6001,*) out_MF_recharge 
      if(out_MF_recharge.eq.1) then
        open(30002,file='swatmf_out_MF_recharge') !rtb
        write(30002,*) 'MODFLOW Recharge (L3/T) (for each cell)'
        write(30002,*) '--Calculated from SWAT HRU deep percolation--'
      endif

      !SWAT Channel Depth (by SWAT subbasin)
      read(6001,*) out_SWAT_channel 
      if(out_SWAT_channel.eq.1) then
        open(30003,file='swatmf_out_SWAT_channel') !rtb
        write(30003,*) 'SWAT channel depth (m) (for each subbasin)'
      endif

      !MODFLOW River Stage (by MODFLOW River Cell)
      read(6001,*) out_MF_channel
      if(out_MF_channel.eq.1) then
        open(30004,file='swatmf_out_MF_riverstage') !rtb
        write(30004,*) 'MODFLOW River Stage (L) (for each River Cell)'
        write(30004,*) '--Calculated from SWAT Channel Depth--'
      endif

      !GW/SW exchange (by MODFLOW River cell)
      read(6001,*) out_MODFLOW_gwsw 
      if(out_MODFLOW_gwsw.eq.1) then
        open(30005,file='swatmf_out_MF_gwsw') !rtb
        write(30005,*) 'Groundwater/Surface Water exchange (L3/T)'
        write(30005,*) 'for each MODFLOW River Cell'
      write(30005,*) 'Positive: River water seeps to the aquifer'
      write(30005,*) 'Negative: Groundwater flows from aquifer to river'
      endif

      !GW/SW exchange (by SWAT subbasin)
      read(6001,*) out_SWAT_gwsw 
      if(out_SWAT_gwsw.eq.1) then
        open(30006,file='swatmf_out_SWAT_gwsw') !rtb
        write(30006,*) 'Groundwater/Surface Water exchange (m3/day)'
        write(30006,*) 'for each SWAT Subbasin'
        write(30006,*) '--Calculated from MODFLOW River Package--'
      write(30006,*) 'Positive: Volume entering stream from the aquifer'
      write(30006,*) 'Negative: Volume seeps from stream to the aquifer'
      endif

      close(6001)
      
      return
      end
