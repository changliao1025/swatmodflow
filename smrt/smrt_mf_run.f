      subroutine smrt_mf_run(eventdata)
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
!!    This subroutine calls conversions from SWAT to MODFLOW, then runs MODFLOW
!!    for one day, then converts back the groundwater results from MODFLOW to
!!    SWAT
!!      
        use modevent
        use parm, only: etremain,perc_no3_conc,sepbtm,nhru !SWAT
        use GLOBAL, only: mf_interval,mf_ran !MODFLOW
        use mf_rt_link, only: rt_active !MODFLOW-(UZF-RT3D) Linkage
        use smrt_parm, only: mf_daycounter, sepbtm_sum, etremain_sum, !smrt Linkage
     &                 perc_no3_conc_sum
        implicit none
        
        class (ieventdata), pointer :: eventdata
        integer i
        
        !Increment day counter for calling MODFLOW
        mf_daycounter = mf_daycounter + 1

        !Sum SWAT output for time-based aggregation
        do i = 1,nhru
          sepbtm_sum(i) = sepbtm_sum(i) + sepbtm(i)
          etremain_sum(i) = etremain_sum(i) + etremain(i)
          
          ! and nitrate from soil
          if(rt_active.eq.1)then
              perc_no3_conc_sum(i) = perc_no3_conc_sum(i)
     &                              + perc_no3_conc(i)
          endif
        enddo
        
        !Only call MODFLOW (and RT3D) if the current day counter matches 'mf_interval'
        if(mf_daycounter.ge.mf_interval)then
          
          !take average of recharge, ET, nitrate
          do i=1,nhru
            sepbtm(i) = sepbtm_sum(i) / mf_interval
            etremain(i) = etremain_sum(i) / mf_interval
            if(rt_active.eq.1) then
              perc_no3_conc(i) = perc_no3_conc_sum(i) / mf_interval
            endif
          enddo
          
          !Convert SWAT units into MODFLOW units
          call smrt_conversion2mf
          
          !run modflow (water table, river discharge/seepage, pumping)
          call mf_run
          mf_ran = .true.
          
          !Reset summing variables
          sepbtm_sum = 0.
          etremain_sum = 0.
          if(rt_active.eq.1) then
            perc_no3_conc_sum = 0.
          endif
          !Reset counter
          mf_daycounter = 0
        endif
        
        !Convert MODFLOW units back to SWAT units
        if(mf_ran) then
          call smrt_conversion2swat
        endif

      end subroutine smrt_mf_run
