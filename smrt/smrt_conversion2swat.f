      subroutine smrt_conversion2swat
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ Purpose ~ ~ ~
!!    This subroutine converts all the MODFLOW variables, to pass into SWAT, into the proper units and SWAT variables
!!
!!    ~ ~ ~ Variables Used ~ ~ ~
!!    name            |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gw_q(:)         |mm H2O           |groundwater contribution to streamflow from
!!                    |                 |HRU on current day
!!    gw_qdeep(:)     |mm H2O           |groundwater contribution to streamflow from deep aquifer from
!!                    |                 |HRU on current day
!!    gwht(:)         |m                |groundwater height
!!    hru_fr(:)       |none             |fraction of subbasin area in HRU
!!    hru_ha(:)       |ha               |area of HRU in hectares
!!    hrutot(:)       |none             |number of HRUs in subbasin
!!    leapyr          |none             |leap year flag:
!!                    |                 |0  leap year
!!                    |                 |1  regular year
!!    msubo           |none             |max number of variables in output.sub
!!    nhru            |none             |number of HRUs in watershed
!!    no3gw(:)        |kg N/ha          |nitrate loading to reach in groundwater
!!                    |                 |in HRU for the day
!!    rchstor(:)      |m^3 H2O          |water stored in reach
!!    rttlc(:)        |m^3 H2O          |transmission losses from reach on day
!!    sub_fr(:)       |km2/km2          |fraction of total watershed area contained
!!                    |                 |in subbasin
!!    sub_km(:)       |km^2             |area of subbasin in square kilometers
!!    LENUNI          |MODFLOW length   |the MODFLOW variable for which length units are being used
!!    ITMUNI          |MODFLOW time     |the MODFLOW variable for which time units are being used
!!    NROW            |n/a              |the MODFLOW variable for number of rows of grids
!!    NCOL            |n/a              |the MODFLOW variable for number of columns of grids
!!    NLAY            |n/a              |the MODFLOW variable for number of layers of grids
!!    HNEW(:,:,:)     |LENUNI           |MODFLOW variable for the new head in the aquifer
!!    BOTM(:,:,:)     |LENUNI           |MODFLOW variable for the elevation of the 
!!                    |                 |bottom of each grid cell layer
!!    RIV(6,:)        |LENUNI           |MODFLOW variable for properties of the river cells
!!                    |n/a              |(1,:) cell layer index
!!                    |n/a              |(2,:) cell row index
!!                    |n/a              |(3,:) cell column index
!!                    |LENUNI           |(4,:) cell river stage (bottom elevation + depth)
!!                    |LENUNI/ITMUNI    |(5,:) cell conductance
!!                    |LENUNI           |(6,:) cell river bottom elevation
!!    grid2seg_id     |none             |a list per MODFLOW river grids (cols) containing 
!!                    |                 |the SWAT river segment IDs within that grid cell
!!    grid2seg_len    |m                |a list per MODFLOW river grids (cols) containing 
!!                    |                 |the SWAT river lengths within that grid cell
!!    nrivcells_subs  |n/a              |number of river grid cells in MODFLOW that are linked with SWAT sub-basins
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Variables Modified ~ ~ ~
!!    name            |units                 |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gwht(:)         |m                     |groundwater height SWAT variable, populated by MODFLOW's HNEW
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Local Definitions ~ ~ ~
!!    name            |units                 |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mf_lengthUnit   |MODFLOW length unit   |the modified integer to represent
!!                    |                      |the MODFLOW unit of length for units.f
!!    mf_timeUnit     |MODFLOW time unit     |the modified integer to represent
!!                    |                      |the MODFLOW unit of time for units.f
!!    IL              |none                  |the layer index for getting info. from RIVR
!!    IC              |none                  |the column index for getting info. from RIVR
!!    IR              |none                  |the row index for getting info. from RIVR
!!    sum_rivrate(1)  |LENUNI**3/ITMUNI      |holder for the sum of river rates contributing to a
!!                    |                      |given subbasin to be mapped to SWAT
!!    wtlocation(:,:) |n/a                   |smrt variable of the rows and columns of 
!!                    |                      |MODFLOW grid cells with a value indicating
!!                    |                      |which layer the water table is located in
!!    gw_tot          |m**3/day              |The total amount of groundwater discharge from the current
!!                    |                      |grid cell to the subbasin outlet
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ End Specifications ~ ~ ~ ~ ~ ~
      
!     Import variables
      use parm, only: gw_q,gwno3,gw_qdeep,gwht,hru_fr,hru_ha,hru1, !SWAT
     &                hrutot,
     &                leapyr,msub,nhru,no3gw,rchstor,rttlc,sub_fr,
     &                sub_km,varoute,sw_gw_q,mhru,hru_dafr,wshddayo,
     &                qdr,hrumono,vadose_thick_hru,sw_gw_no3
      use GLOBAL,only:LENUNI,ITMUNI,NROW,NCOL,NLAY,HNEW,BOTM, !MODFLOW
     &                DELR,DELC,IBOUND
      use GWFUPWMODULE, only: SC2UPW 
      use GWFRIVMODULE, only:RIVR,MXRIVR !MODFLOW
      use mf_rt_link, only: rt_active,rt_rivmass !MODFLOW-(UZF-RT3D) Linkage
      use rt_global, only: CNEW !UZF-RT3D
      use smrt_parm !smrt linkage
      implicit none
      
!     define local variables
      integer found,mf_gridID
      integer mf_lengthUnit,mf_timeUnit,nsubs,dum
      integer IL,IC,IR,i,j,k,h,m,subID,dhruID
      DOUBLE PRECISION HHNEW,CHRIV,RRBOT,CCRIV
      real grid_rivlen, HRIV, CRIV, RBOT, RATE, gw_tot, discharge,
     &     no3_gw_loading,loading,total_discharge,gw_sw_mm,sw_gw_mm,
     &     basin_area,sub_area,sub_gw_sw,sub_sw_gw,sub_gw_sw_no3,
     &     sub_sw_gw_no3,thickness,cell_area,
     &     cell_gw_volume,aquifer_gw_volume,sy,aquifer_gw_m,hru_gw
      real grams_N, Ntot
      real wtlocation(NCOL,NROW)
      real sum_rivrate(1)
      real rt_cnew_location(NCOL,NROW)
      real sub_gw(msub)
      real sub_gwno3(msub)
      real gwht_dhru(dhru),vadose_thick_dhru(dhru)
      
      mf_lengthUnit = LENUNI + 10
      mf_timeUnit = ITMUNI
      wtlocation = 1 !Set default water table location
      rt_cnew_location = 1 !Set UZF-R3D concentration location to the 1st layer, rtb
      subID = 0
      gwht_dhru = 0.
      vadose_thick_dhru = 0.
      gw_tot = 0
      h = 1 !set starting hru index
      
      !Zero out gwmod values. gw_q values will be populated using output from MODFLOW
      gw_q = 0.
      gw_qdeep = 0.
      sw_gw_q = 0.
      rttlc = 0.
      no3gw = 0.

      !zero out array for subbasin groundwater discharge and nitrate mass loading !rtb
      sub_gw = 0. 
      sub_gwno3 = 0.

      !get the total area of the basin
      basin_area = 0.
      open(99999,file='sub_km')
      do i=1,msub
        basin_area = basin_area + sub_km(i) !km2
        write(99999,*) sub_km(i)
      enddo

      !output files (for MODFLOW, SWAT values)
      if(out_MODFLOW_gwsw.eq.1) then
        write(30005,*) !skip line, to start values for current day
        write(30005,*) 'Daily GW/SW Exchange for each River Cell'
      endif

      if(out_SWAT_gwsw.eq.1) then
        write(30006,*) !skip line, to start values for current day
        write(30006,*) 'Daily GW/SW Exchange for each Subbasin'
      endif

      open(88888,file='modflow_discharge')




      !loop through the MODFLOW river cells -------------------------------------------------------------------------------------
      total_discharge = 0.
      do i=1,MXRIVR
        
        !determine if the River cell is linked with a SWAT sub-basin
        IR=RIVR(2,i) !row of MODFLOW river cell
        IC=RIVR(3,i) !column of MODFLOW river cell
        mf_gridID = ((IR-1)*NCOL) + IC !grid ID of MODFLOW river cell
        found = 0
        do m=1,nrivcells_subs
          
          !only proceed if there is a match
          if(mf_gridID.eq.grid2riv_gridID(m)) then
      
        !reset averaged river properties
        grid_rivlen = 0.
        sum_rivrate(1) = 0.
        RATE = 0.
        
        !Use original MODFLOW code to calculate the rate of loss/gain from/to rivers (LENUNI^3/ITMUNI)
        !the below code is borrowed from the MODFLOW subroutine LMT7RIV7 to calculate RATE
        IL=RIVR(1,i)
        IR=RIVR(2,i)
        IC=RIVR(3,i)
        !--GET RIVER PARAMETERS FROM RIVER LIST.
        HRIV=RIVR(4,i)
        CRIV=RIVR(5,i)
        RBOT=RIVR(6,i)
        HHNEW=HNEW(IC,IR,IL)
        CHRIV=CRIV*HRIV
        CCRIV=CRIV
        RRBOT=RBOT
        !--COMPARE HEAD IN AQUIFER TO BOTTOM OF RIVERBED.
        IF(HHNEW.GT.RRBOT) RATE = CHRIV-CCRIV*HHNEW !--AQUIFER HEAD > BOTTOM THEN RATE=CRIV*(HRIV-HNEW).
        IF(HHNEW.LE.RRBOT) RATE = CRIV*(HRIV-RBOT) !--AQUIFER HEAD < BOTTOM THEN RATE=CRIV*(HRIV-RBOT)
        if(out_MODFLOW_gwsw.eq.1) then
          write(30005,*) RATE !write out for analysis !rtb
        endif
        !end MODFLOW code
        

        !retrieve groundwater nitrate mass loading
        if(rt_active.eq.1) then
          no3_gw_loading = rt_rivmass(i,1) !grams of no3-n, from MODFLOW river cell
          no3_gw_loading = no3_gw_loading / 1000. !kg of no3-n
        endif
        
        !The river cell might be in multiple subbasins. Split the groundwater discharge and mass loadings
        !to these subbasins based on stream length (usually, the cell is only in one subbasin).  !rtb

        !Get total stream segment length within current river cell !rtb
        do j=1,riv_nsubs(m) !rtb
          grid_rivlen = grid_rivlen + grid2riv_len(m,j)
        enddo
        
        !loop through the subbasins that have spatial areas within the current river cell  !rtb
        nsubs = riv_nsubs(m) !rtb
        do j=1,nsubs !rtb
          
          !Calculate the volume of groundwater discharge to the current subbasin, and add to
          !the total groundwater discharge for the subbasin  !rtb
          subID = grid2riv_id(m,j) !rtb
          
          !change to positive value (to indicate discharge from the aquifer to the stream) !rtb
          discharge = RATE * (-1) !rtb

          !convert to unites used by SWAT !rtb
          call units(discharge, mf_lengthUnit, 12, 3, 1, leapyr)! convert length unit (LENUNI**3/ITMUNI to m**3/ITMUNI)
          call units(discharge, 4, mf_timeUnit, 1, 1, leapyr)! convert time unit (m**3/ITMUNI to m**3/day)
          !discharge = discharge * 86400. !ft3 for the day !rtb
          !discharge = discharge * (0.3048**3) !m3 for the day !rtb

          !portion of river cell discharge for the subbasin, based on stream length !rtb
          discharge = discharge * (grid2riv_len(m,j)/grid_rivlen) !rtb
            
          if(discharge.gt.0) then
            total_discharge = total_discharge + discharge
          endif
          
          !add river cell discharge to the total groundwater discharge for the subbasin !rtb
          sub_gw(subID) = sub_gw(subID) + discharge !rtb
          
          !add river cell nitrate mass loading to the total groundwater mass loading for the subbasin !rtb
          if(rt_active.eq.1) then
            loading = no3_gw_loading * (-1) !positive now signifies loading to the stream
            loading = loading * (grid2riv_len(m,j)/grid_rivlen) !rtb
            sub_gwno3(subID) = sub_gwno3(subID) + loading !rtb
          endif

        enddo !rtb
        

        !Apply only a portion of the river seepage/discharge rate to each river segment within the
        !current grid cell based on the relative length of the river segment within the grid cell
        do j=1,msub !j should only be used as an index for grid2riv_id and grid2riv_len, but NOT as the actual subbasin index
          
          !determine if the current river cell is part of this subbasin
          subID = grid2riv_id(m,j)
          
          if(subID.ne.0)then
            sum_rivrate(1) = RATE * (grid2riv_len(m,j)/grid_rivlen)
            !for example, if subID's river is all of the river length in the grid, the
            !subbasin would get 100% of the river rate, if 2 rivers (from 2 subbasins) of
            !equal length are within the grid, each subbasin would get 50% of the river rate
            
            if(sum_rivrate(1).lt.0) then !negative: water leaving the aquifer (i.e. discharge to river)
              
              !change so that a positive now represents a contribution to the river
              sum_rivrate(1) = -sum_rivrate(1)
              
              !convert groundwater discharge rate to required SWAT analysis units (mm/day)
              call units(sum_rivrate, mf_lengthUnit, 15, 3, 1, leapyr)! convert length unit (LENUNI**3/ITMUNI to km**3/ITMUNI)
              call units(sum_rivrate, 4, mf_timeUnit, 1, 1, leapyr)! convert time unit (km**3/ITMUNI to km**3/day)
              call units(sum_rivrate, 15, 14, 1, 1, leapyr)!convert length unit (km**3/day to mm-km**2/day)
              sub_area = sub_km(subID)
              gw_tot = sum_rivrate(1) / sub_area !divide by the subbasin area, in km, thus finishes converting units (mm-km**2/day to mm/day)

            else !positive: water entering the aquifer (i.e. leakage from river)
              
              !convert groundwater discharge rate it into rttlc units (m3/day)
              call units(sum_rivrate, mf_lengthUnit, 12, 3, 1, leapyr)! convert length unit (LENUNI**3/ITMUNI to m**3/ITMUNI)
              call units(sum_rivrate, 4, mf_timeUnit, 1, 1, leapyr)! convert time unit (m**3/ITMUNI to m**3/day)
              rttlc(subID) = rttlc(subID) + sum_rivrate(1)

            endif
          endif
        enddo

          endif
        enddo !end loop to determine if MODFLOW River cell has a match with a cell that is linked with SWAT sub-basins     
      
      enddo !go to next MODFLOW River Cell

      write(88888,*) total_discharge

      
      

      !Add discharge/seepage to SWAT routing arrays -----------------------------------------------------------------------------

      !Add subbasin groundwater discharge to the total volume of water in the stream (hence, groundwater
      !discharge will be included in the streamflow when hydrographs are added together and routed down
      !the watershed) !rtb
      do j=1,msub !rtb
        varoute(2,j) = varoute(2,j) + sub_gw(j) !rtb WATER (m3)
        varoute(6,j) = varoute(6,j) + sub_gwno3(j) !rtb NO3-N (kg)
        if(out_SWAT_gwsw.eq.1) then
          write(30006,*) sub_gw(j) !rtb
        endif
      enddo !rtb      
      
      
      
      !Calculate water balance terms (gw_q, sw_gw, gw) --------------------------------------------------------------------------
      
      !calculate gw_q and sw_gw
      do j=1,msub
        
        sub_gw_sw = sub_gw(j)
        sub_gw_sw_no3 = sub_gwno3(j)
        sub_area = sub_km(j)

        !change to mm/day
        sub_gw_sw = sub_gw_sw / (sub_km(j) * 1000000.) !m of water
        sub_gw_sw = sub_gw_sw * 1000. !mm of water

        !change to kg/ha
        sub_gw_sw_no3 = sub_gw_sw_no3 / sub_km(j) !kg/km2
        sub_gw_sw_no3 = sub_gw_sw_no3 / 100. !kg/ha

        if(sub_gw_sw.gt.0) then !groundwater entering stream
          
          !calculate % of subbasin groundwater contributing to entire basin
          wshddayo(104) = wshddayo(104) + 
     &                      (sub_gw_sw * (sub_area/basin_area))

          !calculate % of subbasin groundwater NO3 contributing to entire basin
          wshddayo(110) = wshddayo(110) + 
     &                      (sub_gw_sw_no3 * (sub_area/basin_area))
          
          !now, calculate gw_q for each HRU
          h = hru1(j)
          do k=1,hrutot(j)     
            !scale the value by the % hru area in the subbasin area
            gw_q(h) = sub_gw_sw * hru_fr(h) !mm of water
            gwno3(h) = sub_gw_sw_no3 * hru_fr(h) !kg/ha of NO3
            h = h + 1
          enddo
        
        else !stream water entering groundwater
          sub_sw_gw = sub_gw_sw * (-1)
          sub_sw_gw_no3 = sub_gw_sw_no3 * (-1)

          !calculate % of subbasin sw_gw contributing to entire basin
          wshddayo(114) = wshddayo(114) + 
     &                      (sub_sw_gw * (sub_area/basin_area))

          !calculate % of subbasin sw_gw NO3 contributing to entire basin
          wshddayo(116) = wshddayo(116) + 
     &                      (sub_sw_gw_no3 * (sub_area/basin_area))
          
          !now, calculate gw_q for each HRU
          h = hru1(j)
          do k=1,hrutot(j)     
            !scale the value by the % hru area in the subbasin area
            sw_gw_q(h) = sub_sw_gw * hru_fr(h) !mm of water
            sw_gw_no3(h) = sub_sw_gw_no3 * hru_fr(h) !kg/ha of NO3
            h = h + 1
          enddo

        endif
      enddo


      !calculate gw (total groundwater volume in the watershed)
      aquifer_gw_volume = 0.
      do i=1,NROW
        do j=1,NCOL
          do k=1,NLAY
            if(ibound(j,i,k).ne.0) then
            !Determine saturated thickness of the current layer
            if(HNEW(j,i,k).LT.BOTM(j,i,k-1) .and. !BOTM(J,I,0) contains the top of layer1
     &           HNEW(j,i,k).GT.BOTM(j,i,k)) then 
              thickness = HNEW(j,i,k) - BOTM(j,i,k)
            else
              thickness = BOTM(j,i,k-1) - BOTM(j,i,k)
            endif
            cell_area = DELR(j) * DELC(i)
            sy = SC2UPW(j,i,k) / cell_area !specific yield from cell storage capacity
            cell_gw_volume = thickness * cell_area * sy
            aquifer_gw_volume = aquifer_gw_volume + cell_gw_volume
            endif
          enddo
        enddo
      enddo
      aquifer_gw_m = aquifer_gw_volume/(basin_area*1000000.) !convert m3 to m
      wshddayo(115) = aquifer_gw_m * 1000. !convert m to mm of water

      !include gw and sw_gw values into the overall water yield for the watershed
      wshddayo(6) = wshddayo(6) + wshddayo(104) - wshddayo(114)

      !Add gw_q and sw_gw to total water yield (to be output in output.std)
      hru_gw = 0.
      do h=1,mhru
        hru_gw = hru_gw + (gw_q(h) * hru_dafr(h))
      enddo


      !HRU monthly summations
      do h=1,mhru
        hrumono(6,h) = hrumono(6,h) + gw_q(h)
        hrumono(70,h) = 0. !no gw_qdeep
        hrumono(74,h) = hrumono(74,h) + sw_gw_q(h)
      enddo



      !Determine water table location (layer) and the thickness of the vadose zone ----------------------------------------------
      !(this will be used to determine if the water table is in SWAT's soil profile, and
      !(thus whether the soil processes are affected by the water table)

      !Loop through MODFLOW variables and pull out information
      do j=1, NROW
        do i=1, NCOL
          do k=1, NLAY
            !Loop through and find which layer the water table is in
            if(HNEW(i,j,k).LT.BOTM(i,j,k-1) .and. !BOTM(J,I,0) contains the top of layer1
     &           HNEW(i,j,k).GT.BOTM(i,j,k)) then 
              wtlocation(i,j) = k

              !distance between the ground surface and the water table
              vadose_thick(i,j) = BOTM(i,j,0) - HNEW(i,j,k)

            endif
          enddo
        enddo
      enddo
      
      !Fill in SWAT variable "gwht" with water table elevation from MODFLOW
      call smrt_grid2dhru3D(HNEW, gwht_dhru, wtlocation)
      call units(gwht_dhru, mf_lengthUnit, 12, 1, dhru, leapyr)!convert length unit (LENUNI to m)
      call smrt_dhru2hru(gwht_dhru, gwht, 0) !map dhru to hru
      
      !Convert MODFLOW variable (vadose_thick) into a SWAT variable (first to DHRUs, then to HRUs)
      !(now, each HRU has a thickness of the vadose zone - this is to know if the water table
      ! is within the soil profile)
      call smrt_grid2dhru2D(vadose_thick, vadose_thick_dhru)
      call units(vadose_thick_dhru, mf_lengthUnit, 14, 1, dhru, leapyr)! to convert length units (LENUNI to mm)
      call units(vadose_thick_dhru, mf_timeUnit, 4, 1, dhru, leapyr)! to convert time units (ITMUNI to days)
      call smrt_dhru2hru(vadose_thick_dhru, vadose_thick_hru, 0)


      !if(rt_active.eq.1) then
      !  !Pass back UZF-RT3D information into SMRT variables
      !  call smrt_grid2dhru4D(CNEW, upflux_no3_dhru, rt_cnew_location,1)!note nitrogen concentration is assumed to be in UZF-RT3D's 2nd layer and first index
      !
      !  !Pass back SMRT variables into SWAT variables
      !  call smrt_dhru2hru(upflux_no3_dhru, upflux_no3)
      !end if
        
      !Call additional MODFLOW and UZF-RT3D to SWAT subfunctions
      call smrt_upflux_to_soil
      
      
      return
      end