      subroutine smrt_upflux_to_soil
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    Ryan Bailey, Post-Doc student (2012-2013), Assistant Professor (2013-)
!!    Colorado State University 2012-
!!    Comment initials "rtb"
!!
!!    ~ ~ ~ Purpose ~ ~ ~
!!    Add upflux water (calculated by UZF) to soil profile (SWAT)
!!    Start with the bottom soil layer. When filled to Field Capacity, move to
!!    next layer up.
!!
!!    ~ ~ ~ Variables Used ~ ~ ~
!!    name          |units             |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sol_nly(:)    |none              |number of soil layers 
!!    sol_z(:,:)    |mm soil           |depth to bottom of soil layer 
!!    sol_st(:,:)   |mm H2O            |amount of water stored in the soil layer
!!                  |                  |on any given day (less wp water)
!!    sol_up(:,:)   |mm H2O/mm soil    |water content of soil at -0.033 MPa (field capacity)
!!    sol_sw(:)     |mm H2O            |amount of water stored in soil profile on any
!!                  |                  |given day
!!    sol_wpmm(:,:) |mm H20            |water content of soil at -1.5 MPa (wilting
!!                  |                  |point)
!!    nhru          |none              |number of HRUs in watershed
!!    LENUNI        |unit_in           |the MODFLOW variable for which length units are being used
!!    ITMUNI        |unit_out          |the MODFLOW variable for which time units are being used
!!    NCOL          |n/a               |the MODFLOW variable for number of columns of grids
!!    NROW          |n/a               |the MODFLOW variable for number of rows of grids
!!    NLAY          |n/a               |the MODFLOW variable for number of layers of grids
!!    GWET(:,:,:)   |LENUNI**3/ITMUNI  |modflow variable for the et coming from the groundwater
!!    UZET(:,:,:)   |LENUNI**3/ITMUNI  |modflow variable for the et coming from the UZF package
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Variables Modified ~ ~ ~
!!    name          |units             |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sol_st(:,:)   |mm H2O            |amount of water stored in the soil layer
!!                  |                  |on any given day (less wp water)
!!    sol_no3(:,:)  |kg N/ha           |amount of nitrogen stored in the
!!                  |                  |nitrate pool in soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Local Definitions ~ ~ ~
!!    name          |units               |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    upflux(:)     |mm H20              |depth of upflux water (as calculated
!!                  |                    |from UZET and GWET)
!!    dg            |mm soil             |thickness of soil layer
!!    sol_upmm      |mm H2O              |amount of water at field capacity
!!    j             |none                |HRU number
!!    sol_water     |mm H20              |current amount of water in the soil layer
!!    upflux_mm     |mm H20              |depth of upflux water for the current HRU
!!    sol_deficit   |?                   |the amount of water that can potentially 
!!                  |                    |be added to the soil layer (based on field capacity)
!!    fract_upflux  |none                |determine fraction of total upflux water 
!!                  |                    |that is directed to the current layer
!!    no3mass_add   |??                  |based on fract_upflux this is how much of the no3 mass 
!!                  |                    |should be added to the layer
!!    mf_lengthUnit |MODFLOW length unit |the modified inteteger to represent
!!                  |                    |the MODFLOW unit of length
!!    mf_timeUnit   |MODFLOW time unit   |the modified integer to represent
!!                  |                    |the MODFLOW unit of time
!!    mf_et(:,:)    |LENUNI**3/ITMUNI    |variable to contain each MODFLOW 
!!                  |                    |grid cell's et to pass to SWAT 
!!                  |                    |= UZET + GWET
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!     Import variables
      use parm, only: sol_nly,sol_z,sol_st,sol_up,sol_sw,sol_wpmm, !SWAT
     &                nhru,sol_no3,leapyr
      use GLOBAL, only:LENUNI,ITMUNI,NCOL,NROW,NLAY,DELR,DELC,IUNIT !MODFLOW
      use GWFUZFMODULE, only: GWET, UZET !MODFLOW
      USE GWFEVTMODULE,ONLY: EVTvol !MODFLOW EVT
      use smrt_parm, only: dhru, upflux_no3 !smrt linkage
      implicit none
      
!     Initialize local variables
      integer i,j,k,ly,mf_lengthUnit,mf_timeUnit,nlayers,layer
      real dg,upflux_mm,sol_upmm,sol_water,sol_deficit,fract_upflux
      real no3mass_add
      real mf_et(NCOL,NROW)
      real upflux_dhru(dhru)
      real upflux(nhru)
      mf_et = 0.
      mf_lengthUnit = LENUNI + 10
      mf_timeUnit = ITMUNI
      
      !only proceed if EVT package or UZF package is active
      if(IUNIT(5).gt.0 .or. IUNIT(55).gt.0) then

      !Calculate the total ET upflux from MODFLOW from its uzf groundwater ET (GWET) variable for each grid cell
      do j=1, NROW
        do i=1, NCOL
          do k=1, NLAY
            !If using the EVT package, get ET from there
            if(IUNIT(5).gt.0)then
              mf_et(i,j) = mf_et(i,j) + EVTvol(i,j,k)/(DELR(i)*DELC(j))
            endif
            
            !If using the UZF package, get ET from there
            if(IUNIT(55).gt.0)then
              mf_et(i,j) = mf_et(i,j) + UZET(i,j,k)/(DELR(i)*DELC(j))
            endif
          enddo
          
            
          !If using the UZF package, get addtional ET from there
          if(IUNIT(55).gt.0)then
            mf_et(i,j) = mf_et(i,j) + GWET(i,j)/(DELR(i)*DELC(j))
          endif
       enddo
      enddo
      
!     Convert MODFLOW variable into SMRT variable
      call smrt_grid2dhru2D(mf_et, upflux_dhru)! Map the MODFLOW upflux from grids to dhrus

      
!     Convert SMRT variable into SWAT units
      call units(upflux_dhru, mf_lengthUnit, 15, 3, dhru, leapyr)! to convert length units (LENUNI**3 to km**3)
      call units(upflux_dhru, mf_timeUnit, 4, 1, dhru, leapyr)! to convert time units (ITMUNI to days)
      call units(upflux_dhru, 15, 14, 1, dhru, leapyr)! to convert length units (km to mm)
      
      
!     Convert SMRT variable into SWAT variable
      call smrt_dhru2hru(upflux_dhru, upflux, 2)! Map the MODFLOW upflux from dhrus to HRUs and divide the SWAT variable by hru area (km**3/km**2 = km)

      
      do j=1, nhru
        !upflux_mm: the depth of upflux for the HRU (calculated by dividing the flow rate of (UZET + GWET) by the area of the HRU)
        upflux_mm = upflux(j) !get the upflux for the HRU

        !add upflux water to the soil layers - beginning with the bottom layer ----------------------
      
        !loop through the soil layers (beginning with the bottom layer)
        nlayers = sol_nly(j)
        do k=1,nlayers
        
          !only proceed if there is any upflux water remaining
          if(upflux_mm.gt.0) then 
            layer = nlayers - (k-1)

            !calculate thickness of soil layer
            if(layer.gt.1) then
              dg = sol_z(layer,j) - sol_z(layer-1,j)
            else
              dg = sol_z(layer,j)
            endif

            !determine total water (mm) at Field Capacity (what the soil layer can hold)
            sol_upmm = sol_up(layer,j) * dg !(mm water / mm soil) * mm soil

            !determine the current amount of water in the soil layer
            !(must add wilting point water, since sol_st does not include it)
            sol_water = sol_st(layer,j) + sol_wpmm(layer,j)

            !calculate amount of water that can potentially be added to the soil layer
            !(based on field capacity)
            sol_deficit = sol_upmm - sol_water

            !calculate how much upflux water is added to the soil layer
            if(upflux_mm.ge.sol_deficit) then !Fill up all of the soil layer
              
              !fill the soil layer to field capacity (then subtract the wilting point
              !water, since this is part of the definition of sol_st)
              sol_st(layer,j)=(sol_water+sol_deficit)-sol_wpmm(layer,j)
              
              !determine fraction of total upflux water that is directed to the current layer
              !this is used to determine how much of the no3 mass should be added to the layer
              fract_upflux = sol_deficit / upflux(j)
              no3mass_add = upflux_no3(j) * fract_upflux
              sol_no3(layer,j) = sol_no3(layer,j) + no3mass_add
              
              !decrease the amount of upflux that can be added to the above layers
              !(the next time through the loop)
              upflux_mm = upflux_mm - sol_deficit

            else !Fill up part of the soil layer
              !add all of the upflux (resulting water amount in soil layer should be less
              !than field capacity)
              sol_st(layer,j)=(sol_water + upflux_mm)-sol_wpmm(layer,j)
              upflux_mm = 0
              
              !add all of the nitrogen mass in the upflux water to the current layer
              sol_no3(layer,j) = sol_no3(layer,j) + upflux_no3(j)
            endif
              
          endif
        enddo !go to the above layer

        !update total soil water amount for the profile ---------------------------------------------
        sol_sw(j) = 0.
        do ly = 1, sol_nly(j)
          sol_sw(j) = sol_sw(j) + sol_st(ly,j)
        enddo
      enddo

      endif
        
      return
      end