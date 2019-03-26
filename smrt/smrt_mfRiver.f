      subroutine smrt_mfRiver(eventdata)
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine converts the necessary SWAT variables used by MODFLOW's river package into
!!    the required hydrauilc conductivity and stage in MODFLOW one a daily timestep
!!
!!    ~ ~ ~ Variables Used~ ~ ~
!!    name            |units   |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    LENUNI          |unit_in |the MODFLOW variable for which length units are being used
!!    dep_chan(:)     |m       |average daily water depth in channel
!!    grid2riv_len    |m       |a list per MODFLOW river grids (cols) containing 
!!                    |        |the SWAT river lengths within that grid cell
!!    nrivcells_subs  |n/a     |number of river grid cells in MODFLOW that are linked with SWAT sub-basins
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Variables Modified ~ ~ ~
!!    name            |units   |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    RIVR(4,:)       |        |river stage (depth + bottom elevation) filled in with info from SWAT per day
!!    RIVR(5,:)       |        |river conducance, filled in with info from SWAT per day (in order to overwrite re-reading the .RIVR file every timeset)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Local Definitions ~ ~ ~
!!    name          |units     |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mf_lengthUnit |LENUNI    |the modified inteteger to represent
!!                  |          |the MODFLOW unit of length for units
!!    rivlen        |LENUNI    |variable for total river length in current 
!!                  |          |river grid cell
!!    rivdepth      |ITMUNI    |variable for weighted average (based on river 
!!                  |          |length) for depth of water in current river grid cell
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      !Import variables
      use modevent
      use parm, only:msub,dep_chan,leapyr !SWAT
      use GLOBAL, only:LENUNI !MODFLOW
      use GWFRIVMODULE, only:RIVR !MODFLOW
      use smrt_parm !smrt linkage
      implicit none
      class (ieventdata), pointer :: eventdata

      !Define local variables
      integer mf_lengthUnit,mf_timeUnit,i,j,k,subIndex
      real rivlen(1)
      real rivdepth(1)
      mf_lengthUnit = LENUNI + 10
      
      !output the channel depth for each SWAT subbasin
      if(out_SWAT_channel.eq.1) then
        write(30003,1900) (dep_chan(j),j=1,msub) !rtb
      endif

      ! Loop through the SWAT rivers to get channel depth (dep_chan) to pass into MODFLOW's RIVR
      do i=1,nrivcells_subs
        
        !reset averaged river properties
        rivlen = 0.
        rivdepth = 0.
        
        !Loop through the SWAT rivers attributes needed to calculate conductance
        do j=1,msub
          !Get the river's properties to be based on a weighted average with: 
          !weights = subbasin's river segment length / total river length in grid cell (rivlen)
          rivlen(1) = rivlen(1) + grid2riv_len(i,j)
          rivdepth(1) = rivdepth(1)+dep_chan(j)*grid2riv_len(i,j)
        enddo
        
        if(rivlen(1).eq.0) rivlen(1) = 1. !prevent divide by zero problems
        !Take weighted average of river depth
        rivdepth(1) = rivdepth(1)/rivlen(1)
        
        !Convert into MODFLOW units
        call units(rivdepth, 12, mf_lengthUnit, 1, 1, leapyr);! to convert length units (m to LENUNI)
        
        !Populate MODFLOW's RIVR variable for this time step's river stage
        RIVR(4,i) = rivdepth(1) + RIVR(6,i)! cell stage (depth + bottom elevation)
        
        !Write out values to a file
        if(out_MF_channel.eq.1) then
          write(30004,*) rivdepth(1)
        endif
      enddo
      
      write(30004,*)

 1900 format(1000(f12.4))
      
      return
      end
