      subroutine smrt_conversion2rt3d
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine converts the necessary SWAT variables (per hru) into SMRT variables
!!    (per disaggregated hru, dhru) then converts these into the proper units for UZF-RT3D
!!
!!    ~ ~ ~ Variables Used~ ~ ~
!!    name            |units                 |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    LENUNI          |unit_in               |the MODFLOW variable for which length units are being used
!!    ITMUNI          |unit_out              |the MODFLOW variable for which time units are being used
!!    nitraten(:)     |mg N/L                |nitrate concentration in reach
!!    perc_no3_conc(:)|kgN/mm                |concentration of nitrate in the percolation 
!!                    |                      |water from the bottom layer of SWAT's soil zone per hru
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Variables Modified ~ ~ ~
!!    name            |units                 |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    perc_no3_conc_dhru(:)  |kgN/mm                |concentration of nitrate in the percolation 
!!                           |                      |water from the bottom layer of SWAT's soil zone per dhru
!!                           |                      |populated from perc_no3_conc and the smrt_hru2dhru conversion
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ Local Definitions ~ ~ ~
!!    name          |units                 |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mf_lengthUnit |MODFLOW length unit   |the modified inteteger to represent
!!                  |                      |the MODFLOW unit of length for units.f
!!    mf_timeUnit   |MODFLOW time unit     |the modified integer to represent
!!                  |                      |the MODFLOW unit of time for units.f
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      !Import variables
      use parm, only:leapyr,msub,nitraten,perc_no3_conc !SWAT
      use GLOBAL, only:LENUNI,ITMUNI,NCOL,NROW !MODFLOW
      use GWFRIVMODULE, only:RIVR !MODFLOW
      use mf_rt_link, only: rt_criv,crch !MODFLOW-(UZF-RT3D) linkage

      use smrt_parm !smrt linkage
      implicit none
      
      !Define local variables
      integer mf_lengthUnit,mf_timeUnit
      integer i,j,col,row,lay
      real rivlen(1)
      real rivNitraten(1)
      mf_lengthUnit = LENUNI + 10
      mf_timeUnit = ITMUNI
      
      !Convert SWAT hru based variables into SMRT dis-aggregated hrus for more accurate surface/groundwater interaction
      call smrt_hru2dhru(perc_no3_conc, perc_no3_conc_dhru)
      
      !Convert SMRT variables into UZF-RT3D units
      call units(perc_no3_conc_dhru, 12, 14, 1, dhru, leapyr)! to convert length units (1/mm to 1/m which is like converting m to mm)
      call units(perc_no3_conc_dhru, 32, 33, 1, dhru, leapyr)! to convert area units (1/ha to 1/m^2 which is like converting m^2 to ha)
      call units(perc_no3_conc_dhru, 21, 22, 1, dhru, leapyr)! to convert weight units (kg to g)
      !This results in perc_no3_conc_dhru being in g N/m**3 = mg/L of Nitrogen which are the expected units for UZF-RT3D
      
      !Convert SMRT variables into UZF-RT3D variables
      call smrt_dhru2rtgrid3D(perc_no3_conc_dhru, CRCH, 1)! note: nitrogen concentration is being mapped to the 1st index of UZF-RT3D's variables, tcw
      
      !write out cell-by-cell values to a file (for testing/plotting)
      !open(146000,file='output/swat_no3_cell') !rtb
      !do i=1,nrow
      !  write(146000,100) (CRCH(j,i,1),j=1,ncol)
      !enddo
      !write(146000,*)

      !Convert SWAT's nitrateN variable into UZF-RT3D which should be the same size as MODFLOW's RIVR variable
      do i=1,nrivcells_subs
        
        !reset averaged river properties
        rivlen = 0.
        rivNitraten = 0.

        !Loop through the SWAT rivers (one for each subbasin)
        do j=1,riv_nsubs(i)
          !Get the river's properties to be based on a weighted average with: 
          !weights = subbasin's river segment length / total river length in grid cell (rivlen)
          rivlen(1) = rivlen(1) + grid2riv_len(i,j)
          rivNitraten(1) = rivNitraten(1) + 
     &                     (nitraten(j) * grid2riv_len(i,j))
        enddo
        
        !prevent divide by zero problems
        if(rivlen(1).eq.0) rivlen(1) = 1. 
        
        !Take weighted average of river nitrate concentration
        rivNitraten(1) = rivNitraten(1) / rivlen(1)
        
        !Convert into UZF-RT3D units
        !note nitrateN is already in mg/L which are the expected units for UZF-RT3D
        
        !Populate UZF-RT3D's rt_criv variable for this time step's nitrateN value
        lay = RIVR(1,i)
        row = RIVR(2,i)
        col = RIVR(3,i)
        rt_criv(i,1) = rivNitraten(1) !note that nitrate is hard coded to the 1st entry in UZF-RT3D

      enddo !go to the next river cell

100   format (1000(f12.4))

      return
      end