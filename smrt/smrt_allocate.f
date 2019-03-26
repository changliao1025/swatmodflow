      subroutine smrt_allocate()
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine allocates array sizes for variables used in the SMRT linkage between SWAT, MODFLOW, and UZF-RT3D

      use parm, only: msub,nhru !SWAT
      use smrt_parm !smrt linkage
      use GLOBAL, only:NCOL,NROW !MODFLOW
      implicit none
      
!     Allocate the size of the river-to-grid variables
      allocate(grid2riv_id(nrivcells_subs,msub))
      allocate(grid2riv_len(nrivcells_subs,msub))
      allocate(riv_nsubs(nrivcells_subs))
      allocate(grid2riv_gridID(nrivcells_subs))
      grid2riv_id = 0.
      grid2riv_len = 0.
      riv_nsubs = 0.
      grid2riv_gridID = 0
      
!     Allocate the size of HRU variables before smrt_upfluxToSoil runs
      allocate(upflux_no3(nhru))
      allocate(vadose_thick(NCOL,NROW))
      upflux_no3 = 0.
      vadose_thick = 0.

!     Allocate the size of disaggregated hru variables before MODFLOW and UZF-RT3D runs
      allocate(etremain_dhru(dhru))
      allocate(sepbtm_dhru(dhru))
      allocate(perc_no3_conc_dhru(dhru))
      allocate(upflux_no3_dhru(dhru))
      etremain_dhru = 0.
      sepbtm_dhru = 0.
      perc_no3_conc_dhru = 0.
      upflux_no3_dhru = 0.
      
!     Allocate the size of the SWAT hru variables for time-based aggregation
      allocate(sepbtm_sum(nhru))
      allocate(etremain_sum(nhru))
      allocate(perc_no3_conc_sum(nhru))
	sepbtm_sum = 0.
      etremain_sum = 0.
      perc_no3_conc_sum = 0.
      
      return
      end
