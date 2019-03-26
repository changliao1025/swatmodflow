      module smrt_parm
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This module contains variables used for linking SWAT variables to MODFLOW and 
!!    UZF-RT3D as well as MODFLOW variables to SWAT and UZF-RT3D and UZF-RT3D to SWAT
!!      
!!    Declare global variables for SWAT-MODFLOW-(UZF-RT3D) (SMRT) linkage
      real, dimension (:,:), allocatable :: g2d_r, g2d_c, g2d_area
      real, dimension (:,:), allocatable :: d2g_id, d2g_area
      real, dimension (:,:), allocatable :: d2h_id, d2h_area
      integer :: g2d_size, d2g_size, d2h_size
      real, dimension (:,:), allocatable :: grid2riv_id, grid2riv_len
      integer, dimension (:), allocatable :: riv_nsubs,cell_dhrus

!!    Flag for whether modflow is active
      integer :: mf_active

!!    Flag for whether shallow water table dynamics is active
      integer :: mf_shallow_dynamics
      
!!    Counter the number of days between calls to MODFLOW
      integer :: mf_daycounter
      
!!    SMRT Disaggregated HRU (dhru) variables
      real, dimension (:), allocatable :: etremain_dhru, sepbtm_dhru
      real, dimension (:), allocatable :: perc_no3_conc_dhru, 
     &                                    upflux_no3_dhru
      integer :: dhru
      
!!    MODFLOW/SWAT River segment variables
      integer :: nrivcells_subs
      integer, dimension (:), allocatable :: grid2riv_gridID
      
!!    SWAT variables for smrt_upfluxToSoil as populated by UZF-RT3D
      real, dimension (:), allocatable :: upflux_no3
      real, dimension (:,:), allocatable :: vadose_thick

!!    SWAT time-aggregation variables
      real, dimension (:), allocatable :: sepbtm_sum, etremain_sum
      real, dimension (:), allocatable :: perc_no3_conc_sum

!!    Flags for writing output data (SWAT, MODFLOW)
      integer :: out_SWAT_recharge,out_MF_recharge,
     &           out_SWAT_channel,out_MF_channel,
     &           out_MODFLOW_gwsw,out_SWAT_gwsw

!!    Array for storing MODFLOW observation cells
      integer :: num_MF_obs,mf_obs_flag
      integer, dimension (:,:), allocatable :: MF_obs

      end module smrt_parm
