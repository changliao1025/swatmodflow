      subroutine smrt_close
!!    ~ ~ ~ Author ~ ~ ~
!!    Tyler Wible, Masters student
!!    Colorado State University 2012-2014
!!    Comment initials "tcw"
!!
!!    ~ ~ ~ Purpose ~ ~ ~
!!    This subroutine deallocates the variables from smrt_parm and some
!!    additional MODFLOW and UZF-RT3D variables which had their deallocate calls moved
      
!     Import variables
      use GWFRIVMODULE, only: RIVAUX, RIVR !MODFLOW
      use smrt_parm !smrt linkage
      implicit none
      
!     Deallocate MODFLOW variables
      deallocate(RIVAUX)
      deallocate(RIVR)
      
!     Deallocate smrt variables
      deallocate(g2d_r)
      deallocate(g2d_c)
      deallocate(g2d_area)
      deallocate(d2g_id)
      deallocate(d2g_area)
      deallocate(cell_dhrus)
      deallocate(d2h_id)
      deallocate(d2h_area)
      deallocate(grid2riv_id)
      deallocate(grid2riv_len)
      deallocate(riv_nsubs)
      deallocate(upflux_no3)
      
      deallocate(etremain_dhru)
      deallocate(sepbtm_dhru)
      deallocate(perc_no3_conc_dhru)
      deallocate(upflux_no3_dhru)
      
      return
      end