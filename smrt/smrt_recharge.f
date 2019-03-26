      subroutine smrt_recharge(eventdata)
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
!!    This subroutine converts the necessary SWAT variables for the MODFLOW recharge
!!    (RCH) package's variables
!!      
        !Import variables
        use modevent
        use GLOBAL, only: NCOL,NROW
        use GWFRCHMODULE, only: RECH
        use smrt_parm, only: sepbtm_dhru,out_MF_recharge
        implicit none
        
        !Define local variables
        integer i,j
        class (ieventdata), pointer :: eventdata

        !convert to MODFLOW Recharge array
        call smrt_dhru2grid2D(sepbtm_dhru,RECH,1)

        !output Recharge values to a file
        if(out_MF_recharge.eq.1) then
          write(30002,*)
          write(30002,*) 'daily recharge values provided to MODFLOW'
          do i=1,nrow
            write(30002,100) (RECH(j,i),j=1,ncol)
          enddo
        endif

  100 format(1000(e17.10))

      end subroutine smrt_recharge 
