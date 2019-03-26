      subroutine smrt_updateOutput
!!      ~ ~ ~ Author ~ ~ ~
!!      Tyler Wible, Masters student
!!      Colorado State University 2012-2014
!!      Comment initials "tcw"
!!
!!      ~ ~ ~ Purpose ~ ~ ~
!!      This file is temporary only, please delete it later
!!      This file updates the SWAT output variables with corrected
!!      groundwater flow values from MODFLOW
      
!       Import variables
        use parm !SWAT
        implicit none

        integer :: subID,h, k
      
        !Update SWAT's output variables to reflect the changes to gw_q and gw_qdeep from MODFLOW, like it is done in sumv.f
        
        !loop through the subbasin
        !do subID=1, msub 

          !loop through the HRUs within the current subbasin
        !  h = hru1(subID)
        !  do k=1, hrutot(subID)
            
            !the below code is borrowed from SWAT's sumv.f
        !    if (curyr > nyskip) then
              
              !! HRU summations
        !      hrumono(6,h) = hrumono(6,h) + gw_q(h)
        !      hrumono(70,h) = hrumono(70,h) + gw_qdeep(h)

              !! watershed summations
        !      if (ffcst == 0 .and. iscen == 1) then
        !       wshddayo(104) = wshddayo(104) + gw_q(h) * hru_dafr(h)
        !       wshddayo(113) = wshddayo(113) + gw_qdeep(h) * hru_dafr(h)
        !       wshddayo(114) = wshddayo(114) + sw_gw_q(h) * hru_dafr(h)
        !      else if (ffcst == 1) then
        !        fcstaao(8) = fcstaao(8) + gw_q(h) * hru_dafr(h)
        !      end if
        !    end if
            !end borrowed SWAT code from sumv.f
        !    h = h + 1
        !  enddo
        !enddo

      return
      end