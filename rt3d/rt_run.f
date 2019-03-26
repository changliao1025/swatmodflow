*     This subroutine runs the RT3D simulator for the time period indicated. Typically,
*     the stress period and flow time step are the same as for SWAT (1 day).

*     This subroutine is called for each flow time step of MODFLOW

      subroutine rt_run

      use rt_global
      use mf_rt_link
      use GLOBAL,only:ITMUNI,NROW,NCOL,NLAY,MF_leapyr !MODFLOW

      implicit none
      integer   i,j,k,N,L,dum
      common   /GCGIDX/L(19) !for implicit shceme
           

      !print a message to the screen, indicating that RT3D is running
      print *, '                RT3D is running'

*     This subroutine is called within the MODFLOW flow time step loop. Thus, it
*     calculates the reactive transport of chemical species for 1 day (to match
*     the SWAT step size)
      
      !Calculate the dispersion coefficients for the current flow time step
      if(TRNOP(2)) call dsp_coeff

      dt = rt_delt !(calculated in mf_run)
      t1 = t2
      t2 = t2 + dt
      
      !loop through the transport time steps for the flow time step
      do ntrans=1,5000

        !advance once transport step
        call btn_ts

        !for each mobile species, compute change in concentration due to advection, 
        !dispersion, and source-sink mixing. this is done implicitly
        do icomp=1,ncomp
          if(phase(icomp).eq.1) then !species is mobile
            call rt_implicit
          endif
        enddo
          
        !compute change in concentration due to chemical reactions
        if(trnop(4)) call rct_solve

        !compute mass budgets and output results for the transport step
        do icomp=1,ncomp
          call btn_budget !mass budgets
          call btn_output !save outputs   
        enddo 

        !if end of flow time step, exit
        if(time2.ge.t2) goto 100

      enddo !go to next transport time step

      
      !store concentration values for average outputs
100   call units(dt, ITMUNI, 4, 1, 1, MF_leapyr) !tcw
      sum_tracker = sum_tracker + dt !in days
      !loop through the cells for the current day
      do k=1,nlay
        do i=1,nrow
          do j=1,ncol
            do n=1,ncomp
              !if(ICBUND(j,i,k,n).gt.0) then !only for active cell
              cnew_total(j,i,k,n) = cnew_total(j,i,k,n) + cnew(j,i,k,n)
              if(sum_tracker.ge.(365*rt_year)) then
                cnew_avg(j,i,k,n,rt_year) = cnew_total(j,i,k,n) / 365.0
              endif
              !endif
            enddo
          enddo
        enddo
      enddo

      !increment to a new year if day 365 has been reached
      if(sum_tracker.ge.(365*rt_year)) then
        rt_year = rt_year + 1
        cnew_total = 0.
      endif


      return
	end
