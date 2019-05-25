	PROGRAM SKETCH_N
c======================================================================c
c          SKETCH-N version 0.95: Nodal Neutron Diffusion Code for     c
c            Solving Steady-State & Kinetics Problems                  c
c                                                                      c
c             Moscow Engineering Physics Institute                     c
c                Tokyo Institute of Technology                         c
c              Japan Atomic Energy Research Institute                  c 
c                                                                      c
c       Author:  Vyacheslav G. Zimin                                   c
c                                                                      c
c                    (C) 1999 All Rights Reserved                      c
c                                                                      c
c                               NOTICE                                 c
c                                                                      c
c  Permission to use, copy, modify, and distribute this software and   c
c  its documentation for any purpose and without fee is hereby granted c
c  provided that the above copyright notice appear in all copies and   c
c  that both the copyright notice and this permission notice appear in c
c  supporting documentation.                                           c
c                                                                      c
c  Neither the Institutions  nor the Authors make any                  c
c  representations about the suitability of this software for any      c
c  purpose.  This software is provided ``as is'' without express or    c
c  implied warranty.                                                   c
c======================================================================c
      USE HOMOGENIZATION_XS 
      implicit none
      include '../Include/sketch.fh'
* Input Variables: 
      integer i_int, i_out, i_nonl
* Output Variables 
      integer i_source, i_ssor, i_nonl_tot, i_therm

      real time_sk, time_out
      integer i_trac_end, i_sk_end, tid_parent

      real a_dt_core
	real*8 dt_sketch_send

* Initialization of Computing Time
      call CPU_Timer( time_sk_cpu)
      time_sk_cpu = 0.
      time_th_cpu = 0.
      time_XS_cpu = 0.
      time_nonl_cpu = 0.
      time_cmfd_cpu = 0.
* End of Initialization of Computing Time

* Initial 
      i_source = 0
      i_ssor = 0
      i_nonl_tot = 0
      i_therm = 0

      call CTRL_Input_Neutron(i_sk_end, time_sk)

! TMP than go to CR module
!        OPEN(io_unit, file = 'Input\CR.inp', ACTION='READ')
!          CALL READ_CR_BANK_DATA(io_unit)
!        CLOSE(io_unit)


! TMP STEP PERTURBATION TO CHECK POINT KINETICS
! 	if(Problem_Type.EQ."Kinetics") call XS_Step_Perturbation

      if (TH_Model.EQ."Internal") then

         call CPU_Timer(time_sk_cpu)

         call THM_Init(FILE_INPUT, file_dmp_in,
     &           NZR_Core_Beg, NZR_Core_End, cool_heating, hz )

         call CPU_Timer(time_th_cpu)

      else if (TH_Model.EQ."SKAZKA") then

        write(*,*) 'SKAZKA Input IN'

        call CPU_Timer(time_sk_cpu)

!skazka        call YH_Input_Data(FILE_INPUT)
        IF(file_dmp_skazka_in.ne."") THEN
!skazka           CALL YH_read_dynvar(file_dmp_skazka_in)
        ELSE
!skazka            call yh_initial_dynvar
        END IF                      
!skazka        CALL YH_Get_Core_Feedbacks 


        call CPU_Timer(time_th_cpu)

        write(*,*) 'SKAZKA Input OUT'

      else if (TH_Model.EQ."External") then
           call PVM_SKETCH_Enroll(tid_parent)
           call PVM_SKETCH_Receive(tid_parent, i_sk_end, i_trac_end)
           if(dt_trac.lt.dt_sketch) dt_sketch = dt_trac ! 1st time_sk step determined by SKETCH
      end if


        i_trac_end = i_sk_end

*     END INPUT	

!
      CALL OUTput_Problem_Description

      CALL OUTput_NumResults(i_source,i_ssor, i_nonl_tot,
     &               i_therm, time_sk, 1 )
! Output into SKETCH.GRF file
      CALL GRaPhics_Write_INI_Data
      IF(Problem_Type.EQ."Kinetics".OR.Problem_Type.EQ."Burnup") THEN
        CALL GRaPhics_Write_Distr_Data(time_sk)
      END IF
      call CPU_Timer(time_output_cpu)

*     MAIN COMPUTING PROCEDURE

      do while(i_trac_end.ne.1)

      if(TH_Model.NE."None") then
            i_therm = i_therm + 1
      end if

      write(*,*) 'TEMPERATURE ITERATION NUMBER, time_sk = ',
     &                 i_therm,  time_sk
c         write(*,*)
c         write(*,*) 'NEUTRON CALCULATION'
* solution of the neutron diffusion equations
      IF (Problem_Type.EQ."Kinetics") then

c             CALL CPU_time_sk ( time_sk_begin )
              call Time_Step_Control(time_sk,i_int,i_out,i_nonl, 
     &          i_therm, i_trac_end, i_sk_end, tid_parent)

      ELSE IF(Problem_Type.EQ."Burnup") THEN

              call Burnup_Step_Control(time_sk,i_int,i_out,i_nonl, 
     &          i_therm, i_trac_end, i_sk_end, tid_parent)
	
	ELSE  
c            write(*,*) 'SOLVER IN'
             dt_sketch = 0.
             call CTRL_solver(i_out,i_int,i_nonl,i_sk_end, dt_sketch)
c            write(*,*) 'SOLVER OUT'

c            write(*,*) 'POWER IN'
             call POWer_Compute
             a_dt_core = 0.
c            write(*,*) 'POWER OUT'

      END IF

          i_ssor  = i_ssor + i_int
          i_source = i_source + i_out
          i_nonl_tot = i_nonl_tot + i_nonl

          write(*,*) 'Power Total =', p_total

          IF(Problem_Type.EQ."Kinetics".OR.Problem_Type.EQ."Burnup") 
     &       then

! Computing average feedbacks for output
            CALL THM_Compute_Average_Feedbacks

            call CPU_Timer( time_sk_cpu )

            if(jmod(i_therm,n_zap).eq.0) then
             CALL GRaPhics_Write_Distr_Data(time_sk)
            end if
            if(i_view_out.eq.1) then
             CALL OUTput_NumResults(i_source,i_ssor,
     &               i_nonl_tot, i_therm, time_sk, 0)
            end if
            call CPU_Timer( time_output_cpu )
          end if



       if(Problem_Type.EQ."Steady-State") then
           if(TH_Model.EQ."External") then
c             write(*,*) 'Steady-State '
c            write(*,*) 'SEND IN'
             dt_sketch_send = DBLE(dt_sketch)
             call PVM_SKETCH_Send(tid_parent,i_sk_end, dt_sketch_send)
c            write(*,*) 'SEND OUT'
             call PVM_SKETCH_Receive(tid_parent, i_sk_end, i_trac_end)
c            write(*,*) 'RECEIVE OUT'
          else if(TH_Model.EQ."Internal") then

            call CPU_Timer( time_sk_cpu )

            call THM_Set_Core_Power
            call THM_Compute_Time_Step( a_dt_core)
            call THM_Get_Core_Feedbacks
            call CPU_Timer( time_th_cpu )
            i_trac_end = i_sk_end

          else if(TH_Model.EQ."SKAZKA") then

            call CPU_Timer( time_sk_cpu )
	      write(*,*) 'SKAZKA IN'
!skazka            call YH_Set_Core_Power
!skazka            call YH_Set_Inlet
!skazka            call YH_one_step(a_dt_core,  data_scalar_skazka(1), 
!skazka     &             data_scalar_skazka(2) )
!skazka            CALL YH_Get_Core_Feedbacks 
            CALL THM_Prepare_Output_Dist 
	      write(*,*) 'SKAZKA OUT, ', data_scalar_skazka(1), 
     &                data_scalar_skazka(2)  

            call CPU_Timer( time_th_cpu )
            i_trac_end = i_sk_end

c           pause
           ELSE
            i_trac_end = i_sk_end
         END IF
 
       END IF

      END DO


*     END MAIN COMPUTING PROCEDURE

!      CALL OUTPUT_TH_MODEL_TMP

*     FINAL REMARKS

!        if(TH_Model.EQ."Internal") then 
!           call THM_Write_Restart_File
!        end if

* Neutron Diffusion Calculations
      call CPU_Timer( time_sk_cpu )

      IF(Problem_Type.NE."Kinetics")  THEN
!	.AND.Problem_Type.NE."Burnup") THEN
           CALL KIN_Normalize_Flux 
	     CALL OUTput_Compute_Average_Flux
	! Computing the Surface Flux for Output
	    IF( NonlinearIterations.EQ."Moon") THEN
            CALL HOM_Set_Surface_Flux
            CALL HOM_Average_Surface_Flux
          END IF  
      END IF

      CALL THM_Compute_Average_Feedbacks
      CALL OUTput_NumResults(i_source,i_ssor, i_nonl_tot,
     &               i_therm, time_sk, 0)
      IF(Problem_Type.NE."Kinetics") THEN
        IF( FLAG_XS_HOPMOGENIZATION ) THEN
          CALL HOM_XS_Compute
          CALL HOM_XS_OUTPUT 	 
        END IF
      END IF ! (Problem_Type.NE."Kinetics") 


      IF(Problem_Type.NE."Kinetics".AND.Problem_Type.NE."Burnup") THEN
! Output into SKETCH.GRF file
         time_out = 0.
         CALL GRaPhics_Write_Distr_Data(time_out)
      END IF

      IF(FILE_DMP_OUT_KIN.NE."") then
        if(Problem_Type.NE."Kinetics") then
                call KIN_Initialize(time_sk)
         else
                dt_save = dt_sketch
         end if      
      end if


      call OUTput_Write_Restart_File(time_sk)

!skazka      IF(TH_Model.EQ."SKAZKA".AND.file_dmp_skazka_out.ne."") 
!skazka     &       CALL yh_write_dynvar(file_dmp_skazka_out) 



      if(Problem_Type.NE."Kinetics".AND. File_Reference.NE."" ) then
* A comparison of the SKETCH-N results with the reference solution
             write(*,*) 'reference comparison'
             call OUTput_Reference_Comparison(i_nonl_tot, i_source)
      end if

      call CPU_Timer( time_output_cpu )
      Call CPU_Timer_Output


!         call MAT_Output_COO_Format
!	   write(*,*) 'Matrices are written in COO format'


!         if(Problem_Type.NE."Kinetics".AND. File_DMP_Out_Kin.NE."") 
!     &        call ADJ_Compute

 
         write(*,*) ' SKETCH-N Finished Calculations '
          
         if(TH_Model.EQ."External") then
               call PVM_SKETCH_Exit(tid_parent)
         end if

      STOP
	END

