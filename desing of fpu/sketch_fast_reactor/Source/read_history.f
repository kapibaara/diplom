        MODULE core_history
! experimental data
	  REAL    :: t_eff, p_reactor_hist, hx_hist, cb, t_in, HZAS	 
	  INTEGER :: NZAS, MZAS, MZAS2
          INTEGER :: i_day, i_month, i_year
          REAL :: t_eff_old, t_in_old
          REAL, PARAMETER :: t_in_min = 20.
          REAL, PARAMETER :: p_reactor_min = 3.E-03
! calculations
         INTEGER :: days
	 REAL    :: pow_old, pow_new, power_reference,
     &              time_eff_comp, time_eff_comp_old, 
     &              days_eff

         INTEGER days_old, days_new
         REAL  :: burnup_hist
         REAL  :: vol_reactor
         PARAMETER( vol_reactor = 2.791069E+07 ) 
         PARAMETER( power_reference = 3000. )! MWt
         CHARACTER*100 file_burnup_history 


        END MODULE core_history

        SUBROUTINE read_core_history(Error, dt)

        USE core_history
        USE CR_POSITION

        IMPLICIT NONE
        include '../Include/sketch.fh'
!        include '../yh.fh'

        LOGICAL, INTENT(INOUT) :: error
	  REAL   , INTENT(OUT)   :: dt
        INTEGER, SAVE :: N_LINE_DATA 

        INTEGER, PARAMETER :: N_LINE_HEADER = 3
	  INTEGER            :: N_SKIP_LINE
!        INTEGER, PARAMETER :: io_unit = 3
        INTEGER, EXTERNAL :: NDAYS
        REAL :: pow_dens, h_out
	  INTEGER :: i_bank

!	LOGICAL :: flag_euler_exp = .tru


        OPEN(io_unit+1, file ='Output\days.txt', action='write', 
     &    ACCESS='APPEND')
        OPEN(io_unit+2, file ='Output\burnup_out.txt', 
     &   action='write', ACCESS='APPEND')

        N_SKIP_LINE = N_LINE_DATA + N_LINE_HEADER
        CALL  read_core_history_line(n_skip_line,  error)

        IF (.NOT.error) THEN
           N_LINE_DATA = N_LINE_DATA + 1
!	     write(*,*) ' N_LINE_DATA =', N_LINE_DATA
          IF( p_reactor_hist < p_reactor_min ) 
     & 	       p_reactor_hist=p_reactor_min


           IF( N_LINE_DATA == 1) THEN
! set up minimum reactor power = MKU = 1.E-06*100%

                 pow_old = p_reactor_hist
                 days_old = NDAYS(i_year, i_month, i_day)
                 time_eff_comp = 0.
                 t_eff_old = t_eff
	           days = 1
!              
                 burnup_hist = 0.              
                 time_eff_comp_old = time_eff_comp
!
! there are some errors in the input data, so we set t_in = previous value
              IF( t_in < t_in_min ) t_in = t_in_old
              t_in_old = t_in   
           ELSE
              pow_new = p_reactor_hist
              days_new = NDAYS(i_year, i_month, i_day)
              days = days_new - days_old
              IF(days < 0 ) THEN 
    	        WRITE(*,'(I2,".", I2,".", I4,2F8.2)') 
     &             i_day, i_month, i_year
                WRITE(*,*) 'Error in date, days < 0'
                STOP
              END IF 
              IF( t_eff < t_eff_old ) THEN
    	        WRITE(*,'(I2,".", I2,".", I4,2F8.2)') 
     &             i_day, i_month, i_year
                WRITE(*,*) 'Error in t_eff, t_eff < t_eff_old'
                STOP
              END IF
              CALL compute_effective_days(days,  pow_old, pow_new, 
     &                              power_reference ,time_eff_comp)
              
              days_eff = days
!			time_eff_comp - time_eff_comp_old
! trapezoidal rule
!              pow_dens = 0.5*( pow_old + pow_new)*1.E+6/vol_reactor
! explicit Euler
              pow_dens = pow_old*1.E+6/vol_reactor
              
              call compute_burnup( days_eff, pow_dens, burnup_hist )

              pow_old = pow_new
              days_old = days_new
              t_eff_old = t_eff
              time_eff_comp_old = time_eff_comp
  	      WRITE(io_unit+1,'(I2,".", I2,".", I4,5F8.2)') 
     &  i_day, i_month, i_year, 
     &  t_eff, time_eff_comp, pow_dens, burnup_hist	 
	  WRITE(io_unit+2,'(I2,".", I2,".",I4,5F8.2,3I3,F8.2)') 
     &  i_day, i_month, i_year, 
     &  t_eff, p_reactor_hist, hx_hist, cb, t_in, NZAS, MZAS, MZAS2,
     &  HZAS	 


	     END IF  !  N_LINE_DATA == 1

! setting up the data for SKETCH
	     p_reactor = p_reactor_hist

! CR
! set all CR position OUT from the core
           H_OUT = 355.
           DO i_bank = 1, N_CR_BANK
	        CALL SET_CR_POSITION(i_bank, h_out)  
           END DO
! end  set all CR position OUT from the core

           CALL SET_CR_POSITION(10, hx_hist )
	      IF(NZAS /= 0) THEN
                CALL SET_CR_POSITION(MZAS, HZAS )
	          WRITE(*,'(I2,".", I2,".",I4,2I3,F8.2)') 
     &           i_day, i_month, i_year, NZAS, MZAS, HZAS	 
!	          READ(*,*)
            END IF
! dt

            dt = days
! T_in
           IF(TH_Model.EQ."SKAZKA") then
!skazka              CALL YH_Set_Inlet_history(t_in)
           ELSE IF(TH_Model.EQ."Internal") then
	        CALL THM_Set_Core_Inlet(t_in)
           END IF
              		  
!          ELSE
            
          END IF ! (.NOT.error)
!        END DO
        WRITE(*,*) 'Number of lines with data =', N_LINE_DATA
        CLOSE(io_unit+1)
        CLOSE(io_unit+2)

        RETURN
        END SUBROUTINE read_core_history
        

        SUBROUTINE read_core_history_line(n_skip_line, error)

        USE core_history

        IMPLICIT NONE

        INTEGER, INTENT(in)  :: n_skip_line
	  INTEGER, PARAMETER   :: io_unit = 2
	  INTEGER              :: n
        LOGICAL, INTENT(OUT) :: error
        CHARACTER*100 line  
        INTEGER           :: n_char, n_char_old,n_char_point
        INTEGER, EXTERNAL :: n_point

        IF( file_burnup_history  == "") THEN
         call MSC_ERR_Add_Message('ERROR',' File name '//
     &    'of the burnup history is not specified in SKETCH.INI' )
        END IF                  
 
        OPEN(io_unit, file =file_burnup_history, status='old', 
     &        action='read')

        DO n = 1, N_SKIP_LINE
          read(io_unit,*) 
        END DO

        Error = .False.
        read(io_unit, FMT='(A)', ERR=100, END=100 ) LINE
! 
        n_char = 1
        n_char_old = n_char
        n_char_point = N_POINT(line, n_char) 
        read(LINE(n_char_old:n_char_point -1),*) i_day

        n_char = n_char_point + 1
        n_char_old = n_char
        n_char_point = N_POINT(line, n_char) 
        read(LINE(n_char_old:n_char_point -1),*) i_month

        n_char = n_char_point + 1
        n_char_old = n_char
!        n_char_point = N_POINT(line, n_char) 
!        read(LINE(n_char_old:n_char_point -1),*) i_year
        read(LINE(n_char_old:n_char_old+1),*) i_year

!	write(*,*) 'i_year =', i_year
!	pause  

        IF( i_year <= 100 )  THEN
          IF( i_year > 50 ) THEN
            i_year = i_year + 1900
          ELSE 
            i_year = i_year + 2000
          END IF
        END IF

!        write(*,'(A,I2,".", I2,".", I4)') 'Date = ', 
!     &   i_day, i_month, i_year


        read(LINE(n_char_old+2:),*) t_eff, p_reactor_hist, hx_hist, 
     &      cb, t_in, NZAS, MZAS, MZAS2, HZAS	 

	  WRITE(*,'(I2,".", I2,".", I4,5F8.2,3I3,F8.2)') 
     &  i_day, i_month, i_year, 
     &  t_eff, p_reactor_hist, hx_hist, cb, t_in, NZAS, MZAS, MZAS2,
     &  HZAS	 

        GO TO 200 

  100   Error = .True.

  200   CONTINUE

      CLOSE(io_unit)
   
      RETURN
      END SUBROUTINE read_core_history_line
         
      INTEGER FUNCTION NDAYS(year, month, day)
!======================================================================!
! number of days since 1 March 1600 year to current date               !
!  (year, month, day)	                                               !
!======================================================================!
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: year, month, day
       INTEGER             :: C, D, M, X, y

       x=day
       IF( month.LE.2) THEN
         m=month+10
         y=year-1
       ELSE
         m=month-2
         y=year
       END IF
       c=y/100
       d=y - C*100
       write(*,*) 'X, C, D, M=', X, C, D, M
       NDAYS = 365*(100*(C-16)+D) + 24*C + C/4 + D/4
     &                     + 30*M + ((3*M-1)/5) + X - 419

       RETURN
       END

       INTEGER FUNCTION N_POINT(line, n_char_in)
! functionn  to get the number of position in line with "."
! search start with the position n_char
       CHARACTER*100 line  
       INTEGER, INTENT(IN) :: n_char_in
       INTEGER :: n_char
 
        n_char = n_char_in
        DO  
        IF( LINE(n_char:n_char) == "." ) THEN 
            n_point = n_char
            EXIT
        ELSE
          n_char = n_char + 1
          IF( n_char == 100 ) THEN
            WRITE(*,*) 'could not read the date in the line' 
            WRITE(*,*) TRIM(line)
            STOP
          END IF 
        END IF 
        END DO  
        END FUNCTION N_POINT

       SUBROUTINE compute_effective_days(days,  pow_old, pow_new, 
     &   power_reference ,time_eff_comp )
       INTEGER, INTENT(IN)    :: days
       REAL,    INTENT(IN)    :: pow_old, pow_new, power_reference
       REAL, INTENT(INOUT) :: time_eff_comp 
      time_eff_comp =  time_eff_comp + 0.5*days*(pow_new + pow_old)/
     &    power_reference
       END SUBROUTINE compute_effective_days
         

      SUBROUTINE compute_burnup( days, pow_dens, burnup )
      REAL, PARAMETER :: hm_mass_of_Fuel = 2.41163E-3
      REAL, PARAMETER :: convert_Wt_to_MWt = 1.E-06
      REAL, INTENT(IN) :: days, pow_dens
      REAL, INTENT(INOUT) :: burnup
      
      burnup = burnup + pow_dens*convert_Wt_to_MWt*days/
     &              hm_mass_of_fuel

      RETURN
      END SUBROUTINE compute_burnup
      
