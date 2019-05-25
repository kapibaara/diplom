      MODULE CR_POSITION

        INTEGER, PARAMETER   :: NN_CR_BANK=100, NN_CR_in_Bank=100
	  INTEGER              :: N_CR_BANK, N_CR_TOTAL
        INTEGER              :: N_CR_in_Bank(NN_CR_BANK) 
  	  INTEGER              :: N_CR_POS(NN_CR_in_Bank,NN_CR_BANK) 
        INTEGER              :: N_CR_TYPE_in_BANK(NN_CR_BANK)  
        REAL                 :: BANK_POS(NN_CR_BANK), 
     &                          CR_POS(NN_CR_in_Bank,NN_CR_BANK)
        REAL                 :: ONE_PERCENT_CORE
!     &                          H_AXIAL_REFLECTOR=35.5            
	  INTEGER              :: Index_CR_IN_Bank( NN_CR_in_Bank, 
     &                                                  NN_CR_BANK )
	  LOGICAL              :: Input_Bank_Position, 
     &   Bank_Position_in_Percents, Bank_Position_Absolute

      END MODULE CR_POSITION
         
      SUBROUTINE SET_CR_POSITION(i_bank, position )
      USE CR_POSITION
      IMPLICIT NONE
      include '../Include/sketch.fh'

      INTEGER, INTENT(in) :: i_bank
      REAL,    INTENT(in) :: position

	INTEGER :: n, i_cr

!        DO n = 1,  
        CR_POS(1:N_CR_in_Bank(i_bank) ,i_bank)=HZ_AXIAL_REFLECTOR(1)+
     &      position

	 DO n = 1, N_CR_in_Bank(i_bank)
	    i_cr = Index_CR_IN_Bank( n,  i_bank )
	    Mov_Rods(i_cr) = .True.
	    zrods(i_cr) = CR_POS(n,i_bank)
       END DO
	    
      RETURN
      END SUBROUTINE SET_CR_POSITION

      SUBROUTINE SET_ALL_CR_POSITION_ABS
      USE CR_POSITION
      IMPLICIT NONE
      include '../Include/sketch.fh'

      INTEGER :: i_bank
!      REAL,    INTENT(in) :: position

	INTEGER :: n, i_cr

!        DO n = 1,  

       DO i_bank = 1, N_CR_BANK
        CR_POS(1:N_CR_in_Bank(i_bank) ,i_bank)=HZ_AXIAL_REFLECTOR(1)+
     &      BANK_POS(i_bank)

        DO n = 1, N_CR_in_Bank(i_bank)
	    i_cr = Index_CR_IN_Bank( n,  i_bank )
	    Mov_Rods(i_cr) = .True.
	    zrods(i_cr) = CR_POS(n,i_bank)
        END DO
       END DO
	    
      RETURN
      END SUBROUTINE SET_ALL_CR_POSITION_ABS

      SUBROUTINE SET_ALL_CR_POSITION_PERC
      USE CR_POSITION
      IMPLICIT NONE
      include '../Include/sketch.fh'

      INTEGER :: i_bank
!      REAL,    INTENT(in) :: position

	INTEGER :: n, i_cr

       ONE_PERCENT_CORE =  hz_core / 100.

       DO i_bank = 1, N_CR_BANK
        CR_POS(1:N_CR_in_Bank(i_bank) ,i_bank)=HZ_AXIAL_REFLECTOR(1)+
     &      (100.-BANK_POS(i_bank))*ONE_PERCENT_CORE

  	DO n = 1, N_CR_in_Bank(i_bank)
	    i_cr = Index_CR_IN_Bank( n,  i_bank )
	    Mov_Rods(i_cr) = .True.
	    zrods(i_cr) = CR_POS(n,i_bank)
        END DO
       END DO
	    
      RETURN
      END SUBROUTINE SET_ALL_CR_POSITION_PERC

      SUBROUTINE SET_INDEX_CR_in_BANK
      USE CR_POSITION
      IMPLICIT NONE

	INTEGER :: n, i_cr, nn

	  i_cr = 0
        DO  n = 1, N_CR_BANK
	     DO  nn = 1, N_CR_in_Bank(n)
	       i_cr = i_cr + 1
		   Index_CR_IN_Bank( nn, n ) = i_cr
           END DO		     
        END DO

      RETURN
      END SUBROUTINE SET_INDEX_CR_in_BANK   