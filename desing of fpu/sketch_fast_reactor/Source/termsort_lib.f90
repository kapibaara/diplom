MODULE XS_POL_FTYPES
	   
       USE PRECISION, ONLY : dp
       IMPLICIT NONE
       INTEGER, PARAMETER :: NN_FTYPE = 200
       INTEGER            :: N_FTYPE
       INTEGER            :: index_ftype(NN_FTYPE)
       INTEGER            :: index_internal(NN_FTYPE)
       CHARACTER*300      :: title_ftype(NN_FTYPE)

END MODULE XS_POL_FTYPES

module spline_lib

 USE PRECISION, ONLY : dp
 USE XS_POL_FTYPES

 PRIVATE

 INTEGER, PARAMETER :: NNKNOT=200
 INTEGER, PARAMETER :: N_SPLINE_ORDER=3
 INTEGER, PARAMETER :: max_ny=100

 INTEGER  :: NKNOT(NN_FTYPE), NY_SP(NN_FTYPE)

 REAL(dp) :: X_SP(1:NNKNOT, NN_FTYPE)

 REAL(dp) ::  SP_COEFF(1:max_ny, 1:NNKNOT-1, 0:N_SPLINE_ORDER, NN_FTYPE)

 PUBLIC :: spline_eval, SPLINE_INPUT

 CONTAINS

      SUBROUTINE SPLINE_INPUT(id, i_ftype)
      INTEGER, INTENT(IN) :: id, i_ftype
      CHARACTER           :: end_of_data

         READ(id, *) NKNOT(i_ftype), NY_SP(i_ftype)
         READ(id,*) X_SP(1:NKNOT(i_ftype), i_ftype)
         READ(id,*) SP_COEFF( 1:NY_SP(i_ftype), 1:NKNOT(i_ftype)-1, &
           0:N_SPLINE_ORDER, i_ftype) 
         READ(id,*) end_of_data 
      RETURN
      END SUBROUTINE SPLINE_INPUT 

      function spline_eval(m, u, i_ftype) result(value)
!     SP_COEFF goes through MODULE
      INTEGER, INTENT(IN) :: m, i_ftype
      INTEGER             :: N
! N   -  number of knots
! N-1 -  number of intervals
! M   -  number of y 
      REAL(dp), INTENT(IN) ::  u
!      REAL(dp), INTENT(IN) ::  y(m,n-1), b(m,n-1), c(m,n-1), d(m,n-1)
      REAL(dp) :: value(M), X(NNKNOT)
      integer i, im
      REAL(dp) ::  dx
!      data i/1/

     i = 1

  IF( M .GT. NY_SP(i_ftype) ) THEN
     write(*,*) 'i_ftype =', i_ftype
     write(*,*) 'Error! M .GT. NY_SP(i_ftype) :', M,  NY_SP(i_ftype)
     stop
  ELSE IF ( M .LT. NY_SP(i_ftype) ) THEN
     write(*,*) 'i_ftype =', i_ftype
     write(*,*) 'Warning! M .LT. NY_SP(i_ftype) :', M,  NY_SP(i_ftype)
  END IF

   N = NKNOT(i_ftype)

   X(1:N) = X_SP(1:N, i_ftype)
!
!  this subroutine evaluates the cubic spline function
!
!    seval = y(i) + b(i)*(u-x(i)) + c(i)*(u-x(i))**2 + d(i)*(u-x(i))**3
!
!    where  x(i) .lt. u .lt. x(i+1), using horner's rule
!
!  if  u .lt. x(1) then  i = 1  is used.
!  if  u .ge. x(n) then  i = n  is used.
!
!  input..
!
!    n = the number of data points
!    u = the abscissa at which the spline is to be evaluated
!    x,y = the arrays of data abscissas and ordinates
!    b,c,d = arrays of spline coefficients computed by spline
!
!  if  u  is not in the same interval as the previous call, then a
!  binary search is performed to determine the proper interval.
!

! Binary search should be changed in the fu 
!      SAVE :: i
!      if ( i .ge. n ) i = 1
!      if ( u .lt. x(i) ) go to 10
!      if ( u .le. x(i+1) ) go to 30
!
!  binary search
!
!   10 i = 1
!      j = n+1
!   20 k = (i+j)/2
!      if ( u .lt. x(k) ) j = k
!      if ( u .ge. x(k) ) i = k
!      if ( j .gt. i+1 ) go to 20
!
!  evaluate spline

     CALL XSR_Find_1D_Pointer( N, u, x, i)

!
   30 dx = u - x(i)

!   IF(u.eq.0) THEN 
!       write(*,*) 'i=', i
!       write(*,*) 'x(i) =', x(i) , 'dx=', u - x(i)
!       write(*,*) 'sp_coeff(1,i,0:3,i_ftype)=', sp_coeff(1,i,0:3,i_ftype)
!       pause
!   END IF 

!   IF(u.eq.60) THEN 
!       write(*,*) 'i=', i
!       write(*,*) 'x(i) =', x(i) , 'dx=', u - x(i)
!       write(*,*) 'sp_coeff(1,i,0:3,i_ftype)=', sp_coeff(1,i,0:3,i_ftype)
!       pause
!   END IF 

!	write(*,*) 'spline_eval i=', i
!	pause

      DO im = 1, M
!        value(im) = y(im,i) + dx*(b(im,i) + dx*(c(im,i) + dx*d(im,i)))
! SP_COEFF(NNY,NNROW,0:3)
        value(im) = sp_coeff(im,i,0,i_ftype) + dx*(sp_coeff(im,i,1,i_ftype) + &
          dx*(sp_coeff(im,i,2,i_ftype) + dx*sp_coeff(im,i,3,i_ftype)))
      END DO
      return

      END function spline_eval 

Subroutine XSR_Find_1D_Pointer( NP, x, End_Int, int_pointer)
!=====================================================================!
!      Finding Pointers  in a one-dimensional table                   !
!   (c) Slava 27.V.1999                                               !
!=====================================================================!
      implicit none
! Input : burnup, history_void,
      integer, INTENT(IN)  :: NP ! number of points (number of intervals = NP - 1)
      REAL(dp), INTENT(IN) :: End_Int(NP)
      REAL(dp), INTENT(IN) :: x ! Value
!     REAL(dp) BENTRY(ie,  n) - burnup entries
!     REAL(dp) CENTRY(ic,  n) - history void entries
! Output: 
      INTEGER, INTENT(OUT) :: int_pointer
! Local Variables
      LOGICAL Inside_Int

      int_pointer = 0
      Inside_Int  =  .False.

      do while ( .NOT. (Inside_int) )
  
         int_pointer = int_pointer + 1
         if( (int_pointer + 1) .EQ. NP ) then
           Inside_Int = .True. ! Extrapolation
         else
           Inside_Int  =  (x .LE. End_Int(int_pointer + 1) )
         end if         

      end do

      return
      end    Subroutine XSR_Find_1D_Pointer   


END MODULE spline_lib   

MODULE POLYNOMIAL_ARRAY

CONTAINS 

FUNCTION LEGENDRE_ARRAY(n,x) RESULT (pol)
USE PRECISION, ONLY : dp
IMPLICIT NONE
INTEGER,  INTENT(IN) :: n
REAL(dp), INTENT(IN) :: x 
REAL(dp), DIMENSION(0:n) :: pol

REAL(dp) :: pol0, pol1
INTEGER  :: m

IF( n .LT. 0) THEN
  WRITE(*,*) 'Computing Legendre Polynomials with negative power'
  STOP
END IF

pol0 = 1.
pol(0) = 1.
IF( n == 0) RETURN
pol1 = x
pol(1) = x
IF( n == 1) RETURN

DO m = 2, n
   pol(m) = ((2*m+1)*x*pol1 - m*pol0)/(m+1)
   pol0=pol1
   pol1=pol(m)
END DO

RETURN
END FUNCTION LEGENDRE_ARRAY     

FUNCTION CHEBYSHEV_ARRAY(n,x) RESULT (pol)
USE PRECISION, ONLY : dp
IMPLICIT NONE
INTEGER,  INTENT(IN) :: n
REAL(dp), INTENT(IN) :: x 
REAL(dp), DIMENSION(0:n) :: pol

REAL(dp) :: pol0, pol1
INTEGER  :: m

pol0 = 1.
pol(0) = 1.
IF(n==0) RETURN
pol1 = x
pol(1) = x
IF(n==1) RETURN

DO m = 2, n
   pol(m) = 2.*x*pol1 - pol0
   pol0=pol1
   pol1=pol(m)
END DO

RETURN
END FUNCTION CHEBYSHEV_ARRAY     


FUNCTION MONOMIAL_ARRAY(n,x) RESULT (pol)
USE PRECISION, ONLY : dp
IMPLICIT NONE
INTEGER,  INTENT(IN) :: n
REAL(dp), INTENT(IN) :: x 
REAL(dp), DIMENSION(0:n) :: pol

INTEGER  :: m

pol(0) = 1.
DO m = 1, n
   pol(m) = x*pol(m-1)
END DO

RETURN
END FUNCTION MONOMIAL_ARRAY     

END MODULE POLYNOMIAL_ARRAY

module termsort_lib

 USE PRECISION, ONLY : dp, eps_round_off, real_min_value
 USE XS_POL_FTYPES

 implicit none
 private

 integer, parameter :: max_nx=10, max_ny=100

 integer, parameter :: max_total_term=1000,max_term=200

 integer, parameter :: max_1d_power=10

 integer n_1d_power(max_nx, NN_FTYPE)
 real(dp) :: pol_1d_value( max_nx, 0:max_1d_power) 

 CHARACTER     :: pol_type(NN_FTYPE)
 CHARACTER*6   :: flag_spline(NN_FTYPE)

 
 integer n_y(NN_FTYPE) ! количество полиномов
 integer n_x(NN_FTYPE) ! количество независимых переменных
 integer n_total_term(NN_FTYPE) ! количество слагаемых во всех полиномах
! integer count
 integer pol_power( max_nx, max_total_term, NN_FTYPE)
 real(dp) :: xmin_scale( max_nx, NN_FTYPE ), xmax_scale( max_nx, NN_FTYPE )
 real(dp) pol_term_value(max_total_term)
 integer n_term(max_ny, NN_FTYPE)
 real(dp) pol_coeff(max_ny,max_term, NN_FTYPE)
 integer index_term(max_ny,max_term, NN_FTYPE)
 CHARACTER*18  x_title(max_nx, NN_FTYPE), y_title(max_ny, NN_FTYPE)

 real(dp) :: x_scale(max_nx)

  PUBLIC:: init_lib,ops_lib, write_n_term_lib, write_termref_lib, &
           scale_back
! Debug
  PUBLIC:: lib_flag_spline, get_lib_titles, get_lib_nx_ny
!  , y_title

contains

 function term_eq(a1,a2, n_x) result(eqv)
 logical eqv
 integer n_x 
 integer, intent(in):: a1(n_x),a2(n_x)
 integer i
 do i=1,n_x
  if( a1(i).ne.a2(i) )then
   eqv=.false.
   return
  endif
 enddo
 eqv=.true.
 end function term_eq

! subroutine term_rd(id,a1)
! integer id,i
! type(term) :: a1
! return
! end subroutine term_rd

 function term_val(n_pol_power,n_x) result( value )
 real(dp) value
 integer :: n_x
 integer :: n_pol_power(1:n_x)
 integer i
 
 value=1.
 do i=1,n_x
  value=value*pol_1d_value( i, n_pol_power(i) )
 enddo

 return
 end function term_val

 
 subroutine ops_lib(NX_IN, NY_IN, x, i_ext_ftype, y)
 USE POLYNOMIAL_ARRAY !  functions chebyshev_array, legendre_array, monomial_array
 USE SPLINE_LIB, ONLY : spline_eval 
 INTEGER, INTENT(IN)  :: NX_IN, NY_IN

 real(dp), INTENT(IN) :: x(NX_IN)
 integer,  INTENT(IN) :: i_ext_ftype
! character,INTENT(IN) :: polynomial_type
 real(dp), INTENT(OUT):: y(NY_IN)  
! Local
 REAL(dp) :: sum
 integer i,j, n, i_ftype
! EXTERNAL :: chebyshev_array, legendre_array, monomial_array
!  chebyshev_array, legendre_array, monomial_array

! Changing external fuel type numbering to internal
  i_ftype = index_internal(i_ext_ftype)
!------------------------------------------------------------------------------!
! First of all cheking NX_IN <= n_x(i_ftype) , NY_IN <= n_y(i_ftype)
!------------------------------------------------------------------------------!
  IF( NX_IN .GT. N_X(i_ftype) ) THEN
     write(*,*) 'i_ftype =', i_ftype
     write(*,*) 'Error! NX_IN .GT. N_X(i_ftype) :', NX_IN,  N_X(i_ftype)
     stop
  ELSE IF ( NX_IN .LT. N_X(i_ftype) ) THEN
     write(*,*) 'i_ftype =', i_ftype
     write(*,*) 'Warning! NX_IN .LT. N_X(i_ftype) :', NX_IN,  N_X(i_ftype)
  END IF

  IF( NY_IN .GT. N_Y(i_ftype) ) THEN
     write(*,*) 'i_ftype =', i_ftype
     write(*,*) 'Error! NY_IN .GT. N_Y(i_ftype) :', NY_IN,  N_Y(i_ftype)
     stop
  ELSE IF ( NY_IN .LT. N_Y(i_ftype) ) THEN
    write(*,*) 'i_ftype =', i_ftype
    write(*,*) 'Warning! NY_IN .LT. N_Y(i_ftype) :', NY_IN,  N_Y(i_ftype)
  END IF

!------------------------------------------------------------------------------!
! First of all scaling X
!------------------------------------------------------------------------------!
  CALL SCALE_X(NX_IN, i_ftype, x, x_scale)
!  write(*,*) 'x_scale =', x_scale(1:n_x(i_ftype))
!  pause
!------------------------------------------------------------------------------!
! Computing 1D polynomial terms
!------------------------------------------------------------------------------!
 IF( pol_type(i_ftype).eq. "M") THEN
    DO n = 1, NX_IN
       pol_1d_value(n, 0:n_1d_power(n,i_ftype) ) =  &
          monomial_array( n_1d_power(n,i_ftype), x_scale(n) )
    END DO
 ELSE IF (pol_type(i_ftype) .eq. "C") THEN
    DO n = 1, NX_IN
       pol_1d_value(n, 0:n_1d_power(n,i_ftype) ) =  &
          chebyshev_array( n_1d_power(n,i_ftype), x_scale(n) )
    END DO
 ELSE IF (pol_type(i_ftype).eq. "L") THEN
    DO n = 1, NX_IN
       pol_1d_value(n, 0:n_1d_power(n,i_ftype) ) =  &
          legendre_array( n_1d_power(n,i_ftype), x_scale(n) )
    END DO
 ELSE
   write(*,*) 'FUEL TYPE =', i_ftype
   write(*,*) 'polynomial_type=', pol_type(i_ftype)
    write(*,*) 'polynomial_type can be M, C or L'
    stop
 END IF

! computing multidimensional polynomial terms
 do i=1,n_total_term(i_ftype)
  pol_term_value(i)=term_val(pol_power(1,i,i_ftype),NX_IN )
 enddo

! write(*,*) 'pol_term_value(i)=', pol_term_value(1:5)
! pause

 do i=1, NY_IN
  sum=0.
  do j=1,n_term(i,i_ftype)
   sum=sum+pol_coeff(i,j,i_ftype)*pol_term_value(index_term(i,j,i_ftype))
  enddo
  y(i)=sum
 enddo

  IF( INDEX(flag_spline(i_ftype), "SPLINE") /=0 ) THEN
    y(1:NY_IN) = y(1:NY_IN) + spline_eval(NY_IN, x(1), i_ftype) 
!    WRITE(*,*) 'Spline =', spline_eval(NY_IN, x(1), i_ftype) 
!    pause
  END IF 

 return
 end subroutine ops_lib
 
 subroutine add_poly(id, iy)
 integer id, iy
 integer i,j
 integer :: a1(max_nx)
 logical flag_found

! write(*,*) 'iy =', iy, 'n_ftype  =', n_ftype
 read(id,*) n_term(iy, n_ftype)
! write(*,*) 'n_term(iy, n_ftype)=', n_term(iy, n_ftype)
 read(id,*) (pol_coeff(iy,j, n_ftype),j=1,n_term(iy,n_ftype))
 do i=1,n_term(iy, n_ftype)

  read(id,*) a1(1:n_x(n_ftype))
   
  index_term(iy,i, n_ftype) = 0
  flag_found = .False.
  do j=1,n_total_term(n_ftype)
   if( term_eq(a1, pol_power(1, j, n_ftype), n_x(n_ftype) ) )then 
    index_term(iy, i, n_ftype)=j
    flag_found =.True.
    exit
   endif
  enddo

!  if(j>n_total_term)then
!  if( index_term(n_y,i) == 0 ) THEN
   if( flag_found == .False. ) THEN
     n_total_term(n_ftype) = n_total_term(n_ftype)+1
     index_term(iy,i,n_ftype)=n_total_term(n_ftype)
     pol_power(1:n_x(n_ftype), n_total_term(n_ftype),n_ftype)=a1(1:n_x(n_ftype))
   endif

 enddo

! write(*,*) 'iy =', iy, 'n_ftype=', n_ftype  
! write(*,*) 'n_term(iy, n_ftype)=', n_term(iy, n_ftype)
! write(*,*) 'n_total_term(n_ftype)=', n_total_term(n_ftype)
! write(*,*) 'index_term(iy,i,n_ftype)=',  &
!       index_term(iy,1:n_term(iy, n_ftype),n_ftype)
! pause

 end subroutine add_poly

 subroutine init_lib(id)
 USE SPLINE_LIB
 integer id
 INTEGER iy, i, n, ios, i_ftype

 N_FTYPE = 1

 DO 
!
   read (id, FMT=*, IOSTAT=ios) index_ftype(n_ftype)
!   pause
   IF( ios /= 0 ) THEN 
     n_ftype = n_ftype - 1
     EXIT
   END IF
   write(*,*) 'index_ftype(n_ftype)=', index_ftype(n_ftype)

   index_internal(index_ftype(n_ftype))=n_ftype
   read(id,*) ! header
   read(id,'(A)') title_ftype(n_ftype)
   read(id,*) ! header

   read(id,*) pol_type(n_ftype)
!   write(*,*) 'pol_type(n_ftype)=', pol_type(n_ftype)

   read (id,*) n_x(n_ftype), n_y(n_ftype)! количество x! количество сечений

   read (id,'(5A18)') x_title(1:n_x(n_ftype),n_ftype)
   read (id,'(5A18)') y_title(1:n_y(n_ftype),n_ftype)

   read(id,*) xmin_scale(1:n_x(n_ftype), n_ftype)
   read(id,*) xmax_scale(1:n_x(n_ftype), n_ftype)

!   write(*,*) 'xmin_scale(1:n_x(n_ftype), n_ftype)', &
!                     xmin_scale(1:n_x(n_ftype), n_ftype)
!   write(*,*) 'xmax_scale(1:n_x(n_ftype), n_ftype)', &
!                     xmax_scale(1:n_x(n_ftype), n_ftype)
!   pause
!
   n_total_term(n_ftype)=0 !
   do iy=1, n_y(n_ftype)
      call add_poly(id, iy)
   enddo

   read(id,'(A)') flag_spline(n_ftype)

   IF( INDEX(flag_spline(n_ftype),"SPLINE") /= 0 ) THEN
       CALL SPLINE_INPUT(id, n_ftype)
   END IF

   n_ftype = n_ftype + 1

 END DO ! input

!  write(*,*) 'input of ', n_ftype, ' XS types'
  
!input finished

 DO i_ftype=1, n_ftype
 DO iy = 1, n_y(i_ftype)
!   write(*,*) 'iy, n_total_term', iy, n_term(iy, i_ftype)
!   write(*,'(20I4)') index_term(iy,1:n_term(iy,i_ftype),i_ftype)
 END DO

 n_1d_power(:,i_ftype) = 0
 DO i = 1, n_total_term(i_ftype)
    DO n = 1, n_x(i_ftype)
       n_1d_power(n,i_ftype) = MAX( pol_power(n, i,i_ftype), &
            n_1d_power(n,i_ftype) )
    END DO
 END DO
 
! WRITE(*,*) 'maximum polynomial power ',  n_1d_power(1:n_x(i_ftype), i_ftype)     
 end do ! n_ftype

 RETURN
 end subroutine init_lib

 subroutine write_n_term_lib(io_unit, i_ext_ftype)
 INTEGER, INTENT(IN):: io_unit, i_ext_ftype
 integer n, i_ftype

 i_ftype = index_internal(i_ext_ftype)
 
 DO n = 1, n_y(i_ftype)
    write(io_unit, '(1x,2I5)') n, n_term(n,i_ftype)
 END DO 

 return 
 end subroutine write_n_term_lib

 subroutine write_termref_lib(io_unit, i_ext_ftype)
! integer index_term(max_ny,max_term)
 INTEGER, INTENT(IN):: io_unit, i_ext_ftype
 integer n, j, i_ftype

 i_ftype = index_internal(i_ext_ftype)
 DO n = 1, n_y(i_ftype)
    do j = 1, n_term(n, i_ftype)
    write(io_unit, '(1x,2I5)') n, index_term(n,j,i_ftype)
    end do
 END DO 
 end subroutine write_termref_lib

 SUBROUTINE SCALE_X(NX, i_ftype, X, x_scale)
!=========================================================================!
! Scale of the original data, XMAX, XMIN are given                        !
!=========================================================================!

! xmin_scale( max_nx, NN_FTYPE ), xmax_scale( max_nx, NN_FTYPE ) 
  INTEGER, INTENT(IN) :: NX, i_ftype
  REAL (dp), INTENT(IN) :: X(1:NX)
  REAL (dp), INTENT(OUT) :: x_scale(1:NX)

  INTEGER :: i
  REAL(dp)    :: bb, aa, delta_xmax_xmin

  DO i = 1, NX
!      write(*,*) 'i,i_ftype=', i,i_ftype
!      write(*,*) 'xmin_scale, xmax_scale=', xmin_scale(i,i_ftype), &
!                    xmax_scale(i,i_ftype)
  IF( ABS(xmax_scale(i,i_ftype)) .GT. REAL_MIN_VALUE ) THEN
     delta_xmax_xmin = ABS( (xmax_scale(i,i_ftype) - xmin_scale(i,i_ftype)) &
     /xmax_scale(i,i_ftype) )
  ELSE
     delta_xmax_xmin = ABS( xmax_scale(i,i_ftype) )
  END IF    
      IF( delta_xmax_xmin .GT. EPS_ROUND_OFF ) THEN
      aa = 2./(xmax_scale(i,i_ftype) - xmin_scale(i,i_ftype))
      bb = -  (xmax_scale(i,i_ftype) + xmin_scale(i,i_ftype) )/ &
             ( xmax_scale(i,i_ftype) - xmin_scale(i,i_ftype))
      X_SCALE(i)= aa*X(i)+bb
      ELSE
        X_SCALE(i)= X(i)
      END IF
  END DO

END SUBROUTINE SCALE_X

SUBROUTINE SCALE_BACK(NX, i_ext_ftype, ksi, x)
!=========================================================================!
! External procedure i_ext_ftype  
! recover the original data from the scaled, XMAX, XMIN are given                        !
!=========================================================================!

! xmin_scale( max_nx, NN_FTYPE ), xmax_scale( max_nx, NN_FTYPE ) 
  INTEGER, INTENT(IN) :: NX, i_ext_ftype
  REAL (dp), INTENT(IN) :: ksi(1:NX)
  REAL (dp), INTENT(OUT) :: x(1:NX)

  INTEGER :: i, i_ftype
  REAL(dp)    :: bb, aa, delta_xmax_xmin

  i_ftype = index_internal(i_ext_ftype)
  DO i = 1, NX
  IF( ABS(xmax_scale(i,i_ftype)) .GT. REAL_MIN_VALUE ) THEN
     delta_xmax_xmin = ABS( (xmax_scale(i,i_ftype) - xmin_scale(i,i_ftype)) &
     /xmax_scale(i,i_ftype) )
  ELSE
     delta_xmax_xmin = ABS( xmax_scale(i,i_ftype) )
  END IF    
      IF( delta_xmax_xmin .GT. EPS_ROUND_OFF ) THEN
!      write(*,*) 'i,i_ftype=', i,i_ftype
!      write(*,*) 'xmin_scale, xmax_scale=', xmin_scale(i,i_ftype), &
!                    xmax_scale(i,i_ftype)
      aa =  xmax_scale(i,i_ftype) - xmin_scale(i,i_ftype)
      bb =  xmax_scale(i,i_ftype) + xmin_scale(i,i_ftype) 
      X(i)= 0.5*(aa*ksi(i)+bb)
      ELSE
      X(i)= ksi(i)
      END IF
  END DO

END SUBROUTINE SCALE_BACK

LOGICAL FUNCTION  lib_flag_spline(i_ext_ftype)
INTEGER, INTENT (IN) :: i_ext_ftype
INTEGER              :: i_ftype 

 i_ftype = index_internal(i_ext_ftype)
 
 IF( flag_spline(i_ftype)=="SPLINE" ) THEN
   lib_flag_spline = .True.
 ELSE
   lib_flag_spline = .False.
 END IF     

RETURN
END FUNCTION lib_flag_spline 


SUBROUTINE get_lib_nx_ny(i_ext_ftype, NX, NY)
INTEGER, INTENT (IN) :: i_ext_ftype
INTEGER, INTENT(OUT) :: NX, NY
INTEGER :: i_ftype

! write(*,*) 'i_ext_ftype=', i_ext_ftype
! write(*,*) 'index_internal(8)=', &
! index_internal(i_ext_ftype)
! pause
 i_ftype = index_internal(i_ext_ftype)
! write(*,*) 'i_ftype =', i_ftype, 'i_ext_ftype=', i_ext_ftype
 NX = n_x(i_ftype)
 NY = n_y(i_ftype)

RETURN
END SUBROUTINE get_lib_nx_ny   

SUBROUTINE get_lib_titles(i_ext_ftype, title_x, title_y)
!=========================================================================!
! External procedure i_ext_ftype  
! recover the original data from the scaled, XMAX, XMIN are given                        !
!=========================================================================!

! xmin_scale( max_nx, NN_FTYPE ), xmax_scale( max_nx, NN_FTYPE ) 
  INTEGER, INTENT(IN)       :: i_ext_ftype
  CHARACTER*18, INTENT(OUT) :: title_x(*), title_y(*)

  INTEGER :: i_ftype

  i_ftype = index_internal(i_ext_ftype)

  title_x(1:n_x(i_ftype)) = x_title(1:n_x(i_ftype), i_ftype)
  title_y(1:n_y(i_ftype)) = y_title(1:n_y(i_ftype), i_ftype)


END SUBROUTINE get_lib_titles

end module termsort_lib

!programm main
!use termsort 
!real x(4),y(11)
!data x/0,1,1,1/
!open(10,file='polynom.dat')
!call init(10)
! x(1)=0
! call ops(x,y)
! x(1)=-0.5
! call ops(x,y)
!stop
!end

