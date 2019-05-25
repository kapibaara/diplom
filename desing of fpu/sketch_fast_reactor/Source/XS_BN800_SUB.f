C Program for interpolation power production
C for a defining max values in bounderies
      SUBROUTINE XS_init_lib_bn800 (file_unformatted_xs)
      
      IMPLICIT NONE  
! SLAVA
      include '../Include/sketch.fh'

      character*100 file_unformatted_xs
    
      
      INTEGER N, N1, K , ij, i, j, typ



      INTEGER, PARAMETER ::  o_unit_unform=123
      INTEGER, PARAMETER ::  NZ_B=18, NH_B = 978, NG_B=26, NG_B_TH=325

    
      INTEGER NFMAKR, NFDOP
      DATA NFMAKR/12/, NFDOP/765/
 

      CHARACTER *8 NAME, nmchir, nmsfnr, nmdifr, NMSCR, nmstrr, NMSFR,
     & NMDIFD, NMSADD, NMSFND, NMSTRD, NMSCD, NMSFD, NMCHID	

	REAL 
     &  chi_b(NZ_B,NH_B,NG_B), 
     &  sfn_b(NZ_B,NH_B,NG_B),      dif_b(NZ_B,NH_B,NG_B),  
     &  sf_b(NZ_B,NH_B,NG_B) ,       sc_b(NZ_B,NH_B,NG_B),
     &  sik_b(NZ_B,NH_B,NG_B,NG_B), str_b(NZ_B,NH_B,NG_B_TH),
!----------------------------------------------Misha
     &  str_b_1(3,325), sik_b_1(3,NG_B,NG_B),
     &  sad(18,10)
!----------------------------------------------end_Misha
      DATA
     & NAME   /'        '/,
     & nmchir /'CHI     '/,
     & nmsfnr /'SFN     '/,
     & nmdifr /'DIF     '/,  
     & NMSCR  /'SC      '/, 
     & nmstrr /'STR     '/,
     & NMSFR  /'SF      '/,
!----------------------------------------------Misha
     & NMDIFD /'DDF     '/,
     & NMSADD /'DAD     '/,
     & NMSFND /'DFN     '/,
     & NMSTRD /'DTR     '/,
     & NMSCD  /'DC      '/,
     & NMSFD  /'DF      '/,
     & NMCHID /'DHI     '/
!----------------------------------------------end_Misha 

!----------------------------------------------Misha
  
      IF ( FILE_CD_REFL /= "") THEN
       OPEN(NFDOP,FILE=FILE_CD_REFL ,STATUS='UNKNOWN',
     & ACCESS='DIRECT',RECL=4000)

       
        do n=1,325
          call macnam (NMSTRD,n,name)
          call fread4 (NFDOP,name,sad,ij)
         str_b_1(1,n)=sad(1,8)
         str_b_1(2,n)=sad(1,1)
         str_b_1(3,n)=sad(1,6)
        enddo



        n=1
	do k=1,3
	 n=1
	  do j=2,NG_B
	    do i=1,j-1
	     sik_b_1(k,i,j)=str_b_1(k,n)
	     if (n.ne.NG_B_TH)then
	      n=n+1
	     endif
	    enddo
	  enddo
	enddo
       
       do k=17605,17607
       do j=1,NG_B
        do i=1,NG_B
         sik( k, j, i ) = sik_b_1(k-17604,i,j)
        enddo
       enddo
       enddo
        


      do n=1, 26
         call macnam (NMSCD,n,name)
         call fread4 (NFDOP,name,sad,ij)
         sa(17605,n)=sad(1,8)
         sa(17606,n)=sad(1,1)
         sa(17607,n)=sad(1,6)
         sf_p(17605,n) = 0.035*sa(17605,n) 
         sf_p(17606,n) = 0.035*sa(17606,n)
         sf_p(17607,n) = 0.035*sa(17607,n)
      enddo
      do n=1, 26   
       call macnam (NMDIFD,n,name)
       call fread4 (NFDOP,name,sad,ij)
       d(17605,n)=sad(1,8)
       d(17606,n)=sad(1,1)
       d(17607,n)=sad(1,6)
      enddo
       
      CLOSE(NFDOP)

      END IF ! IF ( FILE_CD_REFL /= "") THEN
!----------------------------------------------end_Misha      


     
! END SLAVA
	 
      OPEN(NFMAKR,FILE=TRIM(file_unformatted_xs), 
     &   STATUS='UNKNOWN',ACCESS='DIRECT',RECL=4000)

      do 8 n=1,NG_B
 
      call macnam (nmchir,n,name)
      call fread4 (nfmakr,name,chi_b(1,1,n),ij)
        if(ij.ne.0) stop 8      
      call macnam (nmsfnr,n,name)
      call fread4 (nfmakr,name,sfn_b(1,1,n),ij)
        if(ij.ne.0) stop 8      
      call macnam (nmdifr,n,name)
      call fread4 (nfmakr,name,dif_b(1,1,n),ij)
        if(ij.ne.0) stop 8       
      call macnam (nmsfr,n,name)
      call fread4 (nfmakr,name,sf_b(1,1,n),ij)
        if(ij.ne.0) stop 8      
      call macnam (nmscr,n,name)
      call fread4 (nfmakr,name,sc_b(1,1,n),ij)
        if(ij.ne.0) stop 8      
        
    8 continue

      do 9 n=1,NG_B_TH
       call macnam (nmstrr,n,name)
       call fread4 (nfmakr,name,str_b(1,1,n),ij)
       if(ij.ne.0) stop 9             
    9 continue

        
	
	n=1
	do k=1,NH_B
	 do n1=1,NZ_B 
	  n=1
	  do j=2,NG_B
	    do i=1,j-1
	     
	       sik_b(n1,k,i,j)=str_b(n1,k,n)
	     if (n.ne.NG_B_TH)then
	     n=n+1
	     endif
	    enddo
	  enddo
	 enddo
	enddo
!	OPEN(123,FILE='macro_26_nod_capture.txt')
!	OPEN(124,FILE='macro_sik.txt')
!	write(123,*)'NG_B Groups'
!	write(123,998)
	typ=1
	do k=1,NH_B
	  !write(123,996)
	  do n1=1,NZ_B
!	    write(123,997)
		do n=1,NG_B
!	    write(123,999)dif_b(n1,k,n),(sc_b(n1,k,n)+sf_b(n1,k,n)),
!     *          sfn_b(n1,k,n),(sf_b(n1,k,n)+0.035*sc_b(n1,k,n))

                d   (typ,n) = dif_b(n1,k,n)
                sa  (typ,n) = sc_b (n1,k,n) +       sf_b(n1,k,n)
                sf  (typ,n) = sfn_b(n1,k,n)
                sf_p(typ,n) = sf_b (n1,k,n) + 0.035*sc_b(n1,k,n) 
!     &           (d(k,n), sa(k,n), sf(k,n), sf_p(k,n), 
!     &                          n = 1, NG)
		enddo
!	    write(123,997)
	      do j=1,NG_B
	      do i=1,NG_B
!	        write(123,995)sik_b(n1,k,i,j)
                 sik( typ, j, i ) = sik_b(n1,k,i,j)
!                WRITE(123,995) 
!     &           ((sik(k,n,m), m = 1, NG), n = 1, NG), i
	        if (i.eq.NG_B)then
!	            write(123,997)
	        endif
	      enddo
	      enddo
!             WRITE(124,'(26ES14.5)') 
!     &           ((sik(typ,i,j), j = 1, NG), i = 1, NG)
!             WRITE(124,*) 'type=', typ

!	  write(123,993)typ
	  typ=typ+1
	  enddo
	 !write(123,*)'k=',k,'k*n1=',(k*(n1-1))
	enddo
      
      CLOSE(NFMAKR)
!      CLOSE(o_unit_unform)
      
!      STOP 0

  101 FORMAT(/10X,'Module Q  started')
!  102 FORMAT(2X,'NVW=',A8,'  NFDR=',I2,' NMDR=',A8)
  103 FORMAT(2X,'NFFGR= ',I2,' NMFGR=',A8)
  104 FORMAT(2X,'   PW= ',1p1g12.5)
  105 FORMAT(2X,'NGTOT=',I2,' NTH=',I5,' NKAS=',I3,' NTY=',I2,' NTZ='
     *,I2,' H=',F6.3,' WTOT=',E12.5)
  106 FORMAT(2X,'NS=',(25I3))
  107 FORMAT(2X,'DZZ=',(8E12.5))
  109 FORMAT(2X,' NKAS=',I5,' NTZ=',I2
     *,' NKASI=',I5,' NKASE=',I5)
  110 FORMAT (2X,'SOSTAV=',(9E12.5))
  111 FORMAT(2X,'KOMPS=',(15I4))
  112 FORMAT(/10X,'Module Q ended')
  !999 FORMAT((9E17.10),2x)   
  999 FORMAT((8E12.5),2x)
  998	FORMAT(2x,'DIF',15x,'SC',15x,'SnF',15x,'SF')   
  997 FORMAT(\,1/)   
  996	FORMAT(1/)
  !995 FORMAT(\,(9E17.10),2x)
  995 FORMAT(\,(8E12.5),2x)
  994 FORMAT(\,I7,2x)
  993 Format(I7,1x,'Type')	

      RETURN 
      END

      SUBROUTINE FWRITE (N,NAME,ARRAY,L)
C *********************************************************************
C Writing of L words of array ARRAY into file N with name NAME
C The first three recordings are the file catalog
C MNAME - array of names,(MNZ - array of starts,ML - array of recording lengths
C NPI-last recording number,NCZ-free recording number,LZ-recording length
C *********************************************************************
      character*8
     *NAME,MNAME,HOUR
      character*4
     *MNAME4
c      character*8 HOUR
c      REAL*8 NAME,MNAME
      character*10 DAT
      dimension
     *MNZ(499),DAT(2),ID(2)
      DIMENSION
     *MNAME(500),MNAME4(1000)
     *,MS(2000),S(2000),ML(499),MD(499),ARRAY(L),MH(499)
      EQUIVALENCE
     *(MS(1),NPI),(MS(2),NCZ),(MS(3),ML(1)),(MS(502),MNZ(1))
     *,(MS(1001),MD(1)),(MS(1501),MH(1))
     *,(HOUR,DAT(2)),(ms(1),S(1))
     *,(MNAME(1),MNAME4(1))
      LZ=1000
      KN=499
c      PRINT 101,N
      call IDR(N,1,MS,2000)
c      PRINT 101,N
c	write(6,103) npi,ncz
c	write(6,103) (mnz(i),i=1,499)
      read(n,rec=3) MNAME4
c	write(6,102) (mname(i),i=1,500)
c	write(6,102) name
      DO 1 I=1,NPI
c	write(6,102) name,mname(i)
      IF(NAME.NE.MNAME(I)) GO TO 1
c	write(6,102) name,mname(i)
      NI=I
c	write(6,103) i,ni,npi
      IF(I.EQ.NPI) GO TO 11
      GO TO 10
    1 CONTINUE
    8 CONTINUE
c      PRINT 101,N
      IF (NPI.LT.KN) GO TO 9
      PRINT 101,N
      STOP
    9 CONTINUE
      NI=NPI+1
      NZ=NCZ
      MNZ(NI)=NZ
      KZ=(L-1)/LZ
      NCZ=NCZ+KZ+1
      NPI=NPI+1
      MNAME(NPI)=NAME
      GO TO 12
   10 NZ=MNZ(NI)
      I=(MNZ(NI+1)-NZ)*LZ
c	write(6,103) i,nz,l
      IF(L.LE.I) GO TO 12
      MNAME(NI)='0'
      GO TO 8
   11 NZ=MNZ(NI)
      NCZ=NZ+(L-1)/LZ+1
      MNAME(NI)=NAME
   12 ML(NI)=L
c      PRINT 101,N
c	write(6,101) n
      call time (HOUR)
c	write(6,102) hour
  102 format(1x,5a10)
  103 format(1x,10i5)
c      PRINT 101,N
c	write(6,101) n
cc      call DATE (DAT(1))
      call DATE_AND_TIME (DAT(1))
c	write(6,102) dat(1),dat(2)
      call convdt (DAT,ID)
c	write(6,101) id(1)
      MD(NI)=ID(1)
      MH(NI)=ID(2)
      call IDW(N,1,MS,2000)
      write(n,rec=3) MNAME4
   13 call DW(N,NZ,ARRAY,L)
      RETURN
  101 FORMAT(1X,'Catalog of the file ',I3,'now is full')
      END

      SUBROUTINE FREAD4 (N,NAME,ARRAY,R)
C Reading of L words from file N into array ARRAY with name NAME
***** implicit integer*4(i-n)
      character*8 NAME,MNAME
      character*4 MNAME4
C      real*8 NAME,MNAME
      INTEGER N,R
      DIMENSION
     *ARRAY(51000),MNAME(500),MNAME4(1000),MS(2000),ML(499),MNZ(499)
      EQUIVALENCE
     * (MS(1),NPI),(MS(2),NCZ)
     *,(MS(3),ML(1)),(MS(502),MNZ(1))
     *,(MNAME(1),MNAME4(1))
      R=0
      call IDR(N,1,MS,2000)
      read(N,rec=3) MNAME4
      DO 1 I=1,NPI
      IF(NAME.NE.MNAME(I)) GO TO 1
      NI=I
      GO TO 2
    1 CONTINUE
      PRINT 100,N,NAME
  100 FORMAT(1X,'File number ',I3,' doesnt contain recording ',A8)
      R=1
      RETURN
   2  L=ML(NI)
      NZ=MNZ(NI)
      CALL DR(N,NZ,ARRAY,L)
      RETURN
      END
c---------------
      subroutine DR(NF,NBN,D,LD)
      dimension D(LD)
      KB=(LD-1)/1000
      NBK=NBN+KB
      do 15 I=NBN,NBK
      IN=(I-NBN)*1000+1
      IK=IN+999
      IF(I.EQ.NBK) IK=IN+LD-1000*KB-1
      read(NF,rec=I) (D(j),j=IN,IK)
  15  continue
      return
      end
      subroutine DW(NF,NBN,D,LD)
      dimension D(LD)
      KB=(LD-1)/1000
      NBK=NBN+KB
      do 15 I=NBN,NBK
      IN=(I-NBN)*1000+1
      IK=IN+999
      IF(I.EQ.NBK) IK=IN+LD-1000*KB-1
      write(NF,rec=I) (D(j),j=IN,IK)
  15  continue
      return
      end
C**************   CODE GEFEST  *****************************
C*********** WRITE UNFULL  ARRAY     ***********************
      SUBROUTINE FW (NF,NAME,ARRAY,L,NOM)
C      CHARACTER *8 NAME,MNAME,MT
      CHARACTER *8 NAME,MNAME
      CHARACTER *4 MNAME4
C      REAL*8 NAME,MNAME,MT
      REAL*8 MT
      DIMENSION
     *ARRAY(L),MNZ(499),ML(499)
     *,MNAME(500),MNAME4(1000),MS(1000),MT(499),AMS(1000)
      EQUIVALENCE
     *(MS(1),NPI),(MS(2),NCZ)
     *,(MS(3),ML(1)),(MS(502),MNZ(1))
     *,(MS(1),MT(1))
     *,(MNAME(1),MNAME4(1))
      LZ=1000
      KN=499
      READ(NF,REC=1) (MS(J),J=1,1000)
      READ(NF,REC=3) (MNAME4(J),J=1,1000)
      DO 1 I=1,NPI
      IF(NAME.NE.MNAME(I)) GO TO 1
      NI=I
      GO TO 2
    1 CONTINUE
      PRINT 101,NF,NAME
      STOP
C    2 READ(NF,REC=2) (MS(j),j=1,1000)
    2 CONTINUE
c      write(6,101) NF,mname(ni)
      NZ=MNZ(NI)
      RR=NOM/1000
      NPLUS=INT(RR)
      NZ=NZ+NPLUS
      J=1
      K=NOM-NPLUS*1000
      READ(NF,REC=NZ) (AMS(IJ),IJ=1,1000)
    3 AMS(K)=ARRAY(J)
      IF(K.EQ.1000) GO TO 5
    4 K=K+1
      J=J+1
      IF(J.NE.L+1)GOTO 3
      WRITE(NF,REC=NZ) (AMS(IJ),IJ=1,1000)
      GO TO 6
    5 WRITE(NF,REC=NZ) (AMS(IJ),IJ=1,1000)
      IF(J.EQ.L+1) GO TO 6
      K=0
      NZ=NZ+1
      READ(NF,REC=NZ) (AMS(IJ),IJ=1,1000)
      GOTO 4
C   6 READ(N'3) MT
C     CALL GETDT (MT(NI))
C     WRITE(N'3) MT
    6 RETURN
  101 FORMAT(1X,'IN FILE ',I3,' IS ABSENT NAME ',A8)
      END
	SUBROUTINE FREAD3 (N,NAME,ARRAY)
C Reading of L words from file N into array ARRAY with name NAME
***** implicit integer*4(i-n)
      character*8 NAME,MNAME
      character*4 MNAME4
C      real*8 NAME,MNAME
      INTEGER R
      DIMENSION
     *ARRAY(51000),MNAME(500),MNAME4(1000),MS(2000),ML(499),MNZ(499)
      EQUIVALENCE
     *(MS(1),NPI),(MS(2),NCZ)
     *,(MS(3),ML(1)),(MS(502),MNZ(1))
     *,(MNAME(1),MNAME4(1))
      R=0
      call IDR(N,1,MS,2000)
      read(N,rec=3) MNAME4
      DO 1 I=1,NPI
      IF(NAME.NE.MNAME(I)) GO TO 1
      NI=I
      GO TO 2
    1 CONTINUE
      PRINT 100,N,NAME
  100 FORMAT(1X,'File number ',I3,' doesnt contain recording ',A8)
      R=1
      RETURN
   2  L=ML(NI)
      NZ=MNZ(NI)
      CALL DR(N,NZ,ARRAY,L)
      RETURN
      END
      subroutine macnam(a,n,aa)
      character*1 a(8),d(8),s(10),p
      character*8 aa,dd
c      character*1 c
c      character*1 p
      equivalence (dd,d(1))
c      data c/'0'/,p/' '/
      data p/' '/
	data s/'0','1','2','3','4','5','6','7','8','9'/
      do 1 i=1,3
    1 d(i)=a(i)
c      write (D(2),'(i3)') n
c      do 2 i=4,6
c      if(b(i).eq.p) b(i)=c
c    2 continue
      n3=n/100
	n2=n/10-10*n3
	n1=n-100*n3-10*n2
      do 2 i=1,10
	j=i-1
	if(n3.eq.j) d(4)=s(i)
	if(n2.eq.j) d(5)=s(i)
	if(n1.eq.j) d(6)=s(i)
    2 continue
      d(7)=P
      d(8)=P
      aa=dd
      return
      end
      subroutine IDR(NF,NBN,ID,LD)
      dimension ID(LD)
      KB=(LD-1)/1000
      NBK=NBN+KB
      do 15 I=NBN,NBK
      IN=(I-NBN)*1000+1
      IK=IN+999
      IF(I.EQ.NBK) IK=IN+LD-1000*KB-1
      read(NF,rec=I) (ID(j),j=IN,IK)
  15  continue
      return
      end
      subroutine IDW(NF,NBN,ID,LD)
      dimension ID(LD)
      KB=(LD-1)/1000
      NBK=NBN+KB
      do 15 I=NBN,NBK
      IN=(I-NBN)*1000+1
      IK=IN+999
      IF(I.EQ.NBK) IK=IN+LD-1000*KB-1
      write(NF,rec=I) (ID(j),j=IN,IK)
  15  continue
      return
      end
      subroutine CONVDT(D,ID)
      character*1  B1(10),B2(10),C(8),aa(3)
      character*8 CD
      character*10 D(2),DD(2)
      character*3 ar(12),az(1),bb(1)
      character*1 ad(10)
      dimension ID(2)
      equivalence (B1(1),DD(1)),(B2(1),DD(2)),(CD,C(1)),(az(1),aa(1))
     *,(b1(4),bb(1))
cc	data
cc     *ar/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT'
cc     *,'NOV','DEC'/
c     *ar/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct'
c     *,'Nov','Dec'/
cc     *,ad/'1','2','3','4','5','6','7','8','9','0'/
      DD(1)=D(1)
      DD(2)=D(2)
cc      do 1 i=1,2
cc    1	c(i)=b1(i)
cc      do 2 i=1,12
cc	if(ar(i).ne.bb(1)) go to 2
cc	if(i.lt.10) c(3)=ad(10)
cc	if(i.ge.10) c(3)=ad(1)
cc	if(i.le.10) c(4)=ad(i)
cc	j=i-10
cc	if(i.ge.10) c(4)=ad(j)
cc    2	continue
c    3 J=1
c      do 5 I=1,10
c      if(B1(I).eq.'-') goto 5
c      cc(J)=B1(I)
c      J=J+1
c   5  continue
c      c(1)=cc(3)
c      c(2)=cc(4)
c      c(3)=cc(1)
c      c(4)=cc(2)
cc      c(5)=b1(8)
cc      c(6)=b1(9)
c      write(6,102) (c(i),i=1,8)
c  102 format(1x,10a1)
c      write(6,104) cd
c  104 format(1x,1a8)
  103 format(1x,5i10)
cc      READ(CD,'(I6)')ID(1)
      c(1)=b1(7)
      c(2)=b1(8)
      c(3)=b1(5)
      c(4)=b1(6)
      c(5)=b1(1)
      c(6)=b1(2)
	c(7)=b1(3)
	c(8)=b1(4)
      READ(CD,'(I8)')ID(1)
c      write(6,103) id(1)
      J=1
      do 8 I=1,8
      if(B2(I).eq.':') goto 8
      C(J)=B2(I)
      J=J+1
   8  continue
      READ(CD,'(I6)')ID(2)
      return
      END
