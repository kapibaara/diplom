c***************************************************************************
c 	 How to create ABBN binary libraries?
c
c  1. GEN_BIN is a special code for creating and edition
c  of any ABBN library.
c  2. There are 3 input data files for creating
c   (1) basic library BNAB90.LIB,
c   (2) multic library BNABMLT.LIB and
c   (3) thermal library BNABTHM.LIB.
c  They are correspondently INP90BAS, INP90MLT and INP90THM.
c  3. To create ABBN library you should specify in CONFIG.DRV all needed
c  paths to ABBN data ASCII files by keyword 'BNAB:'.
c  4. Type a command GEN_BINL. You will be asked 'Name of Input File'.
c  Type a corresponding input (INP90BAS or other).
c  5. Repeat item 4 three times for creating each of 3 ABBN libraries.
c  6. For getting an information about ABBN libraries repeate item 4 and 
c  use input data files INDEXBAS, INDEXMLT and INDEXTHM.
c
c***************************************************************************

      PROGRAM     G E N _ B I N

C**********************************************
C   Program Package for  Constant Libraries   *
C               Generation                    *
C       (Sinitsa V.V., IPPE, Obninsk)         *
C**********************************************
      PARAMETER (NPOOL=500000,NTASK=512,NLIB=32)
      PARAMETER (NBUF=16,LBUF=512)
      COMMON //MP,LP,M(NPOOL)
      COMMON /KEYBNB/ KEYBNB
      COMMON /BTASK/NWORD,TASK(NTASK)
      COMMON /BPARAM/NPAR,PARAM(NTASK)
      COMMON /BLOPEN/LIND(6,NLIB)
      CHARACTER*16 TASK,PARAM,LIB
      COMMON /BINOUT/NIN,NOUT,NLIN,NLOUT
      COMMON /BUFFER/LUNIT(NBUF),
     *NPAGE(NBUF),NCOUNT(NBUF),PAGE(LBUF,NBUF)
      CHARACTER STRING*11,file*64,config*64
      CALL TIME(STRING)
      WRITE(*,1000)
      WRITE(*,*) 'STARTED AT ',STRING
C *** Task Words Number
      NWORD=0
C *** Parameters Number
      NPAR=0
C *** First used word in POOL
      MP=0
C *** Last available word in POOL
      LP=NPOOL
C *** Task Unit
      NIN=1
      file=config('INPUT')
      i=index(file,' ')-1
      if (i.le.0.or.file(1:6).eq.'INPUT ') then
      write(*,'(1x,a)') ' File Name of Input Data ? > '
      read (*,'(a)') file
      i=index(file,' ')-1
      end if
      open (NIN,FILE=file(1:i),status='unknown')
C *** Listing Unit
      NOUT=2
      file=config('RESULT')
      i=index(file,' ')-1
      open (NOUT,FILE=file(1:i),status='unknown')
C *** Input Data Unit
      NLIN=3
C *** Output Data Unit
      NLOUT=4
C *** To clear list of units
      DO 10 IBUF=1,NBUF
      LUNIT(IBUF)=0
   10 CONTINUE
      DO 20 ILIB=1,NLIB
C *** To clear Unit Number
      LIND(5,ILIB)=0
C *** To define default Record Length
      LIND(6,ILIB)=LBUF
   20 CONTINUE
C ***
      KEYBNB=-1
      CALL PROCES(LIB)
      IF (KEYBNB.GT.0) CALL BNABLST(LIB,' ',' ',0,0,-1)
      STOP
C ***
 1000 FORMAT(
     *' ********************************************'/
     *' **    CREATING THE ABBN BINARY LIBRARY    **'/
     *' *   V.V.Sinitsa, Obninsk, 1994 - VER.3/2   *'/
     *' ********************************************')
      END
C
C
      SUBROUTINE PROCES(LIB)
C********************************************************
C     PROCES: Calls modules to proces data              *
C********************************************************
      PARAMETER (NPOOL=500000)
      COMMON //MP,LP,M(NPOOL)
      COMMON /BTASK/NWORD,CTASK(512)
      CHARACTER*16 CTASK,LIB
      COMMON /BINOUT/NIN,NOUT,NLIN,NLOUT
      COMMON /BPROC/MPROC,IWORD
      COMMON /BGET/IRET
      LOGICAL EQCH
      CHARACTER*1 MAST
      CHARACTER*2 MISS
      CHARACTER*8 BLANK,NAME
      CHARACTER*8 MINIT,MLIBR,MCOPY,
     *MPARAM,MBNAB,
     *MEND
      CHARACTER MPROC*80,STRING*11
      DATA MAST/'*'/,MISS/'**'/,BLANK/' '/
C ***         Modules Names       ***
      DATA MINIT/'INIT'/,MLIBR/'LIBR'/,MCOPY/'COPY'/,
     *MPARAM/'PARAM'/,MBNAB/'BNAB'/,
     *MEND/'END'/

   10 READ(NIN,'(A80)',END=2000) MPROC
   20 WRITE(*,'(1X,A78)') MPROC(1:78)
      IF (MPROC(1:1).NE.MAST) GO TO 1000
      IF (MPROC(1:2).EQ.MISS) GO TO 10

C ***         Data Processing         ***

      IW=INDEX(MPROC,':')-1
      IF (IW.LT.0) IW=INDEX(MPROC,' ')-1
      IWORD=IW+2
      KW=MIN0(IW,8)
      NAME=MPROC(2:KW)//BLANK(KW:8)

      IF (EQCH(NAME,MEND,3)) RETURN

      CALL TASK

      CALL TIME(STRING)
      WRITE (*,3000) NAME,STRING

C *** Initial IRET value for GET

      IRET=-1

C *** Library Support Module
      IF (EQCH(NAME,MINIT,8))  CALL INIT
      IF (EQCH(NAME,MLIBR,8))  CALL LIBR(LIB)
      IF (EQCH(NAME,MCOPY,8))  CALL COPY
      IF (EQCH(NAME,MPARAM,8)) CALL PARAM
      IF (EQCH(NAME,MBNAB,8))  CALL BNAB(LIB)
      MP=0

C                     *  *  *

      IF (NWORD.EQ.0) GO TO 20
C *** ERRORS
      WRITE(*,*) ' *** Error in PROCES: Wrong Module Name'
      STOP
 1000 WRITE(*,*) ' *** Error in PROCES: Wrong Command String'
      STOP
 2000 WRITE(*,*) ' *** Error in PROCES: Unexpected End of Data'
      STOP
 3000 FORMAT(' *** Module *',A8,'is called. Time: ',A)
      END
C
C
      SUBROUTINE TASK
      PARAMETER (NTASK=512,NPAR=512)
      COMMON /BTASK/NW,CTASK(NTASK)
      COMMON /BPARAM/NP,PARAM(NPAR)
      COMMON /BPROC/MPROC,ICW
      COMMON /BINOUT/NIN,NOUT,NLIN,NLOUT
      CHARACTER*80 MPROC
      CHARACTER*16 CTASK,PARAM,STR,STRB
      CHARACTER*2 MISS
      CHARACTER*1 CI,CA,CP,CB,CC,CE,CQ
      DATA CA/'*'/,CP/'&'/,CB/' '/,CC/','/,CE/'='/,CQ/'?'/,
     *STRB/' '/,MISS/'**'/
      KW=0
   10 ICC1=ICW
      ICC2=ICW
      IC0=ICW
      IC1=ICW
      IC2=ICW
      DO 20 I=ICW,80
      CI=MPROC(I:I)
      IF (CI.EQ.CB.AND.I.EQ.ICC1) ICC1=I+1
      IF (CI.EQ.CE) ICC2=I
      IF (CI.EQ.CE) IC1=I+1
      IF (CI.EQ.CB.AND.I.EQ.IC1) IC1=I+1
      IF (CI.EQ.CC) GO TO 30
      IF (CI.NE.CB) IC0=I
   20 CONTINUE
      I=80
   30 IC2=IC0
      ICW=I+1
      IF (IC1.GT.80) GO TO 470
      KW=KW+1
      CTASK(KW)=STRB
      IF (IC1.GT.IC2) GO TO 460
      LC=IC2-IC1+1
      IF (LC.GT.16) GO TO 1300
      STR=MPROC(IC1:IC2)//STRB(1:16-LC)
      IF (MPROC(ICC1:ICC1).EQ.CP) THEN
      LCC=ICC2-ICC1
      IF (LCC.GT.16) GO TO 1300
      CTASK(KW)=MPROC(ICC1:ICC2-1)//STRB(1:16-LCC)
      KW=KW+1
      CTASK(KW)=STRB
      END IF
      IF (MPROC(IC1:IC1).EQ.CQ) THEN
C *** To ask Parameters
      ICC1=MAX0(ICC2-15,ICC1)
  100 IF (ICC2.GT.ICC1)
     *WRITE(*,'(17H *** Enter value:,A16,5H ... )')
     *MPROC(ICC1:ICC2)
      IF (ICC2.LE.ICC1)
     *WRITE(*,'(22H *** Enter value: ... )')
      READ(*,'(A16)') STR
      IF (STR.EQ.STRB) GO TO 100
      END IF
      IF (MPROC(IC1:IC1).EQ.CP) THEN
C *** To find Parameters
      MPAR=0
      KW=KW-1
  200 MPAR=MPAR+1
      IF (MPAR.GT.NP) GO TO 1400
      IF (STR.EQ.PARAM(MPAR)) GO TO 250
      GO TO 200
  250 MPAR=MPAR+1
      IF (MPAR.EQ.NP.OR.PARAM(MPAR)(1:1).EQ.CP) GO TO 460
      KW=KW+1
      CTASK(KW)=PARAM(MPAR)
      GO TO 250
      END IF
C ***
      CTASK(KW)=STR
C ***
  460 IF (KW.LT.NW.OR.NW.EQ.0) GO TO 10
      RETURN
  470 READ(NIN,'(A80)',END=1100) MPROC
      ICW=1
      IF (MPROC(1:1).EQ.CA) GO TO 480
      WRITE(*,'(1X,A78)') MPROC(1:78)
      GO TO 10
  480 IF (MPROC(1:2).NE.MISS) GO TO 500
      WRITE(*,'(1X,A78)') MPROC(1:78)
      GO TO 470
  500 IF (NW.GT.0) GO TO 1200
      NW=KW
      RETURN
 1100 WRITE(*,*) ' *** Error in TASK: Unexpected end of data'
      STOP
 1200 WRITE(*,*) ' *** Error in TASK: Unexpected data structure'
      STOP
 1300 WRITE(*,*) ' *** Error in TASK: Word length > 16'
      STOP
 1400 WRITE(*,2000) STR
      STOP
 2000 FORMAT(' *** Error in TASK: Parameter ',A16,' is undefined')
      END
C
C
      SUBROUTINE LOPEN(CLIB,OPT,ILIB)
C     **********************************************************
C     LOPEN:  OPEN DIRECT ACCESS LIBRARY
C     **********************************************************
      PARAMETER (NLIB=32,LREC=512)
      INTEGER UNIT
      COMMON /BLOPEN/LIND(6,NLIB)
      CHARACTER*16 CLIB
      CHARACTER*8 OPT,STATUS,SCRATCH,EMPTY,NEW,UNKNWN
      DATA SCRATCH/'SCRATCH'/,EMPTY/' '/,
     *NEW/'NEW'/,UNKNWN/'UNKNOWN'/
      include 'comdep.h'
      ILIB=LIBRA(CLIB)
      UNIT=LIND(5,ILIB)
      IF (UNIT.GT.0) RETURN
      UNIT=NUNIT(ILIB)
      LIND(5,ILIB)=UNIT
      LIND(6,ILIB)=LREC
      STATUS=UNKNWN
      IF (CLIB.EQ.EMPTY) STATUS=SCRATCH
cPC   LRECL=4*LIND(6,ILIB)
cc PRA
      LRECL=RECUNIT*LIND(6,ILIB)
cCYB  LRECL=LIND(6,ILIB)
      OPEN (UNIT, FILE=CLIB, STATUS=STATUS, ACCESS='DIRECT',
     * FORM='UNFORMATTED', RECL=LRECL, ERR=100)
      IF (OPT.EQ.NEW) RETURN
      ML=0
      CALL TRIC(ILIB,LIND(1,ILIB),-4,ML)
      RETURN
  100 WRITE (*,1000) CLIB
      STOP
 1000 FORMAT(' *** Error in LOPEN: '
     *'It is impossible to open Library ',A16)
      END
C
C
      SUBROUTINE LINIT(ILIB,NREC,NIND)
C********************************************************
C     LINIT: Library Creation                           *
C            UNIT - Unit Number                         *
C            NREC  - Library Length ( record = LREC)    *
C            NIND  - Index Length   ( row = 4  )        *
C********************************************************
      PARAMETER (NLIB=32)
      COMMON /BLOPEN/LIND(6,NLIB)
      INTEGER UNIT,STRING(4)
      UNIT=LIND(5,ILIB)
      LREC=LIND(6,ILIB)
C *** To write last page
      WRITE(UNIT,REC=NREC,ERR=100) (NREC,I=1,LREC)
      do iii=1,nrec
      WRITE(UNIT,REC=iii,ERR=100) (iii,I=1,LREC)
      enddo
C *** To write Index
C              First word in the Library
      LIND(1,ILIB)=4*(NIND+1)
C              First free word in the Library
      LIND(2,ILIB)=LIND(1,ILIB)
C              Last free word in the Library
      LIND(3,ILIB)=NREC*LREC
C              Last word in the Library
      LIND(4,ILIB)=LIND(3,ILIB)
      ML=0
      CALL TRIC(ILIB,LIND(1,ILIB),4,ML)
      STRING(1)=INCH('    ')
      STRING(2)=STRING(1)
      STRING(3)=0
      STRING(4)=0
      DO 10 IN=1,NIND
      CALL TRIC(ILIB,STRING,4,ML)
   10 CONTINUE
      RETURN
  100 WRITE(*,1000) UNIT
      STOP
 1000 FORMAT(' *** Error in LINIT: Library Unit =',
     *I3,' is not created')
      END
C
C
      FUNCTION LIBRA(CLIB)
C********************************************************
C     Library Number Definition from Library Name       *
C********************************************************
      PARAMETER (NLIB=32)
      COMMON /BLOPEN/LIND(6,NLIB)
      CHARACTER*16 LIBR(NLIB),CLIB
      DATA KLIB/0/
      save klib,libr !!PRA
      IF (KLIB.EQ.0) GO TO 20
      DO 10 ILIB=1,KLIB
      IF (LIBR(ILIB).NE.CLIB) GO TO 10
      LIBRA=ILIB
      RETURN
   10 CONTINUE
   20 KLIB=KLIB+1
      IF(KLIB.GT.NLIB) GO TO 100
      LIBR(KLIB)=CLIB
      LIBRA=KLIB
      RETURN
  100 KLIB=NLIB
      WRITE(*,1000) KLIB
      STOP
 1000 FORMAT(' *** Error in NLIBR: Library Number >',I3)
      END
C
C
      FUNCTION NUNIT(ILIB)
C********************************************************
C     Unit Number Definition from Library Number        *
C********************************************************
      PARAMETER (NUNIT0=9)
      NUNIT=NUNIT0+ILIB
      END
C
C
      FUNCTION NLIBR(NUNIT)
C********************************************************
C     Library Number Definition from Unit Number        *
C********************************************************
      PARAMETER (NUNIT0=9)
      NLIBR=NUNIT-NUNIT0
      END
C
C
      FUNCTION MTRIC(ILIB,MI,NW,ML)
C********************************************************
C    MTRIC: Array Exchange with Library                 *
C           ILIB - Library Number                       *
C           MI    - Address in the COMMON//             *
C           IABS(NW)  - Array Length                    *
C           ML    - Address in the Library              *
C             NW<0 - Reading Data from Library,         *
C             NW>0 - Writing Data to Library            *
C             MI<0 - First free word in the COMMON//    *
C             ML<0 - First free word in the Library     *
C********************************************************
      PARAMETER (NLIB=32,NPOOL=500000)
      COMMON //MP,LP,M(NPOOL)
      COMMON /BLOPEN/LIND(6,NLIB)
      MIR=MI
      MLR=ML
      IF (MI.LT.0) MI=MP
      IF (ML.LT.0) ML=LIND(2,ILIB)
      MTRIC=-1
      IF (NW.LT.0.OR.MIR.LT.0) MTRIC=MI
      IF (NW.GT.0.OR.MLR.LT.0) MTRIC=ML
      CALL TRIC(ILIB,M(MI+1),NW,ML)
      MI=MI+IABS(NW)
      IF (MIR.LT.0) MP=MI
      LIND(2,ILIB)=MAX0(LIND(2,ILIB),ML)
      END
C
C
      SUBROUTINE TRIC(ILIB,MW,NW,ML)
C********************************************************
C     TRIC: Data Exchange with Library                  *
C           ILIB - Library Number                       *
C           MW     - Array Name                         *
C           IABS(NW)  - Array Length                    *
C           ML    - Address in the Library              *
C             NW<0 - Reading Data from Library,         *
C             NW>0 - Writing Data to Library            *
C             ML<0 - Reverse Data Exchange              *
C********************************************************
      PARAMETER (NBUF=16,LBUF=512,NLIB=32)
      COMMON /BLOPEN/LIND(6,NLIB)
      COMMON /BUFFER/LUNIT(NBUF),
     *NPAGE(NBUF),NCOUNT(NBUF),MPAGE(LBUF,NBUF)
      character *4 chin
      DIMENSION MW(1)
      INTEGER UNIT
      UNIT=LIND(5,ILIB)
      IREC=LIND(6,ILIB)
      IF (NW.EQ.0) RETURN
C *** To define page number
      NWR=IABS(NW)
      ISL=ISIGN(1,ML)
      IS=(1-ISL)/2
      MIW=0
      IF (IREC.GT.LBUF) GO TO 500
   10 MLR=IABS(ML)
      NREC=(MLR-IS)/IREC+1
C *** Is this page in buffer?
      DO 20 IBUF=1,NBUF
      IF (LUNIT(IBUF).EQ.UNIT.AND.NPAGE(IBUF).EQ.NREC)
     *GO TO 30
   20 CONTINUE
      GO TO 100
C *** Page is in buffer:
C        To define priorities
   30 ICOUNT=NCOUNT(IBUF)
      NCOUNT(IBUF)=-NBUF
      IF (ICOUNT.GT.0.OR.NW.GT.0) NCOUNT(IBUF)=NBUF
      DO 40 JBUF=1,NBUF
      IF (JBUF.EQ.IBUF) GO TO 40
      IF (IABS(NCOUNT(JBUF)).EQ.1) GO TO 40
      JCOUNT=1
      IF (NCOUNT(JBUF).LT.0) JCOUNT=-1
      NCOUNT(JBUF)=NCOUNT(JBUF)-JCOUNT
   40 CONTINUE
      KW=MLR-(NREC-1)*IREC
      LW=MIN0(KW+NWR,IREC)-KW
      IF (ML.LT.0) LW=MIN0(KW,NWR)
      KW=KW+IS
      DO 50 IW=1,LW
      KW=KW+ISL
      IF (NW.LT.0) MW(MIW+IW)=MPAGE(KW,IBUF)
      IF (NW.GT.0) MPAGE(KW,IBUF)=MW(MIW+IW)
   50 CONTINUE
      NWR=NWR-LW
      ML=ML+LW
      MIW=MIW+LW
      IF (NWR.GT.0) GO TO 10
      RETURN
C *** Page is absent:
C       To find page with the lowest priority
  100 MINC=NBUF+1
      DO 110 IBUF=1,NBUF
      ICOUNT=IABS(NCOUNT(IBUF))
      IF (ICOUNT.GE.MINC) GO TO 110
      KBUF=IBUF
      MINC=ICOUNT
  110 CONTINUE
C *** To save page
      KUNIT=LUNIT(KBUF)
      KLIB=NLIBR(KUNIT)
      KREC=LIND(6,KLIB)
      IF (NCOUNT(KBUF).GT.0) then
        WRITE(KUNIT,REC=NPAGE(KBUF)) (MPAGE(I,KBUF),I=1,KREC)
      endif
C *** To read page from Library to buffer
      READ(UNIT,REC=NREC,ERR=600) (MPAGE(I,KBUF),I=1,IREC)
      LUNIT(KBUF)=UNIT
      NPAGE(KBUF)=NREC
      ICOUNT=NBUF
      IF (NW.LT.0) ICOUNT=-NBUF
      NCOUNT(KBUF)=ICOUNT
      GO TO 10
  500 LBUFC=LBUF
      WRITE(*,1000) IREC,LBUFC
      STOP
  600 WRITE(*,2000) NREC
      STOP
 1000 FORMAT(' *** Error in TRIC: Record Lengh LREC=',I5/
     *20x,'exceeds Buffer Length LBUF=',I5)
 2000 FORMAT(' *** Error in TRIC:'
     *'Page number',I4,' is absent in the Library')
      END
C
C
      SUBROUTINE LSAVE(ILIB)
C********************************************************
C     LSAVE: To write page in buffer to the Library     *
C            ILIB - Library Number                      *
C********************************************************
      PARAMETER (NBUF=16,LBUF=512,NLIB=32)
      COMMON /BLOPEN/LIND(6,NLIB)
      COMMON /BUFFER/LUNIT(NBUF),
     *NPAGE(NBUF),NCOUNT(NBUF),PAGE(LBUF,NBUF)
      INTEGER UNIT
      UNIT=LIND(5,ILIB)
      LREC=LIND(6,ILIB)
      DO 10 IBUF=1,NBUF
      IF (LUNIT(IBUF).NE.UNIT) GO TO 10
      IF (NCOUNT(IBUF).LT.0) GO TO 10
      NREC=NPAGE(IBUF)
      WRITE(UNIT,REC=NREC) (PAGE(I,IBUF),I=1,LREC)
   10 CONTINUE
      END
C
C
      SUBROUTINE GET(ILIB,CNAME,MSTR,IRET)
      COMMON /BGET/IRETC
      DIMENSION MCT(4),MST(4),MSTR(4)
      INTEGER NAME(2),BLANK
      CHARACTER*8 CNAME
      IF (IRETC.GE.0) GO TO 30
      NAME(1)=INCH(CNAME(1:4))
      NAME(2)=INCH(CNAME(5:8))
      BLANK=INCH('    ')
C *** To find data in Index
      MLC=0
      CALL TRIC(ILIB,MCT,-4,MLC)
      MCL=MCT(1)
      MLM=MCT(4)
      LIND=(MCL-4)/4
      DO 10 IN=1,LIND
      CALL TRIC(ILIB,MST,-4,MLC)
      IF (MST(1).EQ.NAME(1).AND.MST(2).EQ.NAME(2))
     *GO TO 20
      IF (MST(1).EQ.BLANK.AND.MST(2).EQ.BLANK)
     *GO TO 10
   10 CONTINUE
      GO TO 100
   20 IRETC=0
      MDATC=MST(3)-MLM
      LDATC=MST(4)
      ML=MDATC
      MLR=ML
      LDATI=0
      LDATR=LDATI
   30 IF (IRET.LT.0) THEN
      ML=MDATC
      MLR=ML
      LDATI=0
      LDATR=LDATI
      END IF
C *** To read System Name
      MSTR1=MSTR(1)
      MSTR2=MSTR(2)
   40 IF (IRET.GT.0.AND.IRETC.EQ.0) THEN
      ML=MLR
      LDATI=LDATR
      END IF
      MLR=ML
      ML=MDATC+LDATI
      IF (LDATI.LT.LDATC) GO TO 50
      MSTR(1)=MSTR1
      MSTR(2)=MSTR2
      IRETC=-1
      IRET=-1
      RETURN
   50 CALL TRIC(ILIB,MSTR,-4,ML)
      LSTR=MSTR(3)+2*MSTR(4)
      LDATR=LDATI
      LDATI=LDATI+4+LSTR
      IF (MSTR1.NE.MSTR(1).AND.MSTR1.NE.BLANK) GO TO 40
      IF (MSTR2.NE.MSTR(2).AND.MSTR2.NE.0) GO TO 40
      IF (IRET.GT.0) CALL TRIC(ILIB,MSTR(5),-LSTR,ML)
      IRETC=IRET
      IRET=1
      RETURN
  100 WRITE(*,1000) CNAME
      IRET=-1
      RETURN
 1000 FORMAT( ' *** Warning GET : Data ',A8,' are absent')
      END
C
C
      SUBROUTINE PUT(ILIB,CNAME,MSTR)
      PARAMETER (NLIB=32,NPOOL=500000)
      COMMON //MP,LP,M(NPOOL)
      COMMON /BLOPEN/LIND(6,NLIB)
      CHARACTER*8 CNAME
      INTEGER BLANK,NAME(2),MSTR(1)
C *** To read Index from Library
      BLANK=INCH('    ')
      NAME(1)=INCH(CNAME(1:4))
      NAME(2)=INCH(CNAME(5:8))
      MPR=MP
      MLIB=MP
      MCH=LIND(1,ILIB)
      MLL=LIND(2,ILIB)
      MLH=LIND(3,ILIB)
      MLM=LIND(4,ILIB)
      M(MLIB+1)=MCH
      M(MLIB+2)=MLL
      M(MLIB+3)=MLH
      M(MLIB+4)=MLM
      MSYS=MSTR(1)
      NSYS=MSTR(2)
      LPS=MSTR(3)
      LDS=MSTR(4)
      LTS=4+LPS+2*LDS
      LIN=(MCH-4)/4
      MCL=0
      MLC=4
      DO 20 IN=1,LIN
      MCL=MCL+4
      MI=MLIB+MCL
      CALL TRIC(ILIB,M(MI+1),-4,MLC)
      IF (M(MI+1).EQ.BLANK.AND.M(MI+2).EQ.BLANK)
     *GO TO 30
   20 CONTINUE
   30 MP=MLIB+MCL
      LIN=(MCL-4)/4
      ML=-MLH
      LLI=0
      IF (LIN.EQ.0) GO TO 140
      MIS=MLIB
      DO 130 IN=1,LIN
      MIS=MIS+4
      IF (M(MIS+1).NE.NAME(1)) GO TO 130
      IF (M(MIS+2).NE.NAME(2)) GO TO 130
      MLI=M(MIS+3)-MLM
      LSI=M(MIS+4)
      LLJ=0
      MI=MP+1
      IF (-MLI-LSI.EQ.MLH) MLH=-MLI
      ML=-MLH
   50 IF (LLJ.EQ.LSI) GO TO 150
      CALL TRIC(ILIB,M(MI),-4,MLI)
      LTSI=M(MI+2)+2*M(MI+3)
      LTSJ=4+LTSI
      LLJ=LLJ+LTSJ
      IF (M(MI).NE.MSTR(1)) GO TO 70
      IF (M(MI+1).NE.MSTR(2)) GO TO 70
      MLI=MLI+LTSI
      GO TO 50
   70 LLI=LLI+LTSJ
      IF (MLI.NE.ML+4) GO TO 90
      MLI=MLI+LTSI
      ML=ML+LTSJ
      GO TO 50
   90 CALL TRIC(ILIB,M(MI+4),-LTSI,MLI)
      CALL TRIC(ILIB,M(MI),LTSJ,ML)
      GO TO 50
  130 CONTINUE
  140 MIS=MLIB+MCL
      MCL=MCL+4
  150 continue
      IF (MCL.GT.MCH) GO TO 300
      M(MIS+1)=NAME(1)
      M(MIS+2)=NAME(2)
      M(MIS+3)=MLM-MLH
      M(MIS+4)=LLI+LTS
      MLH=MLH-M(MIS+4)
      IF (MLH.LT.MLL) GO TO 400
      CALL TRIC(ILIB,MSTR(1),LTS,ML)
      M(MLIB+2)=MLL
      M(MLIB+3)=MLH
      LIND(2,ILIB)=MLL
      LIND(3,ILIB)=MLH
      ML=0
      CALL TRIC(ILIB,M(MLIB+1),MCL,ML)
      MP=MPR
      RETURN
  300 WRITE(*,*) ' *** Error in PUT: Library Index is overfilled'
      STOP
  400 WRITE(*,*) ' *** Error in PUT: Library Space is overfilled'
      STOP
      END
C
C
      SUBROUTINE LDEL(NUNIT,NAME)
C********************************************************
C     LDEL:  To delete data from library                *
C             NUNIT - Unit Number                       *
C             NAME  - Data Name                         *
C********************************************************
      DIMENSION MC(4)
      CHARACTER*8 NAME,NAMEC,BLANK
      CHARACTER*4 CHIN
      DATA BLANK/' '/
      MLI=0
      CALL TRIC(NUNIT,MC(1),-4,MLI)
      LIND=MC(1)
      NIND=(LIND-4)/4
C
C ***  To compress Index
C
      MLO=MLI
      KIND=0
      DO 10 I=1,NIND
      CALL TRIC(NUNIT,MC(1),-4,MLI)
      NAMEC=CHIN(MC(1))//CHIN(MC(2))
      IF (NAME.EQ.BLANK.OR.NAME.EQ.NAMEC) GO TO 10
      KIND=KIND+1
      CALL TRIC(NUNIT,MC(1),4,MLO)
   10 CONTINUE
      IF (KIND.EQ.NIND) RETURN
      KIND=KIND+1
      MC(1)=INCH(BLANK(1:4))
      MC(2)=INCH(BLANK(5:8))
      MC(3)=0
      MC(4)=0
      DO 20 I=KIND,NIND
      CALL TRIC(NUNIT,MC(1),4,MLO)
   20 CONTINUE
      END
C
C
      SUBROUTINE LPRES(NUNIT)
C********************************************************
C     LPRES: To press data in library                   *
C             NUNIT - Unit Number                       *
C********************************************************
      PARAMETER (NTASK=512,NPOOL=500000)
      COMMON //MP,LP,M(NPOOL)
      CHARACTER*8 NAME,BLANK
      CHARACTER*4 CHIN
      DATA BLANK/' '/
      MCTL=MP
      MI=MCTL
      ML=0
      CALL TRIC(NUNIT,M(MI+1),-4,ML)
      LIND=M(MI+1)
      MLM=M(MI+4)
      LLIBM=MLM-LIND
      NIND=(LIND-4)/4
      LLIBC=0
C
C ***  To compress index
C
      CALL TRIC(NUNIT,M(MI+5),-(LIND-4),ML)
      MAP=MCTL+M(MCTL+1)
      MA=MAP
      MI=MCTL
      MJ=MI
      KST=0
      DO 10 IST=1,NIND
      MI=MI+4
      NAME=CHIN(M(MI+1))//CHIN(M(MI+2))
      IF (NAME.EQ.BLANK) GO TO 10
      MJ=MJ+4
      KST=KST+1
      M(MA+1)=KST
      M(MA+2)=M(MI+3)
      M(MA+3)=M(MI+4)
      MA=MA+4
      IF (MI.EQ.MJ) GO TO 10
      IPRES=1
      M(MJ+1)=M(MI+1)
      M(MJ+2)=M(MI+2)
      M(MJ+3)=M(MI+3)
      M(MJ+4)=M(MI+4)
   10 CONTINUE
      IF (KST.EQ.0) THEN
C *** The Library is empty
      M(MCTL+2)=M(MCTL+1)
      M(MCTL+3)=M(MCTL+4)
      ML=0
      CALL TRIC(NUNIT,M(MCTL+1),LIND,ML)
      RETURN
      END IF
C
C ***  To compress Parameters
C
      IPRES=0
      CALL TABORD(MAP,KST,4,2)
C *** To calculate new address
      MA=0
      MI=MAP
      DO 20 I=1,KST
      M(MI+4)=MA
      IF (MA.LT.M(MI+2)) IPRES=1
      MA=MA+M(MI+3)
      MI=MI+4
   20 CONTINUE
      M(MCTL+3)=M(MCTL+4)-MA
C
C ***  To compress Data
C
      MI=MAP
      MAD=MAP+4*KST
      MJ=MAD
      NAD=0
      DO 90 IST=1,KST
      MPAR=M(MI+2)-MLM
      LDAT=M(MI+3)
      MI=MI+4
      LDATI=0
   70 CALL TRIC(NUNIT,M(MJ+1),-4,MPAR)
      LPAR=M(MJ+3)
      LAR=M(MJ+4)
      LARR=2*LAR
      MJ1=MJ+2*LARR
      LDATI=LDATI+4+LPAR+LARR
      MPAR=MPAR+LPAR
      CALL TRIC(NUNIT,M(MJ1+1),-LARR,MPAR)
      MJ2=MJ1+LAR
      MK=MJ
      DO 80 I=1,LAR
      NAD=NAD+1
      M(MK+1)=NAD
      M(MK+2)=M(MJ1+I)
      M(MK+3)=M(MJ2+I)
      M(MK+4)=0
      MK=MK+4
   80 CONTINUE
      MJ=MK
      IF (LDATI.LT.LDAT) GO TO 70
   90 CONTINUE
      MF=MAD+4*NAD
C *** To define writing order
      CALL TABORD(MAD,NAD,4,3)
C *** To calculate new address
      MAI=M(MCTL+1)
      IF (NAD.GT.0) THEN
      MI=MAD
      DO 130 I=1,NAD
      M(MI+4)=MAI
      IF (MAI.LT.M(MI+3)) IPRES=2
      MAI=MAI+M(MI+2)
      MI=MI+4
  130 CONTINUE
      M(MCTL+2)=MAI
      END IF
      IF (IPRES.EQ.0) GO TO 300
      IF (IPRES.EQ.1) GO TO 160
      LARM=M(2)-MF
      MI=MAD
      DO 150 I=1,NAD
      LARI=M(MI+2)
      MAI=M(MI+3)
      MAJ=M(MI+4)
      MI=MI+4
      IF (MAI.EQ.MAJ) GO TO 150
  140 LAI=MIN0(LARM,LARI)
      CALL TRIC(NUNIT,M(MF+1),-LAI,MAI)
      CALL TRIC(NUNIT,M(MF+1),LAI,MAJ)
      LARI=LARI-LAI
      IF (LARI.GT.0) GO TO 140
  150 CONTINUE
C ***
  160 CALL TABORD(MAD,NAD,4,1)
      MI=MAP
      MJ=MF
      DO 260 IST=1,KST
      MPAR=M(MI+2)-MLM
      MPARW=M(MI+4)-MLM
      LDAT=M(MI+3)
      MI=MI+4
      LDATI=0
  240 CALL TRIC(NUNIT,M(MJ+1),-4,MPAR)
      LPAR=M(MJ+3)
      LAR=M(MJ+4)
      LARR=2*LAR
      LST=LPAR+LARR
      LSTR=4+LST
      CALL TRIC(NUNIT,M(MJ+5),-LST,MPAR)
      LDATI=LDATI+LSTR
      MK=MJ+4+LPAR
      IF (IPRES.EQ.2) THEN
      DO 250 I=1,LAR
      M(MK+I)=M(MAD+4*(I-1)+2)
      M(MK+LAR+I)=M(MAD+4*(I-1)+4)
  250 CONTINUE
      END IF
      MAD=MAD+4*LAR
      CALL TRIC(NUNIT,M(MJ+1),LSTR,MPARW)
      IF (LDATI.LT.LDAT) GO TO 240
  260 CONTINUE
C *** To correct index
      CALL TABORD(MAP,KST,4,1)
      MI=MCTL
      MJ=MAP
      DO 270 I=1,KST
      MI=MI+4
      MJ=MJ+4
      M(MI+3)=M(MJ)
  270 CONTINUE
      IF (KST.EQ.NIND) GO TO 300
      MBL=INCH(BLANK(1:4))
      KST=KST+1
      DO 280 I=KST,NIND
      MI=MI+4
      M(MI+1)=MBL
      M(MI+2)=MBL
      M(MI+3)=0
      M(MI+4)=0
  280 CONTINUE
  300 ML=0
      CALL TRIC(NUNIT,M(MCTL+1),LIND,ML)
      END
C
C
      FUNCTION INCH(CHIN)
      CHARACTER*4 CHIN
      IN=0
      DO 10 I=1,4
      IN=IN*100
      IC=ICHAR(CHIN(I:I))
      IF (IC.GT.96) IC=IC-32
      IN=IN+IC
   10 CONTINUE
      INCH=IN
      END
C
C
      FUNCTION CHIN(INCH)
      CHARACTER*4 CHIN
      INC=INCH
      DO 10 I=1,4
      IN=INC/100
      CHIN(5-I:5-I)=CHAR(INC-IN*100)
      INC=IN
   10 CONTINUE
      END
C
C
      FUNCTION UPCH(CHIN)
      CHARACTER*4 UPCH,CHIN
      DO 10 I=1,4
      IC=ICHAR(CHIN(I:I))
      IF (IC.GT.96) IC=IC-32
      UPCH(I:I)=CHAR(IC)
   10 CONTINUE
      END
C
C
      LOGICAL FUNCTION EQCH(NAME,MASK,LC)
C ********************************************
C     EQCH: To compare words               *
C             NAME - word be compared        *
C             MASK - control word            *
C             LC   - number of equal symbols *
C ********************************************
      CHARACTER*8 NAME,MASK
      EQCH=.FALSE.
      IB=ICHAR(' ')
      DO 10 I=1,8
      IN=ICHAR(NAME(I:I))
      IF (IN.GT.96) IN=IN-32
      IM=ICHAR(MASK(I:I))
      IF (IM.GT.96) IM=IM-32
      IF (I.LE.LC) THEN
      IF (IN.NE.IM) RETURN
      GO TO 10
      END IF
      IF (IN.NE.IM.AND.IN.NE.IB) RETURN
   10 CONTINUE
      EQCH=.TRUE.
      END
C
C
      SUBROUTINE TABORD(MTAB,NR,NC,KC)
C **************************************************
C     TABORD: To order table elements              *
C            MTAB - table address                  *
C            NR - number of rows                   *
C            NC - number of column                 *
C            KC - ordered column                   *
C **************************************************
      PARAMETER (NPOOL=500000)
      COMMON //MP,LP,M(NPOOL)
      IF (NR.LT.2) RETURN
      DO 30 I=2,NR
      MI=MTAB
      DO 20 J=I,NR
      MJ=MI+NC
      IAI=M(MI+KC)
      IAJ=M(MJ+KC)
      IF (IAI.LE.IAJ) GO TO 20
      DO 10 K=1,NC
      MR=M(MI+K)
      M(MI+K)=M(MJ+K)
      M(MJ+K)=MR
   10 CONTINUE
   20 MI=MI+NC
   30 CONTINUE
      END
C
C
      SUBROUTINE BNAB(LIB)
C********************************************************
C     Module BNAB: Input data in the ABBN-90 format     *
C********************************************************
      PARAMETER (NTASK=512,NBUF=500000)
      COMMON //MP,LP,M(NBUF)
      CHARACTER *64 FILE1,CONFIG
      COMMON/BTASK/NW,TASK(NTASK)
      LOGICAL EQCH,EXIST
      CHARACTER*16 TASK,LIB
      COMMON /BINOUT/NIN,NOUT,NLIN,NLOUT
      CHARACTER*4 EXT
      CHARACTER*8 OLD,WORD,BLANK,OPT,REP,PRES
      CHARACTER*12 MFILE
      DIMENSION LISTF(64)
cCYB  DATA OLD/'OLD'/,EXT/'_'/,BLANK/'        '/
      DATA OLD/'OLD'/,EXT/'.'/,BLANK/'        '/
cPC   DATA OLD/'OLD'/,EXT/'.'/,BLANK/'        '/,
     *REP/'REPLACE'/,PRES/'PRESS'/
      OPEN (UNIT=7,FILE='message',STATUS='unknown')
      MW=1
      LIB=TASK(MW)
      CALL LOPEN(LIB,OLD,ILIB)
      KREP=0
      KPRES=0
   10 MW=MW+1
      OPT=TASK(MW)(1:8)
      IF (EQCH(OPT,REP,3)) THEN
      KREP=1
      GO TO 10
      END IF
      IF (EQCH(OPT,PRES,3)) THEN
      KPRES=1
      GO TO 10
      END IF
      MW=MW-1
      M1=MP
      MW=MW+1
      EXT(2:4)=TASK(MW)(1:3)
      MW=MW+1
      READ(TASK(MW),'(I16)') NF
      IF (NF.GT.0) THEN
      DO 15 I=1,NF
      MW=MW+1
      READ(TASK(MW),'(I16)') LISTF(I)
   15 CONTINUE
      END IF
      I=0
   20 MW=MW+1
      J=0
      IF (MW.GT.NW) GO TO 100
      MP=M1
      K6=INDEX(TASK(MW)(1:8),' ')-1
      IF (K6.LT.0) K6=8
      WORD=BLANK
      WORD(1:K6)=TASK(MW)(1:K6)
      MFILE(1:K6+4)=WORD(1:K6)//EXT
      IPRINT=1
      IF (WORD(1:1).EQ.'*') IPRINT=2
      IF (I.NE.0) GOTO 1000
999   IF (J.EQ.0) I=0
      FILE1=CONFIG('BNAB')
      K7=INDEX(FILE1,' ')-1
      I=I+1
      J=1
      IF (I.GT.20) THEN
      WRITE(*,*) ' DATA ARE ABSENT  : ',MFILE(IPRINT:K6+4)
      STOP ' STOP.'
      END IF
1000  WRITE(*,*) ' **** FROM ==> ',FILE1(1:K7)//MFILE(IPRINT:K6+4)
      WRITE(7,*) ' **** FROM ==> ',FILE1(1:K7)//MFILE(IPRINT:K6+4)
      OPEN (UNIT=NLIN, FILE=FILE1(1:K7)//MFILE(IPRINT:K6+4),
     * STATUS='OLD', ERR=999)
      IF (KREP.GT.0) CALL LDEL(ILIB,WORD)
      CALL RBNAB(LIB,ILIB,IPRINT,WORD,NF,LISTF)
      CLOSE (NLIN)
      GO TO 20
  100 IF (KPRES.GT.0) CALL LPRES(ILIB)
      CALL LSAVE(ILIB)
      NW=0
      CLOSE (7)
      END
C
C
      SUBROUTINE RBNAB(LIB,ILIB,IPRINT,NAME,NF,LISTF)
      PARAMETER (NBUF=500000)
      COMMON //MP,LP,M(NBUF)
      DIMENSION P(1)
      EQUIVALENCE (M(1),P(1))
      PARAMETER (NT0=3,NS0=16)
      COMMON /BINOUT/NIN,NOUT,NLIN,NLOUT
      COMMON /G00000/MSYS,NSYS,LPAR,LARR,
     *AWR,LT,LC,LCOL,LROW,LTAB,
     *MCOLL,MROWL,MTABL,MCOL,MROW,MTAB
      DIMENSION LISTF(1),NUMGR(0:10)
      CHARACTER*1 AST
      CHARACTER*4 NAM,LIBR
      CHARACTER*8 NAME,NAMER
      CHARACTER*16 LIB
      CHARACTER*64 FORM1,FORMA,FORM2
      CHARACTER CARD*256
      LOGICAL TRUE
      DATA NAM/'NAM='/,AST/'*'/
      DATA FORM1/'(4X,A8,5X,A4,1X,2(4X,I4),2(5X,E12.0))'/
      DATA FORM2/'(4X,I3,10X,I4,1X,4X,I4,4X,I4)'/
C
      MSYS=INCH('G   ')
      LPAR=3
      LARR=3
      MF=0
      MT=0
C
   10 READ(NLIN,'(A)',END=600) CARD
      IF (CARD(1:4).NE.NAM) GO TO 10
C
C *** To read Head of Table
C
   20 READ(CARD,FORM1) NAMER,LIBR,MF,MT,AWR,DEL
C
ccc      CALL BNABLST(LIB,NAMER,LIBR,MF,MT,0)
C
      CALL BNABLST(LIB,NAME,LIBR,MF,MT,0)
C
      IF (NF.EQ.0) GO TO 30
      DO 25 I=1,NF
      IF (LISTF(I).EQ.MF) GO TO 30
   25 CONTINUE
      IF (MF.LT.LISTF(NF)) GO TO 10
      GO TO 600
   30 MTNEW=MT
      IF (MT.GT.299) MTNEW=MT/10
      NSYS=(MF*1000+MTNEW)*1000
      WRITE(*,'(1x,A)') CARD(1:64)
      WRITE(7,'(A,A8,A,A4,A,I3,A,I3,1P,2(A,E10.2))') ' NAM=',NAMER,
     *' BIB=',LIBR,' MF=',MF,' MT=',MT,' PAR1=',AWR,' PAR2=',DEL
C
   35 READ(NLIN,'(A)',END=500) CARD
      IF (IPRINT.EQ.2) WRITE(*,*) CARD(1:72)
      IF (CARD(1:1).EQ.AST) GO TO 35
      READ(CARD,FORM2) LV,LT,LC,LS
      I=INDEX(CARD,'(')
      J=INDEX(CARD,') ')
      FORMA=CARD(I:J)
       NLC=(LC-1)/LS+1
       NCI=1
       NCC=0
       TRUE=(MF.EQ.10.AND.MT.EQ.0).OR.MF.GE.300
       IORDER=+1
      IF (LV.LT.0) THEN
       IORDER=-1
       LV=-LV
      END IF
      IF (LV.GT.0) THEN
       NCI=LV/10
       NCC=LV-10*NCI
      ELSE IF (MF.EQ.4.OR.MF.EQ.5.OR.TRUE) THEN
       NCI=2
      END IF
       NCII=1
      IF (MF.EQ.4.OR.MF.EQ.5.OR.
     *    MF.EQ.302.OR.MF.EQ.304.OR.MF.EQ.305) NCII=2
      WRITE(7,'(4(A,I3),A,A,A)') '  LV=',LV,'  LT=',LT,'  LC=',LC,
     * '  NCI=',NCI,'  FORM=',FORMA(1:J-I+1),' << loaded'
      LS=LS-NCI
      LC=LC-NLC*NCI
      LCS=LC/LS
      LCOL=0
      LROW=LT*NCI
      LTA =LC*LT
      LTAB=LTA+7
      MCOL=MP
      MROW=MCOL+LCOL
      MTAB=MROW+LROW
      MSTR=MTAB+LTAB
      NCI=NCI-1
      IC=0
      IF (NCC.NE.0.
     *    OR.MF.EQ.7.OR.MF.EQ.11.OR.MF.EQ.15.
     *    OR.(MF.GT.70.AND.MF.LT.300).
     *    OR.(MF.EQ.0.AND.(MT.GT.1.AND.MT.LT.28))) THEN
       KADD=0
      ELSE
       KADD=2
      END IF
C
   40 READ(NLIN,'(A)',END=500) CARD
      IF (IPRINT.EQ.2) WRITE(*,*) CARD(1:72)
      IF (CARD(1:1).EQ.AST) GO TO 40
      IF (MF.EQ.4.OR.MF.EQ.5) THEN
       READ(CARD,FORMA)(NUMGR(IS),IS=0,NCI),(M(MSTR+IS),IS=1,LS)
      ELSE
       IF (NCC.EQ.0.OR.(NCC.NE.0.AND.IORDER.GT.0))
     * READ(CARD,FORMA)(NUMGR(IS),IS=0,NCI),(P(MSTR+IS),IS=1,LS)
       IF (NCC.NE.0.AND.IORDER.LT.0) 
     * READ(CARD,FORMA)(P(MSTR+IS),IS=1,NCC),
     *     (NUMGR(IS),IS=0,NCI),(P(MSTR+IS),IS=NCC+1,LS)
      END IF
       IF (NCII.EQ.1) MI=MTAB+(IC/LT*(LS-1)-1)*LT+IC+1
       IF (NCII.EQ.2) MI=MTAB+(IC-IC/LT*(LT-1))*LS
      DO 50 IS=1,LS
       IF (NCII.EQ.1) THEN
        P(MI+LT*IS)=P(MSTR+IS)
       ELSE
        IF (TRUE) THEN
         FX=P(MSTR+IS)
        ELSE
         FX=FLOAT(M(MSTR+IS))/1000.
        END IF
         IF ((MF.EQ.4.OR.MF.EQ.304).AND.FX.EQ.0.0) FX=1.0
         P(MI+IS)=FX
       END IF
   50 CONTINUE
       IC=IC+1
      IF (IC.LE.LT) THEN
       IF (KADD.NE.0) NUMGR(0)=NUMGR(0)+KADD
       DO 51 IS=0,NCI
   51  M(MROW+IC+LT*IS)=NUMGR(IS)
      END IF
      IF (IC.LT.LT*NLC) GO TO 40
C
ccc      READ (NAMER,'(2A4)') M(MTAB+LTA+1),M(MTAB+LTA+2)
C
      READ (NAME,'(2A4)') M(MTAB+LTA+1),M(MTAB+LTA+2)
C
      READ (LIBR,'(A4)') P(MTAB+LTA+3)
      P(MTAB+LTA+4)=MF
      P(MTAB+LTA+5)=MT
      P(MTAB+LTA+6)=AWR
      P(MTAB+LTA+7)=DEL
C *** To write Data
      MCOLL=-1
      MROWL=-1
      MTABL=-1
      MCOLL=MTRIC(ILIB,MCOL,LCOL,MCOLL)
      MROWL=MTRIC(ILIB,MROW,LROW,MROWL)
      MTABL=MTRIC(ILIB,MTAB,LTAB,MTABL)
C
      CALL PUT(ILIB,NAME(IPRINT:8),MSYS)
C
ccc      CALL PUT(ILIB,NAMER,MSYS)
C
      GO TO 10
  500 WRITE(NOUT,1000) NAME(IPRINT:8),MF,MT
  600 RETURN
 1000 FORMAT(' *** Warning! RBNAB: Wrong Data Structure in ',A8,
     *' MF=',I4,' MT=',I4)
      END
C
C
      SUBROUTINE LIBR(LIB)
C********************************************************
C     Module LIB: Library Support                       *
C********************************************************
      PARAMETER (NTASK=512,NBUF=500000)
      COMMON //MP,LP,M(NBUF)
      DIMENSION P(1)
      EQUIVALENCE (P(1),M(1))
      COMMON /KEYBNB/ KEYBNB
      COMMON /BTASK/NW,TASK(NTASK)
      CHARACTER*16 TASK,LIB
      LOGICAL EQCH
      CHARACTER*1 AST
      CHARACTER*4 CM,CHIN,UPCH
      CHARACTER*8 INDEX,PRESS,CONT,DUMP,SPACE,DEL
      CHARACTER*8 WORD,CNAME,OPT,OLD,BLANK
      COMMON /BINOUT/NIN,NOUT,NLIN,NLOUT
      DATA PRESS/'PRESS'/,INDEX/'INDEX'/,DEL/'DELETE'/,
     *CONT/'CONT'/,DUMP/'DUMP'/,SPACE/'SPACE'/
      DATA OLD/'OLD'/,AST/'*'/,BLANK/' '/
      MW=1
      LIB=TASK(MW)
      CALL LOPEN(LIB,OLD,ILIB)
      KEY1=KEYBNB
      M1=MP
   10 MW=MW+1
      IF (MW.GT.NW) GO TO 900
      WORD=TASK(MW)(1:8)
C
C
   20 MP=M1
      OPT=WORD
C
C *** To press Library
C
      IF (EQCH(OPT,PRESS,3)) THEN
      CALL LPRES(ILIB)
      GO TO 10
      END IF
C *** To read Index
      MI=MP
      MJ=MI+4
      ML=0
      CALL TRIC(ILIB,M(MI+1),-4,ML)
      LLIBM=M(MI+4)-M(MI+1)
      LLIBA=M(MI+3)-M(MI+2)
      NST=(M(MI+1)-4)/4
      MLM=M(MI+4)
      LLIBC=0
      IF (EQCH(OPT,SPACE,3)) GO TO 60
      IF (EQCH(OPT,INDEX,3)) GO TO 60
      IF (EQCH(OPT,CONT,3)) THEN
      WRITE(NOUT,2000) LIB
      GO TO 60
      END IF
   50 MW=MW+1
      IF (MW.GT.NW) GO TO 900
      WORD=TASK(MW)(1:8)
      IF (EQCH(WORD,INDEX,3)) GO TO 20
      IF (EQCH(WORD,CONT,3))  GO TO 20
      IF (EQCH(WORD,DUMP,3))  GO TO 20
      IF (EQCH(WORD,SPACE,3)) GO TO 20
      IF (EQCH(WORD,PRESS,3)) GO TO 20
      IF (EQCH(WORD,DEL,3)) GO TO 20
   60 ML=4
      KST=0

      DO 90 IST=1,NST

      CALL TRIC(ILIB,M(MI+1),-4,ML)
      CNAME=CHIN(M(MI+1))//CHIN(M(MI+2))

      IF (CNAME.EQ.BLANK) GO TO 90
      KST=KST+1
      WORD=UPCH(WORD(1:4))//UPCH(WORD(5:8))

      IF ((OPT.EQ.DUMP) .AND.
     *            (WORD(1:1).NE.AST.AND.WORD.NE.CNAME)) GO TO 90

      IF (EQCH(OPT,DEL,3).AND.WORD.EQ.CNAME) THEN
      CALL LDEL(ILIB,CNAME)
      GO TO 50
      END IF
      IF (OPT.EQ.CONT) WRITE(NOUT,2100) KST,CNAME,M(MI+3),M(MI+4)
      MPAR=M(MI+3)-MLM
      LDAT=M(MI+4)
      LDATI=0

      IF (OPT.EQ.DUMP) WRITE(NOUT,3000) CNAME

   70 CALL TRIC(ILIB,M(MJ+1),-4,MPAR)
      LPAR=M(MJ+3)
      LARR=M(MJ+4)
      LSTR=LPAR+2*LARR
      LDATI=LDATI+4+LSTR
      LLIBC=LLIBC+4+LSTR
      IF (OPT.EQ.INDEX.AND.KEY1.LT.0) THEN
      MFT=M(MJ+2)/1000
      MF=MFT/1000
      MT=MFT-MF*1000
      CM=CHIN(M(MJ+1))
      CALL BNABLST(LIB,CNAME,'    ',MF,MT,0)
      END IF
      IF (EQCH(OPT,DUMP,3).OR.EQCH(OPT,CONT,3)) THEN
      CM=CHIN(M(MJ+1))
      WRITE(NOUT,3100) CM,(M(MJ+J),J=2,4)
      END IF
      MJJ=MJ+4
      CALL TRIC(ILIB,M(MJJ+1),-LSTR,MPAR)
      IF (OPT.EQ.DUMP) WRITE(NOUT,3200) (M(MJJ+J),P(MJJ+J),J=1,LSTR)
      MJJ=MJJ+LPAR
      MII=MJJ+2*LARR

      DO 80 IARR=1,LARR
      LARRI=M(MJJ+IARR)
      LLIBC=LLIBC+LARRI
      IF (OPT.EQ.DUMP) THEN
      MARRI=M(MJJ+LARR+IARR)
      CALL TRIC(ILIB,M(MII+1),-LARRI,MARRI)
      WRITE(NOUT,3200) (M(MII+I),P(MII+I),I=1,LARRI)
      END IF
   80 CONTINUE

      IF (LDATI.LT.LDAT) GO TO 70
      IF (OPT.EQ.DUMP.AND.WORD(1:1).NE.AST) GO TO 50
   90 CONTINUE

      IF (OPT.EQ.DUMP) GO TO 50
      IF (OPT.EQ.SPACE) THEN
      RLIBC=100.*FLOAT(LLIBC)/FLOAT(LLIBM)
      RLIBA=100.*FLOAT(LLIBA)/FLOAT(LLIBM)
      WRITE(NOUT,4000) LIB,NST,KST,
     *LLIBM,LLIBC,RLIBC,LLIBA,RLIBA
      END IF
      GO TO 10
  900 NW=0
      CALL LSAVE(ILIB)
 1000 FORMAT(' *** Library  ',A16,' is empty')
 2000 FORMAT(' *** Library: ',A16,' Index:'/
     *'    N    Data Name      M     L')
 2100 FORMAT(1X,I4,4X,A8,1X,I7,I6)
 3000 FORMAT(' ***    Data:  ',A8,12X,
     *'MSYS         NSYS         NPAR         NARR')
 3100 FORMAT(35X,A4,1X,I12,2(5X,I8))
 3200 FORMAT(3(1X,I12,1X,1PE12.5))
 4000 FORMAT('  Library: ',A16,' Index Space =',i8,' strings'/
     *                     27X,' Used        =',I8,' strings'/
     *                     27X,' Data Space  =',I8,' words'/
     *                     27X,' Used        =',I8,' words'/
     *                     27X,'                or',F6.1,'%'/
     *                     27X,' Available   =',I8,' words'/
     *                     27X,'                or',F6.1,'%')
      END
C
C
      SUBROUTINE INIT
C********************************************************
C     Module INIT: To init Library                      *
C********************************************************
      PARAMETER (NTASK=512,NBUF=500000)
      COMMON //MP,LP,M(NBUF)
      CHARACTER*8 NEW
      COMMON /BINOUT/NIN,NOUT,NLIN,NLOUT
      COMMON /BTASK/NW,TASK(NTASK)
      CHARACTER*16 TASK,LIB
      DATA NEW/'NEW'/,LREC/512/
ccc PRA
C
C *** To init the Library
C
      MW=0
   10 MW=MW+1
      IF (MW.GT.NW) GO TO 100
      LIB=TASK(MW)
      MW=MW+1
      READ(TASK(MW),'(I16)') NIND
      MW=MW+1
      READ(TASK(MW),'(I16)') NREC
      CALL LOPEN(LIB,NEW,NUNIT)
      CALL LINIT(NUNIT,NREC,NIND)
      CALL LSAVE(NUNIT)
      WRITE(NOUT,1000) LIB,NIND,NREC,LREC
      GO TO 10
  100 NW=0
 1000 FORMAT(' *** Library ',A16/
     *       '       Index Length  = ',I5/
     *       '       Records Number= ',I5/
     *       '               Length= ',I5)
      END
C
C
      SUBROUTINE COPY
C********************************************************
C     Module COPY: To copy strucuture                   *
C     Control parameters:                               *
C     LIBI - Input Library Name                         *
C     LIBO - Output Library Name                        *
C     OPT  - Option: RENAME/REPLACE/PRESS               *
C     LIST -  List of Data Names                        *
C********************************************************
      PARAMETER (NTASK=512,NBUF=500000)
      COMMON //MP,LP,M(NBUF)
      COMMON /BTASK/NW,TASK(NTASK)
      CHARACTER*16 TASK,LIBI,LIBO
      CHARACTER*4 BLANK
      CHARACTER*8 OLD,NAMEI,NAMEO,AST,OPT,REN,REP,PRES
      LOGICAL EQCH
      DATA OLD/'OLD'/,AST/'*'/,BLANK/' '/,
     *REN/'RENAME'/,REP/'REPLACE'/,PRES/'PRESS'/
C *** To open Libraries
      MSYS=MP
      MW=1
      LIBI=TASK(MW)
      LREC=0
      CALL LOPEN(LIBI,OLD,NUNITI)
      MW=MW+1
      LIBO=TASK(MW)
      LREC=0
      CALL LOPEN(LIBO,OLD,NUNITO)
C *** To define options
      KREN=0
      KREP=0
      KPRES=0
   10 MW=MW+1
      IF (MW.GT.NW) GO TO 100
      OPT=TASK(MW)(1:8)
      IF (EQCH(OPT,REN,3)) THEN
      KREN=1
      GO TO 10
      END IF
      IF (EQCH(OPT,REP,3)) THEN
      KREP=1
      GO TO 10
      END IF
      IF (EQCH(OPT,PRES,3)) THEN
      KPRES=1
      GO TO 10
      END IF
      MW=MW-1
   20 MW=MW+1
      IF (MW.GT.NW) GO TO 100
      NAMEI=TASK(MW)(1:8)
      NAMEO=NAMEI
      IF (KREN.GT.0) THEN
      MW=MW+1
      IF (MW.GT.NW) GO TO 200
      NAMEO=TASK(MW)(1:8)
      IF (EQCH(NAMEO,AST,8)) NAMEO=NAMEI
      END IF
      IRET=-2
   30 MP=MSYS
      M(MSYS+1)=INCH(BLANK)
      M(MSYS+2)=0
      CALL GET(NUNITI,NAMEI,M(MSYS+1),IRET)
      IF (IRET.LE.0) GO TO 20
      NPAR=M(MSYS+3)
      NARR=M(MSYS+4)
      MLAR=MSYS+4+NPAR
      MARR=MLAR+NARR
      CALL GET(NUNITI,NAMEI,M(MSYS+1),IRET)
      IF (KREP.GT.0) CALL LDEL(NUNITO,NAMEO)
      IF (NARR.EQ.0) GO TO 90
      MBUF=MARR+NARR
      MP=MBUF
      MLO=-1
      DO 80 I=1,NARR
      LARR=M(MLAR+I)
      MLI=M(MARR+I)
      MI=MBUF
      MI=MTRIC(NUNITI,MI,-LARR,MLI)
      M(MARR+I)=MTRIC(NUNITO,MI,LARR,MLO)
   80 CONTINUE
   90 CALL PUT(NUNITO,NAMEO,M(MSYS+1))
      IRET=0
      GO TO 30
  100 IF (KPRES.GT.0) CALL LPRES(NUNITO)
      CALL LSAVE(NUNITO)
      NW=0
      RETURN
  200 WRITE(*,*) ' * Error in COPY: wrong list '
      STOP
      END
C
C
      SUBROUTINE PARAM
C********************************************************
C     Module PARAM: Input Parameters                    *
C********************************************************
      PARAMETER (NTASK=512,NPAR=512)
      COMMON /BTASK/NW,TASK(NTASK)
      COMMON /BPARAM/NP,PAR(NPAR)
      CHARACTER*16 TASK,PAR
      DO 10 IW=1,NW
      PAR(NP+IW)=TASK(IW)
   10 CONTINUE
      NP=NP+NW
      NW=0
      END
C
C
         SUBROUTINE  BNABLST ( TYTLE,NAME0,LIBR,MF0,MT,KEY )
C        GETS A LISTING OF TABLES
         PARAMETER ( NMAX=600, NIZ=16, MAX=NMAX*NIZ )
         LOGICAL OPENED
         COMMON /KEYBNB/ KEYBNB
         DIMENSION MTF2(NMAX),MTF3(NMAX)
         DIMENSION KOD1(4*NMAX),KOD2(4*NMAX)
         CHARACTER*8 LIST(MAX),NAWR(NMAX)
         CHARACTER*8 NAME2,NAME0
         CHARACTER*4 BLANK,KOD4,KOD5,A,FIL
         CHARACTER PL*1,MCP*10,MON*3,R*256,TYTLE*16,LIBR*4
         DATA LIST/MAX*'    '/,NAWR/NMAX*'    '/,BLANK/'    '/,
     *        A/'NAM='/,PL/'+'/
         DATA INIT/0/
                   IND1(I,J)=(I-1)*NIZ+J
                   IND2(K,L)=(K-1)*2+L
                   IND3(M,N)=(M-1)*4+N

                   MF=MF0
                   IF (MF.GT.300) MF=MF0-300
            IF (INIT.EQ.0) THEN
                   DO 1000 LL=1,NMAX
                   LIST(IND1(LL,1))=BLANK
                   LIST(IND1(LL,2))='....'
                   LIST(IND1(LL,4))=BLANK
                   LIST(IND1(LL,6))=BLANK
                   LIST(IND1(LL,8))=BLANK
                   LIST(IND1(LL,10))=BLANK
                   MTF2(LL)=0
 1000              MTF3(LL)=0
                   DO 1001 II=1,4*NMAX
                   KOD1(II)=0
 1001              KOD2(II)=0
            I=INDEX(TYTLE,' ')-1
            IF (I.LE.0) I=LEN(TYTLE)
cPC         J=INDEX(TYTLE,'.')-1
            J=INDEX(TYTLE,'.')-1
cCYB        J=INDEX(TYTLE,'_')-1
            IF (J.LE.0) J=I
            R(1:J)=TYTLE(1:J)
cPC         R(J+1:J+4)='.ls '
            R(J+1:J+4)='.ls '
cCYB        R(J+1:J+4)='_ls '
            NY=16
1002        NY=NY+1
            INQUIRE (UNIT=NY,OPENED=OPENED)
            IF (OPENED) GOTO 1002
            OPEN (NY,FILE=R(1:J+4),STATUS='UNKNOWN')
            END IF

            IF (KEY.LT.0) GOTO 2000

           DO 99 I=1,INIT+1
           IF (NAWR(IND2(I,1)).EQ.NAME0) GOTO 100
   99      CONTINUE
           I=INIT+1
  100      INIT=I
             KEYBNB=I
               NAWR(IND2(I,1))=NAME0
                 NAME2 = LIBR//' '
                 IF (LIBR.EQ.BLANK) NAME2 = 'BN  '
                 IF(MF.EQ.1.or.(MF.eq.0.and.MT.eq.1)) GOTO 101
                 IF(MF.EQ.2.or.(MF.eq.0.and.MT.eq.2)) GOTO 102
                 IF(MF.EQ.3.or.(MF.eq.0.and.MT.eq.3)) GOTO 103
                 IF(MF.EQ.4.or.(MF.eq.0.and.MT.eq.4)) GOTO 104
                 IF(MF.EQ.5.or.(MF.eq.0.and.MT.eq.5)) GOTO 105
                 IF(MF.EQ.6.or.(MF.eq.0.and.MT.eq.6)) GOTO 106
                 IF(MF.EQ.7.or.(MF.eq.0.and.MT.eq.7)) GOTO 109
                 IF(MF.EQ.8.or.(MF.eq.0.and.MT.eq.8)) GOTO 110
                 IF(MF.EQ.10) GOTO 107
                 IF(MF.EQ.11) GOTO 108
                 IF(MF.EQ.15) GOTO 111
                          GOTO 999
C  MF=1
  101               LIST(IND1(I,2))=NAME2
                          GOTO 999
C  MF=2
  102               LIST(IND1(I,3))=NAME2
                          MTF2(I)=MTF2(I)+1
                          GOTO 999
C  MF=3
  103               LIST(IND1(I,5))=NAME2
                          MTF3(I)=MTF3(I)+1
                          GOTO 999
C  MF=4
  104               LIST(IND1(I,7))=NAME2
                            IF(MT.EQ.1)   KOD1(IND3(I,1))=1
                            IF(MT.EQ.102) KOD1(IND3(I,2))=1
                            IF(MT.EQ.18)  KOD1(IND3(I,3))=1
                            IF(MT.EQ.2)   KOD1(IND3(I,4))=1
                            GOTO 999
C  MF=5
  105               LIST(IND1(I,9))=NAME2
                            IF(MT.EQ.1)   KOD2(IND3(I,1))=1
                            IF(MT.EQ.102) KOD2(IND3(I,2))=1
                            IF(MT.EQ.18)  KOD2(IND3(I,3))=1
                            IF(MT.EQ.2)   KOD2(IND3(I,4))=1
                            GOTO 999
C  MF=6
  106               LIST(IND1(I,11))=NAME2
                            GOTO 999
C  MF=7
  109               LIST(IND1(I,12))=NAME2
                            GOTO 999
C  MF=8
  110               LIST(IND1(I,13))=NAME2
                            GOTO 999
C  MF=10
  107               LIST(IND1(I,14))=NAME2
                            GOTO 999
C  MF=11
  108               LIST(IND1(I,15))=NAME2
                            GOTO 999
C  MF=15
  111               LIST(IND1(I,16))=NAME2
                            GOTO 999

c                ---------------------
c                Printing the contents
c                ---------------------

 2000       WRITE (NY,*)
            WRITE (NY,*) '     컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴 '
            WRITE (NY,*) '      Contents of the Base ABBN-90 Library  '
            WRITE (NY,*) '      for MF= 1,2,3,4,5,6,7,8,10,11 and 15  '
            WRITE (NY,*) '     컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴 '
            WRITE (NY,'(18X,A)') TYTLE
            WRITE (NY,31)
   31 FORMAT(1x,82('�')/
     *       5x,' MF=        1      2      3      4         5   ',
     *       1x,' 6    7    8   10   11   15'/
     *       5x,' No.',10x,'  No.MT  No.MT',2(5x,'tcfe ')/
     *       1x,82('�'))
            DO 9 I=1,INIT
            KOD4='....'
            KOD5='....'
            IF (MTF2(I).GT.0) WRITE(LIST(IND1(I,4)),'(I2)') MTF2(I)
            IF (MTF3(I).GT.0) WRITE(LIST(IND1(I,6)),'(I2)') MTF3(I)
            LIST(IND1(I,1))=NAWR(IND2(I,1))
            IF (I.NE.1.AND.(NAWR(IND2(I,1)).EQ.NAWR(IND2(I-1,1))))
     *      LIST(IND1(I,1))=BLANK
            IF(KOD1(IND3(I,1)).EQ.1) KOD4(1:1)=PL
            IF(KOD1(IND3(I,2)).EQ.1) KOD4(2:2)=PL
            IF(KOD1(IND3(I,3)).EQ.1) KOD4(3:3)=PL
            IF(KOD1(IND3(I,4)).EQ.1) KOD4(4:4)=PL
            IF(KOD2(IND3(I,1)).EQ.1) KOD5(1:1)=PL
            IF(KOD2(IND3(I,2)).EQ.1) KOD5(2:2)=PL
            IF(KOD2(IND3(I,3)).EQ.1) KOD5(3:3)=PL
            IF(KOD2(IND3(I,4)).EQ.1) KOD5(4:4)=PL
            LIST(IND1(I,8))=KOD4
            LIST(IND1(I,10))=KOD5
   9        WRITE (NY,40) I,(LIST(IND1(I,KK)),KK=1,NIZ)
            WRITE (NY,41)
            WRITE (NY,*) '  The End of Scanning Procedure '
            WRITE (NY,*) ' 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�'
   40       FORMAT(1x,I3,1x,A8,2(1x,A4),A2,1x,A4,A2,10(1x,A4))
   41       FORMAT(1x,82('�'))
            CLOSE(NY)
  999 RETURN
      END
C
C
C-----------------------------------------
C    PATHS ALLOCATING PROCEDURE
C-----------------------------------------
C    Manturov G.,Obninsk (08439)96296
C-----------------------------------------
        character*64 function config(name)
        character name*(*),file*64,file0*11
        logical opened,exist0,exist1
        integer i
        data i/0/
        ny=0
        icase=0
        lname= INDEX(name,' ')-1
        if (lname.le.0) lname=LEN(name)
cPC/AT   file0='consys.drv '
        file0='consys.drv '
cCYBER   file0='consys_drv '
        INQUIRE (FILE= file0, EXIST= exist0,
     +          OPENED= opened, NUMBER= ny0)
        if ( exist0 ) then
         ny=ny0
        else
cPC/AT   file0='config.drv '
         file0='config.drv '
cCYBER   file0='config_drv '
         INQUIRE (FILE= file0, EXIST= exist1,
     +            OPENED= opened, NUMBER= ny1)
         if ( exist1 ) ny=ny1
        end if

        if ( .not.exist0 .and. .not.exist1 ) goto 99

        if ( .not.opened ) goto 999

1000    read (ny,'(a)',end=100) file
        if (file(1:1).eq.'*') goto 1000
        l= INDEX(file,':')+1
        if (l.eq.1.or.file(1:l-2).ne.name(1:lname)) goto 1000
        l1=INDEX(file,'  ')+2
        config(1:l1-l+1)= file(l:l1)
        return
999     ny=65
777     ny=ny+1
        INQUIRE ( UNIT= ny, OPENED= opened )
        if ( opened ) goto 777
        OPEN (ny, FILE=file0, STATUS='old',err=99)
        goto 1000
100     REWIND (ny)
        icase=icase+1
        if (icase.lt.3) goto 1000
        i=i+1
99      if(i.eq.0) write(*,*) ' ==> DRIVE file',
     *  ' is absent or invalid'
        i=i+1
        config(1:LEN(name)+1)=name//' '
        end
C
C
