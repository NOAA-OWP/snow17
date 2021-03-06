C MEMBER ADJC19
C  (from old member FCPACK19)
C
      SUBROUTINE ADJC19(TWE,SI,ADC)
C.......................................
C     THIS SUBROUTINE ADJUSTS SNOW MODEL CARRYOVER VALUES FOR A CHANGE
C        IN THE TOTAL WATER-EQUIVALENT.  USED IN THE 'SNOW-17 '
C        OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980

CVK     MODIFIED 4/00 BY V. KOREN: TWO NEW STATES ADDED
C.......................................
      REAL NEGHS,LIQW
      DIMENSION ADC(11)
C
C     COMMON BLOCK
      COMMON/SNCO19/WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,STORGE,

CVK  ADDED TWO MORE STATES
CVK     1   AEADJ,NEXLAG,EXLAG(7)
     1   AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP
     
      COMMON/SNUP19/MFC,SFALLX,WINDC,SCTOL,WETOL,SNOF,UADJC
C.......................................
      OLDWE=WE+LIQW
      AI=ACCMAX
      IF(ACCMAX.GT.SI) AI=SI
      IF(AEADJ.GT.0.0) AI=AEADJ
      OLDAI=AI
      TEX=0.0
      DO 100 N=1,NEXLAG
c     100 TEX=TEX+EXLAG(N)   ! not supported fortran 2018, replace with following 2 lines
  100 END DO
      TEX=TEX+EXLAG(N)
      FREEW=STORGE+TEX
      QUAL=1.0
      IF(WE.GT.0.0)QUAL=1.0+(LIQW/WE)
      WE=(TWE-FREEW)/QUAL
      
CVK  SNOW DEPTH IS ADJUSTED BY RATIO OF THE WE CHANGE
      IF(WE .GT. 0.0) THEN
       SNDPT=SNDPT*WE/(OLDWE-LIQW)
      ELSE
       SNDPT=0.0
       SNTMP=0.0
      ENDIF
CVK--------------------------------------------------
                    
      LIQW=(QUAL-1.0)*WE
      SWE=WE+LIQW
      IF(OLDWE.GT.0.8*ACCMAX) GO TO 110
      IF(SWE.GT.ACCMAX) ACCMAX=SWE
      GO TO 120
  110 ACCMAX=SWE*(ACCMAX/OLDWE)
 120  IF (SWE.GE.AEADJ) AEADJ=0.0
      AI=ACCMAX
      IF(ACCMAX.GT.SI) AI=SI
      IF (AEADJ.GT.0.0) AI=AEADJ
      IF(SWE.LT.AI) GO TO 130
      SB=SWE
      SBWS=SWE
      RETURN
  130 IF((OLDWE.LT.OLDAI).AND.(OLDWE.GT.SB)) GO TO 140
      SB=SWE+SNOF
      SBWS=SWE
      R=(SWE/AI)*10.0+1.0
      GO TO 150
  140 R=SWE/OLDWE
      SB=SB*R
      SBWS=SBWS*R
      IF(SBWS.LT.SB+0.75*SNOF)SBWS=SB+0.75*SNOF
      R=(SB/AI)*10.0+1.0
  150 N=R
      FN=N
      R=R-FN
      SBAESC=ADC(N)+(ADC(N+1)-ADC(N))*R
      IF(SBAESC.GT.1.0) SBAESC=1.0
C.......................................
      RETURN
      END

