C MEMBER AECO19
C  (from old member FCPACK19)
C
      SUBROUTINE AECO19 (COVER,TWE,SI,ADC)
C.......................................
C     THIS SUBROUTINE ADJUSTS SNOW MODEL CARRYOVER VALUES FOR A
C        CHANGE IN THE AREAL EXTENT OF THE SNOW COVER.
C        USED IN THE 'SNOW-17' OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON-HRL APRIL 1981

CVK      MODIFIED 4/00 BY V. KOREN: NEW STATES, SNDPT & SNTMP ADDED
C.......................................
      REAL NEGHS,LIQW
      DIMENSION ADC(11)
C
C     COMMON BLOCK
      COMMON/SNCO19/WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,STORGE,

CVK  ADDED TWO MORE STATES
CVK     1AEADJ,NEXLAG,EXLAG(7)
     1AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP
     
      COMMON/SNUP19/MFC,SFALLX,WINDC,SCTOL,WETOL,SNOF,UADJC
C.......................................
C     DETERMINE IF CURRENTLY ON DEPLETION CURVE OR NEW SNOW LINE.
      TEX=0.0
      DO 90 N=1,NEXLAG
C  90 TEX=TEX+EXLAG(N)    ! not supported fortran 2018, replace with following 2 lines
      TEX=TEX+EXLAG(N)
   90 END DO
      FREEW=STORGE+TEX
      SWE=TWE-FREEW
      AI=ACCMAX
      IF(ACCMAX.GT.SI) AI=SI
      IF(AEADJ.GT.0.0) AI=AEADJ
      IF(SWE.GE.AI) GO TO 100
      IF(SWE.LE.SB) GO TO 100
C.......................................
C     CURRENTLY ON NEW SNOW LINE.
      IF (COVER.LE.SBAESC) GO TO 100
      R=(SWE/AI)*10.0+1.0
      N=R
      FN=N
      R=R-FN
      AESC=ADC(N)+(ADC(N+1)-ADC(N))*R
      IF(COVER.LE.AESC) GO TO 100
C     ADJUST SBWS , LEAVE AEADJ AS IS.
      SBWS=((1.0-SBAESC)/(COVER-SBAESC))*(SWE-SB)+SB
      IF (SBWS.GT.SB+0.75*SNOF) RETURN
      SB=SBWS-0.75*SNOF
      R=(SB/AI)*10.0+1.0
      N=R
      FN=N
      R=R-FN
      SBAESC=ADC(N)+(ADC(N+1)-ADC(N))*R
      RETURN
C.......................................
C     CURRENTLY OR SHOULD BE ON THE DEPLETION CURVE.
  100 DO 101 I=2,11
      IF (COVER.GE.ADC(I)) GO TO 101
      J=I-1
      FJ=J-1
      WEAI=0.1*(FJ+ (COVER-ADC(J))/(ADC(I)-ADC(J)))
      GO TO 105
  101 CONTINUE
      WEAI=1.0
  105 AEADJ=SWE/WEAI
      SBAESC=COVER
C.......................................
      RETURN
      END

