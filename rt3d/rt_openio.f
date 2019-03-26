C**************************  OPENIO ************************************
C
C  open the input and output files
C
C***********************************************************************

      subroutine rt_openio

      use rt_global

      implicit none
      
      INTEGER      I,NSPECIES
      CHARACTER*30 FN1,FN2,FN3,FN4,FN5,FN6,FN7,FN8,FN9,FN10
      CHARACTER*30 FN20
      CHARACTER*30 COMP

*     Set the integer values for the input/output files
      !input files
      FNAMES=5000
      INBTN=5001
      INADV=5002
      INDSP=5003
      INSSM=5004
      INRCT=5005
      INGCG=5006
      INHFF=5007
      OUTRES=5010
      !output files
      IOUT=5020
      ISPC=5023
      ICONC=200
      ICBIN=250
      ISSMASS=300
      IOBS=400
      IMAS=450
      IPLOT=500
      IXSECT=550
      IXSBIN=600
      ILCH=3000
      ILCHBIN=3100
      ILCHOBS=3200
      IMASSOUT=3300

*     Open up the file containing the input and output file names      
      OPEN(FNAMES,FILE='rt3d_filenames',STATUS='OLD')

C	Read the input file names and the main output file, and open them up
C     BTN = Basic Transport: record 1
C     ADV = Advection: record 2
C     DSP = Dispersion: record 3
C     SSM = Source/Sink Mixing: record 4
C     RCT = Chemical Reaction: record 5
C     GCG = General Conjugate Gradient: record 6 
C     HHF = Flow simulation linker file (binary: unformatted file): record 7
C     OUT = General output file: record 20
     
*     Read the names of the files from the RT3DAG.fnames file      
      READ(FNAMES,*) FN1
      READ(FNAMES,*) FN2
      READ(FNAMES,*) FN3
      READ(FNAMES,*) FN4
      READ(FNAMES,*) FN5
      READ(FNAMES,*) FN6
c      READ(FNAMES,*) FN7 !linker file is not needed!
      READ(FNAMES,*) FN10

      OPEN(INBTN, FILE=FN1, STATUS='OLD')
      OPEN(INADV, FILE=FN2, STATUS='OLD')
      OPEN(INDSP, FILE=FN3, STATUS='OLD')
      OPEN(INSSM, FILE=FN4, STATUS='OLD')
      OPEN(INRCT, FILE=FN5, STATUS='OLD')
      OPEN(INGCG, FILE=FN6, STATUS='OLD')
c      OPEN(INHFF, FILE=FN7, STATUS='OLD')
      OPEN(OUTRES, FILE=FN10, STATUS='UNKNOWN')
      
*     Read (from the BTN file) the packages that are used for this simulation
      READ(INBTN,*)
      DO I=1,10
        TRNOP(I)=.FALSE.
      ENDDO
      READ(INBTN,*) (TRNOP(I),I=1,10)    

*     Open up file for CPU timing information
      OPEN(5000,FILE='CPU')

      OPEN(IOUT,FILE='rt_output/RT3Dout',STATUS='UNKNOWN')
	  
      return
      end
