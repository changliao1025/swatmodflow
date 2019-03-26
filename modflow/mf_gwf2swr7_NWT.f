C
C     VERSION 1.01 SWR1 for MODFLOW NWT
C
      MODULE GWFSWRMODULE
        CHARACTER(LEN=64),PARAMETER :: VERSION_SWR =
     +'$Id: gwf2swr7.f 1.01 2012-09-10 15:00:00Z jdhughes $'
C
C---------INVARIANT PARAMETERS
        INTEGER, PARAMETER          :: IUZFOFFS     = 100000
        INTEGER, PARAMETER          :: IZERO        = 0
        REAL, PARAMETER             :: RZERO        = 0.0
        DOUBLEPRECISION, PARAMETER  :: DZERO        = 0.0D0
        REAL, PARAMETER             :: RONE         = 1.0
        DOUBLEPRECISION, PARAMETER  :: DONE         = 1.0D0
        DOUBLEPRECISION, PARAMETER  :: DTWO         = 2.0D0
        DOUBLEPRECISION, PARAMETER  :: DTHREE       = 3.0D0
        DOUBLEPRECISION, PARAMETER  :: DONETHIRD    = 1.0D0 / 3.0D0
        DOUBLEPRECISION, PARAMETER  :: DONEPERCENT  = 1.0D-02
        DOUBLEPRECISION, PARAMETER  :: DONEHALF     = 0.5D00
        DOUBLEPRECISION, PARAMETER  :: DTWOTHIRDS   = 2.0D0 / 3.0D0
        DOUBLEPRECISION, PARAMETER  :: DTHREEHALVES = 3.0D0 / 2.0D0
        DOUBLEPRECISION, PARAMETER  :: DFIVETHIRDS  = 5.0D0 / 3.0D0
        DOUBLEPRECISION, PARAMETER  :: SMALL        = 1.0D-06
        DOUBLEPRECISION, PARAMETER  :: NEARZERO     = EPSILON(1.0D-00)
        DOUBLEPRECISION, PARAMETER  :: DMISSING     = -999.D0
        DOUBLEPRECISION, PARAMETER  :: GRAVITY      = 9.80665D0
        DOUBLEPRECISION, PARAMETER  :: DTWOPI       = 
     2                      6.283185307179586476925286766559005768394D+0
C
C---------SWR DATA TYPE DEFINITIONS
C---------TIMESERIES DATA TYPE
        TYPE TTSDATA
          INTEGER :: INTP
          INTEGER :: NDATA
          INTEGER :: IPOS
          REAL    :: NEXTTIME
          REAL, DIMENSION(:), ALLOCATABLE :: T
          REAL, DIMENSION(:), ALLOCATABLE :: V
          DOUBLEPRECISION :: DVAL   = DZERO
        END TYPE TTSDATA
C---------TABULAR DATA TYPE
        TYPE TTABS
          INTEGER :: ITABTYPE = IZERO
          INTEGER :: ITABUNIT = IZERO
          INTEGER :: NTABRCH  = IZERO
          INTEGER, DIMENSION(:), ALLOCATABLE :: ITABRCH
          INTEGER :: ISTGRES = IZERO
C-----------DERIVED TYPE
          TYPE (TTSDATA) :: TSDATA
          TYPE (TTSDATA), DIMENSION(:), ALLOCATABLE :: STGRES
        END TYPE TTABS
C---------COMMON GEOMETRY DATA TYPE
        TYPE TGEO
          INTEGER :: NGEOPTS              = IZERO
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: ELEV
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: XAREA
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: VOL
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: WETPER
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: TOPWID
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: SAREA
        END TYPE TGEO
C---------IRREGULAR GEOMETER TYPE
        TYPE TIRRGEO
          INTEGER :: NGEOPTS              = IZERO
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: ELEVB
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: XB
        END TYPE TIRRGEO
C---------STRUCTURE SOLUTION DATA TYPE COMPONENTS
        TYPE TSTRSOLN
          DOUBLEPRECISION :: FLOW            = DZERO
          DOUBLEPRECISION :: TOP             = DZERO
          DOUBLEPRECISION :: BOTTOM          = DZERO
          DOUBLEPRECISION :: OPENING         = DZERO
        END TYPE TSTRSOLN
C---------STRUCTURE DATA TYPE
        TYPE TSTRUCT
          INTEGER         :: ISTRTYPE        = IZERO
          INTEGER         :: ISTRCONN        = IZERO
          INTEGER         :: NSTRPTS         = IZERO
          DOUBLEPRECISION :: STRCD           = DZERO
          DOUBLEPRECISION :: STRCD2          = DZERO
          DOUBLEPRECISION :: STRCD3          = DZERO
          DOUBLEPRECISION :: STRINV          = DZERO
          DOUBLEPRECISION :: STRINV2         = DZERO
          DOUBLEPRECISION :: STRWID          = DZERO
          DOUBLEPRECISION :: STRWID2         = DZERO
          DOUBLEPRECISION :: STRLEN          = DZERO
          DOUBLEPRECISION :: STRMAN          = DZERO
          INTEGER         :: ISTRDIR         = IZERO
          DOUBLEPRECISION :: STRVAL          = DZERO
          INTEGER         :: ISTRBC          = IZERO
C         STRUCTURE OPERATION VARIABLES
          INTEGER         :: ISTROVAL        = IZERO
          INTEGER         :: ISTRORCH        = IZERO
          INTEGER         :: ISTRORCHC       = IZERO
          INTEGER         :: ISTROQCON       = IZERO
          INTEGER         :: ISTRLO          = IZERO
          DOUBLEPRECISION :: STRCRIT         = DZERO
          DOUBLEPRECISION :: STRCRITC        = DZERO
          DOUBLEPRECISION :: STRRT           = DZERO
          DOUBLEPRECISION :: STRMAX          = DZERO
          INTEGER         :: ISTROSTG        = IZERO
          INTEGER         :: ISTRTAB         = IZERO
          INTEGER         :: ISTRTSTYPE      = IZERO           
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: STRELEV
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: STRQ
C         CALCULATED STRUCTURE VARIABLES 
          DOUBLEPRECISION :: STRTOPINI       = DZERO
          DOUBLEPRECISION :: STRBOTINI       = DZERO
          DOUBLEPRECISION :: STRTOP0         = DZERO
          DOUBLEPRECISION :: STRBOT0         = DZERO
          DOUBLEPRECISION :: STRTOP          = DZERO
          DOUBLEPRECISION :: STRBOT          = DZERO
C         CALCULATED GATE ELEVATION FOR EACH SWR TIME STEP
C         ONLY ALLOCATED FOR OPERABLE STRUCTURES
          DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: OPRVAL
          TYPE(TSTRSOLN)  :: CURRENT
          TYPE(TSTRSOLN),  DIMENSION(:), ALLOCATABLE :: RSLTS
C         SFR EXCHANGE STRUCTURES
          INTEGER         :: ISFRSEG         = IZERO
          INTEGER         :: ISFRRCH         = IZERO
          INTEGER         :: ISFRSTRM        = IZERO
        END TYPE TSTRUCT
C---------FLOW DATA TYPE
        TYPE TFLOWDATA
          DOUBLEPRECISION :: QMPOSFLOW       = DZERO
          DOUBLEPRECISION :: QLATFLOW        = DZERO
          DOUBLEPRECISION :: QUZFLOW         = DZERO
          DOUBLEPRECISION :: RAIN            = DZERO
          DOUBLEPRECISION :: EVAP            = DZERO
          DOUBLEPRECISION :: QAQFLOW         = DZERO
          DOUBLEPRECISION :: QMNEGFLOW       = DZERO
          DOUBLEPRECISION :: QEXTFLOW        = DZERO
          DOUBLEPRECISION :: QBCFLOW         = DZERO
          DOUBLEPRECISION :: QCRFLOW         = DZERO
          DOUBLEPRECISION :: VOLUME          = DZERO
          DOUBLEPRECISION :: DVOLUME         = DZERO
          DOUBLEPRECISION :: QPOTGWET        = DZERO
        END TYPE TFLOWDATA
C---------BASFLOW SOLUTION DATA TYPE COMPONENTS
        TYPE TQAQTERMS
          DOUBLEPRECISION :: HGW             = DZERO
          DOUBLEPRECISION :: WETTEDPERIMETER = DZERO
          DOUBLEPRECISION :: CONDUCTANCE     = DZERO
          DOUBLEPRECISION :: HEADDIFFERENCE  = DZERO
          DOUBLEPRECISION :: QAQFLOW         = DZERO
        END TYPE TQAQTERMS
C---------NEWTON CORRECTION FOR SWR DATA TYPE
        TYPE TQAQNWT
          DOUBLEPRECISION :: RHS1           = DZERO
          DOUBLEPRECISION :: RHS2           = DZERO
          DOUBLEPRECISION :: CONDN1         = DZERO
          DOUBLEPRECISION :: CONDN2         = DZERO
        END TYPE TQAQNWT
C---------CONNECTION FLOW TERMS
        TYPE TQCONN
          DOUBLEPRECISION :: FLOW            = DZERO
          DOUBLEPRECISION :: AREA            = DZERO
          DOUBLEPRECISION :: DEPTH           = DZERO
          DOUBLEPRECISION :: VELOCITY        = DZERO
        END TYPE TQCONN
C---------JACOBIAN DATA TYPE
        TYPE TJAC
          INTEGER :: IALLO  = IZERO
          INTEGER :: NNZ    = IZERO
          INTEGER :: IBAND  = IZERO
          INTEGER :: NBW    = IZERO
          INTEGER :: NHALFB = IZERO
          INTEGER, DIMENSION(:), ALLOCATABLE :: ICMAP
          INTEGER, DIMENSION(:), ALLOCATABLE :: ISMAP
          INTEGER, DIMENSION(:), ALLOCATABLE :: IA
          INTEGER, DIMENSION(:), ALLOCATABLE :: JA
          DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: FJACU
          DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: FJACL
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: FJACC
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: FSCALE
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: FSCALE2
C           PRECONDITIONER
          INTEGER :: NRLU   = IZERO
          INTEGER :: NNZLU  = IZERO
          INTEGER :: NJLU   = IZERO
          INTEGER :: NJW    = IZERO
          INTEGER :: NWLU   = IZERO
          INTEGER, DIMENSION(:), ALLOCATABLE :: JLU
          INTEGER, DIMENSION(:), ALLOCATABLE :: IU
          INTEGER, DIMENSION(:), ALLOCATABLE :: JW
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: WLU
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: MJACC
C           VARIABLE FOR CONVERSION TO STAGE SOLUTION FROM SOLUTION
C           OF STAGE UPGRADE VECTOR
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: JSKM1
C-----------TEMPORARY STORAGE FOR SOLVERS
C-----------JACOBIAN CALCULATION STORAGE
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: JPSAV
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: JPPH
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: JH
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: JV
C-----------NEWTON STEP STORAGE
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: PS
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: PS0
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: PINF
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: PINF0
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: S
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: S0
C-----------LINEAR SOLVER STORAGE
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: R
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: XI
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: XS
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: X0
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: DX
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: XSI
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: F
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: F0
C-----------LU DECOMP STORAGE
          INTEGER, DIMENSION(:), ALLOCATABLE :: PINDEXLU
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: SLU
C-----------BICGSTAB
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: DBCGS
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: DHATBCGS
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: PBCGS
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: PHATBCGS
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: SBCGS
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: SHATBCGS
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: VBCGS
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: TBCGS
C-----------GMRES          
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: RGMR
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: ZGMR
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: DGMR
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: TGMR
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: CSGMR
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: SNGMR
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: SGMR
          DOUBLEPRECISION, DIMENSION(:),   ALLOCATABLE :: YGMR
          DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: HGMR
          DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: VGMR
        END TYPE TJAC
C---------COMMON REACH DATA TYPE - INCLUDES NESTED TYPES
        TYPE TREACH
C           INPUT DATA
C           REACH LOCATION DATA
          CHARACTER (LEN=2) :: CROUTETYPE
          INTEGER :: IREACH                  = IZERO
          INTEGER :: IROUTETYPE              = IZERO
          INTEGER :: IRGNUM                  = IZERO
          INTEGER :: IRG                     = IZERO
          INTEGER :: KRCH                    = IZERO !LAYER
          INTEGER :: IRCH                    = IZERO !ROW
          INTEGER :: JRCH                    = IZERO !COLUMN
          INTEGER :: LAYSTR                  = IZERO !UPPER LAYER - CALCULATED
          INTEGER :: LAYEND                  = IZERO !LOWER LAYER - CALCULATED
          INTEGER :: LAYACT                  = IZERO !UPPERMOST ACTIVE LAYER
          DOUBLEPRECISION :: DLEN            = DZERO
C           CONNECTIVITY
          INTEGER :: NCONN                   = IZERO
          INTEGER,  DIMENSION(:), ALLOCATABLE :: ICONN
          INTEGER,  DIMENSION(:), ALLOCATABLE :: IRGCONN
          INTEGER,  DIMENSION(:), ALLOCATABLE :: ISTRCONN
          DOUBLEPRECISION,  DIMENSION(:), ALLOCATABLE :: SFM
C           BOUNDARY CODE
          INTEGER :: ISWRBND                 = 1
C           BOUNDARY FLOW
          DOUBLEPRECISION :: RAIN            = DZERO
          DOUBLEPRECISION :: EVAP            = DZERO
          DOUBLEPRECISION :: QLATFLOW        = DZERO
          DOUBLEPRECISION :: QUZFLOW         = DZERO
          DOUBLEPRECISION :: QEXTFLOW        = DZERO
C           GEOMETRIC DATA
          INTEGER :: NGEOPTS                 = IZERO
          INTEGER :: IGEONUM                 = IZERO
          INTEGER :: IGEOTYPE                = IZERO
          INTEGER :: IGCNDOP                 = 1
          DOUBLEPRECISION :: GZSHIFT         = DZERO
          DOUBLEPRECISION :: GMANNING        = DZERO
          DOUBLEPRECISION :: GSSLOPE         = DZERO
          DOUBLEPRECISION :: GWIDTH          = DZERO
          DOUBLEPRECISION :: GTELEV          = DZERO
          DOUBLEPRECISION :: GBELEV          = DZERO
          DOUBLEPRECISION :: GCND            = DZERO
          DOUBLEPRECISION :: GLK             = DZERO
          DOUBLEPRECISION :: GCNDLN          = DZERO
          DOUBLEPRECISION :: GETEXTD         = DZERO
          DOUBLEPRECISION :: RAINAREA        = DZERO
C           STRUCTURE DATA
          INTEGER :: NSTRUCT                 = IZERO
          INTEGER :: BCSTRUCT                = IZERO
          INTEGER :: NOPR                    = IZERO
          TYPE (TSTRUCT), DIMENSION(:), ALLOCATABLE :: STRUCT
          DOUBLEPRECISION :: RPOSFLOW        = DZERO
          DOUBLEPRECISION :: RNEGFLOW        = DZERO
C           PROGRAM CALCULATED DATA
          DOUBLEPRECISION :: STAGE           = DZERO
          INTEGER :: ICALCBFLOW              = IZERO
          DOUBLEPRECISION  :: OFFSET         = DZERO
          DOUBLEPRECISION  :: GWETRATE       = DZERO
          TYPE (TFLOWDATA) :: CURRENT
C           CUMULATIVE SUBSIDENCE
          DOUBLEPRECISION :: GCUMSUB         = DZERO
C           NESTED DATA TYPES
          TYPE (TQAQTERMS), DIMENSION(:),   ALLOCATABLE :: CURRENTQAQ
          TYPE (TQAQTERMS), DIMENSION(:,:), ALLOCATABLE :: QAQRSLTS
          TYPE (TQAQNWT) ,  DIMENSION(:,:), ALLOCATABLE :: QAQNWT
          TYPE (TIRRGEO)  :: IRRGEO
          TYPE (TGEO)     :: GEO
        END TYPE TREACH
C---------COMMON REACH GROUP DATA TYPE - CALCULATED
        TYPE TRCHGRP
          LOGICAL :: CONSTANT              = .FALSE.
          LOGICAL :: INACTIVE              = .FALSE.
          LOGICAL :: DIFFUSIVE             = .FALSE.
          INTEGER :: IRGNUM                = IZERO
          INTEGER :: IRG                   = IZERO
          INTEGER :: IRGUPDATE             = IZERO
          INTEGER :: IRGBND                = IZERO
          INTEGER :: ICALCRCH              = IZERO
          TYPE (TFLOWDATA) :: CURRENT
          INTEGER, DIMENSION(:), ALLOCATABLE :: REACH
          TYPE (TFLOWDATA), DIMENSION(:), ALLOCATABLE :: RSLTS
          DOUBLEPRECISION,  DIMENSION(:), ALLOCATABLE :: RGELEV
          DOUBLEPRECISION,  DIMENSION(:), ALLOCATABLE :: RGVOL
C           REACH GROUP SUMMARY INFORMATION
          INTEGER :: NRGREACH              = IZERO
          INTEGER :: NSTRUCT               = IZERO
          INTEGER :: BCSTRUCT              = IZERO
          LOGICAL :: LRAIN                 = .FALSE.
          LOGICAL :: LEVAP                 = .FALSE.
          LOGICAL :: LBASE                 = .FALSE.
          DOUBLEPRECISION :: DLEN          = DZERO
C           REACH CONNECTIVITY
          INTEGER,  DIMENSION(:), ALLOCATABLE :: IRCHN
          INTEGER,  DIMENSION(:), ALLOCATABLE :: IRCHC
          INTEGER,  DIMENSION(:), ALLOCATABLE :: ISTRRCH
          INTEGER                             :: NCONN = IZERO
          INTEGER,  DIMENSION(:), ALLOCATABLE :: IN
          INTEGER,  DIMENSION(:), ALLOCATABLE :: IR 
          INTEGER,  DIMENSION(:), ALLOCATABLE :: ILOC
C           PROGRAM CALCULATED DATA
          DOUBLEPRECISION :: STAGE           = DZERO
C           RCHGRP CONNECTION FLOW TERMS
          TYPE (TQCONN), DIMENSION(:),   ALLOCATABLE :: QCONN
          TYPE (TQCONN), DIMENSION(:,:), ALLOCATABLE :: QCONNP
        END TYPE TRCHGRP
        TYPE TSWRTIME
          INTEGER :: ITPRN
          INTEGER :: ICNVG
          INTEGER :: ITER
          INTEGER :: ILOCFMAX
          DOUBLEPRECISION :: SWRDT
          DOUBLEPRECISION :: FDELT = DONE
          DOUBLEPRECISION :: FMAX
        END TYPE TSWRTIME
        TYPE TSTABILITY
          DOUBLEPRECISION :: VAL       = DZERO
          INTEGER         :: LOCVAL    = IZERO
          DOUBLEPRECISION :: DVALDL    = DZERO
          INTEGER         :: LOCDVALDL = IZERO
          DOUBLEPRECISION :: DVALDT    = DZERO
          INTEGER         :: LOCDVALDT = IZERO
        END TYPE TSTABILITY
C---------SWR VARIABLES
        DOUBLEPRECISION,SAVE,POINTER  :: SWREXETOT
        INTEGER,SAVE,POINTER          :: ISFRUNIT
        INTEGER,SAVE,POINTER          :: INWTUNIT
C         AUXILIARY VARIABLES
        INTEGER,SAVE,POINTER          :: NAUX
        INTEGER,SAVE,POINTER          :: NQAQCONN
        CHARACTER(LEN=16),SAVE, DIMENSION(:), POINTER :: SWRAUX
        REAL,SAVE,DIMENSION(:,:), POINTER  :: AUX
        REAL,SAVE,DIMENSION(:),   POINTER  :: AUXROW
C         DATASET 1A
        INTEGER,SAVE,POINTER          :: NREACHES
        INTEGER,SAVE,POINTER          :: ISWRONLY
        INTEGER,SAVE,POINTER          :: ISWRCB
        INTEGER,SAVE,POINTER          :: ISWRPRGF
        INTEGER,SAVE,POINTER          :: ISWRPSTG
        INTEGER,SAVE,POINTER          :: ISWRPQAQ
        INTEGER,SAVE,POINTER          :: ISWRPQM
        INTEGER,SAVE,POINTER          :: ISWRPSTR
        INTEGER,SAVE,POINTER          :: ISWRPFRN
        INTEGER,SAVE,POINTER          :: ISWROPT
C         SWR OPTIONS
        INTEGER,SAVE,POINTER          :: ISWRSCRN
        INTEGER,SAVE,POINTER          :: ISWRSDT
        INTEGER,SAVE,POINTER          :: ISWRCNV
        INTEGER,SAVE,POINTER          :: ISWRSRIV
        INTEGER,SAVE,POINTER          :: ISWRALLRIV
        INTEGER,SAVE,POINTER          :: ISWRSAVG
        INTEGER,SAVE,POINTER          :: NTABS
        INTEGER,SAVE,POINTER          :: ITABTIME
        INTEGER,SAVE,POINTER          :: ISWRCONT
        INTEGER,SAVE,POINTER          :: ISWRUPWT
        INTEGER,SAVE,POINTER          :: INEXCTNWT
        INTEGER,SAVE,POINTER          :: ISSSTOR
        INTEGER,SAVE,POINTER          :: ILAGSTROPR
        INTEGER,SAVE,POINTER          :: IDPTHSCL
        INTEGER,SAVE,POINTER          :: IJACSCL
        INTEGER,SAVE,POINTER          :: IRCMRORD
        INTEGER,SAVE,POINTER          :: ISOLSTG
        INTEGER,SAVE,POINTER          :: IWGTHDS
        INTEGER,SAVE,POINTER          :: INWTCORR
        INTEGER,SAVE,POINTER          :: INWTCNT
        INTEGER,SAVE,POINTER          :: ICIQM
        INTEGER,SAVE,POINTER          :: ICIBL
C         SWR VARIABLES
        DOUBLEPRECISION,SAVE,POINTER  :: DLENCONV
        DOUBLEPRECISION,SAVE,POINTER  :: TIMECONV
        DOUBLEPRECISION,SAVE,POINTER  :: QSCALE
        DOUBLEPRECISION,SAVE,POINTER  :: RSCALE
        DOUBLEPRECISION,SAVE,POINTER  :: DMINGRAD
        DOUBLEPRECISION,SAVE,POINTER  :: DMINDPTH
        DOUBLEPRECISION,SAVE,POINTER  :: DUPDPTH
        DOUBLEPRECISION,SAVE,POINTER  :: DMAXRAI
        DOUBLEPRECISION,SAVE,POINTER  :: DMAXSTG
        DOUBLEPRECISION,SAVE,POINTER  :: DMAXINF
        INTEGER,SAVE,POINTER          :: ISOLVER, NOUTER, NINNER
        CHARACTER(LEN=40),SAVE,POINTER:: SOLVERTEXT
        INTEGER,SAVE,POINTER          :: IBT
        INTEGER,SAVE,POINTER          :: NTMIN
        INTEGER,SAVE,POINTER          :: NTMAX
        INTEGER,SAVE,POINTER          :: NUMTIME
        INTEGER,SAVE,POINTER          :: NUMTIME0
        INTEGER,SAVE,POINTER          :: NADPCNT
        INTEGER,SAVE,POINTER          :: NADPCNT0
        INTEGER,SAVE,POINTER          :: NTTSMIN
        LOGICAL,SAVE,POINTER          :: FAILTIME
        INTEGER,SAVE,POINTER          :: ISWRCNVG
        REAL,SAVE,POINTER             :: RTIME
        REAL,SAVE,POINTER             :: RTIME0
        REAL,SAVE,POINTER             :: RTMIN
        REAL,SAVE,POINTER             :: RTMAX
        REAL,SAVE,POINTER             :: RTSTMAX
        REAL,SAVE,POINTER             :: RTPRN
        REAL,SAVE,POINTER             :: RSWRPRN
        REAL,SAVE,POINTER             :: RSWRPRN0
        REAL,SAVE,POINTER             :: RTMULT
        REAL,SAVE,POINTER             :: RTOTIM
        INTEGER,SAVE,POINTER          :: NTMULT
        INTEGER,SAVE,POINTER          :: IADTIME
        DOUBLEPRECISION,SAVE,POINTER  :: TADMULT
        DOUBLEPRECISION,SAVE,POINTER  :: TOLS,TOLR
        DOUBLEPRECISION,SAVE,POINTER  :: TOLA
        DOUBLEPRECISION,SAVE,POINTER  :: DAMPSS, DAMPTR
        INTEGER,SAVE,POINTER          :: IPRSWR
        INTEGER,SAVE,POINTER          :: MUTSWR
        INTEGER,SAVE,POINTER          :: IPC
        INTEGER,SAVE,POINTER          :: NLEVELS
        DOUBLEPRECISION,SAVE,POINTER  :: DROPTOL
        INTEGER,SAVE,POINTER          :: IBTPRT
        INTEGER,SAVE,POINTER          :: ITMP,IPTFLG
        INTEGER,SAVE,POINTER          :: IRDBND,IRDRAI,IRDEVP,IRDLIN
        INTEGER,SAVE,POINTER          :: IRDGEO,IRDSTR,IRDSTG,IRDAUX
        DOUBLEPRECISION,SAVE,DIMENSION(:,:), POINTER  :: BFCNV
        INTEGER,SAVE,POINTER          :: NBDITEMS
        REAL,SAVE,DIMENSION(:,:), POINTER  :: CUMBD
        REAL,SAVE,DIMENSION(:,:), POINTER  :: INCBD
        REAL,SAVE,DIMENSION(:),   POINTER  :: CUMQAQBD
        REAL,SAVE,DIMENSION(:),   POINTER  :: INCQAQBD
        INTEGER,SAVE,POINTER          :: SWRHEADER
        INTEGER,SAVE,POINTER          :: NRCHGRP
        INTEGER,SAVE,POINTER          :: NSOLRG
        INTEGER,SAVE,POINTER          :: ISWRSS
        INTEGER,SAVE,POINTER          :: ISWRDT
        DOUBLEPRECISION,SAVE,POINTER  :: SWRDT
        INTEGER,SAVE,POINTER          :: KMFITER
        DOUBLEPRECISION,SAVE,POINTER  :: SWRHEPS
C---------STORAGE MAPPING FOR COORDINATE INVARIANT FLOW (QM) IN MODEL-BASED CELLS 
        INTEGER,SAVE,POINTER          :: IGEOCI
        INTEGER,SAVE,DIMENSION(:,:), POINTER  :: IQMCI
        DOUBLEPRECISION,SAVE,DIMENSION(:,:), POINTER  :: SFMCI
C---------VARIABLES ACCESSED OUTSIDE SWR - SIMPLE ARRAYS TO SIMPLIFY CALLS
        INTEGER,SAVE,POINTER          :: IEXTFLOW
        REAL,SAVE,DIMENSION(:,:,:),POINTER :: HK
C---------STORAGE OF REACH AND REACH GROUP STAGE
        DOUBLEPRECISION,SAVE,DIMENSION(:,:), POINTER :: RSTAGE
        DOUBLEPRECISION,SAVE,DIMENSION(:,:), POINTER :: GSTAGE
C---------DERIVED DATA TYPES
        TYPE(TTABS),     SAVE,DIMENSION(:),POINTER :: TABDATA
        TYPE(TREACH),    SAVE,DIMENSION(:),POINTER :: REACH
        TYPE(TRCHGRP),   SAVE,DIMENSION(:),POINTER :: RCHGRP
        TYPE(TJAC),      SAVE,             POINTER :: JAC
        TYPE(TSWRTIME),  SAVE,DIMENSION(:),POINTER :: SWRTIME
        TYPE(TSTABILITY),SAVE,POINTER :: FROUDE
C---------PRINT DATA
        INTEGER,SAVE,POINTER          :: NPMAX
        DOUBLEPRECISION,SAVE,DIMENSION(:,:), POINTER :: RSTAGEP
        DOUBLEPRECISION,SAVE,DIMENSION(:,:), POINTER :: GSTAGEP

        TYPE GWFSWRTYPE
          DOUBLEPRECISION, POINTER :: SWREXETOT
          INTEGER,POINTER          :: ISFRUNIT
          INTEGER,POINTER          :: INWTUNIT
C         AUXILIARY VARIABLES
          INTEGER,POINTER          :: NAUX
          INTEGER,POINTER          :: NQAQCONN
          CHARACTER(LEN=16), DIMENSION(:), POINTER :: SWRAUX
          REAL,DIMENSION(:,:), POINTER  :: AUX
          REAL,DIMENSION(:),   POINTER  :: AUXROW
C
          INTEGER,POINTER          :: NREACHES
          INTEGER,POINTER          :: ISWRONLY
          INTEGER,POINTER          :: ISWRCB
          INTEGER,POINTER          :: ISWRPRGF
          INTEGER,POINTER          :: ISWRPSTG
          INTEGER,POINTER          :: ISWRPQAQ
          INTEGER,POINTER          :: ISWRPQM
          INTEGER,POINTER          :: ISWRPSTR
          INTEGER,POINTER          :: ISWRPFRN
          INTEGER,POINTER          :: ISWROPT
C         SWR OPTIONS
          INTEGER,POINTER          :: ISWRSCRN
          INTEGER,POINTER          :: ISWRSDT
          INTEGER,POINTER          :: ISWRCNV
          INTEGER,POINTER          :: ISWRSRIV
          INTEGER,POINTER          :: ISWRALLRIV
          INTEGER,POINTER          :: ISWRSAVG
          INTEGER,POINTER          :: NTABS
          INTEGER,POINTER          :: ITABTIME
          INTEGER,POINTER          :: ISWRCONT
          INTEGER,POINTER          :: ISWRUPWT
          INTEGER,POINTER          :: INEXCTNWT
          INTEGER,POINTER          :: ISSSTOR
          INTEGER,POINTER          :: ILAGSTROPR
          INTEGER,POINTER          :: IDPTHSCL
          INTEGER,POINTER          :: IJACSCL
          INTEGER,POINTER          :: IRCMRORD
          INTEGER,POINTER          :: ISOLSTG
          INTEGER,POINTER          :: IWGTHDS
          INTEGER,POINTER          :: INWTCORR
          INTEGER,POINTER          :: INWTCNT
          INTEGER,POINTER          :: ICIQM
          INTEGER,POINTER          :: ICIBL
C         SWR VARIABLES
          DOUBLEPRECISION,POINTER  :: DLENCONV
          DOUBLEPRECISION,POINTER  :: TIMECONV
          DOUBLEPRECISION,POINTER  :: QSCALE
          DOUBLEPRECISION,POINTER  :: RSCALE
          DOUBLEPRECISION,POINTER  :: DMINGRAD
          DOUBLEPRECISION,POINTER  :: DMINDPTH
          DOUBLEPRECISION,POINTER  :: DUPDPTH
          DOUBLEPRECISION,POINTER  :: DMAXRAI
          DOUBLEPRECISION,POINTER  :: DMAXSTG
          DOUBLEPRECISION,POINTER  :: DMAXINF
          INTEGER,POINTER          :: ISOLVER, NOUTER, NINNER
          CHARACTER(LEN=40),POINTER:: SOLVERTEXT
          INTEGER,POINTER          :: IBT
          INTEGER,POINTER          :: NTMIN
          INTEGER,POINTER          :: NTMAX
          INTEGER,POINTER          :: NUMTIME
          INTEGER,POINTER          :: NUMTIME0
          INTEGER,POINTER          :: NADPCNT
          INTEGER,POINTER          :: NADPCNT0
          INTEGER,POINTER          :: NTTSMIN
          LOGICAL,POINTER          :: FAILTIME
          INTEGER,POINTER          :: ISWRCNVG
          REAL,POINTER             :: RTIME
          REAL,POINTER             :: RTIME0
          REAL,POINTER             :: RTMIN
          REAL,POINTER             :: RTMAX
          REAL,POINTER             :: RTSTMAX
          REAL,POINTER             :: RTPRN
          REAL,POINTER             :: RSWRPRN
          REAL,POINTER             :: RSWRPRN0
          REAL,POINTER             :: RTMULT
          REAL,POINTER             :: RTOTIM
          INTEGER, POINTER         :: NTMULT
          INTEGER, POINTER         :: IADTIME
          DOUBLEPRECISION,POINTER  :: TADMULT
          DOUBLEPRECISION,POINTER  :: TOLS,TOLR
          DOUBLEPRECISION,POINTER  :: TOLA
          DOUBLEPRECISION,POINTER  :: DAMPSS, DAMPTR
          INTEGER,POINTER          :: IPRSWR
          INTEGER,POINTER          :: MUTSWR
          INTEGER,POINTER          :: IPC
          INTEGER,POINTER          :: NLEVELS
          DOUBLEPRECISION,POINTER  :: DROPTOL
          INTEGER,POINTER          :: IBTPRT
          INTEGER,POINTER          :: ITMP,IPTFLG
          INTEGER,POINTER          :: IRDBND,IRDRAI,IRDEVP,IRDLIN
          INTEGER,POINTER          :: IRDGEO,IRDSTR,IRDSTG,IRDAUX
          DOUBLEPRECISION,DIMENSION(:,:), POINTER  :: BFCNV
          INTEGER,POINTER          :: NBDITEMS
          REAL,DIMENSION(:,:), POINTER  :: CUMBD
          REAL,DIMENSION(:,:), POINTER  :: INCBD
          REAL,DIMENSION(:), POINTER  :: CUMQAQBD
          REAL,DIMENSION(:), POINTER  :: INCQAQBD
          INTEGER,POINTER          :: SWRHEADER
          INTEGER,POINTER          :: NRCHGRP
          INTEGER,POINTER          :: NSOLRG
          INTEGER,POINTER          :: ISWRSS
          INTEGER,POINTER          :: ISWRDT
          DOUBLEPRECISION,POINTER  :: SWRDT
          INTEGER,POINTER          :: KMFITER
          DOUBLEPRECISION,POINTER  :: SWRHEPS
C-----------STORAGE MAPPING FOR COORDINATE INVARIANT FLOW (QM) IN MODEL-BASED CELLS 
          INTEGER,POINTER          :: IGEOCI
          INTEGER,DIMENSION(:,:), POINTER  :: IQMCI
          DOUBLEPRECISION,DIMENSION(:,:), POINTER  :: SFMCI
C-----------VARIABLES ACCESSED OUTSIDE SWR - SIMPLE ARRAYS TO SIMPLIFY CALLS
          INTEGER,POINTER          :: IEXTFLOW
          REAL,DIMENSION(:,:,:),POINTER :: HK
C---------STORAGE OF REACH AND REACH GROUP STAGE
          DOUBLEPRECISION,DIMENSION(:,:), POINTER :: RSTAGE
          DOUBLEPRECISION,DIMENSION(:,:), POINTER :: GSTAGE
C-----------DERIVED DATA TYPES
          TYPE(TTABS),     DIMENSION(:),POINTER :: TABDATA
          TYPE(TREACH),    DIMENSION(:),POINTER :: REACH
          TYPE(TRCHGRP),   DIMENSION(:),POINTER :: RCHGRP
          TYPE(TJAC),                   POINTER :: JAC
          TYPE(TSWRTIME),  DIMENSION(:),POINTER :: SWRTIME
          TYPE(TSTABILITY),POINTER :: FROUDE
C-----------PRINT DATA
          INTEGER,POINTER          :: NPMAX
          DOUBLEPRECISION,DIMENSION(:,:), POINTER :: RSTAGEP
          DOUBLEPRECISION,DIMENSION(:,:), POINTER :: GSTAGEP
        END TYPE GWFSWRTYPE

        TYPE(GWFSWRTYPE),SAVE      :: GWFSWRDAT(10)
      END MODULE GWFSWRMODULE
      
      MODULE GWFSWRINTERFACE
        INTERFACE
C-----------ASSUMED SHAPE INTERFACE FOR SORTING A PASSED VECTOR
          SUBROUTINE SSWR_SORT(V)
            DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: V
          END SUBROUTINE SSWR_SORT
C-----------ASSUMED SHAPE INTERFACE FOR CREATING A VECTOR OF UNIQUE VALUES
C           IN A PASSED VECTOR POSSIBLY CONTAINING DUPLICATES
          SUBROUTINE SSWR_UNIQVALUES(A)
            DOUBLEPRECISION, DIMENSION(:), 
     2        ALLOCATABLE, INTENT(INOUT) :: A
          END SUBROUTINE SSWR_UNIQVALUES
C-----------ASSUMED SHAPE INTERFACE FOR LINEARLY INTERPOLATING THE Y AT Z
C           FROM VALUES IN X AND Y VECTORS
          REAL FUNCTION SSWR_RLININT(X,Y,Z) RESULT(v)
            REAL, DIMENSION(:), INTENT(IN) :: X
            REAL, DIMENSION(:), INTENT(IN) :: Y
            REAL, INTENT(IN) :: Z
          END FUNCTION SSWR_RLININT
C-----------ASSUMED SHAPE INTERFACE FOR LINEARLY INTERPOLATING THE Y AT Z
C           FROM VALUES IN X AND Y VECTORS
          DOUBLEPRECISION FUNCTION SSWR_LININT(X,Y,Z) RESULT(v)
            DOUBLEPRECISION, DIMENSION(:), INTENT(IN) :: X
            DOUBLEPRECISION, DIMENSION(:), INTENT(IN) :: Y
            DOUBLEPRECISION, INTENT(IN) :: Z
          END FUNCTION SSWR_LININT
        END INTERFACE
      END MODULE GWFSWRINTERFACE

      SUBROUTINE GWF2SWR7AR(In,Ibcf,Ilpf,Ihuf,Iupw,Isfr,Inwt,Igrid)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR GROUP ROUTING ELEMENTS AND
C     READ NAMED PARAMETER DEFINITIONS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,IFREFM,
     2                      NPER,PERLEN,NSTP,TSMULT
      USE GWFBCFMODULE,ONLY:HYB=>HY,LCB=>LAYCON
      USE GWFLPFMODULE,ONLY:HKL=>HK
      USE GWFHUFMODULE,ONLY:HKH=>HK
      USE GWFUPWMODULE,ONLY:HKU=>HKUPW  !NEWTON
      USE GWFNWTMODULE, ONLY: HEPS      !NEWTON
      USE GWFSWRMODULE
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
      INTEGER, INTENT(IN) :: In
      INTEGER, INTENT(IN) :: Ibcf
      INTEGER, INTENT(IN) :: Ilpf
      INTEGER, INTENT(IN) :: Ihuf
      INTEGER, INTENT(IN) :: Iupw
      INTEGER, INTENT(IN) :: Isfr
      INTEGER, INTENT(IN) :: Inwt
      INTEGER, INTENT(IN) :: Igrid
C     + + + LOCAL DEFINITIONS + + +
      CHARACTER (LEN= 40) :: csolver(3)
      CHARACTER (LEN= 20) :: cmethod(3)
      CHARACTER (LEN= 20) :: cswght(0:1)
      CHARACTER (LEN= 20) :: cdscale(0:2)
      CHARACTER (LEN= 20) :: ciciqm(-1:1)
      CHARACTER (LEN= 20) :: cipc(0:4)
      CHARACTER (LEN= 20) :: cscale(0:2)
      CHARACTER (LEN= 20) :: ctab(0:5)
      CHARACTER (LEN= 20) :: cintp(3)
      CHARACTER (LEN=200) :: line
      CHARACTER (LEN= 20) :: ctabtype, ctabintp
      INTEGER :: iut, iclose
      INTEGER :: i, j, k, n
      INTEGER :: icnt
      INTEGER :: itab
      INTEGER :: irch, irg
      INTEGER :: nconn, iconn
      INTEGER :: lloc, istart, istop, ival
      INTEGER :: idw
      INTEGER :: ierr
      INTEGER :: iu
      INTEGER :: itabunit, ntabrch
      REAL :: r
      REAL :: rt, totim
      REAL :: t0, t1, tf, tp0, tp1
      REAL, ALLOCATABLE, DIMENSION(:,:) :: rlist
      DOUBLEPRECISION :: tim1, tim2
C     + + + LOCAL TYPE FOR SWR1 OPTIONS
      TYPE toptions
        CHARACTER (LEN=35) :: coption
        INTEGER            :: ioptionused
        INTEGER            :: iunit
        INTEGER            :: iconflict
      END TYPE toptions
      CHARACTER (LEN= 10) :: cunit
      CHARACTER (LEN= 35) :: ctemp
      INTEGER, PARAMETER  :: nopt = 25
      INTEGER             :: ifound
      TYPE (toptions), DIMENSION(nopt) :: swroptions = 
     1  (/ toptions('PRINT_SWR_TO_SCREEN                ', 0, 0,  0),  
     2     toptions('SAVE_SWRDT                         ', 0, 1,  0),  
     3     toptions('SAVE_CONVERGENCE_HISTORY           ', 0, 1,  0),  
     4     toptions('SAVE_RIVER_PACKAGE                 ', 0, 1,  5),  
     5     toptions('SAVE_RIVER_PACKAGE_ALL             ', 0, 1,  4),  
     6     toptions('SAVE_AVERAGE_RESULTS               ', 0, 0,  0),  
     7     toptions('USE_TABFILES                       ', 0, 0,  0),  
     8     toptions('USE_NONCONVERGENCE_CONTINUE        ', 0, 0,  0),  
     9     toptions('USE_UPSTREAM_WEIGHTING             ', 0, 0,  0),  
     Z     toptions('USE_INEXACT_NEWTON                 ', 0, 0,  0),  
     1     toptions('USE_STEADYSTATE_STORAGE            ', 0, 0,  0),  
     2     toptions('USE_LAGGED_OPR_DATA                ', 0, 0,  0),  
     3     toptions('USE_LINEAR_DEPTH_SCALING           ', 0, 0,  0),  
     4     toptions('USE_DIAGONAL_SCALING               ', 0, 0, 15),  
     5     toptions('USE_L2NORM_SCALING                 ', 0, 0, 14),  
     6     toptions('USE_RCMREORDERING                  ', 0, 0, 17),  
     7     toptions('USE_RCMREORDERING_IF_IMPROVEMENT   ', 0, 0, 16),  
     8     toptions('USE_STAGE_TRANSFORM                ', 0, 0,  0),  
     9     toptions('USE_WEIGHTED_HEADS                 ', 0, 0,  0),  
     Z     toptions('USE_IMPLICIT_NEWTON_CORRECTION     ', 0, 0, 21),  
     1     toptions('USE_EXPLICIT_NEWTON_CORRECTION     ', 0, 0, 20),  
     2     toptions('USE_ORIGINAL_2D_QM_FORMULATION     ', 0, 0, 23),  
     3     toptions('USE_IMPLICIT_INVARIATE_QM          ', 0, 0, 22),  
     4     toptions('USE_SOURCECELL_INVARIATE_QM        ', 0, 0,  0),  
     5     toptions('END                                ', 0, 0,  0) /)
C     + + + FUNCTIONS + + +
      DOUBLEPRECISION :: SSWR_R2D
C     + + + DATA + + +
      DATA csolver/'LU DECOMPOSITION USING CROUT�S ALGORITHM',
     2             'BICONJUGATE GRADIENT STABILIZED         ',
     3             'GENERALIZED MINIMAL RESIDUAL            '/
      DATA cmethod/'LEVEL-POOL ROUTING  ',
     2             'TILTED-POOL ROUTING ',
     3             'DIFFUSIVE-WAVE      '/
      DATA cswght/'CENTRAL             ',
     2            'UPSTREAM            '/
      DATA cdscale/'NONE                ',
     2             'SIGMOID             ',
     3             'LINEAR              '/
      DATA ciciqm/'IMPLICIT            ',
     2            'NONE                ',
     3            'EXPLICIT            '/
      DATA cipc/'NONE                ',
     2          'JACOBI              ',
     3          'INCOMPLETE LU       ',
     3          'MOD. INCOMPLETE LU  ',
     4          'INCOMPLETE LU THRES.'/
      DATA cscale/'NO SCALING          ',
     2            'SYMMETRIC SCALING   ',
     3            'L2-NORM SCALING     '/
      DATA ctab/'SPECIFIED TIMESTEP  ',
     2          'RAINFALL            ',
     3          'EVAPORATION         ',
     4          'LATERAL INFLOW      ',
     5          'STAGE               ',
     6          'STRUCTURE DATA      '/
      DATA cintp/'NONE                ',
     2           'AVERAGE             ',
     3           'INTERPOLATE         '/
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(1X,/1X,'SWR -- SURFACE-WATER ROUTING PACKAGE,',/,1X,
     2  A64,/1X,'INPUT READ FROM UNIT ',I4)
02001 FORMAT(//1X,'SUMMARY OF SWR1 OPTIONS USED IN SIMULATION',
     2        /1X,'OPTION',1X,'OPTION KEYWORD',21X,1X,' FILE UNIT',
     3        /1X,6('-'),1X,35('-'),1X,10('-'))
02002 FORMAT(  1X,I6,1X,A35,1X,A10)
02003 FORMAT(  1X,53('-'))     
02010 FORMAT(/1X,'NUMBER OF REACHES                         =',I5,
     2       /1X,'SURFACE WATER ONLY SIMULATION (>0=TRUE)   =',I5,
     3       /1X,'LENGTH CONVERSION FACTOR                  =',G15.7,
     4       /1X,'TIME CONVERSION FACTOR                    =',G15.7,
     5       /1X,'MINIMUM GRADIENT FOR FLOW                 =',G15.7,
     6       /1X,'MAXIMUM DEPTH FOR SCALING REACH OUTFLOW   =',G15.7,
     7       /1X,'MINIMUM DEPTH FOR REACH OUTFLOW           =',G15.7,
     8       /1X,'DIFFUSIVE-WAVE APPROX. SPATIAL WEIGHTING  =',1X,A20,
     9       /1X,'DEPTH SCALING                             =',1X,A20,
     Z       /1X,'TWO-DIMENSIONAL COORD.-INVARIANT APPROACH =',1X,A20,
     1       /1X,'MINIMUM ROUTING TIME STEP LENGTH          =',G15.7,
     2       /1X,'MAXIMUM ROUTING TIME STEP LENGTH          =',G15.7,
     3       /1X,'INITIAL ROUTING TIME STEP LENGTH          =',G15.7,
     4       /1X,'OUTPUT PRINT INCREMENT                    =',G15.7,
     5       /1X,'ADAPTIVE TIME STEP INCREMENT (RTMULT)     =',G15.7,
     6       /1X,'ADAPTIVE TIME STEP INCREMENT FREQUENCY    =',I5,
     7       /1X,'MAXIMUM RAINFALL      PER ROUTING STEP    =',G15.7,
     8       /1X,'MAXIMUM STAGE  CHANGE PER ROUTING STEP    =',G15.7,
     9       /1X,'MAXIMUM INFLOW CHANGE PER ROUTING STEP    =',G15.7,/,
     Z       /1X,'CBC OUTPUT UNIT (ISWRCBC)                 =',I5,
     1       /1X,'GROUP FLOW TERMS OUTPUT UNIT (ISWRPRGF)   =',I5,
     2       /1X,'REACH STAGE OUTPUT UNIT (ISWRPSTG)        =',I5,
     3       /1X,'REACH AQ-RCH FLOW OUTPUT UNIT (ISWRPQAQ)  =',I5,
     4       /1X,'REACH LATERAL FLOW OUTPUT UNIT (ISWRPQM)  =',I5,
     5       /1X,'STRUCTURE FLOW OUTPUT UNIT (ISWRPSTR)     =',I5,
     6       /1X,'SUMMARY VELOCITY AND FROUDE INFORMATION   =',L5)
2020  FORMAT(//15X,3('-'),1X,'SWR SOLVER PARAMETERS',1X,3('-'),
     2       /1X,'SOLVER =',1X,A40,
     3       /1X,'PRECONDITIONER                            =',1X,A20,
     4       /1X,'JACOBIAN SCALING                          =',1X,A20,
     5       /1X,'MAXIMUM NUMBER OF OUTER ITERATIONS        =',I5,
     6       /1X,'MAXIMUM NUMBER OF INNER ITERATIONS        =',I5,
     7       /1X,'NON-CONVERGENCE CONT.OF SWR AND MODFLOW   =',L5)
2025  FORMAT( 1X,'NUMBER OF LEVELS FOR ILUT (NLEVELS)       =',I5,
     2       /1X,'DROP TOLERANCE FOR ILUT (DROPTOL)         =',E15.7)
2030  FORMAT( 1X,'LU DECOMPOSITION STORAGE APPROACH         =',A20,
     2       /1X,'OFF DIAGONAL DIMENSION (JAC%NHALFB)       =',I5,
     3       /1X,'MAXIMUM BANDWIDTH (JAC%NBW)               =',I5)
2040  FORMAT( 1X,'MAXIMUM STAGE    TOLERANCE (TOLS)         =',E15.7,
     2       /1X,'MAXIMUM ROOT     TOLERANCE (TOLR)         =',E15.7,
     3       /1X,'INEXACT NEWTON APPROACH (INEXCTNWT)       =',L5,
     4       /1X,'MAXIMUM AQ-RCH FLOW TOLERANCE (TOLA)      =',E15.7,
     5       /1X,'STEADY-STATE DAMPENING PARAMETER (DAMPSS) =',E15.7,
     6       /1X,'TRANSIENT DAMPENING PARAMETER (DAMPTR)    =',E15.7,
     7       /1X,'PSEUDO-TRANSIENT CONTINUATION APPROACH    =',L5,
     8       /1X,'SOLVER OUTPUT FREQUENCY (IPRSWR)          =',I5,
     8       /1X,'CONVERGENCE PRINTING FLAG (MUTSWR)        =',I5,
     9       /1X,'  0 - MAXIMUM RESIDUAL EACH SWR TIMESTEP',
     X       /1X,'  1 - TOTAL NUMBER OF ITERATIONS EACH SWR TIMESTEP',
     1       /1X,'  2 - NO PRINTING',
     2       /1X,'  3 - PRINTING IF CONVERGENCE FAILS',
     3       /1X,'BACKTRACKING ALLOWED                      =',L5)
2050  FORMAT( 1X,'MAXIMUM BACKTRACKING ITERATIONS (IBT)     =',I5,
     2       /1X,'BACKTRACKING OUTPUT FREQUENCY  (IBTPRT)   =',I5)
2060  FORMAT(//1X,'TIME-INVARIANT SURFACE-WATER ROUTING REACH DATA',/,
     2       1X,' REACH',1X,'IROUTETYPE',1X,'SOLUTION METHOD     ',
     3       1X,'IRGNUM',1X,'   IRG',
     4       1X,'   LAY',1X,'   ROW',1X,'   COL',1X,'      DLEN',/
     5       1X,98('-'))
2070  FORMAT(1X,I6,1X,I10,1X,A20,1X,5(I6,1X),1(G10.3,1X))
2080  FORMAT(//1X,'SWR REACH GROUPS:',
     2       1X,'REACHES IN REACH GROUP',/1X,69('-'))
2090  FORMAT(1X,'REACH GROUP =',1X,I5,
     2       1X,'IRGNUM =',1X,I5,1X,'   REACHES',1X,4(I5,1X),
     2       100(:/1X,46X,:4(I5,1X)))
2100  FORMAT(//1X,'SURFACE WATER ONLY SIMULATION',
     2        /1X,'SETTING MODFLOW IBOUND TO ZERO FOR ALL CELLS')
2110  FORMAT(//1X,'SWR PROCESS REQUIRES LAYCON OF 1 OR 3 FOR'
     2       1X,'ALL LAYERS IF THE BCF PACKAGE IS USED',//)
2120  FORMAT(//1X,'SWR PROCESS REQUIRES USE OF THE BCF, LPF,'
     2       1X,'OR HUF FLOW PACKAGES',//)
2130  FORMAT(//1X,'COULD NOT PARSE NTABRCH FOR TABULAR DATA ITEM',
     2         1X,I5)
2140  FORMAT(//1X,'SWR TABULAR DATA:',
     2        /1X,'TYPE                ',1X,' FILE UNIT',
     3         1X,'INTERPOLATION       ',1X,'REACHES',
     4        /1X,82('-'))
2150  FORMAT(1X,A20,1X,I10,1X,A20,1X,5(I5,1X),
     2       100000(:/1X,53X,:5(I5,1X)))
C     + + + CODE + + +
      ALLOCATE(SWREXETOT)
      ALLOCATE(ISFRUNIT)
      ALLOCATE(INWTUNIT)
C       AUXILIARY VARIABLES
      ALLOCATE(NAUX) 
      ALLOCATE(NQAQCONN)
      ALLOCATE(SWRAUX(20))
C
      ALLOCATE(NREACHES)
      ALLOCATE(ISWRONLY)
      ALLOCATE(ISWRCB,ISWRPRGF,ISWRPSTG,ISWRPQAQ,ISWRPQM)
      ALLOCATE(ISWRPSTR)
      ALLOCATE(ISWRPFRN)
      ALLOCATE(ISWROPT)
C       SWR1 OPTIONS
      ALLOCATE(ISWRSCRN,ISWRSDT,ISWRCNV,ISWRSRIV,ISWRALLRIV,ISWRSAVG)
      ALLOCATE(NTABS,ITABTIME)
      ALLOCATE(ISWRCONT)
      ALLOCATE(ISWRUPWT)
      ALLOCATE(INEXCTNWT)
      ALLOCATE(ISSSTOR)
      ALLOCATE(ILAGSTROPR)
      ALLOCATE(IDPTHSCL)
      ALLOCATE(IJACSCL)
      ALLOCATE(IRCMRORD)
      ALLOCATE(ISOLSTG)
      ALLOCATE(IWGTHDS)
      ALLOCATE(INWTCORR)
      ALLOCATE(INWTCNT)
      ALLOCATE(ICIQM)
      ALLOCATE(ICIBL)
C       SWR1 VARIABLES
      ALLOCATE(ISOLVER,SOLVERTEXT,NOUTER,NINNER)
      ALLOCATE(NTMIN,NTMAX,RTMULT,RTOTIM,NTMULT,IADTIME,TADMULT)
      ALLOCATE(NUMTIME,NUMTIME0,NADPCNT,NADPCNT0)
      ALLOCATE(NTTSMIN,FAILTIME,ISWRCNVG)
      ALLOCATE(RTIME,RTIME0,RTMIN,RTMAX,RTSTMAX,RTPRN,RSWRPRN,RSWRPRN0)
      ALLOCATE(IBT)
      ALLOCATE(TOLS,TOLR,TOLA,DAMPSS,DAMPTR,IPRSWR,MUTSWR)
      ALLOCATE(IPC)
      ALLOCATE(NLEVELS)
      ALLOCATE(DROPTOL)
      ALLOCATE(IBTPRT)
      ALLOCATE(DLENCONV,TIMECONV,DMINGRAD,DMINDPTH,DUPDPTH)
      ALLOCATE(DMAXRAI,DMAXSTG,DMAXINF)
      ALLOCATE(QSCALE,RSCALE)
      ALLOCATE(ITMP,IPTFLG,IRDBND,IRDRAI,IRDEVP,IRDLIN)
      ALLOCATE(IRDGEO,IRDSTR,IRDSTG,IRDAUX)
C-------PRINT VARIABLES
      ALLOCATE(NPMAX)
C
C------INTIALIZE SWRTOTEXE TIMER
      SWREXETOT = DZERO
C
C-------START SWR TIMER FOR GWF2SWR7AR
      CALL SSWRTIMER(0,tim1,tim2,SWREXETOT)
C
C-------SET FLAG FOR SFR
      ISFRUNIT = Isfr
C
C-------SET FLAG FOR NWT
      INWTUNIT = Inwt
C
C-------IDENTIFY OPTION AND INITIALIZE # OF GROUP ROUTING ELEMENTS
      WRITE(IOUT,2000) VERSION_SWR, IN
C
C-------READ AND PRINT DIMENSION AND OTHER ASSOCIATED DATA FOR
C       SURFACE-WATER ROUTING PACKAGE
C         PROBLEM DIMENSION AND OUTPUT CONTROL - INPUT ITEM 1
      CALL SSWRDSOUT('1A')
      lloc = 1
      CALL SSWR_RD_COMM(In)
      CALL URDCOM(In,IOUT,line)
      CALL URWORD(line, lloc, istart, istop, 2, NREACHES,  r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, ISWRONLY,  r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, ISWRCB  ,  r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, ISWRPRGF,  r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, ISWRPSTG,  r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, ISWRPQAQ,  r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, ISWRPQM ,  r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, ISWRPSTR,  r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, ISWRPFRN,  r, IOUT, In)
!      CALL URWORD(line, lloc, istart, istop, 2, ISWROPT,   r,-IOUT, In)
C
C-------READ AUXILIARY VARIABLES AND SWROPTIONS FLAG.
      NAUX    = 0
      IRDAUX  = 0
      ISWROPT = 0
      LAUX: DO
        CALL URWORD(line,lloc,istart,istop,1,n,r,IOUT,In)
        IF (line(istart:istop).EQ.'AUXILIARY' .OR.
     1      line(istart:istop).EQ.'AUX') THEN
          CALL URWORD(line,lloc,istart,istop,1,n,r,IOUT,In)
          IF ( NAUX.EQ.20 ) CYCLE LAUX
          NAUX         = NAUX + 1
          SWRAUX(NAUX) = line(istart:istop)
          WRITE(IOUT,12) SWRAUX(NAUX)
   12     FORMAT(1X,'AUXILIARY SWR1 VARIABLE: ',A)
          CYCLE LAUX
        ELSE IF (line(istart:istop).EQ.'SWROPTIONS') THEN
          ISWROPT = 1
          WRITE(IOUT,13)
   13     FORMAT(1X,'SWR1 OPTIONS SPECIFIED IN DATASET 1B')
          CYCLE LAUX
        END IF
        EXIT LAUX
      END DO LAUX
C
C-------EVALUATE ANY KEYWORDS IN DATASET 1A
      ISWRSCRN   = 0
      ISWRSDT    = 0
      ISWRCNV    = 0
      ISWRSRIV   = 0
      ISWRALLRIV = 0
      ISWRSAVG   = 0
      NTABS      = 0
      ISWRCONT   = 0
      ISWRUPWT   = 0
      INEXCTNWT  = 0
      ISSSTOR    = 0
      ILAGSTROPR = 0
      IDPTHSCL   = 1  !DEFAULT IS SIGMOID SCALING
      IJACSCL    = 0
      IRCMRORD   = 0
      ISOLSTG    = 0
      IWGTHDS    = 0
      INWTCORR   = 0
      ICIQM      = 1
      ICIBL      = 1
      icnt       = 0
      READ1B: IF ( ISWROPT.NE.0 ) THEN
        CALL SSWRDSOUT('1B')
        SWROPT: DO
          lloc = 1
          CALL SSWR_RD_COMM(In)
          CALL URDCOM(In,IOUT,line)
          CALL URWORD(line, lloc, istart, istop, 1, ival, r, IOUT, In)
          ifound = 0
          ROPTIONSL: DO i = 1, nopt
            ctemp = swroptions(i)%coption
C             VALID SWR1 OPTION
            IF ( line(istart:istop).EQ.TRIM(ADJUSTL(ctemp)) ) THEN
C               ONLY USE THE FIRST INSTANCE OF A PARTICULAR SWROPTION
              IF ( swroptions(i)%ioptionused.EQ.0 ) THEN
                ifound = 1
                icnt   = icnt + 1
                swroptions(i)%ioptionused = 1
                IF ( swroptions(i)%iunit.NE.0 ) THEN
                  CALL URWORD(line,lloc,istart,istop,2,ival,r,IOUT,In)
                  swroptions(i)%iunit = ival
                END IF
C                 RESET ANOTHER OPTION IF THE CURRENT OPTION INVALIDATES ANOTHER
                j = swroptions(i)%iconflict
                IF ( j.NE.0 ) THEN
                  IF ( swroptions(j)%ioptionused.EQ.1 ) THEN
                    swroptions(i)%ioptionused = 0
                    icnt = icnt - 1
                  END IF
                END IF
C                 TERMINATE READING SWR1 OPTIONS SINCE "END" KEYWORD ENCOUNTERED
                IF ( i.EQ.nopt ) THEN
                  icnt = icnt - 1
                  EXIT SWROPT
                END IF
C                 TERMINATE SEARCH SINCE THE KEYWORD HAS BEEN FOUND
                EXIT ROPTIONSL               
              END IF
            END IF
          END DO ROPTIONSL
          IF ( ifound.EQ.0 ) THEN
            WRITE (IOUT,*) 'UNRECOGNIZED SWR1 OPTION: ', 
     2                      line(istart:istop)
          END IF
        END DO SWROPT
C         PRINT SUMMARY OF OPTIONS SPECIFIED        
        IF ( icnt.GT.0 ) THEN
          WRITE (IOUT,2001)
          POPTIONSL: DO i = 1, (nopt - 1)
            j = swroptions(i)%ioptionused
            IF ( j.NE.0 ) THEN
              cunit = '      NONE'
              IF ( swroptions(i)%iunit.NE.0 ) THEN
                WRITE (cunit,'(I10)') swroptions(i)%iunit
              END IF
              WRITE ( IOUT,2002)
     2           i, swroptions(i)%coption, cunit
            END IF
          END DO POPTIONSL
          WRITE (IOUT,2003)
C           SET APPROPRIATE VARIABLES BASED ON SWR1 OPTIONS
C           PRINT_SWR_TO_SCREEN
          IF ( swroptions( 1)%ioptionused.NE.0 ) ISWRSCRN  =  1
C           SAVE_SWRDT
          IF ( swroptions( 2)%ioptionused.NE.0 ) THEN
            ISWRSDT = swroptions(2)%iunit
          END IF
C           SAVE_CONVERGENCE_HISTORY
          IF ( swroptions( 3)%ioptionused.NE.0 ) THEN
            ISWRCNV = swroptions(3)%iunit
            IF ( ISWRCNV.LT.0 ) THEN
              CALL USTOP('SWR1 ERROR: SAVE_CONVERGENCE_HISTORY UNIT'//
     2                   'MUST BE ASCII')
            END IF
          END IF
C           SAVE_RIVER_PACKAGE
          IF ( swroptions( 4)%ioptionused.NE.0 ) THEN
            ISWRSRIV = swroptions(4)%iunit
          END IF
C           SAVE_RIVER_PACKAGE_ALL
          IF ( swroptions( 5)%ioptionused.NE.0 ) THEN
            ISWRSRIV   = swroptions(5)%iunit
            ISWRALLRIV = 1
          END IF
C           SAVE_AVERAGE_RESULTS
          IF ( swroptions( 6)%ioptionused.NE.0 ) ISWRSAVG   =  1
C           USE_TABFILES
          IF ( swroptions( 7)%ioptionused.NE.0 ) NTABS      =  1
C           USE_NONCONVERGENCE_CONTINUE
          IF ( swroptions( 8)%ioptionused.NE.0 ) ISWRCONT   =  1
C           USE_UPSTREAM_WEIGHTING
          IF ( swroptions( 9)%ioptionused.NE.0 ) ISWRUPWT   =  1
C           USE_INEXACT_NEWTON
          IF ( swroptions(10)%ioptionused.NE.0 ) INEXCTNWT  =  1
C           USE_STEADYSTATE_STORAGE
          IF ( swroptions(11)%ioptionused.NE.0 ) ISSSTOR    =  1
C           USE_LAGGED_OPR_DATA
          IF ( swroptions(12)%ioptionused.NE.0 ) ILAGSTROPR =  1
C           USE_LINEAR_DEPTH_SCALING
          IF ( swroptions(13)%ioptionused.NE.0 ) IDPTHSCL   =  2
C           USE_DIAGONAL_SCALING
          IF ( swroptions(14)%ioptionused.NE.0 ) IJACSCL    =  1
C           USE_L2NORM_SCALING
          IF ( swroptions(15)%ioptionused.NE.0 ) IJACSCL    =  2
C           USE_RCMREORDERING
          IF ( swroptions(16)%ioptionused.NE.0 ) IRCMRORD   =  1
C           USE_RCMREORDERING_IF_IMPROVEMENT
          IF ( swroptions(17)%ioptionused.NE.0 ) IRCMRORD   = -1
C           USE_STAGE_TRANSFORM
          IF ( swroptions(18)%ioptionused.NE.0 ) ISOLSTG    =  1
C           USE_WEIGHTED_HEADS
          IF ( swroptions(19)%ioptionused.NE.0 ) IWGTHDS    =  1
C           USE_IMPLICIT_NEWTON_CORRECTION
          IF ( swroptions(20)%ioptionused.NE.0 ) INWTCORR   =  1
C           USE_EXPLICIT_NEWTON_CORRECTION
          IF ( swroptions(21)%ioptionused.NE.0 ) INWTCORR   = -1
C           USE_ORIGINAL_2D_QM_FORMULATION
          IF ( swroptions(22)%ioptionused.NE.0 ) ICIQM      =  0
C           USE_IMPLICIT_INVARIATE_QM
          IF ( swroptions(23)%ioptionused.NE.0 ) ICIQM      = -1
C           USE_SOURCECELL_INVARIATE_QM          
          IF ( swroptions(24)%ioptionused.NE.0 ) ICIBL      =  0
        END IF
      END IF READ1B
C
C         SOLUTION CONTROLS - INPUT ITEM 2
      CALL SSWRDSOUT('2')
      lloc = 1
      CALL SSWR_RD_COMM(In)
      CALL URDCOM(In,IOUT,line)
      CALL URWORD(line, lloc, istart, istop, 3, ival,      r, IOUT, In)
      DLENCONV = SSWR_R2D(r)
      CALL URWORD(line, lloc, istart, istop, 3, ival,      r, IOUT, In)
      TIMECONV = SSWR_R2D(r)
      CALL URWORD(line, lloc, istart, istop, 3, ival, RTIME, IOUT, In)
      IF ( RTIME.LT.RZERO ) 
     2  CALL USTOP('RTINI MUST BE GREATER THAN ZERO')
      CALL URWORD(line, lloc, istart, istop, 3, ival, RTMIN, IOUT, In)
      IF ( RTMIN.LT.RZERO ) 
     2  CALL USTOP('RTMIN MUST BE GREATER THAN ZERO')
      CALL URWORD(line, lloc, istart, istop, 3, ival, RTMAX, IOUT, In)
      IF ( RTMAX.LT.RZERO ) 
     2  CALL USTOP('RTMAX MUST BE GREATER THAN ZERO') 
      RTSTMAX = RTMAX
      CALL URWORD(line, lloc, istart, istop, 3, ival, RTPRN, IOUT, In)
      IF ( RTPRN.LT.RTMIN ) RTPRN = RZERO
      RSWRPRN  = RTPRN
      RSWRPRN0 = RTPRN
      IF ( RTMAX.LT.RTMIN ) THEN
        CALL USTOP('RTMAX MUST BE GREATER THAN OR EQUAL TO RTMIN') 
      END IF
      IF ( RTIME.LT.RTMIN ) THEN
        CALL USTOP('RTINI MUST BE GREATER THAN OR EQUAL TO RTMIN') 
      END IF
      IF ( RTIME.GT.RTMAX ) THEN
        CALL USTOP('RTINI MUST BE LESS THAN OR EQUAL TO RTMAX') 
      END IF
      CALL URWORD(line, lloc, istart, istop, 3, ival, RTMULT, IOUT, In)
      IF ( RTMULT.LT.RONE ) THEN
        CALL USTOP('RTMULT MUST BE GREATER THAN OR EQUAL TO 1') 
      END IF
      CALL URWORD(line, lloc, istart, istop, 2,NTMULT,     r, IOUT, In)
      IF ( NTMULT.LT.1 ) THEN
        CALL USTOP('NTMULT MUST BE GREATER THAN OR EQUAL TO 1') 
      END IF
      CALL URWORD(line, lloc, istart, istop, 3, ival, r, IOUT, In)
      DMINGRAD = SSWR_R2D(r)
      CALL URWORD(line, lloc, istart, istop, 3, ival, r, IOUT, In)
      IF ( r.LT.RZERO ) THEN
        CALL USTOP('DMINDPTH MUST BE GREATER THAN OR EQUAL TO 0')
      END IF
      DMINDPTH = SSWR_R2D(r)
      IF ( DMINDPTH.LE.DZERO ) THEN
        DMINDPTH = DZERO
        IDPTHSCL = 0
      END IF
      DUPDPTH  = DMINDPTH * 10.0D+00
C       SET DEFAULT VALUES FOR ADAPTIVE TIME STEPPING
      IADTIME = 0
      TADMULT = DONE
      DMAXRAI = DZERO
      DMAXSTG = DZERO
      DMAXINF = DZERO
C       DETERMINE IF ADAPTIVE TIME STEPPING IS BEING USED
      IF ( RTMIN.LT.RTMAX .AND. RTMULT.GT.RONE ) THEN
        IADTIME = 1
        CALL URWORD(line, lloc, istart, istop, 3, ival, r, IOUT, In)
        DMAXRAI  = MAX( DZERO, SSWR_R2D(r) )
        CALL URWORD(line, lloc, istart, istop, 3, ival, r, IOUT, In)
        DMAXSTG  = MAX( DZERO, SSWR_R2D(r) )
        CALL URWORD(line, lloc, istart, istop, 3, ival, r, IOUT, In)
        DMAXINF  = MAX( DZERO, SSWR_R2D(r) )
      END IF
C
C         SOLVER PARAMETERS - INPUT ITEM 3
      CALL SSWRDSOUT('3')
      lloc   = 1
      CALL SSWR_RD_COMM(In)
      CALL URDCOM(In,IOUT,line)
      CALL URWORD(line, lloc, istart, istop, 2,ISOLVER,    r, IOUT, In)
      IF ( ISOLVER.LT.1 ) THEN
        CALL USTOP('ISOLVER MUST MUST BE GREATER THAN 0') 
      END IF
      IF ( ISOLVER.GT.3 ) THEN
        CALL USTOP('ISOLVER MUST MUST BE LESS THAN 3') 
      END IF
      SOLVERTEXT = csolver(ISOLVER)
      CALL URWORD(line, lloc, istart, istop, 2, NOUTER,    r, IOUT, In)
      IF ( NOUTER.LT.1 ) THEN
        CALL USTOP('NOUTER MUST NOT EQUAL 0') 
      END IF
      CALL URWORD(line, lloc, istart, istop, 2, NINNER,    r, IOUT, In)
      IF ( ISOLVER.GT.1 .AND. NINNER.LT.1 ) THEN
        CALL USTOP('NINNER MUST BE GREATER THAN 0 IF '//
     2             'ISOLVER GREATER THAN 1') 
      END IF
      CALL URWORD(line, lloc, istart, istop, 2, IBT,       r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 3, ival,      r, IOUT, In)
      TOLS = SSWR_R2D(r)
      CALL URWORD(line, lloc, istart, istop, 3, ival,      r, IOUT, In)
      TOLR = SSWR_R2D(r)
      CALL URWORD(line, lloc, istart, istop, 3, ival,      r, IOUT, In)
      TOLA = SSWR_R2D(r)
      CALL URWORD(line, lloc, istart, istop, 3, ival,      r, IOUT, In)
      DAMPSS = SSWR_R2D(r)
C-------RESET DAMPSS IF .LE. ZERO
      IF (DAMPSS.LE.DZERO) DAMPSS = DONE
      CALL URWORD(line, lloc, istart, istop, 3, ival,      r, IOUT, In)
      DAMPTR = SSWR_R2D(r)
C-------RESET DAMPTR IF .LE. ZERO
      IF (DAMPTR.LE.DZERO) DAMPTR = DONE
      CALL URWORD(line, lloc, istart, istop, 2, IPRSWR,    r, IOUT, In)
      IF ( IPRSWR.LT.1 ) IPRSWR = 999
      CALL URWORD(line, lloc, istart, istop, 2, MUTSWR,    r, IOUT, In)
      IPC    = 0
      IF (ISOLVER.GT.1) THEN
        CALL URWORD(line, lloc, istart, istop, 2, IPC, r, IOUT, In)
        IF ( IPC.LT.0 ) THEN
          CALL USTOP('IPC MUST BE GREATER THAN OR EQUAL TO 0') 
        END IF
        IF ( IPC.GT.4 ) THEN
          CALL USTOP('IPC MUST BE LESS THAN 4') 
        END IF
        IF ( IPC.EQ.4 ) THEN
          CALL URWORD(line,lloc,istart,istop,2,NLEVELS,r,IOUT,In)
          CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,In)
          DROPTOL = SSWR_R2D(r)
          IF ( NLEVELS.LT.0 ) THEN
            CALL USTOP('ILUT NLEVELS MUST BE GREATER THAN '//
     2                 'OR EQUAL TO 0')
          END IF
          IF ( DROPTOL.LT.DZERO ) THEN
            CALL USTOP('ILUT DROPTOL MUST BE GREATER THAN '//
     2                 'OR EQUAL TO 0.0D0')
          END IF
        END IF
      END IF
      IBTPRT = 0
      IF (IBT.GT.1) THEN
        CALL URWORD(line, lloc, istart, istop, 2, ival, r, IOUT, In)
        IBTPRT = ival
      END IF
C
C-------OUTPUT SWR PROBLEM DIMENSION AND SOLVER DATA
      WRITE (IOUT,2010) NREACHES, ISWRONLY,
     2                  DLENCONV, TIMECONV, DMINGRAD, DUPDPTH, DMINDPTH,
     3                  cswght(ISWRUPWT), cdscale(IDPTHSCL), 
     4                  ciciqm(ICIQM),
     5                  RTMIN, RTMAX, RTIME, RTPRN, RTMULT, NTMULT,
     6                  DMAXRAI, DMAXSTG, DMAXINF, 
     7                  ISWRCB, ISWRPRGF, ISWRPSTG, ISWRPQAQ, ISWRPQM,
     8                  ISWRPSTR, (ISWRPFRN>0)
      WRITE (IOUT,2020) SOLVERTEXT, cipc(IPC), cscale(IJACSCL), 
     2                  NOUTER, NINNER, (ISWRCONT>0)
      IF ( IPC.EQ.4 ) THEN
        WRITE (IOUT,2025) NLEVELS, DROPTOL
      END IF
      WRITE (IOUT,2040) TOLS, TOLR, (INEXCTNWT>0), TOLA, 
     2                  DAMPSS, DAMPTR, (ISSSTOR>0), IPRSWR, MUTSWR, 
     3                  (IBT>1)
      IF (IBT.GT.1) THEN
        WRITE (IOUT,2050) IBT, IBTPRT
      END IF
C
C-------RESET TOLA IF .LE. ZERO
      IF (TOLA.LT.EPSILON(DZERO)) TOLA = -999.D0
C
C-------SET QSCALE AND RSCALE
      QSCALE = DONE / ( DLENCONV * DLENCONV * DLENCONV * TIMECONV )
      RSCALE = DONE / ( DLENCONV * TIMECONV )
C-------SCALE CONSISTENT TOLS, TOLR, AND TOLA TO INTERNAL UNITS (M AND SEC)
      TOLS = TOLS / DLENCONV
      TOLR = TOLR * QSCALE
!      TOLA = TOLA * QSCALE
C
C-------RESET IBOUND IF SURFACE WATER ONLY SIMULATION
      IF ( ISWRONLY.GT.0 ) THEN
        DO k = 1, NLAY
          DO i = 1, NROW
            DO j = 1, NCOL
              IBOUND(j,i,k) = 0
            END DO
          END DO
        END DO
        WRITE (IOUT,2100)
      END IF
C
C-------DETERMINE NTMAX
      NTMAX  = 0
      totim  = RZERO
      RTOTIM = RZERO
      ierr   = 0
      DO n = 1, NPER
        RTOTIM  = RTOTIM + PERLEN(n)
        rt = PERLEN(n) / REAL( NSTP(n), 4 )
        DO i = 1, NSTP(n)
          IF ( TSMULT(n).NE.RONE ) THEN
            IF ( i.GT.1 ) THEN
              rt = rt * TSMULT(n)
            ELSE
              rt = PERLEN(n) * ( RONE - TSMULT(n) ) /
     2            ( RONE - TSMULT(n)**NSTP(n) )
            END IF
          END  IF
          IF ( RTMAX.GT.rt ) THEN
            ierr = 1
            WRITE (IOUT,'(1X,2(A,1X,I10,1X),2(A,1X,G10.3,1X),A)')
     2        'MODFLOW STRESS PERIOD', n,
     3        'TIME STEP', i, 
     4        ': RTMAX (', RTMAX, ') EXCEEDS DELT (', rt, ')'
          END IF
          totim = totim + rt
          NTMAX = MAX( NTMAX, CEILING( rt / RTMIN ) + 1 )
        END DO
      END DO
      NTMAX    = NTMAX
      NUMTIME  = NTMAX
      NADPCNT  = 1
      NADPCNT0 = 1
      IF ( ierr.EQ.1 ) THEN
        WRITE (IOUT,*) 'SWR1 ERROR: REDUCE RTMAX TO A VALUE LESS THAN'//
     2                 ' ALL MODFLOW DELT VALUES'
        CALL USTOP('RTMAX EXCEEDS MODFLOW DELT')
      END IF
C
C-------DETERMINE THE MAXIMUM NUMBER OF PRINT TIMES PER MODFLOW TIME STEP
      NPMAX = 1
      IF ( RTPRN.NE.RZERO ) THEN
        tf  = RZERO
        t0  = RZERO
        tp0 = RZERO
        DO n = 1, NPER
          tf = tf + PERLEN(n)
          rt = PERLEN(n) / REAL( NSTP(n), 4 )
          DO i = 1, NSTP(n)
            IF ( TSMULT(n).NE.RONE ) THEN
              IF ( i.GT.1 ) THEN
                rt = rt * TSMULT(n)
              ELSE
                rt = PERLEN(n) * ( RONE - TSMULT(n) ) /
     2               ( RONE - TSMULT(n)**NSTP(n) )
              END IF
            END  IF
            t1   = t0 + rt
            ival = 0
            DO
              tp1 = tp0 + RTPRN
              IF ( tp1.GT.t1 ) EXIT
              ival = ival + 1
              tp0  = tp1
            END DO
            IF ( ival.GT.NPMAX ) NPMAX = ival
            t0 = t1
          END DO
          t0 = tf
        END DO
      END IF
C       NTMAX IS THE SUM OF RTMIN TIME STEPS PER MODFLOW
C       TIME STEP, THE MAXIMUM NUMBER OF PRINT TIMES PER
C       MODFLOW TIME STEP, AND 2 TO ACCOUNT FOR ADDITIONAL
C       TIME SLIVERS AT THE BEGINNING AND END OF A MODFLOW
C       TIME STEP
      NTMAX = NTMAX + NPMAX + 1      
C
C-------ALLOCATE SPACE FOR DATA OF KNOWN SIZE
      ALLOCATE(REACH(NREACHES))
      ALLOCATE(NRCHGRP,NSOLRG)
      ALLOCATE(JAC)
      ALLOCATE(SWRHEADER)
      ALLOCATE(ISWRSS,ISWRDT,SWRDT,KMFITER)
      ALLOCATE(SWRHEPS)
      NRCHGRP   = IZERO
      NSOLRG    = IZERO
      SWRHEADER = IZERO
      ISWRDT    = IZERO
      ISWRSS    = IZERO
      SWRDT     = DONE
      KMFITER   = IZERO
      SWRHEPS   = 1.0D-7
      ALLOCATE(BFCNV(4,NREACHES))
      BFCNV = DZERO
      ALLOCATE(NBDITEMS)
      NBDITEMS = 9
      ALLOCATE(CUMBD(2,NBDITEMS),INCBD(2,NBDITEMS))
      ALLOCATE(CUMQAQBD(2),INCQAQBD(2))
      CUMBD     = DZERO
      INCBD     = DZERO
      CUMQAQBD  = DZERO
      INCQAQBD  = DZERO

      ALLOCATE(IEXTFLOW)
      IEXTFLOW = 0

      ALLOCATE( IGEOCI )
      IGEOCI   = 0
      IF ( ICIQM.NE.0 ) THEN
        ALLOCATE(IQMCI(NCOL,NROW))
        ALLOCATE(SFMCI(NCOL,NROW))
      ELSE
        ALLOCATE(IQMCI(1,1))
        ALLOCATE(SFMCI(1,1))
      END IF
C
C-------READ REACH DATA 
C       REACH DATA - INPUT ITEM 4A
      CALL SSWRDSOUT('4A')
      CALL SSWR_RD_COMM(In)
      ALLOCATE (rlist(7,NREACHES))
      CALL SSWRLSTRD(In,IOUT,7,NREACHES,rlist,0)
      DO i = 1, NREACHES
C       READ REACH NUMBER
        irch = INT(rlist(1,i))
        IF (irch.GT.NREACHES) CALL USTOP('4A: REACHNO EXCEEDS NREACHES')
        IF (irch.LT.1)        CALL USTOP('4A: REACHNO LESS THAN ONE')
C         EQUATION METHOD          
        REACH(irch)%IREACH     = irch
C         EQUATION METHOD          
        REACH(irch)%IROUTETYPE = INT(rlist(2,i))
C         USER DEFINED REACH GROUP NUMBER
        REACH(irch)%IRGNUM     = INT(rlist(3,i))
C         LAYER          
        REACH(irch)%KRCH       = INT(rlist(4,i))
C         ROW          
        REACH(irch)%IRCH       = INT(rlist(5,i))
C         COL          
        REACH(irch)%JRCH       = INT(rlist(6,i))
C         REACH LENGTH (DLEN)          
        r = rlist(7,i)
        REACH(irch)%DLEN       = SSWR_R2D(r)
C         SET ROUTING TYPE
        SELECT CASE ( REACH(irch)%IROUTETYPE )
          CASE (1)
            REACH(irch)%CROUTETYPE = 'LP'
          CASE (2)
            REACH(irch)%CROUTETYPE = 'TP'
          CASE (3)
            REACH(irch)%CROUTETYPE = 'DW'
          CASE DEFAULT
            CALL USTOP('ERROR: INVALID REACH ROUTING APPROACH '//
     2                 'SPECIFIED - REVISE SWR1 DATASET 4A')
        END SELECT
      END DO
C       CLEAN UP TEMPORARY LIST STORAGE
      DEALLOCATE (rlist)
C
C-------READ REACH DATA 
C       REACH DATA - INPUT ITEM 4B - FULL CONNECTIVITY
      CALL SSWRDSOUT('4B')
      CALL SSWR_RD_COMM(In)
      iclose = 0
      CALL SSWREXTRD(In,IOUT,iut,iclose)
      DO i = 1, NREACHES
        lloc = 1
        CALL URDCOM(iut,IOUT,line)
        CALL URWORD(line, lloc, istart, istop, 2, irch, r, IOUT, iut)
        IF (irch.GT.NREACHES) CALL USTOP('4B: REACHNO EXCEEDS NREACHES')
        IF (irch.LT.1)        CALL USTOP('4B: REACHNO LESS THAN ONE')
        CALL URWORD(line, lloc, istart, istop, 2, nconn, r, IOUT, iut)
        IF (nconn.LT.0)       CALL USTOP('4B: NCONN LESS THAN ZERO')
        REACH(irch)%NCONN = nconn
        ALLOCATE( REACH(irch)%ICONN(nconn) )
        ALLOCATE( REACH(irch)%IRGCONN(nconn) )
        ALLOCATE( REACH(irch)%ISTRCONN(nconn) )
C         EXPLICIT QM MAGNITUDE
        IF ( ICIQM.GT.0 ) THEN
          ALLOCATE( REACH(irch)%SFM(nconn) )
C         IMPLICIT QM MAGNITUDE
        ELSE
          ALLOCATE( REACH(irch)%SFM(1) )
        END IF
        DO n = 1, nconn
          CALL URWORD(line, lloc, istart, istop, 2, iconn, r, IOUT, iut)
          IF (irch.GT.NREACHES) CALL USTOP('4B: ICONN EXCEEDS NREACHES')
          IF (irch.LT.1)        CALL USTOP('4B: ICONN LESS THAN ONE')
          REACH(irch)%ICONN(n)    = iconn
          REACH(irch)%IRGCONN(n)  = 0
          REACH(irch)%ISTRCONN(n) = 0
C           EXPLICIT QM MAGNITUDE
          IF ( ICIQM.GT.0 ) REACH(irch)%SFM(n) = DZERO
        END DO
      END DO
C       DETERMINE IF CONNECTIVITY DATA FOR 4B IS READ FROM AN
C       EXTERNAL FILE SPECIFIED USING OPEN/CLOSE.  IF THIS IS
C       TRUE, USE SSWREXTRD TO CLOSE FILE
      IF ( iclose.NE.0 ) CALL SSWREXTRD(In,IOUT,iut,iclose)
C
C---------CHECK REACH CONNECTIVITY
      CALL SSWR_CNCK_RC()
C
C---------ALLOCATE SPACE FOR REACH GROUP DATA
      CALL SSWR_ALLO_RG()
C
C---------ALLOCATE SPACE FOR SOLUTION DATA SIZED USING NTMAX
C         CALLABLE SUBROUTINE TO ALLOW FOR EXPANSION OF DATA BY
C         ADAPTIVE TIME STEP ALGORITHM
      CALL SSWR_ALLO_SOLN(NTMAX)
C
C-------ALLOCATE AND INITIALIZE SPACE FOR PRINTING REACH AND 
C       REACH GROUP STAGE DATA
      ALLOCATE(RSTAGEP(NREACHES,NPMAX))
      DO i = 1, NREACHES
        DO n = 1, NPMAX
          RSTAGEP(i,n) = DZERO
        END DO
      END DO
      ALLOCATE(GSTAGEP(NRCHGRP,NPMAX))
      DO i = 1, NRCHGRP
        DO n = 1, NPMAX
          GSTAGEP(i,n) = DZERO
        END DO
      END DO
C
      ALLOCATE ( FROUDE )
C
C-------WRITE REACH DATA 
      idw = 0
      WRITE (IOUT,2060)
      DO i = 1, NREACHES
C         DETERMINE IF DIFFUSIVE WAVE REACHES ARE BEING SIMULATED
        IF( REACH(i)%CROUTETYPE.EQ.'DW' ) THEN
          idw = 1
          irg = REACH(i)%IRG
          RCHGRP(irg)%DIFFUSIVE = .TRUE.
        END IF
C         ADDITIONAL ERROR CHECKING OF REACH DATA
        IF(REACH(i)%KRCH.GT.NLAY) CALL USTOP('REACH LAYER EXCEEDS NLAY')
        IF(REACH(i)%IRCH.GT.NROW) CALL USTOP('REACH ROW EXCEEDS NROW')
        IF(REACH(i)%JRCH.GT.NCOL) CALL USTOP('REACH COL EXCEEDS NCOL')
C         NO ERRORS FOR REACH - WRITE TO OUTPUT FILE
        WRITE (IOUT,2070) i,REACH(i)%IROUTETYPE,
     2    ADJUSTL(cmethod(REACH(i)%IROUTETYPE)),
     3    REACH(i)%IRGNUM,REACH(i)%IRG,
     4    REACH(i)%KRCH,REACH(i)%IRCH,REACH(i)%JRCH,REACH(i)%DLEN
      END DO
C
C-------RESET ISWRPFRN IF NO DIFFUSIVE WAVE REACHES
C       NO NEED TO OUTPUT THE LOCATION AND MAGNITUDE OF THE MAXIMUM
C       VELOCITY AND FROUDE NUMBER IF SIMULATION DOES NOT CONTAIN
C       DIFFUSIVE WAVE REACHES OR REACH GROUPS
      IF ( idw.LT.1 ) ISWRPFRN = 0
C      
C-------WRITE REACH GROUP REACH DATA
      WRITE (IOUT,2080)
      DO i = 1, NRCHGRP
        WRITE (IOUT,2090) i, RCHGRP(i)%IRGNUM,
     2        (RCHGRP(i)%REACH(j),j=1,RCHGRP(i)%NRGREACH)
      END DO
C
C-------READ TABULAR DATA DIMENSIONS - DATASET 4C
      ITABTIME = 0
      IF ( NTABS.NE.0 ) THEN
        CALL SSWRDSOUT('4C')
        lloc = 1
        CALL SSWR_RD_COMM(In)
        CALL URDCOM(In,IOUT,line)
        CALL URWORD(line, lloc, istart, istop, 2, NTABS,  r, IOUT, In)
        IF ( NTABS.LT.1 ) THEN
          NTABS = 0
        ELSE
          ALLOCATE (TABDATA(NTABS))
C---------DATASET 4D - TABULAR DATA SPECIFICATIONS
          CALL SSWRDSOUT('4D')
          DO j = 1, NTABS
            lloc = 1
            CALL SSWR_RD_COMM(In)
            CALL URDCOM(In,IOUT,line)
            CALL URWORD(line, lloc, istart, istop, 2, itab, r, IOUT, In)
            CALL URWORD(line, lloc, istart, istop, 0, ival, r, IOUT, In)
            SELECT CASE ( line(istart:istop) )
              CASE ('TIME')
                ival = 0
              CASE ('RAIN')
                ival = 1
              CASE ('EVAP')
                ival = 2
              CASE ('LATFLOW')
                ival = 3
              CASE ('STAGE')
                ival = 4
              CASE ('STRUCTURE')
                ival = 5
              CASE ('SWRSTAGE')
                ival = 4
                TABDATA(itab)%ISTGRES = 1
              CASE DEFAULT
                CALL USTOP('TAB DATA ERROR: UNRECOGNIZED CTABTYPE')
            END SELECT
            TABDATA(itab)%ITABTYPE = ival
            CALL URWORD(line, lloc, istart, istop, 2, ival, r, IOUT, In)
            TABDATA(itab)%ITABUNIT = ival
            CALL URWORD(line, lloc, istart, istop, 0, ival, r, IOUT, In)
            SELECT CASE ( line(istart:istop) )
              CASE ('NONE')
                TABDATA(itab)%TSDATA%INTP = 1
              CASE ('AVERAGE')
                TABDATA(itab)%TSDATA%INTP = 2
              CASE ('INTERPOLATE')
                TABDATA(itab)%TSDATA%INTP = 3
              CASE DEFAULT
                TABDATA(itab)%TSDATA%INTP = 1
            END SELECT
C-------------READ ADDITIONAL TABULAR DATA ITEMS DEFINING REACHES TO APPLY
C             TABULAR RAINFALL, EVAPORATION, LATERAL INFLOW, AND STAGE DATA
            IF ( TABDATA(itab)%ITABTYPE.GT.0 .AND.
     2           TABDATA(itab)%ITABTYPE.LT.5 ) THEN
              CALL URWORD(line, lloc, istart, istop, 0,ival,r,IOUT,In)
              SELECT CASE (line(istart:istop))
                CASE ('ALL')
                  ival = NREACHES
                  TABDATA(itab)%NTABRCH = ival
                  ALLOCATE (TABDATA(itab)%ITABRCH(ival))
                  DO i = 1, TABDATA(itab)%NTABRCH
                    TABDATA(itab)%ITABRCH(i) = i
                  END DO
                CASE DEFAULT
                  READ (line(istart:istop),*,IOSTAT=ierr) ival 
                  IF ( ierr.NE.0 ) THEN
                    WRITE (IOUT,2130) itab
                    CALL USTOP('SWR 4D: COULD NOT PARSE NTABRCH')
                  END IF
                  TABDATA(itab)%NTABRCH = ival
                  ALLOCATE (TABDATA(itab)%ITABRCH(ival))
                  BACKSPACE ( In )
                  READ (In,*) itab, ctabtype, itabunit, ctabintp,
     2              ntabrch, (TABDATA(itab)%ITABRCH(i),i = 1,ntabrch)
              END SELECT
            END IF
C
C-------------IF READING STAGE DATA DETERMINE IF IT IS BEING READ
C             FROM A SWR STAGE OUTPUT FILE
            IF ( TABDATA(itab)%ISTGRES.NE.0 ) THEN
              ALLOCATE( TABDATA(itab)%STGRES(TABDATA(itab)%NTABRCH) )
              DO i = 1, TABDATA(itab)%NTABRCH
                TABDATA(itab)%STGRES(i)%INTP = TABDATA(itab)%TSDATA%INTP
              END DO
            END IF     
C-------------READ TABULAR DATA INTO MEMORY
            iu = TABDATA(itab)%ITABUNIT
            IF ( TABDATA(itab)%ISTGRES.NE.0 ) THEN
              CALL SSWR_RDSTGRES(iu,RTOTIM,TABDATA(itab))
            ELSE
              CALL SSWR_RDTABDATA(iu,RTOTIM,TABDATA(itab)%TSDATA)
            END IF
C-------------SET VARIABLES DEFINING USE OF A SPECIFIED SWR TIMESTEP
            IF ( TABDATA(itab)%ITABTYPE.EQ.0 ) THEN
              ITABTIME = itab
              IADTIME  = -1
              CALL SSWR_RESET_RTMIN()
            END IF
          END DO
        END IF
      END IF
      IF ( NTABS.LT.1 ) THEN
          ALLOCATE (TABDATA(1))
      END IF
C
C-------WRITE SUMMARY OF TABULAR DATA TO OUTPUT FILE
      IF ( NTABS.GT.0 ) THEN
        WRITE (IOUT,2140)
        DO n = 1, NTABS
          i = TABDATA(n)%ITABTYPE
          WRITE (IOUT,2150) ctab(i), TABDATA(n)%ITABUNIT,
     2      cintp(TABDATA(n)%TSDATA%INTP), 
     3      ( TABDATA(n)%ITABRCH(j),j=1,TABDATA(n)%NTABRCH )
        END DO
      END IF
C
C-------POINT SWR1 HK TO APPROPRIATE HORIZONTAL CONDUCTIVITY
      IF ( Ibcf.GT.0 ) THEN
        HK => HYB
C---------MAKE SURE CONFINED AQUIFER ARE NOT SIMULATED
        DO k = 1, NLAY
          IF ( LCB(k).NE.1 .AND. LCB(k).NE.3 ) THEN
            WRITE (IOUT,2110)
            CALL USTOP('CONFINED AQUIFERS CANNOT BE SIMULATED WITH SWR')
          END IF
        END DO
      ELSE IF ( Ilpf.GT.0 ) THEN
        HK => HKL
      ELSE IF ( Ihuf.GT.0 ) THEN
        HK => HKH
      ELSE IF ( Iupw.GT.0 ) THEN
        HK => HKU  !NEWTON
      ELSE
        WRITE (IOUT,2120)
        CALL USTOP('FLOW PACKAGE SPECIFIED INCONSISTENT WITH SWR') 
      END IF
C
C       SET SWRHEPS TO HEPS IF USING NEWTON
      IF ( INWTUNIT.NE.0 ) THEN
        SWRHEPS = HEPS  !NEWTON
      END IF
C
C-------ALLOCATE MEMORY FOR AUXILIARY VARIABLES
      IF ( NAUX.GT.0 ) THEN
        ALLOCATE( AUX(NAUX,NREACHES) )
        ALLOCATE( AUXROW(NAUX) )
        DO i = 1, NREACHES
          DO n = 1, NAUX
            AUX(n,i) = RZERO
          END DO
        END DO
        DO n = 1, NAUX
          AUXROW(n) = RZERO
        END DO
      ELSE
        ALLOCATE( AUX(1,1) )
        ALLOCATE( AUXROW(1) )
        AUX(1,1)  = RZERO
        AUXROW(1) = RZERO
      END IF
C
C-------FINISH SWR TIMER FOR GWF2SWR7AR
      CALL SSWRTIMER(1,tim1,tim2,SWREXETOT)
C
C-------SET POINTERS FOR GRID
      CALL SGWF2SWR7PSV(Igrid)
C
C-------RETURN.
      RETURN
      END SUBROUTINE GWF2SWR7AR
      
      SUBROUTINE GWF2SWR7RP(In,Kkper,Igrid)
C     ******************************************************************
C     READ STRESS PERIOD DATA FOR SWR
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY: IOUT, ISSFLG, 
     +                       NCOL, NROW, NLAY, 
     +                       IFREFM, IBOUND
      USE GWFSWRMODULE
      USE GWFSWRINTERFACE
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
      INTEGER, INTENT(IN) :: In
      INTEGER, INTENT(IN) :: Kkper
      INTEGER, INTENT(IN) :: Igrid
C     + + + LOCAL DEFINITIONS + + +
      LOGICAL :: writece
      CHARACTER (LEN=200) :: line
      CHARACTER (LEN=15)  :: cbnd
      CHARACTER (LEN=10), DIMENSION( 8) :: creach
      CHARACTER (LEN=10), DIMENSION(11) :: cstruct
      CHARACTER (LEN=50), DIMENSION( 2) :: clustor
      CHARACTER (LEN=30), DIMENSION( 5) :: cstrbc
      CHARACTER (LEN=14), DIMENSION(:), ALLOCATABLE :: ccerch
      CHARACTER (LEN=05), DIMENSION(:), ALLOCATABLE :: crg
      CHARACTER (LEN=05), DIMENSION(:), ALLOCATABLE :: cce
      CHARACTER (LEN=05), DIMENSION(:), ALLOCATABLE :: cia
      CHARACTER (LEN=05), DIMENSION(:), ALLOCATABLE :: ciu
      INTEGER :: iut, iclose
      INTEGER :: i, j, k, n
      INTEGER :: kk
      INTEGER :: irch
      INTEGER :: nc
      INTEGER :: irchstg, istrrch, istrnum, istrtype, istrconn, istrbc
      INTEGER :: irchaux
      INTEGER :: nstpstr
      INTEGER :: ibndfail
      INTEGER :: isa
      INTEGER :: irg
      INTEGER :: lloc, istart, istop, ival, indx
      INTEGER :: ierr
      INTEGER :: isfrseg, isfrrch, isfrstrm
      INTEGER :: istr11, istrm0
      INTEGER :: ierr11, ierrstrm
      INTEGER :: istrpts
      INTEGER :: ird
      REAL :: r
      CHARACTER (LEN=10), DIMENSION(:), ALLOCATABLE :: cstage
      INTEGER, DIMENSION(:), ALLOCATABLE :: iconstant
      INTEGER, DIMENSION(:), ALLOCATABLE :: iinactive, iactive
      INTEGER, DIMENSION(:), ALLOCATABLE :: istage
      DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: dstage
      INTEGER :: igeonum, igeotype, igcndop, ngeopts
      DOUBLEPRECISION :: gmanning, gwidth, gbelev, gsslope
      DOUBLEPRECISION :: gcnd, glk, gcndln
      DOUBLEPRECISION :: getextd
      DOUBLEPRECISION :: depth
      DOUBLEPRECISION, DIMENSION(:,:), ALLOCATABLE :: geostor
      DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: ug
      REAL, DIMENSION(:,:), ALLOCATABLE :: rlist
      INTEGER, DIMENSION(:), ALLOCATABLE :: inewstr, istrerr
      CHARACTER (LEN=24) :: aname(4)
      INTEGER :: ird2d
      REAL, DIMENSION(:,:), ALLOCATABLE :: r2d
      REAL, DIMENSION(:,:), ALLOCATABLE :: stage2d
      DOUBLEPRECISION :: fact
      DOUBLEPRECISION :: tim1, tim2
C     + + + FUNCTIONS + + +
      DOUBLEPRECISION :: SSWR_R2D
      INTEGER         :: SSWR_CHKSTR
C     + + + DATA + + +
      DATA clustor/'FULL JACOBIAN STORAGE (NSOLRG X NSOLRG)         ',
     2             'BANDED JACOBIAN STORAGE (NSOLRG X JAC%NBW)       '/
      DATA aname(1) /'        2D RAINFALL DATA'/
      DATA aname(2) /'     2D EVAPORATION DATA'/
      DATA aname(3) /'  2D LATERAL INFLOW DATA'/
      DATA aname(4) /'           2D STAGE DATA'/
      DATA cstrbc/'NONE - CONNECTED REACHES      ',
     2            'ZERO-DEPTH GRADIENT BOUNDARY  ',
     3            'CRITICAL-DEPTH BOUNDARY       ',
     4            'EXTERNAL STRUCTURE BOUNDARY   ',
     5            'SFR PACKAGE INFLOW BOUNDARY   '/
      
C     ------------------------------------------------------------------
C
C     + + + INPUT FORMATS + + +
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(//1X,
     2  'REUSING SURFACE-WATER ROUTING REACHES FROM LAST STRESS PERIOD')
2010  FORMAT(//1X,'ERROR CANNOT REUSE REACH DATA FOR SURFACE-WATER ',
     2  'ROUTING PACKAGE ON FIRST STRESS PERIOD')
2011  FORMAT(//1X,'ERROR CANNOT REUSE REACH GEOMETRY DATA FOR ',
     2  'SURFACE-WATER ROUTING PACKAGE ON FIRST STRESS PERIOD')
2012  FORMAT(//1X,'ERROR CANNOT REUSE REACH STRUCTURE DATA FOR ',
     2  'SURFACE-WATER ROUTING PACKAGE ON FIRST STRESS PERIOD')
2019  FORMAT(//1X,A,
     2       /1X,'ERROR REACH ITEM',I10,' WITH SPECIFIED ',A10,' = ',
     2       I10,/3X,' IS LESS THAN ZERO OR GREATER THAN NREACHES [',
     3       I10,']')
2020  FORMAT(//1X,'ERROR REACH ITEM',I10,' WITH SPECIFIED ',A10,' = ',
     2       I10,/3X,' IS LESS THAN ZERO OR GREATER THAN NREACHES [',
     3       I10,']')
2021  FORMAT(//1X,'STAGE DATA FOR STRESS PERIOD',1X,I10,/,
     2       1X,'REACH',1X,'     STAGE',1X,'    STATUS',/,1X,27('-')) 
2022  FORMAT(1X,I5,1X,G10.3,1X,2X,A8)
2030  FORMAT(//1X,'SURFACE-WATER ROUTING GENERIC GEOMETRY DATA ',
     2       'FOR STRESS PERIOD',I10,//1X,
     3       '  GEO ITEM',1X,'   IGEONUM',1X,'  IGEOTYPE',1X,
     4       '   IGCNDOP',1X,'   NGEOPTS',1X,
     5       '  GMANNING',1X,'    GWIDTH',1X,'    GBELEV',1X,
     6       '   GSSLOPE',1X,'  CONDUCT.',1X,
     7       '  LEAKANCE',1X,'    GCNDLN',1X,'   GETEXTD',1X,
     8       /1X,142('-'))
2040  FORMAT(//1X,'ERROR REACH ITEM',I10,' WITH SPECIFIED ',A10,' = ',
     2       F15.7,' IS LESS THAN ZERO')
2050  FORMAT(//1X,'ERROR REACH ITEM',I10,' WITH SPECIFIED ',A10,' = ',
     2       G15.7,' IS LESS THAN',1X,G15.7)
2060  FORMAT(1X,5(I10,1X),8(A10,1X))
2061  FORMAT(/1X,'IRREGULAR CROSS-SECTION FOR REACH =',1X,I5,/,1X,
     2       '         N',1X,'H. DISTANCE [L]',1X,
     3       '  ELEVATION [L]',
     5       /1X,10('-'),1X,2(15('-'),1X))
2062  FORMAT(1X,I10,2(1X,G15.7))
2063  FORMAT(/1X,'SPECIFIED STAGE, AREA, VOLUME, WETTED PERIMETER, ',
     2       'TOP WIDTH RELATION FOR REACH =',1X,I5,/,1X,
     3       '         N',1X,'  ELEVATION [L]',1X,
     4       ' XSECTAREA [L2]',1X,'    VOLUME [L3]',1X,
     5       'WET. PERIM. [L]',1X,'  TOP WIDTH [L]',1X,
     6       'SURF. AREA [L2]',/1X,10('-'),1X,6(15('-'),1X))
2064  FORMAT(1X,I10,6(1X,G15.7))
2070  FORMAT(/1X,'SURFACE-WATER ROUTING REACH STRESS DATA ',/1X,
     2       '     REACH',1X,'      RAIN',1X,'      EVAP',1X,
     3       '  QLATFLOW',/1X,43('-'))
2080  FORMAT(1X,1(I10,1X),3(E10.3,1X))
2081  FORMAT(/1X,'SURFACE-WATER ROUTING BOUNDARY DATA ',/1X,
     2       '     REACH',1X,'   ISWRBND',1X,
     3       'BND TYPE       ',/1X,36('-'))
2082  FORMAT(1X,2(I10,1X),A15)
2090  FORMAT(/1X,'SURFACE-WATER ROUTING REACH STRUCTURE DATA ',/1X,
     2       '     REACH',1X,' STRUCTURE',1X,'  ISTRTYPE',1X,
     3       '  ISTRCONN',1X,'     STRCD',1X,'    STRCD2',1X,
     4       '    STRCD3',1X,'    STRINV',1X,'   STRINV2',1X,
     5       '    STRWID',1X,'   STRWID2',1X,'    STRLEN',1X,
     6       '    STRMAN',1X,'    STRVAL',1X,' FLOW DIR.',1X,
     7       'BOUNDARY TYPE',
     8       /1X,194('-'))
2100  FORMAT(//1X,'ERROR REACH ITEM',I10,' AT STRUCTURE ITEM',I10,
     2       ' WITH SPECIFIED ',A10,' = ',
     3       F15.7,' IS LESS THAN ZERO')
2110  FORMAT(G10.3)
2115  FORMAT(I10)
2120  FORMAT(1X,4(I10,1X),11(A10,1X),A30)
2121  FORMAT(//1X,91('*'),/,35X,
     2       '- - - WARNING - - -',/4X,'STRUCTURE ITEM =',I5,1X,
     3       'IN REACH =',I5,1X,
     3       'ALLOWS FOR UPSTREAM OR BI-DIRECTIONAL FLOW ',/,4X,
     4       '(ISTRDIR <= 0) AND DOWNSTREAM REACH =',I5,1X,
     5       'HAS A SPECIFIED WATER SLOPE = ',G10.3,//,7X,
     6       'THIS COMBINATION CAN RESULT IN UNREALISTIC UPSTREAM ',
     7       'FLOW OF WATER DRIVEN BY THE',/,8X,'SPECIFIED WATER ',
     8       'SURFACE SLOPE INSTEAD OF DRIVEN BY SIMULATED STAGE ',
     X       'GRADIENTS',/,1X,91('*'))
2130  FORMAT(//1X,'REACH GROUPS FOR STRESS PERIOD',
     2       20X,A10,/1X,90('-'))
2140  FORMAT(1X,'REACH GROUP =',1X,I5,1X,A10,1X,8(I5,1X),
     2       /1X,41X,10(8(I5,1X)))
2141  FORMAT(//25X,'CONSTANT STAGE/INACTIVE REACH GROUPS',
     2        /1X,90('-'))
2142  FORMAT(1X,'REACH GROUP =',1X,I5,1X,'IS DEFINED AS A ',
     2       A14,1X,'ELEMENT')
2143  FORMAT(/1X,'WARNING -- ',
     2  'REACH GROUP =',1X,I5,1X,'IS DEFINED AS A',1X,
     2  'CONSTANT STAGE ELEMENT',/,12X,'BUT REACH =',1X,I5,1X,
     3  'IS DEFINED WITH A ISWRBND (',I5,') GREATER THAN -1.',/,12X,
     5  'TO RESOLVE THE INCONSISTENCY ISWRBND IS BEING RESET TO -999.') 
2150  FORMAT(//1X,'REACH OFFSETS FOR STRESS PERIOD',
     2       /1X,10(I10,1X),100(:/1X,10(I10,1X)))
2160  FORMAT(1X,109('-'),/1X,10(F10.4,1X),100(:/1X,10(F10.4,1X)))
2170  FORMAT(//1X,'DOWNSTREAM REACH FOR EACH REACH GROUP ',
     2       '(RCHGRP) FOR STRESS PERIOD',/1X,83('-'),
     3       100(:/,6(1X,A2,I4.4,1X,'=',1X,I4)))
2171  FORMAT(//1X,'REACH RATING CURVE DATA FOR STRESS PERIOD',
     2       /1X,90('-'))
2172  FORMAT(/1X,'REACH   =',1X,I5,/,1X,
     2       'LAYSTR  =',1X,I5,/,1X,'LAYEND  =',1X,I5,/,1X,
     3       '         N',1X,'  ELEVATION [L]',1X,
     4       ' XSECTAREA [L2]',1X,'    VOLUME [L3]',1X,
     5       'WET. PERIM. [L]',1X,'  TOP WIDTH [L]',1X,
     6       'SURF. AREA [L2]',/1X,10('-'),1X,6(15('-'),1X))
2173  FORMAT(1X,I10,6(1X,G15.7))
2180  FORMAT(//1X,'NEW RATING CURVE FOR REACH GROUPS FOR ',
     2       'STRESS PERIOD',/1X,90('-'))
2190  FORMAT(/1X,'REACH GROUP =',1X,I5,
     2       1X,'GROUP =',1X,I5,//1X,
     3       '         N',1X,'  ELEVATION [L]',1X,
     4       '      DEPTH [L]',1X,'    VOLUME [L3]',
     5       /1X,10('-'),1X,3(15('-'),1X))
2200  FORMAT(1X,I10,2(1X,G15.7),1X,G15.7)
2400  FORMAT(//1X,'STEADY-STATE STRESS PERIOD')
4100  FORMAT( /8X,'    SURFACE-WATER ROUTING     ',
     2        /8X,' STRUCTURES USING TIMESERIES  ',
     3        /8X,'     DATA FOR OPERATION       ',
     4        /1X,43('-'),
     5        /1X,'     REACH',1X,' STRUCTURE',1X,
     6            ' DATA TYPE',1X,' TAB. FILE',
     9        /1X,43('-'))
4110  FORMAT(1X,2(I10,1X),A10,1X,I10)
4120  FORMAT(1X,43('-'))
4221  FORMAT(//1X,'AUXILIARY VARIABLES DATA FOR STRESS PERIOD',1X,I10,/,
     2       1X,'REACH',20(:1X,A16)) 
4222  FORMAT(1X,'-----',20(:A17))
4223  FORMAT(1X,I5,20(:7X,G10.3))
C
C     + + + CODE + + +
C-------SET POINTERS FOR GRID
      CALL SGWF2SWR7PNT(Igrid)
C
C-------START SWR TIMER FOR GWF2SWR7RP
      CALL SSWRTIMER(0,tim1,tim2,SWREXETOT)
C
C-------CONFIRM THAT THERE ARE ACTIVE SURFACE-WATER ROUTING REACHES
      IF( NREACHES.LT.1 ) RETURN
C
C-------SET STEADY-STATE SWR FLAG FOR CURRENT STRESS PERIOD
C         PSEUDO-TRANSIENT CONTINUATION APPROACH USED IF STEADY-STATE
      ISWRSS = 0
      IF ( ISSFLG(Kkper).EQ.1 ) THEN
        WRITE (IOUT,2400)
        ISWRSS = 1
        IF ( ISSSTOR.NE.0 ) THEN
          ISWRSS = 2
        END IF
      END IF
C
C-------READ ITMP(FLAG TO REUSE DATA) - INPUT ITEM 5
      CALL SSWRDSOUT('5')
      CALL SSWR_RD_COMM(In)
      lloc = 1
      CALL URDCOM(In,IOUT,LINE)
      CALL URWORD(line, lloc, istart, istop, 2, ITMP,   r, IOUT, In)
C
C-------USE DATA FROM LAST STRESS-PERIOD OF ITMP LESS THAN 1
C       ISSUE ERROR MESSAGE IF ITMP LESS THAN 1 AND FIRST STRESS PERIOD
      IF (ITMP.LT.1) THEN
        IF (Kkper.GT.1) THEN
          WRITE (IOUT,2000)
          RETURN
        ELSE
          WRITE (IOUT,2010)
          CALL USTOP(' ')
        END IF
      END IF
C
C-------READ REST OF STRESS PERIOD DATA FLAGS - INPUT ITEM 5
      CALL URWORD(line, lloc, istart, istop, 2, IRDBND, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, IRDRAI, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, IRDEVP, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, IRDLIN, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, IRDGEO, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, IRDSTR, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, IRDSTG, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, IPTFLG, r, IOUT, In)
      IF ( NAUX.GT.0 ) THEN
        CALL URWORD(line, lloc, istart, istop, 2, IRDAUX, r,-IOUT, In)
      END IF
C-------CHECK FOR ERRORS ON FIRST STRESS PERIOD
      IF (Kkper.EQ.1) THEN
        IF ( IRDBND.LT.NREACHES ) THEN
          CALL USTOP('IRDBND MUST BE EQUAL TO NREACHES ON THE FIRST '//
     2               'STRESS PERIOD')
        END IF
        IF ( IRDGEO.LT.NREACHES ) THEN
          WRITE (IOUT,2011)
          CALL USTOP('IRDGEO MUST BE EQUAL TO NREACHES ON THE FIRST '//
     2               'STRESS PERIOD')
        END IF
        IF ( NAUX.GT.0 ) THEN
          IF ( IRDAUX.LT.NREACHES ) THEN
            CALL USTOP('IRDAUX MUST BE EQUAL TO NREACHES ON THE '//
     2                 'FIRST STRESS PERIOD')
          END IF
        END IF
      END IF
C-------CHECK FOR ERRORS WITH FLAGS
      IF ( IRDBND.GT.NREACHES ) THEN
          CALL USTOP('IRDBND MUST NOT EXCEED NREACHES')
      END IF
      IF ( IRDRAI.GT.NREACHES ) THEN
          CALL USTOP('IRDRAI MUST NOT EXCEED NREACHES')
      END IF
      IF ( IRDEVP.GT.NREACHES ) THEN
          CALL USTOP('IRDEVP MUST NOT EXCEED NREACHES')
      END IF
      IF ( IRDLIN.GT.NREACHES ) THEN
          CALL USTOP('IRDLIN MUST NOT EXCEED NREACHES')
      END IF
      IF ( IRDGEO.GT.NREACHES ) THEN
          CALL USTOP('IRDGEO MUST NOT EXCEED NREACHES')
      END IF
      IF ( IRDSTR.GT.NREACHES ) THEN
          CALL USTOP('IRDSTR MUST NOT EXCEED NREACHES')
      END IF
      IF ( IRDSTG.GT.NREACHES ) THEN
          CALL USTOP('IRDSTG MUST NOT EXCEED NREACHES')
      END IF
      IF ( IRDAUX.GT.NREACHES ) THEN
          CALL USTOP('IRDAUX MUST NOT EXCEED NREACHES')
      END IF
C
C-------INITIALIZE AND ALLOCATE TEMPORARY STORAGE
      REACH(:)%IGEONUM = -1
      isa = 0
      IF (IRDSTR.GT.0) THEN
        ALLOCATE(inewstr(NREACHES),istrerr(NREACHES))
        inewstr = 0
        istrerr = 0
      END IF
      RCHGRP(:)%IRGUPDATE = IZERO
C
C-------READ DATA FOR THIS STRESS PERIOD
C-------BOUNDARY DATA
      IF ( IRDBND.GT.IZERO ) THEN
        CALL SSWRDSOUT('6')
        CALL SSWR_RD_COMM(In)
        ALLOCATE (rlist(2,IRDBND))
        CALL SSWRLSTRD(In,IOUT,2,IRDBND,rlist,0)
        DO i = 1, IRDBND
C           READ REACH NUMBER
          irch = INT(rlist(1,i))
          IF ( irch.LT.1 .OR. irch.GT.NREACHES ) THEN
            WRITE (IOUT,2019) 'INPUT ITEM 6: SWR BOUNDARY',
     2                        i, 'REACH', irch, NREACHES
            CALL USTOP(' ')
          END IF
          REACH(irch)%ISWRBND = INT(rlist(2,i))
C           START: RESET CURRENT VALUES TO ZERO FOR INACTIVE SWR REACHES
          IF ( REACH(irch)%ISWRBND.EQ.0 ) THEN
            REACH(irch)%LAYACT            = NLAY + 1
            REACH(irch)%CURRENT%QMPOSFLOW = DZERO
            REACH(irch)%CURRENT%QLATFLOW  = DZERO
            REACH(irch)%CURRENT%QUZFLOW   = DZERO
            REACH(irch)%CURRENT%RAIN      = DZERO
            REACH(irch)%CURRENT%EVAP      = DZERO
            REACH(irch)%CURRENT%QAQFLOW   = DZERO
            REACH(irch)%CURRENT%QMNEGFLOW = DZERO
            REACH(irch)%CURRENT%QEXTFLOW  = DZERO
            REACH(irch)%CURRENT%QBCFLOW   = DZERO
            REACH(irch)%CURRENT%QCRFLOW   = DZERO
            REACH(irch)%CURRENT%QPOTGWET  = DZERO
            DO k = 1, NLAY
              REACH(irch)%CURRENTQAQ(k)%WETTEDPERIMETER = DZERO
              REACH(irch)%CURRENTQAQ(k)%CONDUCTANCE     = DZERO
              REACH(irch)%CURRENTQAQ(k)%QAQFLOW         = DZERO
              IF ( INWTCORR.NE.0 ) THEN
                DO n = 1, NTMAX
                  REACH(irch)%QAQNWT(n,k)%RHS1   = DZERO
                  REACH(irch)%QAQNWT(n,k)%RHS2   = DZERO
                  REACH(irch)%QAQNWT(n,k)%CONDN1 = DZERO
                  REACH(irch)%QAQNWT(n,k)%CONDN2 = DZERO
                END DO
              END IF
            END DO
          END IF
C           END:   RESET CURRENT VALUES TO ZERO FOR INACTIVE SWR REACHES
        END DO
C         CLEAN UP TEMPORARY LIST STORAGE
        DEALLOCATE (rlist)
C
C---------CHECK THAT BOUNDARY DATA IS CORRECTLY SPECIFIED FOR
C           REACH GROUPS WITH MORE THAN ONE REACH
        RCHGRP(:)%CONSTANT = .FALSE.
        RCHGRP(:)%INACTIVE = .FALSE.
        ALLOCATE ( iconstant(NRCHGRP) )
        ALLOCATE ( iinactive(NRCHGRP), iactive(NRCHGRP) )
        iconstant = 0
        iinactive = 0
        iactive   = 0
        DO i = 1, NREACHES
          irg = REACH(i)%IRG
          RCHGRP(irg)%IRGBND = REACH(i)%ISWRBND
          IF ( REACH(i)%ISWRBND.LT.0 ) THEN
            RCHGRP(irg)%CONSTANT = .TRUE.
            iconstant(irg) = iconstant(irg) + 1
          END IF
          IF ( REACH(i)%ISWRBND.EQ.0 ) THEN
            RCHGRP(irg)%INACTIVE = .TRUE.
            iinactive(irg) = iinactive(irg) + 1
          END IF
C           ACTIVE REACHES
          IF ( REACH(i)%ISWRBND.GT.0 ) THEN
            iactive(irg) = iactive(irg) + 1
            REACH(i)%CURRENT%QCRFLOW = DZERO
          END IF
        END DO
C---------WRITE SUMMARY OF BOUNDARIES FOR EACH REACH GROUP
        ibndfail = 0
        WRITE (IOUT,'(//2X,A,/1X,4(A10,1X),/1X,4(10("-"),1X))') 
     2    'SUMMARY OF SWR REACH BOUNDARY CONDITIONS',
     3    '       IRG','  INACTIVE','  CONSTANT','    ACTIVE'
        DO n = 1, NRCHGRP
          WRITE (IOUT,'(1X,4(I10,1X))') 
     2      n, iinactive(n), iconstant(n), iactive(n)
C           INACTIVE AND ACTIVE REACHES IN REACH GROUP - ERROR CONDITION
          IF ( iinactive(n).GT.IZERO .AND. iactive(n).GT.IZERO ) THEN
            ibndfail = 1
          END IF
C           CONSTANT AND ACTIVE REACHES IN REACH GROUP - ERROR CONDITION
          IF ( iconstant(n).GT.IZERO .AND. iactive(n).GT.IZERO ) THEN
            ibndfail = 1
          END IF
C           INACTIVE AND CONSTANT REACHES IN REACH GROUP - ERROR CONDITION
          IF ( iconstant(n)*iinactive(n).GT.IZERO ) THEN
            ibndfail = 1
          END IF
        END DO
C---------CHECK FOR CORRECT SPECIFICATION OF BOUNDARIES FOR REACH GROUPS
        IF ( ibndfail.GT.IZERO ) THEN
          DO i = 1, NREACHES
            irg = REACH(i)%IRG
            IF ( REACH(i)%ISWRBND.GT.0 ) THEN
              IF ( RCHGRP(irg)%CONSTANT ) THEN
                WRITE (IOUT,'(/1X,A,1X,I5,1X,A)') 
     2            'ALL REACHES IN RCHGRP',irg,
     3            'SHOULD BE SPECIFIED AS CONSTANT STAGE REACHES'
              END IF
              IF ( RCHGRP(irg)%INACTIVE ) THEN
                WRITE (IOUT,'(/1X,A,1X,I5,1X,A)') 
     2            'ALL REACHES IN RCHGRP',irg,
     3            'SHOULD BE SPECIFIED AS INACTIVE REACHES'
              END IF
            END IF
          END DO
          CALL USTOP('ERROR SPECIFYING ISWRBND')
        END IF
C        
C---------WRITE SUMMARY OF CONSTANT STAGE REACH GROUPS
        k = 0
        WRITE (IOUT,2141)
        DO n = 1, NRCHGRP
          IF ( RCHGRP(n)%CONSTANT ) THEN
            k = 1
            WRITE (IOUT,2142) n, 'CONSTANT STAGE'
          END IF
          IF ( RCHGRP(n)%INACTIVE ) THEN
            k = 1
            WRITE (IOUT,2142) n, 'INACTIVE      '
          END IF
        END DO
        IF ( k.EQ.IZERO ) WRITE (IOUT,'(1X,A,/)') 'ALL REACHES ACTIVE'
C
C-------CLEAN UP TEMPORARY SWR BOUNDARY STORAGE
        DEALLOCATE( iconstant, iinactive, iactive )
      END IF
C
C-------REACH RAINFALL DATA
      IF ( IRDRAI.GT.0 ) THEN      
        CALL SSWRDSOUT('7A')
        CALL SSWR_RD_COMM(In)
        ALLOCATE (rlist(2,IRDRAI))
        CALL SSWRLSTRD(In,IOUT,2,IRDRAI,rlist,0)
        DO i = 1, IRDRAI
C         READ REACH NUMBER
          irch = INT(rlist(1,i))
          IF ( irch.LT.1 .OR. irch.GT.NREACHES ) THEN
            WRITE (IOUT,2019) 'INPUT ITEM 7A: SWR REACH RAINFALL',
     2                        i, 'REACH', irch, NREACHES
            CALL USTOP(' ')
          END IF
          REACH(irch)%RAIN = SSWR_R2D(rlist(2,i))
        END DO
C         CLEAN UP TEMPORARY LIST STORAGE
        DEALLOCATE (rlist)
C         2D RAINFALL DATA
      ELSE IF ( IRDRAI.LT.0 ) THEN
        CALL SSWRDSOUT('7B')
        ALLOCATE( r2d(NCOL,NROW) )
        kk = 0
        CALL SSWR_RD_COMM(In)
        CALL U2DREL(r2d(:,:),aname(1),NROW,NCOL,kk,In,IOUT)
C       PROCESS 2D DATA
        DO irch = 1, NREACHES
          i = REACH(irch)%IRCH  !ROW
          j = REACH(irch)%JRCH  !COL
          REACH(irch)%RAIN     = SSWR_R2D( r2d(j,i) )
        END DO
        DEALLOCATE( r2d )
      END IF
C       REACH EVAPORATION DATA
      IF ( IRDEVP.GT.0 ) THEN      
        CALL SSWRDSOUT('8A')
        CALL SSWR_RD_COMM(In)
        ALLOCATE (rlist(2,IRDEVP))
        CALL SSWRLSTRD(In,IOUT,2,IRDEVP,rlist,0)
        DO i = 1, IRDEVP
C         READ REACH NUMBER
          irch = INT(rlist(1,i))
          IF ( irch.LT.1 .OR. irch.GT.NREACHES ) THEN
            WRITE (IOUT,2019) 'INPUT ITEM 8A: SWR REACH EVAPORATION',
     2                        i, 'REACH', irch, NREACHES
            CALL USTOP(' ')
          END IF
          REACH(irch)%EVAP = SSWR_R2D(rlist(2,i))
        END DO
C         CLEAN UP TEMPORARY LIST STORAGE
        DEALLOCATE (rlist)
C         2D EVAPORATION DATA
      ELSE IF ( IRDEVP.LT.0 ) THEN      
         CALL SSWRDSOUT('8B')
        ALLOCATE( r2d(NCOL,NROW) )
        kk = 0
        CALL SSWR_RD_COMM(In)
        CALL U2DREL(r2d(:,:),aname(2),NROW,NCOL,kk,In,IOUT)
C       PROCESS 2D DATA
        DO irch = 1, NREACHES
          i = REACH(irch)%IRCH  !ROW
          j = REACH(irch)%JRCH  !COL
          REACH(irch)%EVAP     = SSWR_R2D( r2d(j,i) )
        END DO
        DEALLOCATE( r2d )
      END IF
C       REACH LATERAL INFLOW DATA
      IF ( IRDLIN.GT.0 ) THEN      
        CALL SSWRDSOUT('9A')
        ALLOCATE (rlist(2,IRDLIN))
        CALL SSWRLSTRD(In,IOUT,2,IRDLIN,rlist,0)
        DO i = 1, IRDLIN
C         READ REACH NUMBER
          irch = INT(rlist(1,i))
          IF ( irch.LT.1 .OR. irch.GT.NREACHES ) THEN
            WRITE (IOUT,2019) 'INPUT ITEM 9A: SWR REACH LATERAL INFLOW',
     2                        i, 'REACH', irch, NREACHES
            CALL USTOP(' ')
          END IF
          REACH(irch)%QLATFLOW = SSWR_R2D(rlist(2,i))
        END DO
C         CLEAN UP TEMPORARY LIST STORAGE
        DEALLOCATE (rlist)
C         2D LATERAL INFLOW DATA
      ELSE IF ( IRDLIN.LT.0 ) THEN
        CALL SSWRDSOUT('9B')
        ALLOCATE( r2d(NCOL,NROW) )
        kk = 0
        CALL SSWR_RD_COMM(In)
        CALL U2DREL(r2d(:,:),aname(3),NROW,NCOL,kk,In,IOUT)
C       PROCESS 2D DATA
        DO irch = 1, NREACHES
          i = REACH(irch)%IRCH  !ROW
          j = REACH(irch)%JRCH  !COL
          irg  = REACH(irch)%IRG
          fact = REACH(irch)%DLEN / RCHGRP(irg)%DLEN 
          REACH(irch)%QLATFLOW = fact * SSWR_R2D( r2d(j,i) ) 
        END DO
        DEALLOCATE( r2d )
      END IF
C
C-------PRINT REACH BOUNDARY AND STRESS DATA
      IF (IPTFLG.GT.0) THEN
C         BOUNDARY DATA
        IF ( IRDBND.GT.0 ) THEN
          WRITE (IOUT,2081)
          DO i = 1, NREACHES
            SELECT CASE (REACH(i)%ISWRBND)
              CASE (:-1)
                cbnd = ' CONSTANT STAGE'
              CASE (0)
                cbnd = '       INACTIVE'
              CASE (1:)
                cbnd = '         ACTIVE'
            END SELECT
            WRITE (IOUT,2082) i, REACH(i)%ISWRBND, ADJUSTL(cbnd)
          END DO
        END IF
C         STRESS DATA
        IF( IRDRAI.GT.0 .OR. IRDEVP.GT.0 .OR. IRDLIN.GT.0 ) THEN
          WRITE (IOUT,2070)
          DO i = 1, NREACHES
C             ERROR CHECKING OF REACH DATA
            IF ( REACH(i)%RAIN.LT.DZERO ) THEN
              CALL USTOP('POSITIVE REACH RAIN VALUE REQUIRED')
            END IF
            IF ( REACH(i)%EVAP.LT.DZERO ) THEN
              CALL USTOP('POSITIVE REACH EVAP VALUE REQUIRED')
            END IF
C             WRITE DATA
            WRITE (IOUT,2080) i, REACH(i)%RAIN, REACH(i)%EVAP,
     2        REACH(i)%QLATFLOW
          END DO
        END IF
      END IF      
C
C-------GEOMETRY DATA
      IF ( IRDGEO.GT.IZERO ) THEN
        ALLOCATE ( ug(IRDGEO) )
C         DATA SET 10
        CALL SSWRDSOUT('10')
        CALL SSWR_RD_COMM(In)
        ALLOCATE (rlist(3,IRDGEO))
        CALL SSWRLSTRD(In,IOUT,3,IRDGEO,rlist,0)
        DO i = 1, IRDGEO
C         READ REACH NUMBER
          irch = INT(rlist(1,i))
          IF ( irch.LT.1 .OR. irch.GT.NREACHES ) THEN
            WRITE (IOUT,2019) 
     2                   'INPUT ITEM 10: SWR REACH GEOMETRY DEFINITION',
     3                   i, 'REACH', irch, NREACHES
            CALL USTOP(' ')
          END IF
          ival = INT(rlist(2,i))
          IF ( ival.GT.IZERO ) THEN
            REACH(irch)%IGEONUM = ival
            ug(i) = REAL( ival, 8 )
            irg = REACH(irch)%IRG
            RCHGRP(irg)%IRGUPDATE = 1
            REACH(irch)%GZSHIFT = SSWR_R2D(rlist(3,i))
          END IF
        END DO
C         CLEAN UP TEMPORARY LIST STORAGE
        DEALLOCATE (rlist)
C         GET UNIQUE GEOMETRY NUMBERS      
        CALL SSWR_UNIQVALUES(ug)
C       DATA SET 11A-11C - GEOMETRY DATA
        CALL SSWRDSOUT('11')
        CALL SSWR_RD_COMM(In)
        iclose = 0
        CALL SSWREXTRD(In,IOUT,iut,iclose)
        DO i = 1, SIZE(ug)
C           INITIALIZE DATA
          ngeopts     = IZERO
          gwidth      = DZERO
          gbelev      = DZERO
          gmanning    = DZERO
          gsslope     = DZERO
          gcnd        = DZERO
          glk         = DZERO
          gcndln      = DZERO
C           READ GENERIC GEOMETRY DATA        
          CALL SSWR_RD_COMM(iut)
          lloc = 1
          CALL URDCOM(iut,IOUT,LINE)
          CALL URWORD(line,lloc,istart,istop,2, igeonum,r,IOUT,iut)
          CALL URWORD(line,lloc,istart,istop,2,igeotype,r,IOUT,iut)
          CALL URWORD(line,lloc,istart,istop,2, igcndop,r,IOUT,iut)
          CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
          gmanning = SSWR_R2D(r)
          IF (gmanning.LT.DZERO) THEN
            WRITE (IOUT,2040) i, 'GMANNING', gmanning
            CALL USTOP(' ')
          END IF
          IF (igeotype.EQ.3 .OR. igeotype.EQ.4) THEN
            CALL URWORD(line,lloc,istart,istop,2,ngeopts,r,IOUT,iut)
          ELSE
            ngeopts = 1
          END IF
          ALLOCATE(geostor(ngeopts,5))
          geostor = DZERO
C           gwidth AND gbelev
          IF (igeotype.EQ.1 .OR. igeotype.EQ.2) THEN
            CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
            gwidth = SSWR_R2D(r)
            IF (gwidth.LT.SMALL) THEN
              WRITE (IOUT,2050) i, 'GWIDTH', gwidth, SMALL
              CALL USTOP(' ')
            END IF
            CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
            gbelev = SSWR_R2D(r)
            IF (igeotype.EQ.2) THEN
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              gsslope = SSWR_R2D(r)
              IF (gsslope.LT.DZERO) THEN
                WRITE (IOUT,2040) i, 'GSSLOPE', gsslope
                CALL USTOP(' ')
              END IF
            END IF
          END IF
C           LEAKANCE DATA
          IF (igcndop.EQ.0) THEN
            CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
            gcnd = SSWR_R2D(r)
            IF (gcnd.LT.DZERO) THEN
              WRITE (IOUT,2040) i, 'GCND', gcnd
              CALL USTOP(' ')
            END IF
          END IF
          IF (igcndop.EQ.1 .OR. igcndop.EQ.3) THEN
            CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
            glk = SSWR_R2D(r)
            IF (glk.LT.DZERO) THEN
              WRITE (IOUT,2040) i, 'GLK', glk
              CALL USTOP(' ')
            END IF
          END IF
          IF (igcndop.EQ.2 .OR. igcndop.EQ.3) THEN
            CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
            gcndln = SSWR_R2D(r)
            IF (gcndln.LT.DZERO) THEN
              WRITE (IOUT,2040) i, 'GCNDLN', gcndln
              CALL USTOP(' ')
            END IF
          END IF
          IF ( igeotype.EQ.5 ) THEN
            CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
            getextd = SSWR_R2D(r)
          END IF
C          
C-----------READ IRREGULAR GEOMETRY AND SPECIFIED RATING CURVE
          IF (igeotype.EQ.3 .OR. igeotype.EQ.4) THEN
            CALL SSWR_RD_COMM(iut)
            IF ( igeotype.EQ.3 ) THEN
              ALLOCATE (rlist(2,ngeopts))
              CALL SSWRLSTRD(iut,IOUT,2,ngeopts,rlist,0)
            ELSE IF ( igeotype.EQ.4 ) THEN
              ALLOCATE (rlist(5,ngeopts))
              CALL SSWRLSTRD(iut,IOUT,5,ngeopts,rlist,0)
            END IF
            DO j = 1, ngeopts
              geostor(j,1) = SSWR_R2D(rlist(1,j))
              geostor(j,2) = SSWR_R2D(rlist(2,j))
              IF (igeotype.EQ.4) THEN
                geostor(j,3) = SSWR_R2D(rlist(3,j))
                geostor(j,4) = SSWR_R2D(rlist(4,j))
                geostor(j,5) = SSWR_R2D(rlist(5,j))
              END IF
            END DO
            DEALLOCATE (rlist)
          END IF
C
C-----------FILL EACH REACH WITH SPECIFIED GEOMETRY DATA
          FILLRCH: DO irch = 1, NREACHES
            IF (REACH(irch)%IGEONUM.LT.1) CYCLE FILLRCH
            IF (REACH(irch)%IGEONUM.NE.igeonum) CYCLE FILLRCH
            CALL SSWR_CALC_RCHGEODATA(irch,igeotype,igcndop,ngeopts,
     2        gmanning,gwidth,gbelev,gsslope,
     3        gcnd,glk,gcndln,getextd,geostor)
C-------------SET STARTING AND ENDING LAYER
            CALL SSWR_SET_RCHLAY(irch)         
          END DO FILLRCH
C
C--------RECALCULATE THE TOTAL NUMBER OF QAQ CONNECTIONS
          NQAQCONN = 0
          CQAQCONN: DO irch = 1, NREACHES
            NQAQCONN = NQAQCONN + 
     2        REACH(irch)%LAYEND - REACH(irch)%LAYSTR + 1
          END DO CQAQCONN
C
C--------WRITE GEOMETRY DATA TO OUTPUT FILE 
          IF (IPTFLG.GT.IZERO) THEN
            IF (i.EQ.1) WRITE (IOUT,2030) Kkper
            creach = '          '
            WRITE (creach(1),2110) gmanning
C             RECTANGULAR OR TRAPEZOIDAL DATA
            IF (igeotype.EQ.1 .OR. igeotype.EQ.2) THEN
              WRITE (creach(2),2110) gwidth
              WRITE (creach(3),2110) gbelev
C               TRAPEZOIDAL DATA
              IF (igeotype.EQ.2) THEN
                WRITE (creach(4),2110) gsslope
              END IF
C             IRREGULAR CROSS-SECTION
            ELSE IF (igeotype.EQ.3) THEN
C             SPECIFIED STAGE-AREA-VOLUME RELATION
            ELSE IF (igeotype.EQ.4) THEN
C             POTENTIAL SURFACE STORAGE COVERING THE ENTIRE CELL (E.G. LAKE)
            ELSE IF (igeotype.EQ.5) THEN
            END IF
            IF (igcndop.EQ.0) THEN
              WRITE (creach(5),2110) gcnd
            ELSE IF (igcndop.EQ.1) THEN
              WRITE (creach(6),2110) glk
            ELSE IF (igcndop.EQ.2) THEN
              WRITE (creach(7),2110) gcndln
            ELSE IF (igcndop.EQ.3) THEN
              WRITE (creach(6),2110) glk
              WRITE (creach(7),2110) gcndln
            END IF
            IF ( igeotype.EQ.5 ) THEN
              WRITE (creach(8),2110) getextd
            END IF
            WRITE (IOUT,2060) i, igeonum, 
     2        igeotype, igcndop, ngeopts,
     3        (creach(k),k=1,8)   
          END IF        
C           CLEAN UP TEMPORARY STORAGE
          DEALLOCATE(geostor)
        END DO
C         CLEAN UP TEMPORARY STORAGE
        DEALLOCATE(ug)
C         DETERMINE IF GEOMETRY DATA FOR 11B IS READ FROM AN
C         EXTERNAL FILE SPECIFIED USING OPEN/CLOSE.  IF THIS IS
C         TRUE, USE SSWREXTRD TO CLOSE FILE
        IF ( iclose.NE.0 ) CALL SSWREXTRD(In,IOUT,iut,iclose)
      END IF
C
C       RESET TWO-DIMENSIONAL GEOMETRY DATA
      IF ( IRDGEO.GT.0 .AND. ICIQM.NE.0 ) THEN
        IGEOCI = 0
        DO i = 1, NROW
          DO j = 1, NCOL
            IQMCI(j,i) = IZERO
            SFMCI(j,i) = DZERO
          END DO
        END DO
        DO irch = 1, NREACHES
          IF (REACH(irch)%IGEONUM.LT.1) CYCLE
          igeotype = REACH(irch)%IGEOTYPE
          IF ( igeotype.EQ.5 ) THEN
            i = REACH(irch)%IRCH
            j = REACH(irch)%JRCH
            IGEOCI     = 1
            IQMCI(j,i) = irch
          END IF
        END DO
      END IF
C
C       OUTPUT IRREGULAR CROSS-SECTION DATA      
      IF ( IPTFLG.GT.IZERO .AND. IRDGEO.GT.0 ) THEN
C         RAW DATA
        DO irch = 1, NREACHES
          IF (REACH(irch)%IGEONUM.LT.1) CYCLE
          igeotype = REACH(irch)%IGEOTYPE
          ngeopts = REACH(irch)%NGEOPTS
          IF (igeotype.EQ.3) THEN
              WRITE (IOUT,2061) irch
              DO j = 1, ngeopts
                WRITE (IOUT,2062) j, 
     2          REACH(irch)%IRRGEO%XB(j),REACH(irch)%IRRGEO%ELEVB(j)
            END DO
          ELSE IF (igeotype.EQ.4) THEN
            WRITE (IOUT,2063) irch
            DO j = 1, ngeopts
              WRITE (IOUT,2064) j, REACH(irch)%GEO%ELEV(j),
     2          REACH(irch)%GEO%XAREA(j), REACH(irch)%GEO%VOL(j),
     3          REACH(irch)%GEO%WETPER(j), 
     4          REACH(irch)%GEO%TOPWID(j), REACH(irch)%GEO%SAREA(j)
            END DO
          END IF
        END DO
C         PROCESSED DATA        
        WRITE (IOUT,2171)
        WPROGDATA: DO irch = 1, NREACHES
          IF (REACH(irch)%IGEONUM.LT.1) CYCLE WPROGDATA
          WRITE (IOUT,2172) irch,REACH(irch)%LAYSTR,REACH(irch)%LAYEND
          DO j = 1, REACH(irch)%GEO%NGEOPTS
            WRITE (IOUT,2173) j, REACH(irch)%GEO%ELEV(j),
     2        REACH(irch)%GEO%XAREA(j), REACH(irch)%GEO%VOL(j),
     3        REACH(irch)%GEO%WETPER(j), 
     4        REACH(irch)%GEO%TOPWID(j), REACH(irch)%GEO%SAREA(j)
          END DO
        END DO WPROGDATA
      END IF
C
C---------STRUCTURE DATA
      IF ( IRDSTR.GT.0 ) THEN
        CALL SSWRDSOUT('12')
        nstpstr = IZERO
        CALL SSWR_RD_COMM(In)
        ALLOCATE (rlist(2,IRDSTR))
        CALL SSWRLSTRD(In,IOUT,2,IRDSTR,rlist,0)
C         DATA SET 12
        DO i = 1, IRDSTR
          irch = INT(rlist(1,i))
          IF ( irch.LT.1 .OR. irch.GT.NREACHES ) THEN
            WRITE (IOUT,2019) 
     2                  'INPUT ITEM 12: SWR REACH STRUCTURE DEFINITION',
     3                  i, 'REACH', irch, NREACHES
            CALL USTOP(' ')
          END IF
          ival = INT(rlist(2,i))
          IF ( ival.GT.IZERO ) THEN
            REACH(irch)%NSTRUCT  = ival
            REACH(irch)%BCSTRUCT = IZERO
            nstpstr = nstpstr + ival
            inewstr(irch) = 1
C           ALLOCATE SPACE FOR STRUCTURE
            IF ( ALLOCATED(REACH(irch)%STRUCT) ) THEN
              DEALLOCATE(REACH(irch)%STRUCT)
            END IF
            ALLOCATE(REACH(irch)%STRUCT(REACH(irch)%NSTRUCT))
          END IF
        END DO
C         CLEAN UP TEMPORARY LIST STORAGE
        DEALLOCATE (rlist)
C
C         DATA SET 13A
C         READ REACH STRUCTURE ASSIGNMENT DATA
        CALL SSWRDSOUT('13')
        CALL SSWR_RD_COMM(In)
        iclose = 0
        CALL SSWREXTRD(In,IOUT,iut,iclose)
        DO i = 1, nstpstr
          lloc = 1
          CALL URDCOM(iut,IOUT,LINE)
          CALL URWORD(line,lloc,istart,istop,2,istrrch,r,IOUT,iut)
C           ERROR WITH  DATA
          IF ( istrrch.LT.1 .OR. istrrch.GT.NREACHES ) THEN
            CALL USTOP('ISTRRCH.LT.1 OR .GT.NREACHES')
          END IF
          CALL URWORD(line,lloc,istart,istop,2,istrnum,r,IOUT,iut)
          IF ( istrnum.LT.1 .OR. 
     2         istrnum.GT.REACH(istrrch)%NSTRUCT ) THEN
            CALL USTOP('ISTRNUM.LT.1 OR .GT.REACH(ISTRRCH)%NSTRUCT')
          END IF
          CALL URWORD(line,lloc,istart,istop,2,istrconn,r,IOUT,iut)
          REACH(istrrch)%STRUCT(istrnum)%ISTRCONN = istrconn
          istrbc = 1
          REACH(istrrch)%STRUCT(istrnum)%ISTRBC   = istrbc
          IF ( istrconn.LT.1 ) THEN
            istrconn = 0
            REACH(istrrch)%STRUCT(istrnum)%ISTRCONN = 0
            REACH(istrrch)%STRUCT(istrnum)%ISTRBC   = 4
            REACH(istrrch)%BCSTRUCT = 1
          END IF
C           CHECK THAT SPECIFIED STRUCTURE CONNECTION IS VALID FOR THE REACH
          ierr = SSWR_CHKSTR(istrrch,istrconn,istrnum,istrerr)
C           GET STRUCTURE TYPE
          CALL URWORD(line,lloc,istart,istop,2,istrtype,r,IOUT,iut)
          REACH(istrrch)%STRUCT(istrnum)%ISTRTYPE = ABS( istrtype )
          SELECT CASE( ABS(istrtype) )
C             NO STRUCTURE
            CASE (0)
C             EXCESS VOLUME CONTROLLED BY DEPTH OR STAGE
            CASE (1)
C             FREE CONNECTION
            CASE (2)
              IF ( istrconn.EQ.IZERO ) THEN
C                 ZERO-DEPTH GRADIENT BOUNDARY CONDITION
                IF ( istrtype.LT.0 ) THEN
                  istrbc = 2
C                 CRITICAL-DEPTH BOUNDARY CONDITION
                ELSE
                  istrbc = 3
                END IF
                REACH(istrrch)%STRUCT(istrnum)%ISTRBC = istrbc
              END IF
              IF ( istrtype.LT.0 ) THEN
                CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
                REACH(istrrch)%STRUCT(istrnum)%STRINV = SSWR_R2D(r) 
                istrtype = ABS( istrtype )
              ELSE
                REACH(istrrch)%STRUCT(istrnum)%STRINV = 
     2            REACH(istrrch)%GEO%ELEV(1) + SMALL
              END IF
C             SPECIFIED DISCHARGE
            CASE (3)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRVAL = SSWR_R2D(r)
C               UPDATE INITAL PUMP RATE
              REACH(istrrch)%STRUCT(istrnum)%STRTOP0   = SSWR_R2D(r)
              REACH(istrrch)%STRUCT(istrnum)%STRTOPINI = SSWR_R2D(r)
C             SPECIFIED H-Q RELATION
            CASE (4)
              CALL URWORD(line,lloc,istart,istop,2,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%NSTRPTS = ival
              IF(ALLOCATED(REACH(istrrch)%STRUCT(istrnum)%STRELEV))THEN
                DEALLOCATE(REACH(istrrch)%STRUCT(istrnum)%STRELEV,
     2                     REACH(istrrch)%STRUCT(istrnum)%STRQ)
              END IF
              ALLOCATE(REACH(istrrch)%STRUCT(istrnum)%STRELEV(ival),
     2                 REACH(istrrch)%STRUCT(istrnum)%STRQ(ival))
              CALL SSWR_RD_COMM(iut)
              ALLOCATE (rlist(2,ival))
              CALL SSWRLSTRD(iut,IOUT,2,ival,rlist,0)
              DO n = 1, ival
                REACH(istrrch)%STRUCT(istrnum)%STRELEV(n) = 
     2            SSWR_R2D( rlist(1,n) )
                REACH(istrrch)%STRUCT(istrnum)%STRQ(n) =
     2            SSWR_R2D( rlist(2,n) )
              END DO
C               CLEAN UP TEMPORARY LIST STORAGE
              DEALLOCATE (rlist)
C               CULVERT
            CASE (5)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRCD = SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRCD2 = SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRINV = SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRINV2 = SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRWID = ABS( SSWR_R2D(r) )
              IF ( r.LT.RZERO ) THEN
                CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
                REACH(istrrch)%STRUCT(istrnum)%STRWID2 = SSWR_R2D(r)
              END IF
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRLEN = SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRMAN = SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,2,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%ISTRDIR = ival
C               FIXED AND MOVABLE CREST WEIR
            CASE (6,8)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRCD = SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRCD3 = SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRINV    = SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRWID = SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRVAL = SSWR_R2D(r)
C               UPDATE GATE BOTTOM
              REACH(istrrch)%STRUCT(istrnum)%STRBOT0 = 
     2          REACH(istrrch)%STRUCT(istrnum)%STRINV + SSWR_R2D(r)
              REACH(istrrch)%STRUCT(istrnum)%STRBOTINI = 
     2          REACH(istrrch)%STRUCT(istrnum)%STRINV + SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,2,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%ISTRDIR = ival
C               FIXED AND OPERABLE GATED SPILLWAY
            CASE (7,9,10)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRCD = SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRCD2 = SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRCD3 = SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRINV    = SSWR_R2D(r)
              REACH(istrrch)%STRUCT(istrnum)%STRTOP0   = SSWR_R2D(r)
              REACH(istrrch)%STRUCT(istrnum)%STRBOT0   = SSWR_R2D(r)
              REACH(istrrch)%STRUCT(istrnum)%STRTOPINI = SSWR_R2D(r)
              REACH(istrrch)%STRUCT(istrnum)%STRBOTINI = SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRWID = SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRVAL = SSWR_R2D(r)
C               UPDATE GATE TOP
              REACH(istrrch)%STRUCT(istrnum)%STRTOP0 = 
     2          REACH(istrrch)%STRUCT(istrnum)%STRINV + SSWR_R2D(r)
              REACH(istrrch)%STRUCT(istrnum)%STRTOPINI = 
     2          REACH(istrrch)%STRUCT(istrnum)%STRINV + SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,2,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%ISTRDIR = ival
C             INFLOW FROM SFR PACKAGE DIVERSION
            CASE (11)
              IF ( istrconn.EQ.IZERO ) THEN
                istrbc = 5
                REACH(istrrch)%STRUCT(istrnum)%ISTRBC = istrbc
              ELSE
                WRITE (IOUT,4250) istrrch, istrnum
                CALL USTOP('SWR1 ERROR: ISTRCONN MUST BE 0 '//
     2                     'IF ISTRTYPE=11')
              END IF
              IF ( ISFRUNIT.LT.1 ) THEN
                WRITE (IOUT,4255) istrrch, istrnum
                CALL USTOP('SWR1 ERROR: SFR MUST BE ACTIVE '//
     2                     'IF ISTRTYPE=11')
              END IF
C             UNDEFINED STRUCTURE TYPE              
            CASE DEFAULT
          END SELECT
C
C           ADDITIONAL BOUNDARY STRUCTURE DATA FOR COUPLING TO SFR
          IF ( istrconn.EQ.0 .AND. ISFRUNIT.GT.0 ) THEN
            CALL URWORD(line,lloc,istart,istop,2,ival,r,IOUT,iut)
            REACH(istrrch)%STRUCT(istrnum)%ISFRSEG = ival
            IF ( ival.GT.0 ) THEN
              CALL URWORD(line,lloc,istart,istop,2,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%ISFRRCH = ival
              CALL SSWR_SETSFR_STRM(REACH(istrrch)%STRUCT(istrnum))
            END IF
          END IF          
C           ALLOCATE SPACE FOR RECALCULATING STRUCTURE FLOWS
          IF ( ABS(istrtype).EQ.3 .OR. ABS(istrtype).EQ.8 .OR.
     2         ABS(istrtype).EQ.9 .OR. ABS(istrtype).EQ.10 ) THEN
            ALLOCATE(REACH(istrrch)%STRUCT(istrnum)%OPRVAL(NTMAX))
          END IF
C           ALLOCATE SPACE FOR SAVING STRUCTURE DATA IF NECESSARY
          IF ( ISWRPSTR.NE.0 ) THEN
            ALLOCATE(REACH(istrrch)%STRUCT(istrnum)%RSLTS(NPMAX))
          END IF
C           DATASET 13B - STRUCTURE OPERATION DATA
          OPRDATA: IF ( ABS(istrtype).EQ.1  .OR.
     3                  ABS(istrtype).EQ.3  .OR.
     4                  ABS(istrtype).EQ.8  .OR.
     5                  ABS(istrtype).EQ.9  .OR.
     6                  ABS(istrtype).EQ.10 ) THEN
            REACH(istrrch)%NOPR = REACH(istrrch)%NOPR + 1
            IF ( istrtype.LT.0 ) THEN
              CALL USTOP('OPERABLE STRUCTURE TYPE MUST BE GREATER '//
     2                   ' THAN 0')
            END IF
            lloc = 1
            CALL URDCOM(iut,IOUT,LINE)
            CALL URWORD(line,lloc,istart,istop,0,ival,r,IOUT,iut)
            SELECT CASE ( line(istart:istop) )
              CASE ('STAGE')
                REACH(istrrch)%STRUCT(istrnum)%ISTROVAL = 1
              CASE ('FLOW')
                REACH(istrrch)%STRUCT(istrnum)%ISTROVAL = 2
              CASE DEFAULT
                CALL USTOP('ERROR: UNRECOGNIZED STRUCTURE OPERATION '//
     2                     'VARIABLE.')
            END SELECT
            IF ( istrtype.EQ.1 ) THEN
              REACH(istrrch)%STRUCT(istrnum)%ISTRORCH = istrrch
              REACH(istrrch)%STRUCT(istrnum)%ISTRLO   = 2
            ELSE
              CALL URWORD(line,lloc,istart,istop,2,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%ISTRORCH = ival
              IF ( ival.LT.1 .OR. ival.GT.NREACHES ) THEN
                CALL USTOP('ISTRORCH MUST BE GREATER THAN '//
     2                     '0 AND LESS THAN NREACHES + 1')
              END IF
              IF ( REACH(istrrch)%STRUCT(istrnum)%ISTROVAL.EQ.2 ) THEN
                CALL URWORD(line,lloc,istart,istop,2,ival,r,IOUT,iut)
                REACH(istrrch)%STRUCT(istrnum)%ISTRORCHC = ival
              END IF
              CALL URWORD(line,lloc,istart,istop,0,ival,r,IOUT,iut)
              SELECT CASE ( line(istart:istop) )
                CASE ('LT')
                  REACH(istrrch)%STRUCT(istrnum)%ISTRLO = 1
                CASE ('GE')
                  REACH(istrrch)%STRUCT(istrnum)%ISTRLO = 2
                CASE DEFAULT
                  CALL USTOP('ERROR: UNRECOGNIZED STRUCTURE LOGICAL '//
     2                       'OPERAND (LOPR).')
              END SELECT
            END IF
            CALL URWORD(line,lloc,istart,istop,0,ival,r,IOUT,iut)
            ird = 0
            indx = INDEX(line(istart:istop),'TABDATA')
            IF ( indx.GT.0 ) THEN
              ird = 1
              REACH(istrrch)%STRUCT(istrnum)%ISTRTSTYPE = 1
              READ (line(istart+7:istop),*) ival
              REACH(istrrch)%STRUCT(istrnum)%ISTRTAB = ival
              r = DZERO
            END IF
            IF ( ird.EQ.0 ) THEN
              indx = INDEX(line(istart:istop),'REACH')
              IF ( indx.GT.0 ) THEN
                IF ( REACH(istrrch)%STRUCT(istrnum)%ISTROVAL.NE.1 ) THEN
                  CALL USTOP('SIMULATED SWR1 STAGE STRCRIT ONLY FOR '//
     2                       'CSTROTYP=STAGE')
                END IF
                ird = 1
                READ (line(istart+5:istop),*) ival
                REACH(istrrch)%STRUCT(istrnum)%ISTROSTG = ival
                r = DZERO
              END IF
            END IF
            IF ( ird.EQ.0 ) THEN
              READ (line(istart:istop),*) r
            END IF
            REACH(istrrch)%STRUCT(istrnum)%STRCRIT = SSWR_R2D(r)
            IF ( istrtype.GT.1 ) THEN
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRCRITC = SSWR_R2D(r)
              CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
              REACH(istrrch)%STRUCT(istrnum)%STRRT = SSWR_R2D(r)
            END IF
            CALL URWORD(line,lloc,istart,istop,3,ival,r,IOUT,iut)
            REACH(istrrch)%STRUCT(istrnum)%STRMAX = SSWR_R2D(r)
            CALL URWORD(line,lloc,istart,istop,0,ival,r,IOUT,iut)
            IF ( istrtype.GT.1 ) THEN
              indx = INDEX(line(istart:istop),'TABDATA')
              IF ( indx.GT.0 ) THEN
                ival = REACH(istrrch)%STRUCT(istrnum)%ISTRTSTYPE
                IF ( ival.GT.0 ) THEN
                  CALL USTOP('ERROR: TIMESERIES SPECIFIED FOR '//
     2                       'STRCRIT AND STRVAL.')
                END IF
                REACH(istrrch)%STRUCT(istrnum)%ISTRTSTYPE = 2
                READ (line(istart+7:istop),*) ival
                REACH(istrrch)%STRUCT(istrnum)%ISTRTAB     = ival
              END IF
            END IF
            ival = REACH(istrrch)%STRUCT(istrnum)%ISTRTAB
            IF ( ival.GT.0 ) THEN
              IF ( NTABS.LT.1 ) THEN
                CALL USTOP('ERROR: TABULAR DATA MUST BE SPECIFIED '//
     2                     'TO USE EXTERNAL STRUCTURE DATA')
              END IF
              IF ( TABDATA(ival)%ITABTYPE.NE.5 ) THEN
                CALL USTOP('ERROR: TABULAR STRUCTURE DATA MUST BE '//
     2                     'SPECIFIED AS ITABTYPE=5')
              END IF
            END IF
          END IF OPRDATA   
        END DO
C         CHECK IF ANY STRUCURE CONNECTION ERRORS DETECTED
        ierr = SUM( istrerr )
        IF ( ierr.GT.0 ) THEN
          CALL USTOP('ERROR: INCONSISTENT STRUCTURE CONNECTIONS '//
     2               'SEE LISTING FILE')
        END IF
C         RECALCULATE NUMBER OF STRUCTURES IN EACH REACH GROUP
        DO n = 1, NRCHGRP
          RCHGRP(n)%NSTRUCT  = IZERO
          RCHGRP(n)%BCSTRUCT = IZERO
          DO i = 1, RCHGRP(n)%NRGREACH  
            irch = RCHGRP(n)%REACH(i)
            RCHGRP(n)%NSTRUCT = RCHGRP(n)%NSTRUCT + 
     2                           REACH(irch)%NSTRUCT
            IF ( REACH(irch)%BCSTRUCT.GT.0 ) THEN
              RCHGRP(n)%BCSTRUCT = 1
            END IF
          END DO
        END DO
C         WRITE STRUCTURE DATA
        IF (IPTFLG.GT.0) THEN
          WRITE (IOUT,2090)
          WSDATA: DO i = 1, NREACHES
            IF ( inewstr(i).LT.1 ) CYCLE WSDATA
            DO j = 1, REACH(i)%NSTRUCT
              cstruct = '          '
              IF (REACH(i)%STRUCT(j)%STRWID.LT.DZERO) THEN
                WRITE (IOUT,2100) i, 0, 'STRWID', 
     2                            REACH(i)%STRUCT(j)%STRWID
                CALL USTOP(' ')
              END IF
              IF (REACH(i)%STRUCT(j)%STRCD.LT.DZERO) THEN
                WRITE (IOUT,2100) i, j, 'STRCD', 
     2                            REACH(i)%STRUCT(j)%STRCD
                CALL USTOP(' ')
              END IF
              IF (REACH(i)%STRUCT(j)%STRCD2.LT.DZERO) THEN
                WRITE (IOUT,2100) i, j, 'STRCD2', 
     2                            REACH(i)%STRUCT(j)%STRCD2
                CALL USTOP(' ')
              END IF
              IF (REACH(i)%STRUCT(j)%STRCD3.LT.DZERO) THEN
                WRITE (IOUT,2100) i, j, 'STRCD3', 
     2                            REACH(i)%STRUCT(j)%STRCD3
                CALL USTOP(' ')
              END IF
              SELECT CASE (REACH(i)%STRUCT(j)%ISTRTYPE)
C                 NO STRUCTURE
                CASE (0)
C                 EXCESS VOLUME CONTROLLED BY STAGE
                CASE (1)
C                 UNCONTROLLED FLOW
                CASE (2)
                  IF ( REACH(i)%STRUCT(j)%ISTRBC.EQ.2 ) THEN
                    WRITE (cstruct( 5),2110) REACH(i)%STRUCT(j)%STRINV
                  END IF
                  IF (REACH(i)%STRUCT(j)%ISTRDIR.EQ.1) THEN
                    cstruct(11) = ' WEST CON.'
                  ELSE IF (REACH(i)%STRUCT(j)%ISTRDIR.EQ.2) THEN
                    cstruct(11) = ' EAST CON.'
                  ELSE IF (REACH(i)%STRUCT(j)%ISTRDIR.EQ.3) THEN
                    cstruct(11) = 'SOUTH CON.'
                  ELSE IF (REACH(i)%STRUCT(j)%ISTRDIR.EQ.4) THEN
                    cstruct(11) = 'NORTH CON.'
                  ELSE
                    cstruct(11) = 'UNKN. CON.'
                  END IF
C                 SPECIFIED DISCHARGE
                CASE (3)
                  WRITE (cstruct(10),2110) REACH(i)%STRUCT(j)%STRVAL
                  IF (REACH(i)%STRUCT(j)%STRVAL.LT.0) THEN
                    cstruct(11) = 'INFLOW STR'
                  ELSE
                    cstruct(11) = 'DISCH. STR'
                  END IF
C                 SPECIFIED H-Q RELATION
                CASE (4)
C                 CULVERTS              
                CASE (5)
                  WRITE (cstruct( 1),2110) REACH(i)%STRUCT(j)%STRCD
                  WRITE (cstruct( 2),2110) REACH(i)%STRUCT(j)%STRCD2
                  WRITE (cstruct( 4),2110) REACH(i)%STRUCT(j)%STRINV
                  WRITE (cstruct( 5),2110) REACH(i)%STRUCT(j)%STRINV2
                  WRITE (cstruct( 6),2110) REACH(i)%STRUCT(j)%STRWID
                  IF ( REACH(i)%STRUCT(j)%STRWID.GT.DZERO) THEN
                    WRITE (cstruct( 7),2110) REACH(i)%STRUCT(j)%STRWID2
                  END IF
                  WRITE (cstruct( 8),2110) REACH(i)%STRUCT(j)%STRLEN
                  WRITE (cstruct( 9),2110) REACH(i)%STRUCT(j)%STRMAN
                  IF (REACH(i)%STRUCT(j)%ISTRDIR.LT.0) THEN
                    cstruct(11) = '  U/S FLOW'
                  ELSE IF (REACH(i)%STRUCT(j)%ISTRDIR.GT.0) THEN
                    cstruct(11) = '  D/S FLOW'
                  ELSE
                    cstruct(11) = 'BIDIR FLOW'
                  END IF
C                 FIXED AND OPERABLE CREST WEIRS              
                CASE (6,8)
                  WRITE (cstruct( 1),2110) REACH(i)%STRUCT(j)%STRCD
                  WRITE (cstruct( 3),2110) REACH(i)%STRUCT(j)%STRCD3
                  WRITE (cstruct( 4),2110) REACH(i)%STRUCT(j)%STRINV
                  WRITE (cstruct( 6),2110) REACH(i)%STRUCT(j)%STRWID
                  IF (REACH(i)%STRUCT(j)%ISTRDIR.LT.0) THEN
                    cstruct(11) = '  U/S FLOW'
                  ELSE IF (REACH(i)%STRUCT(j)%ISTRDIR.GT.0) THEN
                    cstruct(11) = '  D/S FLOW'
                  ELSE
                    cstruct(11) = 'BIDIR FLOW'
                  END IF
C                 FIXED AND OPERABLE GATED SPILLWAY
                CASE (7,9,10)
                  WRITE (cstruct( 1),2110) REACH(i)%STRUCT(j)%STRCD
                  WRITE (cstruct( 2),2110) REACH(i)%STRUCT(j)%STRCD2
                  WRITE (cstruct( 3),2110) REACH(i)%STRUCT(j)%STRCD3
                  WRITE (cstruct( 4),2110) REACH(i)%STRUCT(j)%STRINV
                  WRITE (cstruct( 6),2110) REACH(i)%STRUCT(j)%STRWID
                  WRITE (cstruct(10),2110) REACH(i)%STRUCT(j)%STRVAL
                  IF (REACH(i)%STRUCT(j)%ISTRDIR.LT.0) THEN
                    cstruct(11) = '  U/S FLOW'
                  ELSE IF (REACH(i)%STRUCT(j)%ISTRDIR.GT.0) THEN
                    cstruct(11) = '  D/S FLOW'
                  ELSE
                    cstruct(11) = 'BIDIR FLOW'
                  END IF
              END SELECT
              WRITE (IOUT,2120) i, j, REACH(i)%STRUCT(j)%ISTRTYPE,
     2                          REACH(i)%STRUCT(j)%ISTRCONN,
     3                          (ADJUSTR(cstruct(k)),k=1,11),
     4                          cstrbc(REACH(i)%STRUCT(j)%ISTRBC)
            END DO
          END DO WSDATA
C
C-----------WRITE SUMMARY OF RATING CURVE STRUCTURES
          indx = 0
          WRCRVDATA: DO i = 1, NREACHES
            IF ( inewstr(i).LT.1 ) CYCLE WRCRVDATA
            DO j = 1, REACH(i)%NSTRUCT
              istrtype =  REACH(i)%STRUCT(j)%ISTRTYPE
              IF ( istrtype.EQ.4 ) THEN
                IF ( indx.EQ.0 ) WRITE (IOUT,4280)
                indx = indx + 1
                DO istrpts = 1, REACH(i)%STRUCT(j)%NSTRPTS
                  WRITE (cstruct( 1),2110) 
     2              REACH(i)%STRUCT(j)%STRELEV(istrpts)
                  WRITE (cstruct( 2),2110) 
     2              REACH(i)%STRUCT(j)%STRQ(istrpts)
                  WRITE (IOUT,2120) i, j, REACH(i)%STRUCT(j)%ISTRTYPE,
     2                              REACH(i)%STRUCT(j)%ISTRCONN,
     3                              (ADJUSTR(cstruct(k)),k=1,2)
                END DO
                WRITE (IOUT,4290)
              END IF
            END DO
          END DO WRCRVDATA

C
C-----------WRITE SUMMARY OF OPERATIONAL DATA FOR STRUCTURES
          indx = 0
          WOPRDATA: DO i = 1, NREACHES
            IF ( inewstr(i).LT.1 ) CYCLE WOPRDATA
            DO j = 1, REACH(i)%NSTRUCT
              istrtype =  REACH(i)%STRUCT(j)%ISTRTYPE
              IF ( istrtype.EQ.1  .OR.
     2             istrtype.EQ.3  .OR.
     3             istrtype.EQ.8  .OR.
     4             istrtype.EQ.9  .OR.
     5             istrtype.EQ.10 ) THEN
                IF ( indx.EQ.0 ) WRITE (IOUT,4200)
                indx = indx + 1
                cstruct = '          '
                SELECT CASE (REACH(i)%STRUCT(j)%ISTROVAL)
                  CASE (1)
                    cstruct( 1) = '     STAGE'
                  CASE (2)
                    cstruct( 1) = '      FLOW'
                END SELECT
                WRITE (cstruct( 2),2115) REACH(i)%STRUCT(j)%ISTRORCH
                IF ( REACH(i)%STRUCT(j)%ISTROVAL.EQ.2 ) THEN
                  WRITE (cstruct( 3),2115) REACH(i)%STRUCT(j)%ISTRORCHC
                END IF
                SELECT CASE (REACH(i)%STRUCT(j)%ISTRLO)
                  CASE (1)
                    cstruct( 4) = '      .LT.'
                  CASE (2)
                    cstruct( 4) = '      .GE.'
                END SELECT
                WRITE (cstruct( 5),2110) REACH(i)%STRUCT(j)%STRCRIT
                IF ( istrtype.NE.1 ) THEN
                  WRITE (cstruct( 6),2110) REACH(i)%STRUCT(j)%STRCRITC
                  WRITE (cstruct( 7),2110) REACH(i)%STRUCT(j)%STRRT
                END IF
                WRITE (cstruct( 8),2110) REACH(i)%STRUCT(j)%STRMAX
                WRITE (IOUT,2120) i, j, REACH(i)%STRUCT(j)%ISTRTYPE,
     2                            REACH(i)%STRUCT(j)%ISTRCONN,
     3                            (ADJUSTR(cstruct(k)),k=1,8)
              END IF
            END DO
          END DO WOPRDATA
C
C-----------WRITE SUMMARY OF SFR DATA FOR STRUCTURES
          indx     = 0
          ierr11   = 0
          ierrstrm = 0
          WSFRDATA: DO i = 1, NREACHES
            IF ( inewstr(i).LT.1 ) CYCLE WSFRDATA
            istr11 = 0
            istrm0 = 0
            DO j = 1, REACH(i)%NSTRUCT
              isfrrch  =  REACH(i)%STRUCT(j)%ISFRRCH
C               ONLY WRITE FOR STRUCTURES CONNECTED TO SFR
              IF ( isfrrch.LT.1 ) CYCLE
              istrtype =  REACH(i)%STRUCT(j)%ISTRTYPE
              isfrseg  =  REACH(i)%STRUCT(j)%ISFRSEG
              isfrstrm =  REACH(i)%STRUCT(j)%ISFRSTRM
C               ERROR CHECKING
C                 EVALUATE IF MORE THAN ONE ISTRTYPE=11 SPECIFIED FOR
C                 A REACH
              IF ( istrtype.EQ.11 ) THEN
                istr11 = istr11 + 1 
                IF ( istr11.GT.1 ) ierr11 = ierr11 + 1
              END IF
C                 EVALUATE IF MORE THAN ONE SFR REACH IS SPECIFIED AS THE
C                 DISCHARGE LOCATION FOR SWR STRUCTURES IN A SINGLE SWR REACH
              IF ( istrm0.EQ.0 ) istrm0 = isfrstrm
              IF ( isfrstrm.NE.istrm0 ) THEN
                ierrstrm = ierrstrm + 1
              END IF
              istrm0 = isfrstrm 
C               WRITE SFR DATA
              IF ( indx.EQ.0 ) WRITE (IOUT,4260)
              indx = indx + 1
              cstruct = '          '
              WRITE (cstruct( 1),2115) isfrseg
              WRITE (cstruct( 2),2115) isfrrch
              WRITE (cstruct( 3),2115) isfrstrm
              WRITE (IOUT,2120) i, j, istrtype,
     2                            REACH(i)%STRUCT(j)%ISTRCONN,
     3                            (ADJUSTR(cstruct(k)),k=1,3)
            END DO
C             IF SFR ERRORS ARE ENCOUNTERED WRITE ERROR MESSAGES AND TERMINATE
            IF ( ierr11.GT.0 ) THEN
              WRITE (IOUT,4265) ierr11
              CALL USTOP('SWR1 ERROR: MORE THAN ONE ISTRTYPE=11 '//
     2                   'SPECIFIED AT LEAST ONE REACH')
            END IF
            IF ( ierrstrm.GT.0 ) THEN
              WRITE (IOUT,4270) ierrstrm
              CALL USTOP('SWR1 ERROR: MORE THAN ONE SFR COUPLING '//
     2                   'LOCATION SPECIFIED AT LEAST ONE REACH')
            END IF
          END DO WSFRDATA
4200  FORMAT(/1X,'SURFACE-WATER ROUTING STRUCTURE OPERATION DATA ',/1X,
     2       '     REACH',1X,' STRUCTURE',1X,'  ISTRTYPE',1X,
     3       '  ISTRCONN',1X,'O VARIABLE',1X,'  ISTRORCH',1X,
     4       ' ISTRORCHC',1X,' OPR LOGIC',1X,'   STRCRIT',1X,
     5       '  STRCRITC',1X,'     STRRT',1X,'    STRMAX',
     6       /1X,132('-'))
4250  FORMAT(//1X,'SWR1 ERROR: ISTRCONN MUST BE 0 IF ISTRTYPE=11',
     2        /1X,'            REACH:',1X,I10,1X,'STRUCTURE:',1X,I10)
4255  FORMAT(//1X,'SWR1 ERROR: THE SFR PACKAGE MUST BE ACTIVE'
     2         1X,'IF ISTRTYPE=11',
     3        /1X,'            REACH:',1X,I10,1X,'STRUCTURE:',1X,I10)
4260  FORMAT(/1X,'SURFACE-WATER SFR STRUCTURE CONNECTION DATA ',/1X,
     2       '     REACH',1X,' STRUCTURE',1X,'  ISTRTYPE',1X,
     3       '  ISTRCONN',1X,'   ISFRSEG',1X,'   ISFRRCH',1X,
     4       '  ISFRSTRM',
     5       /1X,76('-'))
4265  FORMAT(//1X,'SWR1 ERROR: MORE THAN ONE SFR INFLOW STRUCTURE'
     2         1X,'(ISTRTYPE=11) SPECIFIED FOR AT LEAST ONE REACH',
     2        /3X,'NUMBER OF ISTRTYPE=11 ERRORS:',1X,I10)
4270  FORMAT(//1X,'SWR1 ERROR: DIFFERENT SFR COUPLING LOCATIONS'
     2         1X,' SPECIFIED FOR SWR STRUCTURES IN AT LEAST ONE REACH',
     2        /3X,'NUMBER OF SFR COUPLING LOCATION ERRORS:',1X,I10)
4280  FORMAT(/1X,'SURFACE-WATER STAGE-DISCHARGE STRUCTURE DATA ',/1X,
     2       '     REACH',1X,' STRUCTURE',1X,'  ISTRTYPE',1X,
     3       '  ISTRCONN',1X,'   STRELEV',1X,'      STRQ',
     6       /1X,66('-'))
4290  FORMAT(/1X)

C
C-----------WRITE SUMMARY OF STRUCTURES THAT USE TIMESERIES DATA
C           TO SPECIFY STRCRIT OR STRVAL
          indx = 0
          WTSSDATA: DO i = 1, NREACHES
            IF ( inewstr(i).LT.1 ) CYCLE WTSSDATA
            DO j = 1, REACH(i)%NSTRUCT
              IF ( REACH(i)%STRUCT(j)%ISTRTSTYPE.GT.0 ) THEN
                IF ( indx.EQ.0 ) WRITE (IOUT,4100)
                indx = REACH(i)%STRUCT(j)%ISTRTAB
                SELECT CASE (REACH(i)%STRUCT(j)%ISTRTSTYPE)
                  CASE (1)
                    cstruct(1) = 'STRCRIT   '
                  CASE (2)
                    cstruct(1) = 'STRVAL     '
                  CASE DEFAULT
                    CALL USTOP('PROGRAMMING ERROR: UNDEFINED ISTRTYPE')
                END SELECT
                WRITE (IOUT,4110) i, j, cstruct(1), indx
              END IF
            END DO
          END DO WTSSDATA
          IF ( indx.GT.0 ) WRITE (IOUT,4120)
        END IF
      END IF
C       END OF STRUCTURE DATA
C
C-------READ STAGE DATA - APPLIED ON STRESS PERIOD 1 OR FOR CONSTANT STAGE REACHES
      ird2d = 0
      IF ( IRDSTG.LT.IZERO ) THEN
        ird2d = 1
        IRDSTG = ABS( IRDSTG )
      END IF
      IF ( IRDSTG.GT.IZERO ) THEN
        IF ( ird2d.LT.1 ) THEN
          i = IRDSTG
        ELSE
          i = NREACHES
        END IF
        ALLOCATE(istage(i),dstage(i),cstage(i))
        istage = IZERO
        dstage = DZERO
        cstage ='     RAW'
        CALL SSWR_RD_COMM(In)
        IF ( ird2d.LT.1 ) THEN
          CALL SSWRDSOUT('14A')
          ALLOCATE (rlist(2,IRDSTG))
          CALL SSWRLSTRD(In,IOUT,2,IRDSTG,rlist,0)
          DO i = 1, IRDSTG
C             READ REACH NUMBER
            irchstg = INT(rlist(1,i))
            IF ( irchstg.GT.0 .AND. irchstg.LE.NREACHES ) THEN
              istage(i) = irchstg
              dstage(i) = SSWR_R2D(rlist(2,i))
              irg = REACH(irchstg)%IRG
              RCHGRP(irg)%IRGUPDATE = 1
            END IF
          END DO
          DEALLOCATE (rlist)
        ELSE
          CALL SSWRDSOUT('14B')
          ALLOCATE( stage2d(NCOL,NROW) )
          kk = 0
          CALL U2DREL(stage2d(:,:),aname(4),NROW,NCOL,kk,In,IOUT)
C           PROCESS 2D DATA
          DO irch = 1, NREACHES
            i = REACH(irch)%IRCH  !ROW
            j = REACH(irch)%JRCH  !COL
            istage(irch) = irch
            dstage(irch) = SSWR_R2D( stage2d(j,i) )
            irg = REACH(irch)%IRG
            RCHGRP(irg)%IRGUPDATE = 1
          END DO
C           DEALLOCATE TEMPORARY STORAGE FOR 2D STAGE DATA      
          DEALLOCATE( stage2d )
        END IF
C         UPDATE OFFSETS FOR RCHGRP
        CALL SSWR_SET_RCHOFF(istage,dstage)
        IF (IPTFLG.GT.0) WRITE (IOUT,2021) Kkper
C         UPDATE dstage IF LESS THAN REACH BOTTOM
C         AND WRITE RESULTS TO IOUT
        DO i = 1, IRDSTG
          irchstg = istage(i)
          IF ( irchstg.LT.1 ) CYCLE
          IF ( dstage(i).LT.REACH(irchstg)%GBELEV ) THEN
            dstage(i) = REACH(irchstg)%GBELEV + DMINDPTH
            cstage(i) = 'ADJUSTED'
          END IF
          IF (Kkper.EQ.1 .OR. REACH(irchstg)%ISWRBND.LT.IZERO) THEN
            REACH(irchstg)%STAGE = dstage(i)
          END IF
          IF (IPTFLG.GT.0) THEN
            WRITE (IOUT,2022) irchstg, dstage(i), cstage(i)
          END IF
        END DO
        IF (IPTFLG.GT.0) THEN
C           REACH OFFSET
          WRITE (IOUT,2150) (j,j=1,NREACHES)
          WRITE (IOUT,2160) (REACH(j)%OFFSET,j=1,NREACHES)
          WRITE (IOUT,2170) ('RG',j,RCHGRP(j)%ICALCRCH,j=1,NRCHGRP)
        END IF
C         CLEAN UP TEMPORARY STAGE STORAGE
        DEALLOCATE(istage,dstage,cstage)
      END IF
C
C-------SET REACH STAGE TO BOTTOM OF EACH REACH ON THE FIRST STRESS PERIOD IF
C       IRDSTG.EQ.0
      IF ( Kkper.EQ.1 .AND. IRDSTG.EQ.IZERO ) THEN
        DO i = 1, NREACHES
          REACH(i)%STAGE = REACH(i)%GBELEV + DMINDPTH
        END DO
        IRDSTG = NREACHES  
      END IF
C       END OF REACH DATA
C         
C-------SET REACH OFFSET RELATIVE TO GROUP ELEVATION FOR EACH REACH AND 
C         DOWNSTREAM REACH FOR EACH REACH GROUP (RCHGRP)
      IF ( IRDGEO.GT.0 .OR. IRDSTG.GT.0 ) THEN
C
C---------CALCULATE STAGE-VOLUME RELATION FOR EACH REACH GROUP
        CALL SSWR_CALC_RATCURVE()
        IF (IPTFLG.GT.0) THEN
C           RATING CURVE FOR REACH GROUP
          writece = .FALSE.
          DO i = 1, NRCHGRP
            IF (RCHGRP(i)%IRGUPDATE.LT.1) CYCLE
C             WRITE HEADER IF IT HAS NOT BEEN WRITTEN ALREADY
            IF (.NOT.writece) THEN
              WRITE (IOUT,2180)
              writece = .TRUE.
            END IF
C             WRITE DATA            
            k = RCHGRP(i)%IRG
            WRITE (IOUT,2190) i, k
            DO j = 1, SIZE(RCHGRP(k)%RGELEV)
              depth = RCHGRP(k)%RGELEV(j) - RCHGRP(k)%RGELEV(1)
              WRITE (IOUT,2200) j, 
     2            RCHGRP(k)%RGELEV(j), depth, RCHGRP(k)%RGVOL(j)
            END DO
          END DO
        END IF
      END IF
C
C-------READ AUXILIARY VARIABLES DATA - APPLIED ON STRESS PERIOD 1 OR FOR CONSTANT STAGE REACHES
      IF ( IRDAUX.GT.IZERO ) THEN
        CALL SSWR_RD_COMM(In)
        CALL SSWRDSOUT('15')
        ALLOCATE (rlist(NAUX+1,IRDAUX))
        CALL SSWRLSTRD(In,IOUT,NAUX+1,IRDAUX,rlist,0)
        DO i = 1, IRDAUX
C           READ REACH NUMBER
          irchaux = INT(rlist(1,i))
          IF ( irchaux.GT.0 .AND. irchaux.LE.NREACHES ) THEN
            DO j = 1, NAUX
              AUX(j,irchaux) = SSWR_R2D( rlist(j,i) )
            END DO
          END IF
        END DO
        IF ( IPTFLG.GT.0 ) THEN
          WRITE (IOUT,4221) Kkper, ( ADJUSTR(SWRAUX(i)),i=1,NAUX )
          WRITE (IOUT,4222) ('-----------------',i=1,NAUX)
C           WRITE NEW AUX VALUES TO IOUT
          DO i = 1, IRDAUX
            irchaux = INT( rlist(1,i) )
            WRITE (IOUT,4223) irchaux, ( AUX(j,irchaux),j=1,NAUX )
          END DO
        END IF
C         CLEAN UP TEMPORARY STAGE STORAGE
        DEALLOCATE (rlist)
      END IF
C
C-------CALCULATE CONNECTIVITY, ALLOCATE/FILL IA AND JA FOR
C         COMPRESSED STORAGE OF CONNECTIVITITY, AND REACH
C         CONNECTIVITY FOR EACH RCHGRP
      IF (IRDGEO.GT.IZERO .OR. IRDSTR.GT.IZERO .OR.
     2    IRDBND.GT.IZERO) THEN
        CALL SSWR_SET_CONN()
!        CALL SSWR_SET_QCONN()
C
C---------ALLOCATE NECESSARY MEMORY FOR QCONN ON FIRST STRESS PERIOD
        IF ( Kkper.EQ.1 ) THEN
          DO irg = 1, NRCHGRP
            nc = RCHGRP(irg)%NCONN
            IF ( nc.GT.0 ) THEN
              ALLOCATE(RCHGRP(irg)%QCONN(nc))
              IF ( ISWRPQM.NE.IZERO ) THEN
                ALLOCATE(RCHGRP(irg)%QCONNP(nc,NPMAX))
              END IF
            END IF
          END DO
        END IF

C         WRITE CONNECTIVITY DATA TO OUTPUT FILE
        IF (IPTFLG.GT.0) THEN
C           WRITE SUMMARY OF REACH GROUPS
          WRITE (IOUT,2360) NRCHGRP 
C           WRITE PROBLEM DIMENSION DATA FOR NEWTON SOLVER
          IF ( IRDBND.GT.0 ) THEN
            IF ( ISOLVER.LT.2 ) THEN
              WRITE (IOUT,2370) clustor(JAC%IBAND+1)
              IF (JAC%IBAND.GT.IZERO) THEN
                WRITE (IOUT,2380) JAC%NHALFB, JAC%NBW
              END IF
            END IF
          END IF
C           WRITE CONNECTIVITY DATA
          IF ( NSOLRG.GT.0 ) THEN
            ALLOCATE(crg(JAC%NNZ))
            ALLOCATE(ccerch(NREACHES),cce(JAC%NNZ),cia(JAC%NNZ+1))
            ALLOCATE(ciu(JAC%NNZ))
            crg = '     '
            cce = '     '
            cia = '     '
            ciu = '     '
            DO n = 1, NSOLRG
              WRITE (cRG(JAC%IA(n)),'(I5)') JAC%ISMAP(n)
              DO i = JAC%IA(n), JAC%IA(n+1) - 1
                WRITE (cce(i),'(I5)') n
              END DO
              WRITE (cia(JAC%IA(n)),'(I5)') JAC%IA(n)
              WRITE (ciu(JAC%IA(n)),'(I5)') JAC%IU(n)
            END DO
            WRITE (cia(JAC%NNZ+1),'(I5)') JAC%IA(NSOLRG+1)
C           JA, IA, AND IRG
            WRITE (IOUT,2210)
            DO n = 1, JAC%NNZ+1, 9
              k  = MIN(n+8,JAC%NNZ)
              kk = MIN(n+8,JAC%NNZ+1)
              WRITE (IOUT,2220) 'POS.',(j,j=n,kk)
              WRITE (IOUT,2230)
              WRITE (IOUT,2240) 'RG  ',(crg(j),j=n,k)
              WRITE (IOUT,2240) 'SE  ',(cce(j),j=n,k)
              WRITE (IOUT,2220) 'JA  ',(JAC%JA(j),j=n,k)
              WRITE (IOUT,2240) 'IA  ',(cia(j),j=n,kk)
              WRITE (IOUT,2240) 'IU  ',(ciu(j),j=n,k)
              IF (kk.LT.JAC%NNZ+1) WRITE (IOUT,2250)
            END DO
            WRITE (IOUT,2230)
C           IRCHN AND IRCHC FOR RG
            WRITE (IOUT,2320)
            DO n = 1, NRCHGRP
              j = MAX(RCHGRP(n)%NCONN,1)
              WRITE (IOUT,2330) n, RCHGRP(n)%IRGNUM,(i,i=1,j) 
              WRITE (IOUT,2230)     
              ccerch = '     NONE     '
              DO i = 1, RCHGRP(n)%NCONN
                WRITE (ccerch(i),'(I5,1X,A2,1X,I5,1X)') 
     2            RCHGRP(n)%IRCHN(i),'TO',RCHGRP(n)%IRCHC(i)
              END DO
              WRITE (IOUT,2350) 'IRCHC',
     2          (ccerch(i),i=1,MAX(RCHGRP(n)%NCONN,1))
              WRITE (IOUT,2230)
            END DO
            DEALLOCATE(ccerch,crg,cce,cia,ciu)
          END IF
        END IF
      END IF
2210  FORMAT(/1X,59('-'),
     2     /1X,'MODIFIED COMPRESSED ROW STORAGE DATA STRUCTURE',
     3     /3X,'DIAGONAL IS FIRST ENTRY IN ENTRIES FOR INDIVIDUAL',
     4     /3X,'SOLUTION REACH GROUPS')
2220  FORMAT(A4,1X,9(I5,1X))
2230  FORMAT(1X,59('-'))
2240  FORMAT(A4,1X,9(A5,1X))
2250  FORMAT(/,'cont.')
2320  FORMAT(/1X,59('-'),
     2     /1X,'FULL REACH CONNECTIVITY FOR EACH REACH GROUP',
     3     /3X,'A NEGATIVE IRCHC REACH NUMBER INDICATES THE CONNECTION',
     4     /3X,'IS STRUCTURE CONTROLLED')
2330  FORMAT(/1X,59('-'),/1X,'RCHGRP =',1X,I5,/1X,'IRGNUM =',1X,I5,
     2     /1X,14X,17('-'),'CONNECTIONS',17('-'),
     2     /1X,14X,3(I14,1X),:100(/1X,14X,3(I14,1X)))
2350  FORMAT(1X,A5,1X,'REACHES',1X,3(A14,1X),:100(/1X,14X,3(A14,1X)))
2360  FORMAT(/1X,59('-'),
     2       /1X,'NUMBER OF REACH GROUPS                  = ',I5)
2370  FORMAT( 1X,'LU DECOMPOSITION STORAGE APPROACH:',/,3X,A50)
2380  FORMAT( 1X,'OFF DIAGONAL DIMENSION (JAC%NHALFB)     =',I5,
     2       /1X,'MAXIMUM BANDWIDTH (JAC%NBW)             =',I5)
C
C-------SET CONNECTIVITY FOR STRUCTURES USING FLOW CRITERIA
      IF ( IRDSTR.GT.0 ) THEN
        STRFLOWCONN: DO istrrch = 1, NREACHES
          IF ( inewstr(istrrch).LT.1 ) CYCLE STRFLOWCONN
          DO istrnum = 1, REACH(istrrch)%NSTRUCT
            IF ( REACH(istrrch)%STRUCT(istrnum)%ISTROVAL.EQ.2 ) THEN
C               REACH CONNECTION AND REACH GROUP
              ival = REACH(istrrch)%STRUCT(istrnum)%ISTRORCHC
              irg  = REACH(istrrch)%IRG
              EACHCONN: DO nc = 1, RCHGRP(irg)%NCONN
                IF ( ABS( RCHGRP(irg)%IRCHC(nc) ).EQ.ival ) THEN
                  REACH(istrrch)%STRUCT(istrnum)%ISTROQCON = nc
                  EXIT EACHCONN
                END IF
              END DO EACHCONN
            END IF
          END DO
        END DO STRFLOWCONN
      END IF
C
C
C-------INITIALIZE IRG AND REACH VOLUMES ON FIRST STRESS PERIOD 
C       BASED ON UPDATED STAGES AND VOLUMES
      IF ( Kkper.EQ.1 ) THEN
        CALL SSWR_SET_INITVOL()
      END IF
C
C-------UPDATE STAGES USING CURRENT STAGES TO CALCULATE VOLUME AND OFFSETS
      IF ( IRDGEO.NE.IZERO .OR. IRDSTG.NE.IZERO ) THEN
        CALL SSWR_VOL2STG_UPDATE()
      END IF
C
C-------CLEAN UP TEMPORARY STORAGE
      IF ( IRDSTR.GT.0 ) THEN
        DEALLOCATE(inewstr,istrerr)
      END IF
C
C-------FINISH SWR TIMER FOR GWF2SWR7RP
      CALL SSWRTIMER(1,tim1,tim2,SWREXETOT)
C
C-------RETURN
      RETURN
      END SUBROUTINE GWF2SWR7RP

      SUBROUTINE GWF2SWR7AD(Kper,Igrid,Iunitsub)
C     ******************************************************************
C     UPDATE PREVIOUS GROUP CONDITIONS WITH RESULTS FROM 
C     THE LAST TIMESTEP
C
C     COMPUTE SURFACE-WATER ROUTING STRUCTURE FLOWS FOR TIME STEP AT 
C     SURFACE-WATER REACHES WITH TIME-VARYING DATA.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,HNEW,HOLD,PERLEN
      USE GWFBASMODULE,ONLY:PERTIM
!      USE GWFSUBMODULE, ONLY: DVZ,SUBLNK,LPFLNK  !SUB-Linkage rth
      USE GWFSWRMODULE
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
      INTEGER, INTENT(IN) :: Kper
      INTEGER, INTENT(IN) :: Igrid
      INTEGER, INTENT(IN) :: Iunitsub
C
C     + + + LOCAL ARGUMENTS + + +
      INTEGER :: n
      INTEGER :: irch
      INTEGER :: irg
      DOUBLE PRECISION FRAC
      DOUBLEPRECISION :: tim1, tim2
C     ------------------------------------------------------------------
      CALL SGWF2SWR7PNT(Igrid)
C
C-------START SWR TIMER FOR GWF2SWR7AD
      CALL SSWRTIMER(0,tim1,tim2,SWREXETOT)
C
C-------IF NSWRS<=0 THEN THERE ARE NO SURFACE-WATER ROUTING REACHES.
C-------RETURN.
      IF(NREACHES.LE.0) RETURN
C
C-------VERTICALY ADJUST DATA USING SUBSIDENCE RESULTS IF NECESSARY.
C       DONE PRIOR TO SETTING PREVIOUS DATA TO CURRENT DATA.
      IF( Iunitsub.GT.0 )THEN
!        IF ( SUBLNK .OR. LPFLNK ) THEN
!          CALL SSWRVADJ(DVZ)
!        END IF
      END IF
C
C-------SET PREVIOUS REACH CONDITIONS (REACH(:)%PREVIOUS TO CONDITIONS FROM
C         END OF LAST SWR TIMESTEP (REACH(:)%CURRENT)
      DO irch = 1, NREACHES
        RSTAGE(irch,0) = REACH(irch)%STAGE
      END DO
C
C-------SET PREVIOUS RCHGRP CONDITIONS (RCHGRP(:)%PREVIOUS) TO CONDITIONS 
C         FROM LAST TIME STEP (RCHGRP(:)%CURRENT)
      DO irg = 1, NRCHGRP
        GSTAGE(irg,0) = RCHGRP(irg)%STAGE
      END DO
C
C-------UPDATE INITIAL GATE OPENING DATA FOR CURRENT MODFLOW TIMESTEP TO
C       GATE OPENING AT THE END OF THE LAST SWR1 TIMESTEP
      IF ( Kper.GT.1 ) THEN
        DO irch = 1, NREACHES
          IF ( REACH(irch)%NSTRUCT.LT.1 ) CYCLE
          DO n = 1, REACH(irch)%NSTRUCT
            REACH(irch)%STRUCT(n)%STRTOPINI = 
     2        REACH(irch)%STRUCT(n)%STRTOP
            REACH(irch)%STRUCT(n)%STRBOTINI = 
     2        REACH(irch)%STRUCT(n)%STRBOT
          END DO
        END DO
      END IF
C
C-------SET NUMTIME0 = NUMTIME TO PRESERVE TIMESTEP FROM PREVIOUS
C       TIMESTEP
      NUMTIME0 = NUMTIME
      RTIME0   = RTIME
      RSWRPRN0 = RSWRPRN
      NADPCNT0 = NADPCNT
C
C-------COMPUTE PROPORTION OF STRESS PERIOD TO CENTER OF THIS TIME STEP
      IF (PERLEN(Kper).EQ.0.0) THEN
        FRAC=1.0
      ELSE
        FRAC=REAL(PERTIM/PERLEN(Kper),8)
      ENDIF
C
C-------FINISH SWR TIMER FOR GWF2SWR7AD
      CALL SSWRTIMER(1,tim1,tim2,SWREXETOT)
C
C-------RETURN
      RETURN
      END SUBROUTINE GWF2SWR7AD

      SUBROUTINE GWF2SWR7EX(Igrid,Iflag,Nlen,Imap,Rval)
C     *****************************************************************
C     FILL THE EXTERNAL FLOW ACCUMULATORS QUZFLOW AND QEXTFLOW FOR 
C       THE SWR PROCESS
C
C     THIS SUBROUTINE IS CALLED EXTERNAL TO THE SWR PROCESS.
C
C       Nlen, Imap, AND Rval MUST BE ALLOCATED AND SPECIFIED EXTERNALLY
C         TO THE SWR PROCESS.
C
C       Iflag IS AN INTEGER FLAG THAT IDENTIFIES THE APPROPRIATE EXTERNAL
C         REACH FLOW TERM TO ADD THE VALUE (Rval) TO.  CURRENTLY Iflag=1
C         IDENTIFIES Rval IS ADDED TO QUZFLOW AND ANY OTHER VALUE RESULTS 
C         IN Rval BEING ADDED TO QEXTFLOW.
C
C       Nlen IS THE LENGTH OF THE MAPPING (Imap) AND VALUE (Rval) 
C         VECTORS.
C
C       Imap IS A MAPPING VECTOR TO IDENTIFIES THE REACH NUMBER 
C         ASSOCIATED WITH THE DATA IN Rval.  IF Iflag=1, IUZFOFFS
C         IS SUBTRACTED FROM Imap TO DETERMINE THE SWR REACH NUMBER.
C
C       Rval IS A SINGLE PRECISION VECTOR THAT CONTAINS THE EXTERNALLY
C         CALCULATED (EXTERNAL TO THE SWR PROCESS) FLOW DATA.  Rval IS
C         ADDED TO QUZFLOW OR QEXTFLOW WHICH IS ADDED TO THE CONTINUITY
C         EQUATION SOLVED FOR EACH REACH GROUP.
C
C       IEXTFLOW IS SET TO ZERO AT THE END OF THE GWF2SWR7FM SUBROUTINE
C         WHICH INDICATES THE QUZFLOW AND QEXTFLOW ACCUMULATORS ARE TO 
C         BE RESET TO ZERO AT THE BEGINNING OF THE GWF2SWR7EX SUBROUTINE.
C         AFTER SETTING QUZFLOW AND QEXTFLOW TO ZERO IEXTFLOW IS SET 
C         TO ONE.
C
C       POSITIVE Rval VALUES INDICATE A SOURCE OF WATER TO A REACH.
C       NEGATIVE Rval VALUES INDICATE A SINK   OF WATER TO A REACH.
C
C     VERSION  1.0: JUNE 28, 2011
C     *****************************************************************
        USE GWFSWRMODULE, ONLY: IZERO, DZERO, IUZFOFFS, SWREXETOT,
     2                          NREACHES, 
     3                          IEXTFLOW, REACH  !QUZFLOW, QEXTFLOW
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Igrid
        INTEGER, INTENT(IN) :: Iflag
        INTEGER, INTENT(IN) :: Nlen
        INTEGER, DIMENSION(Nlen), INTENT(IN) :: Imap
        REAL, DIMENSION(Nlen), INTENT(IN) :: Rval
C       + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        INTEGER :: irch
        INTEGER :: ioffset
        DOUBLEPRECISION :: d
        DOUBLEPRECISION :: tim1, tim2
C       + + + FUNCTIONS + + +
C       + + + CODE + + +
C
C---------SET POINTERS FOR GRID
        CALL SGWF2SWR7PNT(Igrid)
C
C---------START SWR TIMER FOR GWF2SWR7EX
        CALL SSWRTIMER(0,tim1,tim2,SWREXETOT)
C
C-------REINITIALIZE QUZFLOW AND QEXTFLOW IF IEXTFLOW = 0
        IF ( IEXTFLOW.LT.1 ) THEN
          DO n = 1, NREACHES
            REACH(n)%QUZFLOW  = DZERO
            REACH(n)%QEXTFLOW = DZERO
          END DO
          IEXTFLOW = 1
        END IF
C
C---------DETERMINE OFFSET FOR THE Imap VECTOR TO ASSIGN
C         Rval TO THE APPROPRIATE SWR REACH
        SELECT CASE (Iflag)
          CASE (1)
            ioffset = IUZFOFFS
          CASE DEFAULT
            ioffset = IZERO
        END SELECT
C
C---------ADD Rval TO APPROPRIATE EXTERNAL REACH FLOW TERM (QUZFLOW OR QEXTFLOW)
        DO n = 1, Nlen
          irch = Imap(n) - ioffset
          IF ( irch.LT.1 .OR. irch.GT.NREACHES ) CYCLE
C           CONVERT SINGLE PRECISION VALUE TO DOUBLE PRECISION VALUE
          d    = DBLE( Rval(n) )       !SIMPLE TYPE CAST
          SELECT CASE (Iflag)
            CASE (1)
              REACH(irch)%QUZFLOW  = REACH(irch)%QUZFLOW  + d
            CASE DEFAULT
              REACH(irch)%QEXTFLOW = REACH(irch)%QEXTFLOW + d
          END SELECT
        END DO
C
C---------START SWR TIMER FOR GWF2SWR7EX
        CALL SSWRTIMER(1,tim1,tim2,SWREXETOT)
C
C---------RETURN
        RETURN      
      END SUBROUTINE GWF2SWR7EX

      SUBROUTINE GWF2SWR7FM(Kkiter, Kkper, Kkstp, Igrid)
C     *****************************************************************
C     ADD SURFACE-WATER TERMS TO RHS AND HCOF IF FLOW OCCURS IN MODEL CELL
C     VERSION  7.1: JUNE 29, 2006
C     *****************************************************************
      USE GWFSWRMODULE
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,
     +                       ISSFLG, IBOUND, HNEW, HOLD, HCOF, RHS, 
     +                       DELR, DELC,
     +                       PERLEN, NSTP, TSMULT
      USE GWFBASMODULE, ONLY:DELT, TOTIM, HDRY
      USE GWFNWTMODULE, ONLY: A, IA, ICELL  !NEWTON
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
      INTEGER, INTENT(IN) :: Kkiter
      INTEGER, INTENT(IN) :: Kkper
      INTEGER, INTENT(IN) :: Kkstp
      INTEGER, INTENT(IN) :: Igrid
C     + + + LOCAL DEFINITIONS + + +
      LOGICAL :: checkresult
      LOGICAL :: rhsonly
      INTEGER :: i, k, n
      INTEGER :: kact
      INTEGER :: ir, jc, kl
      INTEGER :: irch
      INTEGER :: irg
      INTEGER :: ipos
      INTEGER :: iouter
      INTEGER :: isoln
      INTEGER :: nfail
      INTEGER :: itss
      INTEGER :: iprn
      INTEGER :: irstt
      INTEGER :: ilast
      INTEGER :: ird
      INTEGER :: iinit
      INTEGER :: ij
      DOUBLEPRECISION :: t
      DOUBLEPRECISION :: rmax
      DOUBLEPRECISION :: smax, pinfmax, amax
      DOUBLEPRECISION :: fmax
      DOUBLEPRECISION :: in, r, e, b, c, v
      DOUBLEPRECISION :: qm, bc
      DOUBLEPRECISION :: rbot
      DOUBLEPRECISION :: h0, h1, h
      DOUBLEPRECISION :: fmax0
      DOUBLEPRECISION :: dtscale
      DOUBLEPRECISION :: ge, gwet, getextd
      REAL            :: stpend, swrtot, ttot, dt
      REAL            :: dtlast
      DOUBLEPRECISION :: tim1, tim2
      DOUBLEPRECISION :: rc, hc
      DOUBLEPRECISION :: rs
      DOUBLEPRECISION :: q
      DOUBLEPRECISION :: fdelt
C     + + + FUNCTIONS + + +
      CHARACTER (LEN=10) :: SSWR_SCCHAR
      DOUBLEPRECISION :: SSWRMXRN
      DOUBLEPRECISION :: SSWR_RG_QLI
      DOUBLEPRECISION :: SSWR_RG_QPR
      DOUBLEPRECISION :: SSWR_RG_QEV
      DOUBLEPRECISION :: SSWR_RG_QAQ
      DOUBLEPRECISION :: SSWR_RG_VOL
      DOUBLEPRECISION :: SSWR_RG_QM
      DOUBLEPRECISION :: SSWR_RG_QBC
      DOUBLEPRECISION :: SSWR_RG_QCS
      DOUBLEPRECISION :: SSWR_RG_QINF
      DOUBLEPRECISION :: SSWR_CALC_QAQ
      DOUBLEPRECISION :: SSWR_CALC_QEV

C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(/1X,'WARNING:',1X,A,1X,'SOLVER',/,10X,
     2      'DID NOT CONVERGE FOR EACH',1X,
     3      'SURFACE WATER ROUTING TIMESTEP',/,10X,
     4      'TOLR EXCEEDED',1X,I5,1X,
     5      'TIMES IN STRESS PERIOD',1X,I5,1X,
     6      'AND TIME STEP',1X,I5,/,10X,
     7      'MAXIMUM ABSOLUTE ERROR (',G15.7,') IN RCHGRP',1X,I5,/,10X,
     8      'DURING SURFACE WATER ROUTING TIMESTEP',1X,I5,/)
2010  FORMAT(1X,/,1X,'MAXIMUM RESIDUAL FOR EACH SWR TIMESTEP',
     2       1X,'(NEG. ISWRDT VALUES INDICATE REL. TOLR EXCEEDED)',/,
     3       1X,3('ISWRDT,ITER.,MAX.RESID., LOC.',1X),/,
     4       1X,89('-'))
2020  FORMAT(1X,3(:'(',I5,',',I5,',',G10.3,',',I5,')'))
2030  FORMAT(1X,/,1X,'SWR TIME STEP SUMMARY',/,
     2       1X,5('ISWRDT,     SWRDT '),/,
     3       1X,89('-'))
2040  FORMAT(1X,5(:'(',I5,',',G10.3,')'))
2050  FORMAT(1X,89('-'))
2060  FORMAT(/1X,
     2       /1X,27X,'ITERATIONS FOR EACH SWR TIMESTEP',
     3       /1X,14(I5,1X),100000(:/1X,14(I5,1X)))
2070  FORMAT(1X,84('-'))
2080  FORMAT(1X,14(I5,1X))
C     + + + CODE + + +
C-------SET POINTERS FOR GRID
      CALL SGWF2SWR7PNT(Igrid)
C
C-------START SWR TIMER FOR GWF2SWR7FM
      CALL SSWRTIMER(0,tim1,tim2,SWREXETOT)
C
      KMFITER = Kkiter
C
C-------SCALE TIME BASED ON RAINFALL
C       THIS IS A PREEMPTIVE STRIKE TO ADJUST THE TIMESTEP BASED ON RAINFALL INTENSITY 
      RTSTMAX = MIN( RTMAX, DELT )
      IF ( IADTIME.GT.IZERO .AND. Kkiter.EQ.1 ) THEN
        IF ( DMAXRAI.GT.DZERO ) THEN 
          rmax = SSWRMXRN()
          IF ( rmax.GT.DONE ) THEN
            RTSTMAX = MAX( RTMIN, RTMAX / REAL( rmax, 4 ) )
          END IF
        END IF
      END IF
C
C-------SET FAILTIME
      IF ( Kkiter.EQ.1 ) FAILTIME = .FALSE.
C
C-------SET INITIAL AND RESTART POSITION
      DO i = 1, NREACHES
        IF ( Kkiter.NE.1 ) THEN
          rs = RSTAGE(i,1)
        ELSE
          rs = RSTAGE(i,0)
        END IF
        REACH(i)%STAGE = rs
        JAC%S0(i)      = rs
      END DO
C
C-------INITIALIZE PRINT TIMES
      SWRTIME(:)%ITPRN = 0
C
C-------SET END OF TIMESTEP
      swrtot = RZERO
      stpend = RZERO
!C
C-------TIME AT END OF LAST STRESS PERIOD
      DO n = 1, Kkper-1
        stpend = stpend + PERLEN(n)
      END DO
      swrtot = stpend
C
C-------TIME AT THE END OF THE CURRENT TIME STEP 
C       IN THE CURRENT STRESS PERIOD
      DO n = 1, Kkstp
        IF ( n.EQ.1 ) THEN
          dt = PERLEN(Kkper) / REAL( NSTP(Kkper), 4 )
          IF ( TSMULT(Kkper).NE.RONE ) THEN
            dt = PERLEN(Kkper) * (RONE - TSMULT(Kkper))/
     2           (RONE - TSMULT(Kkper)**NSTP(Kkper))
          END IF
        ELSE
          dt = dt * TSMULT(Kkper)
        END IF
        stpend = stpend + dt
        IF ( n.LT.Kkstp ) swrtot = stpend
      END DO
C
      IF ( RTPRN.EQ.RZERO ) THEN
        RSWRPRN = stpend
      ELSE
        IF ( RSWRPRN0.LE.swrtot ) THEN    
          RSWRPRN = RSWRPRN0 + RTPRN
        ELSE
          RSWRPRN = RSWRPRN0
        END IF
      END IF
C
C-------SET ISWRSS
      itss   = IZERO
C
C-------INITIALIZE FAILTIME ON FIRST ITERATION OF THE FIRST TIME STEP IN 
C       THE FIRST STRESS PERIOD
      IF ( Kkper.EQ.1 .AND. Kkstp.EQ.1 .AND. Kkiter.EQ.1 ) THEN
        FAILTIME = .TRUE.
      END IF
C
C-------SET RTIME TO RTIME0 ON THE FIRST SWR TIME STEP OF THE CURRENT
C       MODFLOW TIME STEP
      RTIME   = RTIME0
      NADPCNT = NADPCNT0
C       RESET RTIME TO RTMIN IF LAST STRESS PERIOD WAS STEADY-STATE
      IF ( Kkper.GT.1 ) THEN
        IF ( ISSFLG(Kkper-1).EQ.1 ) THEN
          RTIME   = RTMIN
          NADPCNT = 1
        END IF
      END IF
C
C-------UPDATE GROUNDWATER HEAD (HGW) USED IN QAQ CALCULATIONS
      CALL SSWR_UPDATE_QAQHGW()
C
C-------INITIALIZE VARIABLES
      nfail    =  IZERO
      ipos     =  1
C
C---------SOLVE FOR EACH SWR TIMESTEP
      iinit   = 1
      NUMTIME = 1
      fmax    = DZERO
      fdelt   = DONE
      SWRSUBTIME: DO
        n      = NUMTIME
        ISWRDT = NUMTIME
        iprn   = IZERO
        ilast  = IZERO
        irstt  = IZERO
        dtlast = RTIME
        ird    = 0
C
C---------SET LOWER BOUND FOR NEWTON ITERATIONS
        INWTCNT = 2
        IF ( INWTCORR.GT.0 ) INWTCNT = 1
C
C-------SET INITIAL SURFACE WATER ROUTING TIMESTEP BASED ON MODFLOW TIMESTEP LENGTH
        IF ( IADTIME.NE.IZERO ) THEN
          IF ( .NOT.FAILTIME ) THEN
            IF ( NADPCNT.GT.NTMULT ) THEN
              RTIME   = MIN( RTIME * RTMULT, RTSTMAX )
              dtlast  = RTIME
              NADPCNT = 1
            END IF
          END IF
        END IF
C
C---------DETERMINE RTIME USING TABULAR DATA
        IF( IADTIME.LT.IZERO ) THEN
          CALL SSWR_GETDT(swrtot,RTIME)
        END IF
C
C---------SET RTIME TO DELT IF STEADY-STATE STRESS PERIOD
        IF ( ISWRSS.GT.0 ) THEN
          NUMTIME = 1
          RTIME   = DELT
        END IF
C
C---------MAKE SURE RTIME NO SMALLER THAN RTMIN
        IF ( RTIME.LT.RTMIN ) THEN
          RTIME = RTMIN
        END IF
C
C---------MAKE SURE THE END OF THE CURRENT MODFLOW TIME STEP 
C         IS NOT EXCEEDED
        ttot = swrtot + RTIME
        IF ( (stpend-ttot).LT.RTMIN .OR.
     2        ttot.GT.stpend ) THEN
          RTIME = stpend - swrtot
          irstt = 1
          ilast = 1
          IF ( RTPRN.EQ.RZERO ) iprn = 1
        END IF
C
C---------MAKE SURE THE CURRENT PRINT TIME IS NOT EXCEEDED
        ttot = swrtot + RTIME
        IF ( (RSWRPRN-ttot).LT.RTMIN .OR.
     2        ttot.GT.RSWRPRN ) THEN
          RTIME = RSWRPRN - swrtot
          irstt = 1
        END IF
        IF ( RTPRN.GT.RZERO ) THEN      
          ttot = swrtot + RTIME
          IF ( ABS(ttot-RSWRPRN).LT.RTMIN ) iprn = 1
        END IF
C
C---------SET FLAG INDICATING LAST GOOD SWRDT SHOULD BE USED
C         ON NEXT TIME STEP
        IF ( iprn.NE.0 ) irstt  = 1
C
C---------SET FLAG FOR LAST SWR1 TIME STEP OF CURRENT 
C         MODFLOW TIME STEP
        ttot = swrtot + RTIME
        IF ( ABS(ttot-stpend).LT.RTMIN ) ilast = 1
C
C---------UPDATE STRCRIT AND STRVAL FOR STRUCTURES
C         OPERATED USING TIMESERIES DATA
        ttot = swrtot + RTIME
        CALL SSWR_GET_STRGATE(swrtot,ttot)
C
C---------UPDATE REACH STRESS DATA
        IF ( NTABS.GT.0 ) THEN
          ttot = swrtot + RTIME
          CALL SSWR_GET_TABS(swrtot,ttot)
        END IF
C
C---------SET RESTART POSITION FOR EACH SWR TIME STEP
        DO i = 1, NREACHES
          JAC%S0(i) = REACH(i)%STAGE
        END DO
C
C---------GROUNDWATER FLOW NWT OUTER LOOP
        GWFNWT: DO
C
C---------ADAPTIVE TIME STEP ALGORITHIM
        ADAPTIVE: DO
C
C-----------SET TIMESTEP FOR SURFACE-WATER ROUTING PACKAGE        
          SWRDT = REAL( RTIME, 8 )
C
C-----------SET HEAD WEIGHTING FACTOR
          IF ( IWGTHDS.NE.0 ) THEN
            IF ( ISWRDT.GT.1 ) THEN
              SWRTIME(ISWRDT)%FDELT = SWRTIME(ISWRDT-1)%FDELT +
     2                                SWRDT / REAL( DELT, 8 )
            ELSE
              SWRTIME(ISWRDT)%FDELT = SWRDT / REAL( DELT, 8 )
            END IF 
          END IF
C
C-----------INITIALIZATION OF GROUP STAGE (ps) AND TEMPORARY REACH STAGES (s)
          DO i = 1, NREACHES
            JAC%S(i) = REACH(i)%STAGE 
          END DO
          DO i = 1, NRCHGRP
            irch   = RCHGRP(i)%ICALCRCH
            JAC%PS(i)  = REACH(irch)%STAGE
C             STEADY STATE
            IF ( ISWRSS.GT.0 ) JAC%PS0(i) = JAC%PS(i)
C             STEADY STATE
            IF ( IADTIME.GT.IZERO ) THEN
              IF ( DMAXSTG.GT.DZERO .AND. ISWRSS.LT.1 ) THEN
                JAC%PS0(i) = JAC%PS(i)
              END IF
              IF ( DMAXINF.GT.DZERO ) THEN
                JAC%PINF(i)  = SSWR_RG_QINF(RCHGRP(i))
                JAC%PINF0(i) = JAC%PINF(i)
              END IF
            END IF
          END DO
C
C-----------UPDATE STRUCTURE DISCHARGE (PUMP) RATES AND GATE LEVELS
          CALL SWR_CALC_STRVALS(JAC%PS)
C
C-----------SOLUTION USING APPROXIMATE NEWTON METHOD AND SPECIFIED SOLVER
          isoln = 0
          fmax0 = DZERO
          CALL SSWR_GSOLWRP(iouter,isoln,JAC%PS,checkresult)
          
          CALL SSWR_CALC_RGRESI(JAC%PS,JAC%R)
          SWRTIME(n)%ICNVG    = n
          SWRTIME(n)%ITER     = isoln
          SWRTIME(n)%ILOCFMAX = IZERO
          SWRTIME(n)%FMAX     = DZERO
          t = DZERO
          DO i = 1, NRCHGRP
            IF ( ABS(JAC%R(i))/QSCALE.GT.fmax0 ) THEN
              t = ABS(JAC%R(i)) / QSCALE
              fmax0 = t
              SWRTIME(n)%ILOCFMAX = RCHGRP(i)%IRGNUM
              SWRTIME(n)%FMAX     = t
            END IF
          END DO
          IF ( n.EQ.1 ) THEN
            fmax = SWRTIME(n)%FMAX
          END IF
          IF ( SWRTIME(n)%FMAX.GT.fmax ) THEN
            fmax = SWRTIME(n)%FMAX
            ipos = n
          END IF
C           RESET checkresult IF TOLR EXCEEDED AFTER RETURN FROM
C           LINEAR SOLVER FOR TRANSIENT CASES USING ADAPTIVE TIME-STEPPING
          IF ( IADTIME.GT.IZERO .AND. ISWRSS.LT.1 ) THEN
            IF ( (fmax0 * QSCALE).GT.TOLR ) checkresult=.TRUE.
          END IF
          IF ( checkresult ) THEN
            SWRTIME(n)%ICNVG = -SWRTIME(n)%ICNVG
            nfail = nfail + 1
          END IF
C
C-----------TEMPORARY OUTPUT OF RESULTS
          IF ( ISWRSCRN.GT.0 .AND. INWTCNT.EQ.2 ) THEN
            IF ( Kkiter.EQ.1 .AND. NUMTIME.EQ.1 .AND. iinit.EQ.1 ) THEN
              WRITE (*,5)
              iinit = 0
            END IF
            WRITE (*,10) 
     2        Kkper, Kkstp, NUMTIME, 
     3        SSWR_SCCHAR(swrtot), SSWR_SCCHAR(swrtot+REAL(SWRDT,4)), 
     4        SSWR_SCCHAR(REAL(SWRDT,4)), 
     5        SWRTIME(n)%ITER, SWRTIME(n)%ILOCFMAX, 
     4        SSWR_SCCHAR(REAL(SWRTIME(n)%FMAX,4)), checkresult
          END IF
0005  FORMAT(1X,' ',' Per: Stp: SWRS:     Time0:     Time1:     SWRDT:',
     2          ' Iters:  MaxL:  Max Resid: C')
0010  FORMAT(1X,'+',2(I4,':'),I5,':',A10,':',A10,
     2       ':',A10,':',
     3       1X,I5,':',1X,I5,
     4       ':',1X,A10,':',1X,L1)

C
C-----------SOLVER FAILURE - RESTART ADAPTIVE AT BEGINNING OF SWR TIMESTEP
          FAILTIME = .FALSE.
          IF ( checkresult ) THEN
            FAILTIME = .TRUE.
            NADPCNT = 1
            IF ( IADTIME.GT.IZERO .AND. ISWRSS.LT.1 ) THEN
              IF ( RTIME.GT.RTMIN ) THEN
                iprn  = 0
                ilast = 0
                ird = ird + 1
                DO i = 1, ird
                  RTIME = RTIME / RTMULT
                  IF ( RTIME.LE.RTMIN ) THEN
                    RTIME = RTMIN
                    EXIT
                  END IF
                END DO
                dtlast = RTIME
C                 RESET REACH STAGES TO STAGES FROM THE END OF PREVIOUS TIMESTEP
                DO i = 1, NREACHES
                  REACH(i)%STAGE = JAC%S0(i)
                END DO
                CYCLE GWFNWT
              ELSE
                IF ( ISWRCONT.LT.1 ) THEN
                  CALL USTOP('SWR SOLUTION DID NOT CONVERGE')
                END IF
                EXIT ADAPTIVE
              END IF
C-------------NOT USING ADAPTIVE TIMESTEPPING
            ELSE
              IF ( ISWRCONT.LT.1 ) THEN
                CALL USTOP('SWR SOLUTION DID NOT CONVERGE')
              END IF
            END IF
          END IF
          ird = 0
C
C-----------RESTART SWRSUBTIME AT BEGINNING OF SWR TIMESTEP IF
C           STAGE CHANGES EXCEED DMAXSTG OR INFLOW EXCEEDS DMAXINF
          IF ( IADTIME.GT.IZERO .AND. ISWRSS.LT.1 ) THEN
            smax    = DZERO
            pinfmax = DZERO
            amax    = DZERO
            IF ( DMAXSTG.GT.DZERO ) THEN
              smax    = ABS ( MAXVAL( JAC%PS - JAC%PS0 ) )
              smax    = smax / DMAXSTG
            END IF
            IF ( DMAXINF.GT.DZERO ) THEN
              DO i = 1, NRCHGRP
                JAC%PINF(i)  = SSWR_RG_QINF(RCHGRP(i))
              END DO
              pinfmax = ABS( MAXVAL( JAC%PINF - JAC%PINF0 ) )
              pinfmax = pinfmax / DMAXINF
            END IF
            IF ( smax.GT.DONE .OR. pinfmax.GT.DONE ) THEN
              FAILTIME = .TRUE.
              NADPCNT  = 1
              IF ( RTIME.GT.RTMIN ) THEN
                amax    = MAX( smax, pinfmax, RTMULT )
                RTIME   = MAX( RTIME / REAL( amax, 4 ), RTMIN )
C                 CHECK IF NEW RTIME IS ESSENTIAL EQUAL TO PREVIOUS RTIME
                IF ( ABS( dtlast - RTIME ).LT.EPSILON( RTMIN ) ) THEN
                  EXIT ADAPTIVE
                END IF
C                 RESET FLAGS
                iprn  = 0
                ilast = 0
                dtlast  = RTIME
C               RESET REACH STAGES TO STAGES FROM THE END OF PREVIOUS TIMESTEP
                DO i = 1, NREACHES
                  REACH(i)%STAGE = JAC%S0(i)
                END DO
                CYCLE GWFNWT
              END IF
            END IF
          END IF
C
C-----------EXIT ADAPTIVE TIMESTEP
          EXIT ADAPTIVE
C---------ADAPTIVE END
        END DO ADAPTIVE
C---------INCREMENT GROUNDWATER FLOW NEWTON COUNTER
          IF ( INWTCNT.EQ.2 ) EXIT GWFNWT         
          INWTCNT = INWTCNT + 1
C---------GROUNDWATER FLOW NEWTON STEP END
        END DO GWFNWT
C
C---------SAVE RESULTS AT THE END OF THE ADAPTIVE SWR TIME STEP
C
C---------UPDATE REACH STAGE AND CURRENT GROUP USING ESTIMATED STAGE
        CALL SSWR_SET_TEMPRCHSTG(JAC%PS,JAC%S)
        DO i = 1, NRCHGRP
          t   = JAC%PS(i)
          r   = SSWR_RG_QPR(RCHGRP(i))
          e   = SSWR_RG_QEV(RCHGRP(i),JAC%PS(i))
          b   = SSWR_RG_QAQ(RCHGRP(i),JAC%PS(i))
          qm  = SSWR_RG_QM(RCHGRP(i),JAC%PS)
          in  = SSWR_RG_QLI(RCHGRP(i))
          bc  = SSWR_RG_QBC(RCHGRP(i),JAC%PS)
          c   = SSWR_RG_QCS(RCHGRP(i),in,r,e,b,qm,bc)
          v   = SSWR_RG_VOL(RCHGRP(i),JAC%PS(i))
          CALL SSWR_RG_UPDATE(RCHGRP(i),v,r,e,b,bc,c)
        END DO
C
C---------ADD RCHGRP TERMS FOR TIMESTEP TO INCREMENTAL TERMS
        DO i = 1, NRCHGRP
          GSTAGE(i,n) = RCHGRP(i)%STAGE
        END DO
C
C---------SAVE CURRENT STAGE
        DO i = 1, NREACHES
          RSTAGE(i,n) = REACH(i)%STAGE
        END DO
C
C---------SAVE CURRENT TIME STEP LENGTH
        swrtot = swrtot + RTIME
        SWRTIME(n)%SWRDT = SWRDT
        IF ( iprn.EQ.1 ) THEN
          SWRTIME(n)%ITPRN = 1
          RSWRPRN = RSWRPRN + RTPRN
        END IF
C
C---------SET RTIME TO PREVIOUS RTIME IF IT WAS 
C         REDUCED THIS SWR1 TIME STEP TO HIT END OF MODFLOW
C         TIME STEP OR PRINT TIME
        IF ( irstt.GT.0 ) THEN
          RTIME = dtlast
        END IF
C
C---------DETERMINE IF END OF MODFLOW TIMESTEP        
        IF ( ilast.NE.0 ) THEN
          EXIT SWRSUBTIME
        END IF
        IF ( swrtot.GE.stpend ) EXIT SWRSUBTIME
C
C---------UPDATE NUMTIME
        NUMTIME = NUMTIME + 1
        IF ( RTIME.GE.RTMIN ) NADPCNT = NADPCNT + 1
C
C---------EXTEND THE SIZE OF SOLUTION ARRAYS IF NUMTINE IS GREATER THAN NTMAX
        IF ( NUMTIME.GT.NTMAX ) THEN
          WRITE (*,*) 'NUMTIME:',NUMTIME,' NTMAX: ', NTMAX
          CALL SSWR_ALLO_SOLN(NUMTIME)
!          CALL SSWR_SET_QCONN()
          WRITE (*,*) 'NUMTIME:',NUMTIME,' NTMAX: ', NTMAX
          PAUSE
        END IF
C
C-------SWRSUBTIME END        
      END DO SWRSUBTIME
C
C-------SKIP GROUNDWATER PROCESS INTERACTION CALCULATIONS IF
C       SURFACE-WATER ONLY SIMULATION ( ISWRONLY > 0 )
      IF ( ISWRONLY.NE.0 ) GOTO 200
C
C-------ADD INCREMENTAL AQUIFER-REACH TERMS FOR TIMESTEP TO HCOF AND RHS
      SWRADD2GWL: DO i = 1, NREACHES
C         DO NOT EVALUATE INACTIVE REACHES
        IF ( REACH(i)%ISWRBND.EQ.0 )    CYCLE SWRADD2GWL
C         DO NOT EVALUATE REACHES WITH ZERO CONDUCTANCES 
        IF ( REACH(i)%ICALCBFLOW.LT.1 ) CYCLE SWRADD2GWL
C         DO NOT EVALUATE REACHES WITH NO UNDERLYING ACTIVE CELLS
        IF ( REACH(i)%LAYACT.GT.NLAY )  CYCLE SWRADD2GWL
        ir   = REACH(i)%IRCH
        jc   = REACH(i)%JRCH
        rbot = REACH(i)%GEO%ELEV(1)
        QAQFMTIME: DO n = 1, NUMTIME
          fdelt = SWRTIME(n)%FDELT
C-----------UPDATE CURRENTQAQ
          irg = REACH(i)%IRG
          rs  = RSTAGE(i,n)
          ISWRDT  = n
          q = SSWR_CALC_QAQ(REACH(i),rs)
          dtscale = SWRTIME(n)%SWRDT / REAL( DELT, 8 )
!C-----------DO NOT EVALUATE REACHES WITH NO UNDERLYING ACTIVE CELLS
!          IF ( REACH(i)%LAYACT.GT.NLAY ) CYCLE SWRADD2GWL
C-----------CYCLE THROUGH EACH LAYER
          LAYERS: DO kl = REACH(i)%LAYSTR, REACH(i)%LAYEND
            kact = MAX( kl, REACH(i)%LAYACT )
            if ( kact.gt.NLAY ) then
              write (*,*) jc, ir, kl, kact
              stop
            end if
C-------------CURRENT ESTIMATE OF GROUNDWATER HEAD
            h0   = REAL( HOLD(jc,ir,kact), 8 )
            h1   = HNEW(jc,ir,kact)
            h    = ( DONE - fdelt ) * h0 + fdelt * h1
C-------------DEFAULT CONDITION IS ADDING TERMS TO HCOF+RHS 
            rhsonly = .FALSE.
C-------------TEST FOR CASE WHERE HEAD IN LAYER IS BELOW REACH BOTTOM
            IF (    h.LT.rbot ) rhsonly = .TRUE.
            IF ( kact.GT.kl   ) rhsonly = .TRUE.
!C-------------CURRENT ESTIMATE OF REACH STAGE
!            dtscale = SWRTIME(n)%SWRDT / REAL( DELT, 8 )
!C-------------UPDATE CURRENTQAQ
!            irg = REACH(i)%IRG
!            rs  = RSTAGE(i,n)
!            ISWRDT  = n
!            hc = SSWR_CALC_QAQ(REACH(i),rs)
            IF ( rhsonly ) THEN
C             SIGN CONVENTION FOR REACH QAQFLOW FOLLOWS MODFLOW CONVENTION
C             QAQFLOW IS CALCULATED IN SSWR_RG_QAQ FUNCTION CALL
              rc = REACH(i)%CURRENTQAQ(kl)%QAQFLOW * dtscale
              hc = DZERO
            ELSE
              rc = REACH(i)%CURRENTQAQ(kl)%CONDUCTANCE * 
     2          RSTAGE(i,n) * dtscale
              hc = REACH(i)%CURRENTQAQ(kl)%CONDUCTANCE * dtscale
            END IF
            IF ( INWTCNT.EQ.2 ) THEN
              RHS(jc,ir,kact)  = RHS(jc,ir,kact)  - rc 
              HCOF(jc,ir,kact) = HCOF(jc,ir,kact) - hc
            END IF
          END DO LAYERS
C
C-------------CALCULATE EXPLICIT SWR1 NEWTON TERMS
            IF ( INWTCORR.LT.0 ) THEN
              irg = REACH(i)%IRG
              rs  = RSTAGE(i,n)
              ISWRDT  = n
              INWTCNT = 1
              hc = SSWR_CALC_QAQ(REACH(i),rs)
              INWTCNT = 2
              hc = SSWR_CALC_QAQ(REACH(i),rs)
            END IF
        END DO QAQFMTIME
      END DO SWRADD2GWL
C
C------ADD NEWTON TERMS IF NECESSARY      
      IF ( INWTCORR.NE.0 ) THEN
        SWRADDNWTCL: DO i = 1, NREACHES
C           DO NOT EVALUATE INACTIVE REACHES
          IF ( REACH(i)%ISWRBND.EQ.0 )    CYCLE SWRADDNWTCL
C           DO NOT EVALUATE REACHES WITH ZERO CONDUCTANCES 
          IF ( REACH(i)%ICALCBFLOW.LT.1 ) CYCLE SWRADDNWTCL
C           DO NOT EVALUATE REACHES WITH NO UNDERLYING ACTIVE CELLS
          IF ( REACH(i)%LAYACT.GT.NLAY ) CYCLE SWRADDNWTCL
C-----------CYCLE THROUGH EACH REACH
          NWTCLAYERS: DO kl = REACH(i)%LAYSTR, REACH(i)%LAYEND
            ir   = REACH(i)%IRCH
            jc   = REACH(i)%JRCH
            kact = MAX( kl, REACH(i)%LAYACT )
C-------------CURRENT ESTIMATE OF GROUNDWATER HEAD
            h0   = REAL( HOLD(jc,ir,kact), 8 )
            h1   = HNEW(jc,ir,kact)
            hc   = DZERO
            DO n = 1, NUMTIME
              fdelt = SWRTIME(n)%FDELT
              h  = ( DONE - fdelt ) * h0 + fdelt * h1
              rs = RSTAGE(i,n)
              CALL SSWRNWT(n,kl,i,h,hc)
            END DO
C             NEWTON CORRECTION OF HCOF MATRIX FOR MODFLOW-2005
            IF ( INWTUNIT.EQ.0 ) THEN
              HCOF(jc,ir,kact) = HCOF(jc,ir,kact) + hc
C             NEWTON CORRECTION OF A MATRIX FOR MODFLOW-NWT
            ELSE
              ij   = 0
            !NEWTON
              ij   = ICELL(jc,ir,kact)  
              IF ( ij.GT.0 ) THEN
                A( IA(ij) )      = A(IA(ij)) + hc
              END IF
            END IF
          END DO NWTCLAYERS
        END DO SWRADDNWTCL
      END IF
C
C-------CALCULATE GROUNDWATER EVAPOTRANSPIRATION
      DO i = 1, NREACHES
        IF ( REACH(i)%IGEOTYPE.NE.5 ) CYCLE
        REACH(i)%GWETRATE = DZERO
C         ONLY PROCESS FOR REACHES THAT HAD ANY POTENTIAL FOR EVAPORATION
        IF ( REACH(i)%EVAP.EQ.DZERO ) CYCLE
C         ONLY CONTINUE PROCESS FOR REACHES WITH REMAINING EVAPORATION
        ir=REACH(i)%IRCH
        jc=REACH(i)%JRCH
        gwet = DZERO
        DO n = 1, NUMTIME
          rs  = RSTAGE(i,n)
          q  = SSWR_CALC_QEV(i,rs)
          dtscale = SWRTIME(n)%SWRDT / REAL( DELT, 8 )
          ge   = REACH(irch)%CURRENT%QPOTGWET * dtscale
          gwet = gwet + ge
        END DO
        getextd = REACH(irch)%GETEXTD
        IF ( ABS( gwet ).LT.NEARZERO ) CYCLE   
        IF ( getextd.LT.DZERO ) CYCLE
        REACH(i)%GWETRATE = gwet
C         FIND UPPER MOST ACTIVE LAYER          
        k = 0
        ETLAYER: DO kl = 1, NLAY
          h = HNEW(jc,ir,kl)
C-----------DETERMINE IF CELL IS DRY
          IF (ABS(h-DBLE(HDRY)).LT.NEARZERO) CYCLE
          k = kl
          EXIT ETLAYER
        END DO ETLAYER
        IF ( k.EQ.IZERO ) CYCLE
        IF( IBOUND(jc,ir,k).LE.0 ) CYCLE
C         GROUNDWATER LEVEL ABOVE TOP OF MODEL          
        IF ( h.GE.REACH(i)%GBELEV ) THEN
          RHS(jc,ir,k) = RHS(jc,ir,k) + gwet
C         GROUNDWATER LEVEL BELOW TOP OF MODEL BUT ABOVE EXTINCTION DEPTH
C         LINEAR RELATION BETWEEN GROUNDWATER HEAD AND GROUNDWATER EVAPOTRANSPIRATION
        ELSE IF ( h.GE.( REACH(i)%GBELEV-getextd ) ) THEN
          RHS(jc,ir,k)  = RHS(jc,ir,k) + gwet -
     2                    gwet * REACH(i)%GBELEV / getextd
          HCOF(jc,ir,k) = HCOF(jc,ir,k) - gwet / getextd
        END IF
C         DO NOT SIMULATE GROUNDWATER EVAPOTRANSPIRATION IF
C         GROUNDWATER LEVEL IS BELOW EXTINCTION DEPTH
      END DO
C
C-------SET ISWRCNVG
00200 ISWRCNVG = 1
      IF ( nfail.GT.IZERO ) ISWRCNVG = 0
C
C-------RESET FLAG TO RESET QEXTFLOW TO 0.0
      IEXTFLOW = 0
C
C-------END SWR TIMER FOR GWF2SWR7FM
      CALL SSWRTIMER(1,tim1,tim2,SWREXETOT)
C
C-------RETURN
      RETURN
      END SUBROUTINE GWF2SWR7FM

      SUBROUTINE GWF2SWR7CV(Kkiter, Igrid, Icnvg, Mxiter)
C     *****************************************************************
C     CHECK FOR CONVERGENCE OF SWR QAQFLOW RATES BETWEEN SUBSEQUENT
C     INNER ITERATIONS - VERSION  7.0.10: JUNE 19, 2009
C     *****************************************************************
      USE GLOBAL,       ONLY: IOUT, NSTP
      USE GWFBASMODULE, ONLY: DELT
      USE GWFSWRMODULE
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
      INTEGER, INTENT(IN)    :: Kkiter
      INTEGER, INTENT(IN)    :: Igrid
      INTEGER, INTENT(INOUT) :: Icnvg
      INTEGER, INTENT(INOUT) :: Mxiter
C     + + + LOCAL DEFINITIONS + + +
      INTEGER :: irch
      INTEGER :: irg
      INTEGER :: k, n
      DOUBLEPRECISION :: ssdv, gs, gs0, v, v0, dv
      DOUBLEPRECISION :: dtscale
      DOUBLEPRECISION :: bfcrit
      DOUBLEPRECISION :: bt
      DOUBLEPRECISION :: tim1, tim2
      DOUBLEPRECISION :: tt
      DOUBLEPRECISION :: swrbf, mfbf
      DOUBLEPRECISION :: swrsum, mfsum
      DOUBLEPRECISION :: incfrac
      DOUBLEPRECISION :: swrtot, mftot, totdiff
      DOUBLEPRECISION :: rs, q
C     + + + FUNCTIONS + + +
      DOUBLEPRECISION :: SSWR_RG_VOL
      DOUBLEPRECISION :: SSWR_CALC_QAQ
      DOUBLEPRECISION :: SSWR_MFQAQ
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2020  FORMAT(1X,'*** MODFLOW CONVERGENCE FAILURE ***',/,
     2       1X,'***      CONTINUING ANYWAY      ***',//)
C     + + + CODE + + +
C-------SET POINTERS FOR GRID
      CALL SGWF2SWR7PNT(Igrid)
C
C-------START SWR TIMER FOR GWF2SWR7CV
      CALL SSWRTIMER(0,tim1,tim2,SWREXETOT)
C
C-------ONLY A SINGLE CALL TO SWR SOLUTION REQUIRED IS 
C       SURFACE WATER ONLY SIMULATION (ISWRONLY > 0 )
      IF ( ISWRONLY.GT.0 ) THEN
        ICNVG = 1
        RETURN
      END IF
C
C-------RETURN IF NOT A STEADY STATE SIMULATION AND TOLA LESS THAN ZERO
      IF ( ISWRSS.LT.1 .AND. TOLA.LT.DZERO .AND. ISWRCONT.LT.1 ) RETURN
C
C-------STEADY STATE EVALUATION OF VOLUME CHANGE
      IF ( ISWRSS.GT.0 ) THEN
        ssdv  = DZERO
        DO n = 1, NRCHGRP
          gs0 = GSTAGE(n,0)
          v0 = SSWR_RG_VOL(RCHGRP(n),gs0)
          gs  = RCHGRP(n)%STAGE
          v  = SSWR_RG_VOL(RCHGRP(n),gs)
          dv = ABS( v - v0 )
          IF ( dv.GT.ssdv ) THEN
            ssdv  = dv
          END IF
        END DO
        DO n = 1, NREACHES
          RSTAGE(n,0) = REACH(n)%STAGE
        END DO
        DO n = 1, NRCHGRP
          GSTAGE(n,0) = RCHGRP(n)%STAGE
        END DO
        IF ( ( ssdv.GT.TOLR .AND. Kkiter.LT.Mxiter ) .OR. 
     2       Kkiter.EQ.1 ) THEN
          Icnvg = 0
          RETURN
        END IF
      END IF
C
C-------CHECK FOR BASFLOW CONVERGENCE, IF NECESSARY
      IF ( TOLA.LT.DZERO .AND. ISWRCONT.LT.1 ) GOTO 9999
C
C-------TOLA BASEFLOW CONVERGENCE TEST
!      tt = TOLA / QSCALE
      tt = TOLA
      IF ( TOLA.GT.DZERO ) THEN
        bfcrit = DZERO
C---------INITIALIZE BFCNV
        DO irch = 1, NREACHES
          BFCNV(1,irch) = DZERO
          BFCNV(2,irch) = DZERO
          BFCNV(3,irch) = DZERO
          BFCNV(4,irch) = DZERO
        END DO
C---------SET CURRENT QAQFLOW SUMMATION
        incfrac   = DZERO
        swrtot    = DZERO
        mftot     = DZERO
        DO irch = 1, NREACHES
C           DO NOT EVALUATE REACHES WITH ZERO CONDUCTANCES 
          IF ( REACH(irch)%ICALCBFLOW.LT.1 ) CYCLE
          swrbf  = DZERO
          DO n = 1, NUMTIME
            ISWRDT  = n
            irg     = REACH(irch)%IRG
            rs      = RSTAGE(irch,n)
            q       = SSWR_CALC_QAQ(REACH(irch),rs)
            dtscale = SWRTIME(n)%SWRDT / REAL( DELT, 8 )
            DO k = REACH(irch)%LAYSTR, REACH(irch)%LAYEND
              swrbf = swrbf + 
     2                REACH(irch)%CURRENTQAQ(k)%QAQFLOW * dtscale
            END DO
          END DO
C           DETERMINE MODFLOW REACH-AQUIFER EXCHANGE
          mfbf = SSWR_MFQAQ( REACH(irch) )
C
          swrtot = swrtot + swrbf
          mftot  = mftot  + mfbf
C           
          IF ( swrbf.GT.DZERO ) THEN
            BFCNV(1,irch) = swrbf
          ELSE
            BFCNV(2,irch) = ABS( swrbf )
          END IF
          IF ( mfbf.GT.DZERO ) THEN
            BFCNV(3,irch) = mfbf
          ELSE
            BFCNV(4,irch) = ABS( mfbf )
          END IF
          swrsum = ABS( BFCNV(1,irch) - BFCNV(2,irch) )
          mfsum  = ABS( BFCNV(3,irch) - BFCNV(4,irch) )
          bt     = DZERO
          IF ( mfsum.GT.DZERO ) THEN
            bt     = ABS( swrsum - mfsum ) / mfsum
            IF ( bt.GT.incfrac ) THEN
              incfrac = bt
            END IF
          END IF
        END DO
        totdiff = ( swrtot - mftot ) / mftot
C
C---------MAKE SURE AT LEAST TWO OUTER ITERATIONS ARE PERFORMED
        IF ( Kkiter.EQ.1 ) THEN
          Icnvg = 0
          GOTO 9999
        END IF
C
C---------IF CONVERGENCE ACHIEVED IN ITERATIVE MODFLOW SOLVER
C         MAKE SURE DIFFERENCES IN QAQFLOW SUMMATION FOR EACH 
C         REACH IS LESS THAN TOLA
        IF ( Icnvg.EQ.1 ) THEN
          IF ( incfrac.GT.tt ) THEN
            Icnvg    = 0
            ISWRCNVG = 0
          END IF
        END IF
C-------END OF TOLA BASEFLOW CONVERGENCE TEST
      END IF
C
C-------CONTINUE MODFLOW SIMULATION IF SWR CONTINUATION FLAG SET
      IF ( ISWRCONT.GT.0 .AND. Kkiter.EQ.Mxiter ) THEN
        WRITE (IOUT,2020) 
        Icnvg = 1
        GOTO 9999
      END IF
C
C-------END SWR TIMER FOR GWF2SWR7CV
09999 CALL SSWRTIMER(1,tim1,tim2,SWREXETOT)
C
C-------RETURN
      RETURN
      END SUBROUTINE GWF2SWR7CV

      SUBROUTINE GWF2SWR7BD(Kstp,Kper,Igrid)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR GROUP ROUTING REACHES
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY: IOUT,ISSFLG,NCOL,NROW,NLAY,IBOUND,
     2                       HNEW,HOLD,BUFF
      USE GWFBASMODULE,ONLY: MSUM,ICBCFL,IAUXSV,DELT,PERTIM,TOTIM,
     2                       VBVL,VBNM,
     3                       HDRY
      USE GWFSWRMODULE
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
      INTEGER, INTENT(IN) :: Kstp
      INTEGER, INTENT(IN) :: Kper
      INTEGER, INTENT(IN) :: Igrid
C     + + + LOCAL DEFINITIONS + + +
      LOGICAL :: rhsonly
      INTEGER :: ibd, ibdlbl
      INTEGER :: kl, ir, jc
      INTEGER :: kact, kk
      INTEGER :: i, j, k, n
      INTEGER :: irch
      INTEGER :: irg
      INTEGER :: iprn
      INTEGER :: iu
      INTEGER :: iaux
      INTEGER :: ival
      REAL :: rin, rout, rate
      REAL :: qaqincin,  qaqcumin
      REAL :: qaqincout, qaqcumout
      DOUBLEPRECISION :: rs, q
      DOUBLEPRECISION :: dtscale
      DOUBLEPRECISION :: h0, h1, h, s
      DOUBLEPRECISION :: fdelt
      DOUBLEPRECISION :: rbot
      DOUBLEPRECISION :: b
      DOUBLEPRECISION :: gwet, getextd, qq, hhcof, rrhs
      DOUBLEPRECISION :: ratin, ratout, rrate, tsrate
      DOUBLEPRECISION, DIMENSION(NLAY) :: layrate
      DOUBLEPRECISION :: swrtot, dt, t0
      DOUBLEPRECISION :: tim1, tim2
      CHARACTER (LEN=16) :: text(2)
      CHARACTER (LEN=15) :: cline(5)
      DATA text(1) /'     SWR LEAKAGE'/
      DATA text(2) /'        SWR GWET'/
C     + + + FUNCTIONS + + +
      CHARACTER (LEN=10) :: SSWR_SCCHAR
      DOUBLEPRECISION :: SSWR_CALC_MFQAQ
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(1X,/1X,79('-'),
     2          /1X,28X,'SWR1 OUTPUT PRINT TIMES',
     3          /1X,23X,'STRESS PERIOD ',I4,'   TIME STEP ',I5,
     4          /1X,5(15('-'),1X))
2010  FORMAT(G15.5)
2020  FORMAT(5(1X,A15))
2030  FORMAT(1X,79('-'))
2040  FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
2050  FORMAT(1X,'REACH ',I6,'   LAYER ',I3,'   ROW ',I5,'   COL ',I5,
     1       '   RATE',1PG15.6)
C
C     + + + CODE + + +
      CALL SGWF2SWR7PNT(Igrid)
C
C-------START SWR TIMER FOR GWF2SWR7BD
      CALL SSWRTIMER(0,tim1,tim2,SWREXETOT)
C
C-------WRITE SOLVER PRINTOUT
      CALL SSWR_PCNVG(Kper,Kstp)
C
C-------WRITE PRINT TIMES
      swrtot = REAL( TOTIM - DELT, 8 )
      iprn = 0
      cline(1:5) = '          '
      DO n = 1, NUMTIME
        dt = SWRTIME(n)%SWRDT
        swrtot = swrtot + dt
        IF ( SWRTIME(n)%ITPRN.NE.0 ) THEN
          IF ( iprn.LT.1 ) THEN
            WRITE (IOUT,2000) Kper, Kstp
            iprn = 1
          END IF
          WRITE ( cline(iprn),2010 ) REAL( swrtot, 4 )
          iprn = iprn + 1
        END IF
        IF ( iprn.GT.5 .OR. n.EQ.NUMTIME ) THEN
          WRITE (IOUT,2020) ( cline(i), i=1,5 )
          cline(1:5) = '          '
          iprn = 1
        END IF
      END DO
      IF ( iprn.GT.0 ) WRITE (IOUT,2030)
C
C-------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
C-------ACCUMULATORS (RATIN AND RATOUT).
      ratin  = DZERO
      ratout = DZERO
      ibd = 0
      IF(ISWRCB.LT.0 .AND. ICBCFL.NE.0) ibd = -1
      IF(ISWRCB.GT.0) ibd = ICBCFL
      ibdlbl = 0
C
C-------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(ibd.EQ.2) THEN
        iaux = NAUX
        IF(IAUXSV.EQ.0) iaux = 0
        CALL UBDSV4(Kstp,Kper,text(1),iaux,SWRAUX,ISWRCB,
     2              NCOL,NROW,NLAY,
     3              NQAQCONN,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C-------CLEAR THE BUFFER.
      DO k = 1, NLAY
        DO i = 1, NROW
          DO j = 1, NCOL
            BUFF(j,i,k) = RZERO
          END DO
        END DO
      END DO
C
C-------IF NO REACHES, SKIP FLOW CALCULATIONS.
      IF (NREACHES.EQ.0) GO TO 200
C
C-------IF SURFACE WATER ONLY - SKIP FLOW CALCULATIONS
      IF ( ISWRONLY.NE.0 ) GO TO 200
C
C-------LOOP THROUGH EACH RIVER REACH CALCULATING FLOW.
      REACHES: DO irch = 1, NREACHES
        tsrate  = DZERO
        layrate = DZERO
C---------GET ROW & COLUMN OF CELL CONTAINING REACH.
        ir      = REACH(irch)%IRCH
        jc      = REACH(irch)%JRCH
C---------DO NOT EVALUATE REACHES WITH ZERO CONDUCTANCES 
        IF ( REACH(irch)%ICALCBFLOW.LT.1 ) GOTO 199
!        IF ( REACH(irch)%ICALCBFLOW.LT.1 ) CYCLE REACHES
C---------REACH DATA
        rbot = REACH(irch)%GBELEV
!C---------GET ROW & COLUMN OF CELL CONTAINING REACH.
!        ir      = REACH(irch)%IRCH
!        jc      = REACH(irch)%JRCH
!        tsrate  = DZERO
!        layrate = DZERO
        TIMES: DO n = 1, NUMTIME
C-----------SET UPPER MOST ACTIVE LAYER FOR REACH
          kact = REACH(irch)%LAYACT
C-----------IF NO ACTIVE CELLS UNDERLYING REACH MOVE ON TO NEXT TIME FOR REACH.
          IF( kact.GT.NLAY ) CYCLE TIMES
          dtscale = SWRTIME(n)%SWRDT / REAL( DELT, 8 )
C-----------CALCULATE BASEFLOW FOR THIS TIME
          ISWRDT = n
          irg    = REACH(irch)%IRG
          rs     = RSTAGE(irch,n)
          q      = SSWR_CALC_MFQAQ(REACH(irch),rs)
C-----------EVALUATE QAQ FOR EACH LAYER          
          LAYERS: DO kl = REACH(irch)%LAYSTR, REACH(irch)%LAYEND
C-------------SET RATE
            rrate = DZERO
            rate  = RZERO
C-------------SET GROUNDWATER LAYER TO USE
            kk    = MAX( kl, kact )
C-------------IF CELL IS CONSTANT-HEAD MOVE ON TO NEXT LAYER FOR REACH.
            IF( IBOUND(jc,ir,kk).LT.0 ) CYCLE LAYERS  
            h0    = REAL( HOLD(jc,ir,kk), 8 )
            h1    = HNEW(jc,ir,kk)
            fdelt = SWRTIME(n)%FDELT
            h  = ( DONE - fdelt ) * h0 + fdelt * h1
C-------------DETERMINE SWR1 AQUIFER-REACH EXCHANGE APPROACH FOR REACH
C             DEFAULT CONDITION IS AQUIFER-REACH EXCHANGE ADDED TO HCOF+RHS
            rhsonly = .FALSE.
            IF ( h.LT.rbot ) THEN
              h = rbot
              rhsonly = .TRUE.
            END IF
C-------------CALCULATE AQUIFER-REACH EXCHANGE USING APPROPRIATE METHOD
            IF ( rhsonly ) THEN
              b     = REACH(irch)%CURRENTQAQ(kl)%QAQFLOW
            ELSE
              s     = RSTAGE(irch,n)
              b     = REACH(irch)%CURRENTQAQ(kl)%CONDUCTANCE * (s - h)
            END IF
            rrate       = b * dtscale
            tsrate      = tsrate + rrate
            layrate(kk) = layrate(kk) + rrate
          END DO LAYERS
        END DO TIMES
C---------PROCESS ACCUMULATED QAQFLOW DATA FOR CBC AND LIST OUTPUT
C-----------SET UPPER MOST ACTIVE LAYER FOR REACH
        kact = REACH(irch)%LAYACT
        kk   = kact
        LAYCBC: IF ( kact.LE.NLAY ) THEN 
          kk = MAX( REACH(irch)%LAYEND, kact )
          LAYEROUTPUT: DO kl = REACH(irch)%LAYSTR, kk
            rate = layrate(kl)
C-------------ADD AVERAGE RATE FOR EACH SWR TIMESTEP TO BUFFER.
            BUFF(jc,ir,kl) = BUFF(jc,ir,kl) + rate
C-------------PRINT THE INDIVIDUAL RATES IF REQUESTED(IRIVCB<0).
            IF ( IBD.LT.0 ) THEN
               IF (IBDLBL.EQ.0) WRITE(IOUT,2040) text(1),Kper,Kstp
               WRITE (IOUT,2050) irch, kl, ir, jc, rate
               ibdlbl=1
            END IF
          END DO LAYEROUTPUT
          rate = tsrate
C-----------SEE IF FLOW IS INTO AQUIFER OR INTO REACH.
          IF(rate.LT.RZERO) THEN
C-----------AQUIFER IS DISCHARGING TO REACH SUBTRACT RATE FROM RATOUT.
            ratout = ratout - tsrate
          ELSE
C-----------AQUIFER IS RECHARGED FROM RIVER; ADD RATE TO RATIN.
            ratin = ratin + tsrate
          END IF
        END IF LAYCBC
C
C---------IF SAVING CELL-BY-CELL FLOWS IN A LIST, WRITE FLOW.
00199   IF( IBD.EQ.2 ) THEN
          IF ( NAUX.GT.0 ) THEN
            AUXROW = AUX(:,irch)
            ival   = NAUX
          ELSE
            AUXROW = AUX(1,1)
            ival   = 1
          END IF
          DO kl = REACH(irch)%LAYSTR, REACH(irch)%LAYEND
            rate = layrate(kl)
            CALL UBDSVB(ISWRCB,NCOL,NROW,jc,ir,kl,rate,
     1                  AUXROW,ival,iaux,1,IBOUND,NLAY)
          END DO
        END IF
      END DO REACHES
C
C-------IF CELL-BY-CELL FLOW WILL BE SAVED AS A 3-D ARRAY,
C-------CALL UBUDSV TO SAVE THEM.
      IF (IBD.EQ.1) CALL UBUDSV(Kstp,Kper,text(1),ISWRCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
C
C-------MOVE RATES,VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
  200 rin  = ratin
      rout = ratout
      VBVL(3,MSUM) = rin
      VBVL(4,MSUM) = rout
      VBVL(1,MSUM) = VBVL(1,MSUM) + rin  * DELT
      VBVL(2,MSUM) = VBVL(2,MSUM) + rout * DELT
      VBNM(MSUM)   = text(1)
C
C-------MODFLOW BUDGET TERMS FOR TIMESTEP - COVERTED TO SWR1 BASED VALUES
C       INFLOW AND OUTFLOW ARE SWITCHED
C       SWR1 INFLOW  = MODFLOW OUTFLOW
C       SWR1 OUTFLOW = MODFLOW INFLOW
      qaqincout    = rin
      qaqcumout    = VBVL(1,MSUM)
      qaqincin     = rout
      qaqcumin     = VBVL(2,MSUM)
C
C-------INCREMENT BUDGET TERM COUNTER.
      MSUM = MSUM + 1
C
C-------CALCULATE BUDGET FOR SWR GROUNDWATER EVAPOTRANSPIRATION
C
C-------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
C-------ACCUMULATORS (RATIN AND RATOUT).
      ratin  = DZERO
      ratout = DZERO
      ibdlbl = 0
C
C-------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
C       AUXILIARY VARIABLES NOT WRITTEN FOR SWR1 CALCULATED ET      
      IF( ibd.EQ.2 ) THEN
        iaux = 0
        IF(IAUXSV.EQ.0) iaux = 0
        CALL UBDSV4(Kstp,Kper,text(2),iaux,SWRAUX,ISWRCB,
     2              NCOL,NROW,NLAY,
     3              NREACHES,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C-------CLEAR THE BUFFER.
      DO k = 1, NLAY
        DO i = 1, NROW
          DO j = 1, NCOL
            BUFF(j,i,k) = RZERO
          END DO
        END DO
      END DO
C
C-------IF NO REACHES, SKIP FLOW CALCULATIONS.
      IF ( NREACHES.EQ.0 ) GO TO 300
C
C-------IF SURFACE WATER ONLY - SKIP FLOW CALCULATIONS
      IF ( ISWRONLY.NE.0 ) GO TO 300
C
C---------CALCULATE GROUNDWATER EVAPOTRANSPIRATION
      DO irch = 1, NREACHES
        rate = RZERO
!        IF ( REACH(irch)%IGEOTYPE.NE.5 ) CYCLE
        IF ( REACH(irch)%IGEOTYPE.NE.5 ) GO TO 299
C         ONLY PROCESS FOR REACHES WITH REMAINING EVAPORATION
        ir=REACH(irch)%IRCH
        jc=REACH(irch)%JRCH
        gwet    = REACH(irch)%GWETRATE
        getextd = REACH(irch)%GETEXTD
!        IF ( gwet.EQ.DZERO    ) CYCLE   
!        IF ( getextd.LT.DZERO ) CYCLE   
        IF ( gwet.EQ.DZERO    ) GOTO 299
        IF ( getextd.LT.DZERO ) GOTO 299
C         FIND UPPER MOST ACTIVE LAYER          
        k = 0
        ETLAYER: DO kl = 1, NLAY
          h = HNEW(jc,ir,kl)
C-----------DETERMINE IF CELL IS DRY
          IF (ABS(h-DBLE(HDRY)).LT.NEARZERO) CYCLE ETLAYER
          k = kl
          EXIT ETLAYER
        END DO ETLAYER
!        IF ( k.EQ.IZERO ) CYCLE
!        IF( IBOUND(jc,ir,k).LE.0 ) CYCLE
        IF ( k.EQ.IZERO ) GOTO 299
        IF( IBOUND(jc,ir,k).LE.0 ) GOTO 299
C         GROUNDWATER LEVEL ABOVE TOP OF MODEL          
        IF ( h.GE.REACH(irch)%GBELEV ) THEN
          qq = -gwet
C         GROUNDWATER LEVEL BELOW TOP OF MODEL BUT ABOVE EXTINCTION DEPTH
C         LINEAR RELATION BETWEEN GROUNDWATER HEAD AND GROUNDWATER EVAPOTRANSPIRATION
        ELSE IF ( h.GE.( REACH(irch)%GBELEV-getextd ) ) THEN
          hhcof = - gwet / getextd
          rrhs  = ( gwet * REACH(irch)%GBELEV / getextd ) 
     2            - gwet
          qq    = h * hhcof + rrhs
C         DO NOT SIMULATE GROUNDWATER EVAPOTRANSPIRATION IF
C         GROUNDWATER LEVEL IS BELOW EXTINCTION DEPTH
        ELSE
          qq = DZERO
        END IF
        rate   = qq
        ratout = ratout - qq
C---------ADD AVERAGE RATE FOR EACH SWR TIMESTEP TO BUFFER.
        BUFF(jc,ir,k) = BUFF(jc,ir,k) + rate
C-----------PRINT THE INDIVIDUAL RATES IF REQUESTED(IRIVCB<0).
        IF ( IBD.LT.0 ) THEN
           IF (IBDLBL.EQ.0) WRITE(IOUT,2040) text(2),Kper,Kstp
           WRITE (IOUT,2050) irch, k, ir, jc, rate
           ibdlbl=1
        END IF
!        ratout = ratout - qq
C-----------IF SAVING CELL-BY-CELL FLOWS IN A LIST, WRITE FLOW.
00299   IF( IBD.EQ.2 ) THEN
          AUXROW = AUX(1,1)
          ival   = 1
          CALL UBDSVB(ISWRCB,NCOL,NROW,jc,ir,k,rate,
     1                AUXROW,ival,iaux,1,IBOUND,NLAY)
        END IF
      END DO
C
C-------IF CELL-BY-CELL FLOW WILL BE SAVED AS A 3-D ARRAY,
C-------CALL UBUDSV TO SAVE THEM.
      IF (IBD.EQ.1) CALL UBUDSV(Kstp,Kper,text(2),ISWRCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
C
C-------MOVE RATES,VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
  300 rin  = ratin
      rout = ratout
      VBVL(3,MSUM) = rin
      VBVL(4,MSUM) = rout
      VBVL(1,MSUM) = VBVL(1,MSUM) + rin  * DELT
      VBVL(2,MSUM) = VBVL(2,MSUM) + rout * DELT
      VBNM(MSUM)   = text(2)
C
C-------INCREMENT BUDGET TERM COUNTER.
      MSUM = MSUM + 1
C
C-------CALCULATE AND WRITE INCREMENTAL AND INCREMENTAL BUDGETS
      CALL SSWRBUDGET(Kper,Kstp,
     2                qaqcumin,qaqincin,qaqcumout,qaqincout)
C
C-------WRITE GROUP FLUXES TO OUTPUT FILE
      IF ( ISWRPRGF.NE.0 ) THEN
        CALL SSWR_P_RGFLOW(Kper,Kstp)
      END IF
C
C-------WRITE REACH STAGES TO OUTPUT FILE
      IF ( ISWRPSTG.NE.0 ) THEN
        CALL SSWR_P_RCHSTG(Kper,Kstp)
      END IF
C
C-------WRITE QAQFLOW FLUXES TO OUTPUT FILE
      IF ( ISWRPQAQ.NE.0 .OR. ISWRSRIV.NE.0 ) THEN
        CALL SSWR_P_QAQFLOW(Kper,Kstp)
      END IF
C
C-------WRITE QAQFLOW FLUXES TO OUTPUT FILE
      IF ( ISWRPQM.NE.0 ) THEN
        CALL SSWR_P_QMFLOW(Kper,Kstp)
      END IF
C
C-------WRITE STRUCTURE FLOWS TO OUTPUT FILE
      IF ( ISWRPSTR.NE.0 ) THEN
        CALL SSWR_P_STRFLOW(Kper,Kstp)
      END IF
C
C-------WRITE SUMMARY OF THE LOCATION AND MAGNITUDE OF THE MAXIMUM
C       VELOCITY AND FROUDE NUMBER FOR DIFFUSIVE WAVE REACH GROUPS
      IF ( ISWRPFRN.NE.0 ) THEN
        CALL SSWR_P_FROUDE(Kper,Kstp)
      END IF
C
C-------WRITE SWRDT TO EXTERNAL FILE
      iu = ABS(ISWRSDT)
      IF ( iu.GT.0 ) THEN
        swrtot = REAL( TOTIM - DELT, 8 )
        DO n = 1, NUMTIME
          dt = SWRTIME(n)%SWRDT
          swrtot = swrtot + dt
          IF ( swrtot.GT.TOTIM ) swrtot = REAL( TOTIM, 4 )
          IF ( ISWRSDT.GT.0 ) THEN
            WRITE ( iu, * ) REAL( swrtot, 4 ), dt
          ELSE
            WRITE ( iu )    REAL( swrtot, 4 )
            WRITE ( iu )    REAL( dt    , 4 )
          END IF
        END DO
      END IF
C
C-------WRITE SWR1 CONVERGENCE HISTORY
      iu = ISWRCNV
      IF ( iu.GT.0 ) THEN
        IF ( SWRHEADER.NE.1 ) THEN
          WRITE (iu,2100)
        END IF
        swrtot = REAL( TOTIM - DELT, 8 )
        DO n = 1, NUMTIME
          dt = SWRTIME(n)%SWRDT
          t0     = swrtot
          swrtot = swrtot + dt
          WRITE ( iu,2105 )
     2      Kper, Kstp, n, 
     3      SSWR_SCCHAR(REAL(t0,4)), SSWR_SCCHAR(REAL(swrtot,4)), 
     4      SSWR_SCCHAR(REAL(dt,4)), 
     5      SWRTIME(n)%ITER, SWRTIME(n)%ILOCFMAX,
     5      SSWR_SCCHAR(REAL(SWRTIME(n)%FMAX,4)), (SWRTIME(n)%ICNVG>0)
        END DO
      END IF
2100  FORMAT( 1X,'    STRESS',1X,'   MODFLOW',1X,'      SWR1',1X,
     2           '     START',1X,'       END',1X,'      SWR1',1X,
     3           '      SWR1',1X,'   MAXIMUM',1X,'   MAXIMUM',1X,
     4           '      SWR1',
     5       /1X,'    PERIOD',1X,' TIME STEP',1X,' TIME STEP',1X,
     6           '      TIME',1X,'      TIME',1X,'     SWRDT',1X,
     7           'ITERATIONS',1X,'RCHGRP LOC',1X,'  RESIDUAL',1X,
     8           ' CONVERGED',
     9       /1X,109('-'))
2105  FORMAT(1X,3(I10,1X),3(A10,1X),2(I10,1X),A10,1X,L10)
C
C-------SET FLAG FOR HEADER OUTPUT
      SWRHEADER = 1
C
C-------END SWR TIMER FOR GWF2SWR7BD
      CALL SSWRTIMER(1,tim1,tim2,SWREXETOT)
C
C-------RETURN
      RETURN
      END SUBROUTINE GWF2SWR7BD

      SUBROUTINE GWF2SWR7OT(IGRID)
C     ******************************************************************
C     OUTPUT SWR TIMER RESULTS - FLOATING POINT OPERATIONS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:IOUT
      USE GWFSWRMODULE
      IMPLICIT NONE
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER, INTENT(IN) :: IGRID
C     + + + LOCAL DEFINITIONS + + +
C     + + + FUNCTIONS + + +
C     + + + FORMATS + + +
2000  FORMAT(//,1X,'SUMMARY OF SWR1 PROCESS EXECUTION TIME',
     &        /,1X,'TOTAL SWR1 EXECUTION TIME:',1X,G16.7,1X,'SECONDS')
C     + + + CODE + + +
C
C         SET POINTERS
      CALL SGWF2SWR7PNT(Igrid)
C
      WRITE (IOUT,2000) SWREXETOT
C
C-------RETURN
      RETURN
C
      END SUBROUTINE GWF2SWR7OT

      SUBROUTINE GWF2SWR7DA(Igrid)
C  Deallocate SWR data for a grid
      USE GWFSWRMODULE
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
      INTEGER, INTENT(IN) :: Igrid
C
        DEALLOCATE(GWFSWRDAT(Igrid)%SWREXETOT)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISFRUNIT)
        DEALLOCATE(GWFSWRDAT(Igrid)%INWTUNIT)
C         AUXILIARY VARIABLES
        DEALLOCATE(GWFSWRDAT(Igrid)%NAUX)
        DEALLOCATE(GWFSWRDAT(Igrid)%NQAQCONN)
        DEALLOCATE(GWFSWRDAT(Igrid)%SWRAUX)
        DEALLOCATE(GWFSWRDAT(Igrid)%AUX)
        DEALLOCATE(GWFSWRDAT(Igrid)%AUXROW)
C        
        DEALLOCATE(GWFSWRDAT(Igrid)%NREACHES)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRONLY)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRCB)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRPRGF)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRPSTG)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRPQAQ)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRPQM)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRPSTR)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRPFRN)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWROPT)
C         SWR1 OPTIONS
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRSCRN)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRSDT)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRCNV)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRSRIV)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRALLRIV)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRSAVG)
        DEALLOCATE(GWFSWRDAT(Igrid)%NTABS)
        DEALLOCATE(GWFSWRDAT(Igrid)%ITABTIME)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRCONT)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRUPWT)
        DEALLOCATE(GWFSWRDAT(Igrid)%INEXCTNWT)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISSSTOR)
        DEALLOCATE(GWFSWRDAT(Igrid)%ILAGSTROPR)
        DEALLOCATE(GWFSWRDAT(Igrid)%IDPTHSCL)
        DEALLOCATE(GWFSWRDAT(Igrid)%IJACSCL)
        DEALLOCATE(GWFSWRDAT(Igrid)%IRCMRORD)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISOLSTG)
        DEALLOCATE(GWFSWRDAT(Igrid)%IWGTHDS)
        DEALLOCATE(GWFSWRDAT(Igrid)%INWTCORR)
        DEALLOCATE(GWFSWRDAT(Igrid)%INWTCNT)
        DEALLOCATE(GWFSWRDAT(Igrid)%ICIQM)
        DEALLOCATE(GWFSWRDAT(Igrid)%ICIBL)
C         SWR1 VARIABLES
        DEALLOCATE(GWFSWRDAT(Igrid)%DLENCONV)
        DEALLOCATE(GWFSWRDAT(Igrid)%TIMECONV)
        DEALLOCATE(GWFSWRDAT(Igrid)%QSCALE)
        DEALLOCATE(GWFSWRDAT(Igrid)%RSCALE)
        DEALLOCATE(GWFSWRDAT(Igrid)%DMINGRAD)
        DEALLOCATE(GWFSWRDAT(Igrid)%DMINDPTH)
        DEALLOCATE(GWFSWRDAT(Igrid)%DUPDPTH)
        DEALLOCATE(GWFSWRDAT(Igrid)%DMAXRAI)
        DEALLOCATE(GWFSWRDAT(Igrid)%DMAXSTG)
        DEALLOCATE(GWFSWRDAT(Igrid)%DMAXINF)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISOLVER)
        DEALLOCATE(GWFSWRDAT(Igrid)%SOLVERTEXT)
        DEALLOCATE(GWFSWRDAT(Igrid)%NOUTER)
        DEALLOCATE(GWFSWRDAT(Igrid)%NINNER)
        DEALLOCATE(GWFSWRDAT(Igrid)%IBT)
        DEALLOCATE(GWFSWRDAT(Igrid)%NTMIN)
        DEALLOCATE(GWFSWRDAT(Igrid)%NTMAX)
        DEALLOCATE(GWFSWRDAT(Igrid)%NUMTIME)
        DEALLOCATE(GWFSWRDAT(Igrid)%NUMTIME0)
        DEALLOCATE(GWFSWRDAT(Igrid)%NADPCNT)
        DEALLOCATE(GWFSWRDAT(Igrid)%NADPCNT0)
        DEALLOCATE(GWFSWRDAT(Igrid)%NTTSMIN)
        DEALLOCATE(GWFSWRDAT(Igrid)%FAILTIME)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRCNVG)
        DEALLOCATE(GWFSWRDAT(Igrid)%RTIME)
        DEALLOCATE(GWFSWRDAT(Igrid)%RTIME0)
        DEALLOCATE(GWFSWRDAT(Igrid)%RTMIN)
        DEALLOCATE(GWFSWRDAT(Igrid)%RTMAX)
        DEALLOCATE(GWFSWRDAT(Igrid)%RTSTMAX)
        DEALLOCATE(GWFSWRDAT(Igrid)%RTPRN)
        DEALLOCATE(GWFSWRDAT(Igrid)%RSWRPRN)
        DEALLOCATE(GWFSWRDAT(Igrid)%RSWRPRN0)
        DEALLOCATE(GWFSWRDAT(Igrid)%RTMULT)
        DEALLOCATE(GWFSWRDAT(Igrid)%RTOTIM)
        DEALLOCATE(GWFSWRDAT(Igrid)%NTMULT)
        DEALLOCATE(GWFSWRDAT(Igrid)%IADTIME)
        DEALLOCATE(GWFSWRDAT(Igrid)%TADMULT)
        DEALLOCATE(GWFSWRDAT(Igrid)%TOLS)
        DEALLOCATE(GWFSWRDAT(Igrid)%TOLR)
        DEALLOCATE(GWFSWRDAT(Igrid)%TOLA)
        DEALLOCATE(GWFSWRDAT(Igrid)%DAMPSS)
        DEALLOCATE(GWFSWRDAT(Igrid)%DAMPTR)
        DEALLOCATE(GWFSWRDAT(Igrid)%IPRSWR)
        DEALLOCATE(GWFSWRDAT(Igrid)%MUTSWR)
        DEALLOCATE(GWFSWRDAT(Igrid)%IPC)
        DEALLOCATE(GWFSWRDAT(Igrid)%NLEVELS)
        DEALLOCATE(GWFSWRDAT(Igrid)%DROPTOL)
        DEALLOCATE(GWFSWRDAT(Igrid)%IBTPRT)
        DEALLOCATE(GWFSWRDAT(Igrid)%ITMP)
        DEALLOCATE(GWFSWRDAT(Igrid)%IPTFLG)
        DEALLOCATE(GWFSWRDAT(Igrid)%IRDBND)
        DEALLOCATE(GWFSWRDAT(Igrid)%IRDRAI)
        DEALLOCATE(GWFSWRDAT(Igrid)%IRDEVP)
        DEALLOCATE(GWFSWRDAT(Igrid)%IRDLIN)
        DEALLOCATE(GWFSWRDAT(Igrid)%IRDGEO)
        DEALLOCATE(GWFSWRDAT(Igrid)%IRDSTR)
        DEALLOCATE(GWFSWRDAT(Igrid)%IRDSTG)
        DEALLOCATE(GWFSWRDAT(Igrid)%IRDAUX)
        DEALLOCATE(GWFSWRDAT(Igrid)%BFCNV)
        DEALLOCATE(GWFSWRDAT(Igrid)%NBDITEMS)
        DEALLOCATE(GWFSWRDAT(Igrid)%CUMBD)
        DEALLOCATE(GWFSWRDAT(Igrid)%INCBD)
        DEALLOCATE(GWFSWRDAT(Igrid)%CUMQAQBD)
        DEALLOCATE(GWFSWRDAT(Igrid)%INCQAQBD)
        DEALLOCATE(GWFSWRDAT(Igrid)%SWRHEADER)
        DEALLOCATE(GWFSWRDAT(Igrid)%NRCHGRP)
        DEALLOCATE(GWFSWRDAT(Igrid)%NSOLRG)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRSS)
        DEALLOCATE(GWFSWRDAT(Igrid)%ISWRDT)
        DEALLOCATE(GWFSWRDAT(Igrid)%SWRDT)
        DEALLOCATE(GWFSWRDAT(Igrid)%KMFITER)
        DEALLOCATE(GWFSWRDAT(Igrid)%SWRHEPS)
        DEALLOCATE(GWFSWRDAT(Igrid)%IGEOCI)
        DEALLOCATE(GWFSWRDAT(Igrid)%IQMCI)
        DEALLOCATE(GWFSWRDAT(Igrid)%SFMCI)
        DEALLOCATE(GWFSWRDAT(Igrid)%IEXTFLOW)
C---------REACH AND REACH GROUP STAGE
        DEALLOCATE(GWFSWRDAT(Igrid)%RSTAGE)
        DEALLOCATE(GWFSWRDAT(Igrid)%GSTAGE)
C---------DERIVED TYPES        
        DEALLOCATE(GWFSWRDAT(Igrid)%TABDATA)
        DEALLOCATE(GWFSWRDAT(Igrid)%REACH)
        DEALLOCATE(GWFSWRDAT(Igrid)%RCHGRP)
        DEALLOCATE(GWFSWRDAT(Igrid)%JAC)
        DEALLOCATE(GWFSWRDAT(Igrid)%SWRTIME)
        DEALLOCATE(GWFSWRDAT(Igrid)%FROUDE)
C---------PRINT DATA
        DEALLOCATE(GWFSWRDAT(Igrid)%NPMAX)
        DEALLOCATE(GWFSWRDAT(Igrid)%RSTAGEP)
        DEALLOCATE(GWFSWRDAT(Igrid)%GSTAGEP)
C-------RETURN
      RETURN
      END SUBROUTINE GWF2SWR7DA

      SUBROUTINE SGWF2SWR7PNT(Igrid)
C  Set pointers to SWR data for a grid
      USE GWFSWRMODULE
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
      INTEGER, INTENT(IN) :: Igrid
C
        SWREXETOT=>GWFSWRDAT(Igrid)%SWREXETOT
        ISFRUNIT=>GWFSWRDAT(Igrid)%ISFRUNIT
        INWTUNIT=>GWFSWRDAT(Igrid)%INWTUNIT
C         AUXILIARY VARIABLES
        NAUX=>GWFSWRDAT(Igrid)%NAUX
        NQAQCONN=>GWFSWRDAT(Igrid)%NQAQCONN
        SWRAUX=>GWFSWRDAT(Igrid)%SWRAUX
        AUX=>GWFSWRDAT(Igrid)%AUX
        AUXROW=>GWFSWRDAT(Igrid)%AUXROW
C        
        NREACHES=>GWFSWRDAT(Igrid)%NREACHES
        ISWRONLY=>GWFSWRDAT(Igrid)%ISWRONLY
        ISWRCB=>GWFSWRDAT(Igrid)%ISWRCB
        ISWRPRGF=>GWFSWRDAT(Igrid)%ISWRPRGF
        ISWRPSTG=>GWFSWRDAT(Igrid)%ISWRPSTG
        ISWRPQAQ=>GWFSWRDAT(Igrid)%ISWRPQAQ
        ISWRPQM=>GWFSWRDAT(Igrid)%ISWRPQM
        ISWRPSTR=>GWFSWRDAT(Igrid)%ISWRPSTR
        ISWRPFRN=>GWFSWRDAT(Igrid)%ISWRPFRN
        ISWROPT=>GWFSWRDAT(Igrid)%ISWROPT
C         SWR1 OPTIONS
        ISWRSCRN=>GWFSWRDAT(Igrid)%ISWRSCRN
        ISWRSDT=>GWFSWRDAT(Igrid)%ISWRSDT
        ISWRCNV=>GWFSWRDAT(Igrid)%ISWRCNV
        ISWRSRIV=>GWFSWRDAT(Igrid)%ISWRSRIV
        ISWRALLRIV=>GWFSWRDAT(Igrid)%ISWRALLRIV
        ISWRSAVG=>GWFSWRDAT(Igrid)%ISWRSAVG
        NTABS=>GWFSWRDAT(Igrid)%NTABS
        ITABTIME=>GWFSWRDAT(Igrid)%ITABTIME
        ISWRCONT=>GWFSWRDAT(Igrid)%ISWRCONT
        ISWRUPWT=>GWFSWRDAT(Igrid)%ISWRUPWT
        INEXCTNWT=>GWFSWRDAT(Igrid)%INEXCTNWT
        ISSSTOR=>GWFSWRDAT(Igrid)%ISSSTOR
        ILAGSTROPR=>GWFSWRDAT(Igrid)%ILAGSTROPR
        IDPTHSCL=>GWFSWRDAT(Igrid)%IDPTHSCL
        IJACSCL=>GWFSWRDAT(Igrid)%IJACSCL
        IRCMRORD=>GWFSWRDAT(Igrid)%IRCMRORD
        ISOLSTG=>GWFSWRDAT(Igrid)%ISOLSTG
        IWGTHDS=>GWFSWRDAT(Igrid)%IWGTHDS
        INWTCORR=>GWFSWRDAT(Igrid)%INWTCORR
        INWTCNT=>GWFSWRDAT(Igrid)%INWTCNT
        ICIQM=>GWFSWRDAT(Igrid)%ICIQM
        ICIBL=>GWFSWRDAT(Igrid)%ICIBL
C         SWR1 VARIABLES
        DLENCONV=>GWFSWRDAT(Igrid)%DLENCONV
        TIMECONV=>GWFSWRDAT(Igrid)%TIMECONV
        QSCALE=>GWFSWRDAT(Igrid)%QSCALE
        RSCALE=>GWFSWRDAT(Igrid)%RSCALE
        DMINGRAD=>GWFSWRDAT(Igrid)%DMINGRAD
        DMINDPTH=>GWFSWRDAT(Igrid)%DMINDPTH
        DUPDPTH=>GWFSWRDAT(Igrid)%DUPDPTH
        DMAXRAI=>GWFSWRDAT(Igrid)%DMAXRAI
        DMAXSTG=>GWFSWRDAT(Igrid)%DMAXSTG
        DMAXINF=>GWFSWRDAT(Igrid)%DMAXINF
        ISOLVER=>GWFSWRDAT(Igrid)%ISOLVER
        SOLVERTEXT=>GWFSWRDAT(Igrid)%SOLVERTEXT
        NOUTER=>GWFSWRDAT(Igrid)%NOUTER
        NINNER=>GWFSWRDAT(Igrid)%NINNER
        IBT=>GWFSWRDAT(Igrid)%IBT
        NTMIN=>GWFSWRDAT(Igrid)%NTMIN
        NTMAX=>GWFSWRDAT(Igrid)%NTMAX
        NUMTIME=>GWFSWRDAT(Igrid)%NUMTIME
        NUMTIME0=>GWFSWRDAT(Igrid)%NUMTIME0
        NADPCNT=>GWFSWRDAT(Igrid)%NADPCNT
        NADPCNT0=>GWFSWRDAT(Igrid)%NADPCNT0
        NTTSMIN=>GWFSWRDAT(Igrid)%NTTSMIN
        FAILTIME=>GWFSWRDAT(Igrid)%FAILTIME
        ISWRCNVG=>GWFSWRDAT(Igrid)%ISWRCNVG
        RTIME=>GWFSWRDAT(Igrid)%RTIME
        RTIME0=>GWFSWRDAT(Igrid)%RTIME0
        RTMIN=>GWFSWRDAT(Igrid)%RTMIN
        RTMAX=>GWFSWRDAT(Igrid)%RTMAX
        RTSTMAX=>GWFSWRDAT(Igrid)%RTSTMAX
        RTPRN=>GWFSWRDAT(Igrid)%RTPRN
        RSWRPRN=>GWFSWRDAT(Igrid)%RSWRPRN
        RSWRPRN0=>GWFSWRDAT(Igrid)%RSWRPRN0
        RTMULT=>GWFSWRDAT(Igrid)%RTMULT
        RTOTIM=>GWFSWRDAT(Igrid)%RTOTIM
        NTMULT=>GWFSWRDAT(Igrid)%NTMULT
        IADTIME=>GWFSWRDAT(Igrid)%IADTIME
        TADMULT=>GWFSWRDAT(Igrid)%TADMULT
        TOLS=>GWFSWRDAT(Igrid)%TOLS
        TOLR=>GWFSWRDAT(Igrid)%TOLR
        TOLA=>GWFSWRDAT(Igrid)%TOLA
        DAMPSS=>GWFSWRDAT(Igrid)%DAMPSS
        DAMPTR=>GWFSWRDAT(Igrid)%DAMPTR
        IPRSWR=>GWFSWRDAT(Igrid)%IPRSWR
        MUTSWR=>GWFSWRDAT(Igrid)%MUTSWR
        IPC=>GWFSWRDAT(Igrid)%IPC
        NLEVELS=>GWFSWRDAT(Igrid)%NLEVELS
        DROPTOL=>GWFSWRDAT(Igrid)%DROPTOL
        IBTPRT=>GWFSWRDAT(Igrid)%IBTPRT
        ITMP=>GWFSWRDAT(Igrid)%ITMP
        IPTFLG=>GWFSWRDAT(Igrid)%IPTFLG
        IRDBND=>GWFSWRDAT(Igrid)%IRDBND
        IRDRAI=>GWFSWRDAT(Igrid)%IRDRAI
        IRDEVP=>GWFSWRDAT(Igrid)%IRDEVP
        IRDLIN=>GWFSWRDAT(Igrid)%IRDLIN
        IRDGEO=>GWFSWRDAT(Igrid)%IRDGEO
        IRDSTR=>GWFSWRDAT(Igrid)%IRDSTR
        IRDSTG=>GWFSWRDAT(Igrid)%IRDSTG
        IRDAUX=>GWFSWRDAT(Igrid)%IRDAUX
        BFCNV=>GWFSWRDAT(Igrid)%BFCNV
        NBDITEMS=>GWFSWRDAT(Igrid)%NBDITEMS
        CUMBD=>GWFSWRDAT(Igrid)%CUMBD
        INCBD=>GWFSWRDAT(Igrid)%INCBD
        CUMQAQBD=>GWFSWRDAT(Igrid)%CUMQAQBD
        INCQAQBD=>GWFSWRDAT(Igrid)%INCQAQBD
        SWRHEADER=>GWFSWRDAT(Igrid)%SWRHEADER
        NRCHGRP=>GWFSWRDAT(Igrid)%NRCHGRP
        NSOLRG=>GWFSWRDAT(Igrid)%NSOLRG
        ISWRSS=>GWFSWRDAT(Igrid)%ISWRSS
        ISWRDT=>GWFSWRDAT(Igrid)%ISWRDT
        SWRDT=>GWFSWRDAT(Igrid)%SWRDT
        KMFITER=>GWFSWRDAT(Igrid)%KMFITER
        SWRHEPS=>GWFSWRDAT(Igrid)%SWRHEPS
        IGEOCI=>GWFSWRDAT(Igrid)%IGEOCI
        IQMCI=>GWFSWRDAT(Igrid)%IQMCI
        SFMCI=>GWFSWRDAT(Igrid)%SFMCI
        IEXTFLOW=>GWFSWRDAT(Igrid)%IEXTFLOW
        HK=>GWFSWRDAT(Igrid)%HK
C---------REACH AND REACH GROUP STAGE
        RSTAGE=>GWFSWRDAT(Igrid)%RSTAGE
        GSTAGE=>GWFSWRDAT(Igrid)%GSTAGE
C
C---------DERIVED TYPES        
        TABDATA=>GWFSWRDAT(Igrid)%TABDATA
        REACH=>GWFSWRDAT(Igrid)%REACH
        RCHGRP=>GWFSWRDAT(Igrid)%RCHGRP
        JAC=>GWFSWRDAT(Igrid)%JAC
        SWRTIME=>GWFSWRDAT(Igrid)%SWRTIME
        FROUDE=>GWFSWRDAT(Igrid)%FROUDE
C---------PRINT DATA
        NPMAX=>GWFSWRDAT(Igrid)%NPMAX
        RSTAGEP=>GWFSWRDAT(Igrid)%RSTAGEP
        GSTAGEP=>GWFSWRDAT(Igrid)%GSTAGEP
C
      RETURN
      END SUBROUTINE SGWF2SWR7PNT
C
C-------SAVE POINTERS TO SWR DATA FOR A GRID
      SUBROUTINE SGWF2SWR7PSV(Igrid)
      USE GWFSWRMODULE
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: Igrid
C
        GWFSWRDAT(Igrid)%SWREXETOT=>SWREXETOT
        GWFSWRDAT(Igrid)%ISFRUNIT=>ISFRUNIT
        GWFSWRDAT(Igrid)%INWTUNIT=>INWTUNIT
C         AUXILIARY VARIABLES
        GWFSWRDAT(Igrid)%NAUX=>NAUX
        GWFSWRDAT(Igrid)%NQAQCONN=>NQAQCONN
        GWFSWRDAT(Igrid)%SWRAUX=>SWRAUX
        GWFSWRDAT(Igrid)%AUX=>AUX
        GWFSWRDAT(Igrid)%AUXROW=>AUXROW
C
        GWFSWRDAT(Igrid)%NREACHES=>NREACHES
        GWFSWRDAT(Igrid)%ISWRONLY=>ISWRONLY
        GWFSWRDAT(Igrid)%ISWRCB=>ISWRCB
        GWFSWRDAT(Igrid)%ISWRPRGF=>ISWRPRGF
        GWFSWRDAT(Igrid)%ISWRPSTG=>ISWRPSTG
        GWFSWRDAT(Igrid)%ISWRPQAQ=>ISWRPQAQ
        GWFSWRDAT(Igrid)%ISWRPQM=>ISWRPQM
        GWFSWRDAT(Igrid)%ISWRPSTR=>ISWRPSTR
        GWFSWRDAT(Igrid)%ISWRPFRN=>ISWRPFRN
        GWFSWRDAT(Igrid)%ISWROPT=>ISWROPT
C         SWR1 OPTIONS        
        GWFSWRDAT(Igrid)%ISWRSCRN=>ISWRSCRN
        GWFSWRDAT(Igrid)%ISWRSDT=>ISWRSDT
        GWFSWRDAT(Igrid)%ISWRCNV=>ISWRCNV
        GWFSWRDAT(Igrid)%ISWRSRIV=>ISWRSRIV
        GWFSWRDAT(Igrid)%ISWRALLRIV=>ISWRALLRIV
        GWFSWRDAT(Igrid)%ISWRSAVG=>ISWRSAVG
        GWFSWRDAT(Igrid)%NTABS=>NTABS
        GWFSWRDAT(Igrid)%ITABTIME=>ITABTIME
        GWFSWRDAT(Igrid)%ISWRCONT=>ISWRCONT
        GWFSWRDAT(Igrid)%ISWRUPWT=>ISWRUPWT
        GWFSWRDAT(Igrid)%INEXCTNWT=>INEXCTNWT
        GWFSWRDAT(Igrid)%ISSSTOR=>ISSSTOR
        GWFSWRDAT(Igrid)%ILAGSTROPR=>ILAGSTROPR
        GWFSWRDAT(Igrid)%IDPTHSCL=>IDPTHSCL
        GWFSWRDAT(Igrid)%IJACSCL=>IJACSCL
        GWFSWRDAT(Igrid)%IRCMRORD=>IRCMRORD
        GWFSWRDAT(Igrid)%ISOLSTG=>ISOLSTG
        GWFSWRDAT(Igrid)%IWGTHDS=>IWGTHDS
        GWFSWRDAT(Igrid)%INWTCORR=>INWTCORR
        GWFSWRDAT(Igrid)%INWTCNT=>INWTCNT
        GWFSWRDAT(Igrid)%ICIQM=>ICIQM
        GWFSWRDAT(Igrid)%ICIBL=>ICIBL
C         SWR1 VARIABLES
        GWFSWRDAT(Igrid)%DLENCONV=>DLENCONV
        GWFSWRDAT(Igrid)%TIMECONV=>TIMECONV
        GWFSWRDAT(Igrid)%QSCALE=>QSCALE
        GWFSWRDAT(Igrid)%RSCALE=>RSCALE
        GWFSWRDAT(Igrid)%DMINGRAD=>DMINGRAD
        GWFSWRDAT(Igrid)%DMINDPTH=>DMINDPTH
        GWFSWRDAT(Igrid)%DUPDPTH=>DUPDPTH
        GWFSWRDAT(Igrid)%DMAXRAI=>DMAXRAI
        GWFSWRDAT(Igrid)%DMAXSTG=>DMAXSTG
        GWFSWRDAT(Igrid)%DMAXINF=>DMAXINF
        GWFSWRDAT(Igrid)%ISOLVER=>ISOLVER
        GWFSWRDAT(Igrid)%SOLVERTEXT=>SOLVERTEXT
        GWFSWRDAT(Igrid)%NOUTER=>NOUTER
        GWFSWRDAT(Igrid)%NINNER=>NINNER
        GWFSWRDAT(Igrid)%IBT=>IBT
        GWFSWRDAT(Igrid)%NTMIN=>NTMIN
        GWFSWRDAT(Igrid)%NTMAX=>NTMAX
        GWFSWRDAT(Igrid)%NUMTIME=>NUMTIME
        GWFSWRDAT(Igrid)%NUMTIME0=>NUMTIME0
        GWFSWRDAT(Igrid)%NADPCNT=>NADPCNT
        GWFSWRDAT(Igrid)%NADPCNT0=>NADPCNT0
        GWFSWRDAT(Igrid)%NTTSMIN=>NTTSMIN
        GWFSWRDAT(Igrid)%FAILTIME=>FAILTIME
        GWFSWRDAT(Igrid)%ISWRCNVG=>ISWRCNVG
        GWFSWRDAT(Igrid)%RTIME=>RTIME
        GWFSWRDAT(Igrid)%RTIME0=>RTIME0
        GWFSWRDAT(Igrid)%RTMIN=>RTMIN
        GWFSWRDAT(Igrid)%RTMAX=>RTMAX
        GWFSWRDAT(Igrid)%RTSTMAX=>RTSTMAX
        GWFSWRDAT(Igrid)%RTPRN=>RTPRN
        GWFSWRDAT(Igrid)%RSWRPRN=>RSWRPRN
        GWFSWRDAT(Igrid)%RSWRPRN0=>RSWRPRN0
        GWFSWRDAT(Igrid)%RTMULT=>RTMULT
        GWFSWRDAT(Igrid)%RTOTIM=>RTOTIM
        GWFSWRDAT(Igrid)%NTMULT=>NTMULT
        GWFSWRDAT(Igrid)%IADTIME=>IADTIME
        GWFSWRDAT(Igrid)%TADMULT=>TADMULT
        GWFSWRDAT(Igrid)%TOLS=>TOLS
        GWFSWRDAT(Igrid)%TOLR=>TOLR
        GWFSWRDAT(Igrid)%TOLA=>TOLA
        GWFSWRDAT(Igrid)%DAMPSS=>DAMPSS
        GWFSWRDAT(Igrid)%DAMPTR=>DAMPTR
        GWFSWRDAT(Igrid)%IPRSWR=>IPRSWR
        GWFSWRDAT(Igrid)%MUTSWR=>MUTSWR
        GWFSWRDAT(Igrid)%IPC=>IPC
        GWFSWRDAT(Igrid)%NLEVELS=>NLEVELS
        GWFSWRDAT(Igrid)%DROPTOL=>DROPTOL
        GWFSWRDAT(Igrid)%IBTPRT=>IBTPRT
        GWFSWRDAT(Igrid)%ITMP=>ITMP
        GWFSWRDAT(Igrid)%IPTFLG=>IPTFLG
        GWFSWRDAT(Igrid)%IRDBND=>IRDBND
        GWFSWRDAT(Igrid)%IRDRAI=>IRDRAI
        GWFSWRDAT(Igrid)%IRDEVP=>IRDEVP
        GWFSWRDAT(Igrid)%IRDLIN=>IRDLIN
        GWFSWRDAT(Igrid)%IRDGEO=>IRDGEO
        GWFSWRDAT(Igrid)%IRDSTR=>IRDSTR
        GWFSWRDAT(Igrid)%IRDSTG=>IRDSTG
        GWFSWRDAT(Igrid)%IRDAUX=>IRDAUX
        GWFSWRDAT(Igrid)%BFCNV=>BFCNV
        GWFSWRDAT(Igrid)%NBDITEMS=>NBDITEMS
        GWFSWRDAT(Igrid)%CUMBD=>CUMBD
        GWFSWRDAT(Igrid)%INCBD=>INCBD
        GWFSWRDAT(Igrid)%CUMQAQBD=>CUMQAQBD
        GWFSWRDAT(Igrid)%INCQAQBD=>INCQAQBD
        GWFSWRDAT(Igrid)%SWRHEADER=>SWRHEADER
        GWFSWRDAT(Igrid)%NRCHGRP=>NRCHGRP
        GWFSWRDAT(Igrid)%NSOLRG=>NSOLRG
        GWFSWRDAT(Igrid)%ISWRSS=>ISWRSS
        GWFSWRDAT(Igrid)%ISWRDT=>ISWRDT
        GWFSWRDAT(Igrid)%SWRDT=>SWRDT
        GWFSWRDAT(Igrid)%KMFITER=>KMFITER
        GWFSWRDAT(Igrid)%SWRHEPS=>SWRHEPS
        GWFSWRDAT(Igrid)%IGEOCI=>IGEOCI
        GWFSWRDAT(Igrid)%IQMCI=>IQMCI
        GWFSWRDAT(Igrid)%SFMCI=>SFMCI
        GWFSWRDAT(Igrid)%IEXTFLOW=>IEXTFLOW
        GWFSWRDAT(Igrid)%HK=>HK
C---------REACH AND REACH GROUP STAGE
        GWFSWRDAT(Igrid)%RSTAGE=>RSTAGE
        GWFSWRDAT(Igrid)%GSTAGE=>GSTAGE
C---------DERIVED TYPES        
        GWFSWRDAT(Igrid)%TABDATA=>TABDATA
        GWFSWRDAT(Igrid)%REACH=>REACH
        GWFSWRDAT(Igrid)%RCHGRP=>RCHGRP
        GWFSWRDAT(Igrid)%JAC=>JAC
        GWFSWRDAT(Igrid)%SWRTIME=>SWRTIME
        GWFSWRDAT(Igrid)%FROUDE=>FROUDE
C---------PRINT DATA
        GWFSWRDAT(Igrid)%NPMAX=>NPMAX
        GWFSWRDAT(Igrid)%RSTAGEP=>RSTAGEP
        GWFSWRDAT(Igrid)%GSTAGEP=>GSTAGEP
C
      RETURN
      END SUBROUTINE SGWF2SWR7PSV
C
C  SWR UTILITY SUBROUTINES
      SUBROUTINE SSWR_RD_COMM(Iu)
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Iu
C       + + + LOCAL DEFINITIONS + + +
        CHARACTER (LEN=2), PARAMETER :: comment = '//'
        CHARACTER (LEN=200) :: line
        LOGICAL :: iscomment
        INTEGER :: ios
        line = comment
        DO
          READ (Iu,'(A)',IOSTAT=ios) line
          IF (ios /= 0) CALL USTOP('COULD NOT READ FROM UNIT Iu')
          IF (LEN_TRIM(line).LT.1) THEN
            line = comment
            CYCLE
          END IF
          line = TRIM(ADJUSTL(line))
          iscomment = .FALSE.
          SELECT CASE (line(1:1))
            CASE ('#')
              iscomment = .TRUE.
            CASE ('!')
              iscomment = .TRUE.
            CASE DEFAULT
              IF (line(1:2).EQ.comment) iscomment = .TRUE.
          END SELECT
          IF (.NOT.iscomment) THEN
            BACKSPACE(Iu)
            RETURN
          END IF
        END DO
        RETURN
      END SUBROUTINE SSWR_RD_COMM

      SUBROUTINE SSWR_UNIQVALUES(A)
        USE GWFSWRINTERFACE, ONLY: SSWR_SORT
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: A
C       + + + LOCAL DEFINITIONS + + +
        INTEGER :: count
        INTEGER :: n
        DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: b
C       + + + FUNCTIONS + + +
C       + + + CODE + + +
C---------SORT A IN INCREASING ORDER            
        CALL SSWR_SORT(A)
C---------DETERMINE THE NUMBER OF UNIQUE VALUES
        count = 1
        DO n = 2, SIZE(A)
          IF ( A(n).GT.A(n-1) ) count = count + 1
        END DO
C---------ALLOCATE b FOR UNIQUE VALUES
        ALLOCATE(b(count))
C---------FILL b WITH UNIQUE VALUES
        b(1) = A(1)
        count = 1
        DO n = 2, SIZE(A)
          IF ( A(n).GT.A(n-1) ) THEN
            count = count + 1
            b(count) = A(n)
          END IF
        END DO
C---------REALLOCATE A TO SIZE OF b
        DEALLOCATE(A)
        ALLOCATE(A(SIZE(b)))
C---------SET A TO b
        A = b
        DEALLOCATE(b)
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_UNIQVALUES

      SUBROUTINE SSWR_SORT(V)
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: V
C       + + + LOCAL DEFINITIONS + + +
        INTEGER, PARAMETER :: nn=15, nstack=50
        DOUBLE PRECISION :: a
        INTEGER :: n, k, i, j, jstack, l, r
        INTEGER, DIMENSION(nstack) :: istack
C       + + + FUNCTIONS + + +
C       + + + CODE + + +
        n = SIZE(V)
        jstack = 0
        l = 1
        r = n
        DO
          IF (r - l.LT.nn) THEN
            DO j = (l+1), r
                a = V(j)
                DO i = (j-1), l, -1
                    IF (V(i).LE.a) EXIT
                    V(i+1) = V(i)
                END DO
                V(i+1) = a
            END DO
            IF (jstack.EQ.0) RETURN
            r = istack(jstack)
            l = istack(jstack-1)
            jstack = jstack - 2
          ELSE
            k = (l+r)/2
            CALL SSWR_SWAP(V(k),V(l+1))
            CALL SSWR_MSWAP(V(l),V(r),V(l).GT.V(r))
            CALL SSWR_MSWAP(V(l+1),V(r),V(l+1).GT.V(r))
            CALL SSWR_MSWAP(V(l),V(l+1),V(l).GT.V(l+1))
            i = l + 1
            j = r
            a = V(l+1)
            DO
              DO
                  i = i + 1
                  IF (V(i).GE.a) EXIT
              END DO
              DO
                  j = j - 1
                  IF (V(j).LE.a) EXIT
              END DO
              IF (j.LT.i) EXIT
              CALL SSWR_SWAP(V(i),V(j))
            END DO
            V(l+1) = V(j)
            V(j) = a
            jstack = jstack + 2
            IF (jstack.GT.nstack) THEN
              CALL USTOP('jstack.GT.nstack GWFSWR')
            END IF
            IF ((r-i+1).GE.(j-1)) THEN
              istack(jstack)   = r
              istack(jstack-1) = i
              r = j - 1
            ELSE
              istack(jstack)   = j-1
              istack(jstack-1) = l
              l = i
            END IF
          END IF
        END DO
        RETURN
      END SUBROUTINE SSWR_SORT
C
C-------UNCONDITIONAL SWAP
      SUBROUTINE SSWR_SWAP(A,B)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, INTENT(INOUT) :: A,B
C     + + + LOCAL DEFINITIONS + + +
        DOUBLE PRECISION :: d
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        d = A 
        A = B
        B = d
        RETURN
      END SUBROUTINE SSWR_SWAP
C
C-------CONDITIONAL SWAP - SWAP ONLY IF PASSED MASK IS TRUE
      SUBROUTINE SSWR_MSWAP(A,B,MASK)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, INTENT(INOUT) :: A,B
        LOGICAL, INTENT(IN) :: MASK
C     + + + LOCAL DEFINITIONS + + +
        DOUBLE PRECISION :: d
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        IF (MASK) THEN
            d = A 
            A = B
            B = d
        END IF
        RETURN
      END SUBROUTINE SSWR_MSWAP
C
C  SWR UTILITY FUNCTIONS
C
C-------SIMPLE FUNCTION TO CONVERT REAL DATA READ FROM ASCII FILE
C       TO EQUIVALENT DOUBLEPRECISION REPRESENTATION
      DOUBLEPRECISION FUNCTION SSWR_R2D(R) RESULT(D)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        REAL, INTENT(IN) :: R
C     + + + LOCAL DEFINITIONS + + +
        CHARACTER (LEN=15) :: cr
        WRITE (CR,'(E15.7)') R
        READ (CR,*) D
        RETURN
      END FUNCTION SSWR_R2D
      
C-------SIMPLE FUNCTION FOR LINEAR INTERPOLATION OF TWO VECTORS
C       FUNCTION ASSUMES X DATA IS SORTED IN ASCENDING ORDER
      DOUBLEPRECISION FUNCTION SSWR_LININT(X,Y,Z) RESULT(v)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, DIMENSION(:), INTENT(IN) :: X
        DOUBLEPRECISION, DIMENSION(:), INTENT(IN) :: Y
        DOUBLEPRECISION, INTENT(IN) :: Z
C     + + + LOCAL DEFINITIONS + + +
        INTEGER         :: i
        DOUBLEPRECISION :: dx, dydx
C     + + + CODE + + +
        v = 0.0d0
C---------BELOW BOTTOM OF RANGE - SET TO LOWEST VALUE
        IF (Z.LT.X(1)) THEN
          v = Y(1)
C----------ABOVE HIGHEST VALUE
C          SLOPE CALCULATED FROM INTERVAL BETWEEN SIZE(X) AND SIZE(X)-1
        ELSE IF (Z.GT.X(SIZE(X))) THEN
          dx   = X(SIZE(X)) - X(SIZE(X)-1)
          dydx = 0.0D0
          IF ( ABS(dx).GT.0.0D0 ) THEN
            dydx = ( Y(SIZE(X)) - Y(SIZE(X)-1) ) / dx
          END IF
          dx   = ( Z - X(SIZE(X)) )
          v = Y(SIZE(X)) + dydx * dx
C---------BETWEEN LOWEST AND HIGHEST VALUE IN CURRENT INTERVAL
        ELSE
          DO i = 2, SIZE(X)
            dx   = X(i) - X(i-1)
            dydx = 0.0D0
            IF ( Z.GE.X(i-1) .AND. Z.LE.X(i) ) THEN
              IF ( ABS(dx).GT.0 ) THEN
                dydx = ( Y(i) - Y(i-1) ) / dx
              END IF
              dx   = ( Z - X(i-1) )
              v = Y(i-1) + dydx * dx
              EXIT
            END IF
          END DO
        END IF
C---------RETURN
        RETURN
      END FUNCTION SSWR_LININT

C-------SIMPLE FUNCTION FOR LINEAR INTERPOLATION OF TWO VECTORS
C       FUNCTION ASSUMES X DATA IS SORTED IN ASCENDING ORDER
      REAL FUNCTION SSWR_RLININT(X,Y,Z) RESULT(v)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        REAL, DIMENSION(:), INTENT(IN) :: X
        REAL, DIMENSION(:), INTENT(IN) :: Y
        REAL, INTENT(IN) :: Z
C     + + + LOCAL DEFINITIONS + + +
        INTEGER         :: i
        REAL            :: dx, dydx
C     + + + CODE + + +
        v = 0.0E0
C---------BELOW BOTTOM OF RANGE - SET TO LOWEST VALUE
        IF (Z.LT.X(1)) THEN
          v = Y(1)
C----------ABOVE HIGHEST VALUE
C          SLOPE CALCULATED FROM INTERVAL BETWEEN SIZE(X) AND SIZE(X)-1
        ELSE IF (Z.GT.X(SIZE(X))) THEN
          dx   = X(SIZE(X)) - X(SIZE(X)-1)
          dydx = 0.0D0
          IF ( ABS(dx).GT.0.0D0 ) THEN
            dydx = ( Y(SIZE(X)) - Y(SIZE(X)-1) ) / dx
          END IF
          dx   = ( Z - X(SIZE(X)) )
          v = Y(SIZE(X)) + dydx * dx
C---------BETWEEN LOWEST AND HIGHEST VALUE IN CURRENT INTERVAL
        ELSE
          DO i = 2, SIZE(X)
            dx   = X(i) - X(i-1)
            dydx = 0.0D0
            IF ( Z.GE.X(i-1) .AND. Z.LE.X(i) ) THEN
              IF ( ABS(dx).GT.0 ) THEN
                dydx = ( Y(i) - Y(i-1) ) / dx
              END IF
              dx   = ( Z - X(i-1) )
              v = Y(i-1) + dydx * dx
              EXIT
            END IF
          END DO
        END IF
C---------RETURN
        RETURN
      END FUNCTION SSWR_RLININT
      
C
C-------CHECK IF VALID STRUCTURE CONNECTION SPECIFIED
      INTEGER FUNCTION SSWR_CHKSTR(Istrrch,Istrconn,Istrnum,Istrerr) 
     2   RESULT(ierr)
        USE GWFSWRMODULE
        USE GLOBAL,       ONLY: IOUT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Istrrch
        INTEGER, INTENT(IN) :: Istrconn
        INTEGER, INTENT(IN) :: Istrnum
        INTEGER, DIMENSION(NREACHES), INTENT(INOUT) :: Istrerr
C     + + + LOCAL DEFINITIONS + + +
        INTEGER         :: nc
        INTEGER         :: iconn
C     + + + FUNCTIONS + + +
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
02000   FORMAT(//1X,'SPECIFIED STRUCTURE CONNECTION ERRORS:',
     2          /1X,88('-'))
02010   FORMAT(1X,'INVALID STRUCTURE CONNECTION FOR REACH',1X,I5,1X,
     2            'STRUCTURE',1X,I5,1X,'SPECIFIED CONNECTION',1X,I5)
C     + + + CODE + + +
        ierr = 0
C---------RETURN IF BOUNDARY STRUCTURE
        IF ( Istrconn.LT.1 ) RETURN
C---------IDENTIFY IF Istrconn IS A VALID CONNECTION
        DO nc = 1, REACH(Istrrch)%NCONN
          iconn = REACH(Istrrch)%ICONN(nc)
          IF ( Istrconn.EQ.iconn ) RETURN
        END DO
        ierr = 1
        Istrerr(Istrrch) = Istrerr(Istrrch) + 1
C---------WRITE ERROR INFORMATION
        IF ( SUM( istrerr ).EQ.1 ) WRITE (IOUT,2000) 
        WRITE (IOUT,2010) Istrrch, Istrnum, Istrconn
C---------RETURN
        RETURN
      END FUNCTION SSWR_CHKSTR
C
C  SURFACE-WATER ROUTING CALCULATION SUBROUTINES
C
C-------CHECK USER SPECIFIED REACH CONNECTIVITY DATA
      SUBROUTINE SSWR_CNCK_RC()
        USE GWFSWRMODULE
        USE GWFSWRINTERFACE
        USE GLOBAL,       ONLY: IOUT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
C     + + + LOCAL DEFINITIONS + + +
        CHARACTER (LEN=14), DIMENSION(:), ALLOCATABLE :: ccerch
        INTEGER :: i, n
        INTEGER :: nn
        INTEGER :: irch1, irch2
        INTEGER :: nconn1, nconn2, iconn2
        INTEGER :: icount, ifound
        DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: dconnlist
C     + + + FUNCTIONS + + +
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000    FORMAT(//1X,'ERRORS IN SPECIFIED REACH CONNECTIONS',
     2          /1X,'   NON-SYMMETRIC REACH CONNECTIONS   ',
     3          /4X,'     REACH CONN. NO.      CONN.',
     4          /4X,30('-'))
2010    FORMAT(  4X,3(I10))
2020    FORMAT(  4X,30('-'),
     2         //1X,I6,1X,'REACH CONNECTIVITY ERRORS')
2030   FORMAT(/1X,59('-'),
     2        /1X,'FULL REACH CONNECTIVITY FOR EACH REACH')
2040   FORMAT(/1X,59('-'),/1X,'REACH  =',1X,I5,
     2        /1X,14X,17('-'),'CONNECTIONS',17('-'),
     2        /1X,14X,3(I14,1X),:100(/1X,14X,3(I14,1X)))
2050   FORMAT(1X,59('-'))

2060   FORMAT(1X,'REACHES',7X,3(A14,1X),:100(/1X,14X,3(A14,1X)))
     
C     + + + CODE + + +
C
C---------FIND UNIQUE CONNECTIONS FOR EACH REACH AND ELIMINATE ANY DUPLICATE CONNECTIONS
        DO irch1 = 1, NREACHES
          nconn1 = REACH(irch1)%NCONN
          IF ( nconn1.LT.1 ) CYCLE
          ALLOCATE ( dconnlist(nconn1) )
          DO n = 1, nconn1
            dconnlist(n) = REAL( REACH(irch1)%ICONN(n), 8 )
          END DO
          CALL SSWR_UNIQVALUES( dconnlist )
          nconn2 = SIZE( dconnlist )
          IF ( nconn2.LT.nconn1 ) THEN
            DEALLOCATE( REACH(irch1)%ICONN )
            REACH(irch1)%NCONN = nconn2
            ALLOCATE( REACH(irch1)%ICONN(nconn2) )
          END IF
          DO n = 1, nconn2
            REACH(irch1)%ICONN(n) = INT( dconnlist(n) )
          END DO
          DEALLOCATE ( dconnlist )
        END DO
C
C---------CHECK REACH CONNECTIONS FOR SYMMETRY
        icount = 0
        DO irch1 = 1, NREACHES
          nconn1 = REACH(irch1)%NCONN
          DO n = 1, nconn1
            irch2 = REACH(irch1)%ICONN(n)
            ifound = 0
            nconn2 = REACH(irch2)%NCONN
            DO nn = 1, nconn2
              iconn2 = REACH(irch2)%ICONN(nn)
              IF ( iconn2.EQ.irch1 ) THEN
                ifound = 1
                EXIT
              END IF
            END DO
            IF ( ifound.EQ.0 ) THEN
              IF ( icount.EQ.0 ) THEN
                WRITE (IOUT,2000)
              END IF
              WRITE (IOUT,2010) irch1, n, irch2
              icount = icount + 1
            END IF
          END DO
        END DO
        IF ( icount.GT.0 ) THEN
          WRITE (IOUT,2020) icount
          CALL USTOP('4B: ASSYMETRY IN REACH CONNECTIONS')
        END IF
C
C---------WRITE REACH CONNECTIVITY TO OUTPUT FILE
        ALLOCATE(ccerch(NREACHES))
        WRITE (IOUT,2030)
        DO irch1 = 1, NREACHES
          nconn1 = REACH(irch1)%NCONN
          WRITE (IOUT,2040) irch1, (i,i=1,nconn1) 
          WRITE (IOUT,2050)     
          ccerch = '     NONE     '
          DO n = 1, nconn1
            WRITE (ccerch(n),'(I5,1X,A2,1X,I5,1X)') 
     2          irch1,'TO',REACH(irch1)%ICONN(n)
          END DO
          WRITE (IOUT,2060) (ccerch(i),i=1,MAX(nconn1,1))
          WRITE (IOUT,2050)
        END DO
        DEALLOCATE(ccerch)
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_CNCK_RC
C
C-------ALLOCATE MEMORY FOR RCHGRP DATA
      SUBROUTINE SSWR_ALLO_RG()
        USE GWFSWRMODULE
        USE GWFSWRINTERFACE
        USE GLOBAL,       ONLY: IOUT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
C     + + + LOCAL DEFINITIONS + + +
        CHARACTER (LEN=2) :: croutetype1, croutetype2
        INTEGER :: i, n
        INTEGER :: irch
        INTEGER :: ios
        INTEGER :: icount
        INTEGER :: nn
        INTEGER :: irg1, irg2
        INTEGER :: iconn, imaxconn
        INTEGER :: irch1, irch2
        INTEGER :: iroutetype1, iroutetype2
        INTEGER, DIMENSION(:), ALLOCATABLE :: irgnumlist
        DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: drgnumlist
        INTEGER, DIMENSION(:), ALLOCATABLE :: irgconn
C     + + + FUNCTIONS + + +
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000    FORMAT(//1X,'ERRORS IN SPECIFIED REACH CONNECTIONS:  ',
     2          /1X,' MULTIPLE REACH CONNECTIONS TO THE SAME ',
     3          /1X,' DIFFUSIVE-WAVE APPROXIMATION REACH.    ',
     4          /1X,40('-'),
     5          /1X,'     ERROR     REACH  C.RCHGRP NO. CONN.',
     6          /1X,' --------- --------- --------- ---------')
2010    FORMAT(  1X,4(I10))
2020    FORMAT(  1X,40('-'))
2030    FORMAT(//1X,'ERRORS IN SPECIFIED REACH ROUTING       ',
     2          /1X,' APPROACH: ALL REACHES IN A REACH GROUP ',
     3          /1X,' MUST USE THE SAME ROUTING APPROACH.    ',
     4          /1X,50('-'),
     5          /1X,'     ERROR     REACH  RCH IROUTETYPE ',
     6              ' 1ST IROUTETYPE',
     6          /1X,' --------- --------- --------------- ',
     8              '---------------')
2040    FORMAT(  1X,2(I10),2(I15))
2050    FORMAT(  1X,50('-'))
C
C---------FIND UNIQUE REACH GROUP NUMBERS AND SET NRCHGRP
        ALLOCATE (drgnumlist(NREACHES))
        drgnumlist = REAL(REACH(:)%IRGNUM,8)
        CALL SSWR_UNIQVALUES(drgnumlist)
        NRCHGRP = SIZE(drgnumlist)
C---------ALLOCATE REACH GROUP DATA
        ALLOCATE(irgnumlist(NRCHGRP))
        ALLOCATE(RCHGRP(NRCHGRP))
        irgnumlist = INT(drgnumlist)
C---------CREATE UNIQUE MAPPING OF IRGNUM TO IRG
        DO n = 1, NRCHGRP
          RCHGRP(n)%IRGNUM = irgnumlist(n)
          RCHGRP(n)%IRG    = n
          DO i = 1, NREACHES
            IF (REACH(i)%IRGNUM.EQ.irgnumlist(n)) REACH(i)%IRG = n
          END DO
        END DO        
C
C---------ALLOCATE SPACE FOR RCHGRP(n)%REACH FOR SIMULATION
        DO n = 1, NRCHGRP
C           COUNT OF REACHES
          icount = 0
          DO i = 1, NREACHES
            IF (REACH(i)%IRG.EQ.n) icount = icount + 1
          END DO
C           ALLOCATE RCHGRP(n)%REACH
          RCHGRP(n)%NRGREACH = icount
          ALLOCATE (RCHGRP(n)%REACH(icount),STAT=ios)
          IF (ios.NE.0) THEN
            CALL USTOP('COULD NOT ALLOCATE RCHGRP(n)%REACH')
          END IF
C           FILL REACHES
          icount = 0
          DO i = 1, NREACHES
            IF (REACH(i)%IRG.EQ.n) THEN
              icount = icount + 1
              RCHGRP(n)%REACH(icount) = i
            END IF
          END DO
        END DO
C
C---------CHECK THAT DIFFUSIVE WAVE REACHES DO NOT HAVE MORE THAN ONE
C         THE ONE CONNECTION TO A SINGLE REACH GROUP
          ALLOCATE( irgconn(NRCHGRP) )
          icount = 0
          DO i = 1, NREACHES
            irgconn = 0
            IF ( REACH(i)%CROUTETYPE.EQ.'DW' ) THEN
              irg1 = REACH(i)%IRG
              DO n = 1, REACH(i)%NCONN
                iconn = REACH(i)%ICONN(n)
                IF ( REACH(iconn)%CROUTETYPE.EQ.'DW' ) THEN
                  irg2 = REACH(iconn)%IRG
                  IF ( irg2.NE.irg1 ) irgconn(irg2) = irgconn(irg2) + 1
                END IF
                imaxconn = MAXVAL( irgconn )
                IF ( imaxconn.GT.1 ) THEN
C                   WRITE HEADER FOR ERROR                  
                  IF ( icount.LT.1 ) THEN
                    WRITE (IOUT,2000)
                  END IF
                  icount = icount + 1
                  DO nn = 1, NRCHGRP
                    IF ( irgconn(nn).GT.1 ) THEN
                      WRITE (IOUT,2010) icount, i, nn, irgconn(nn)
                    END IF
                  END DO
                END IF
              END DO
            END IF
          END DO
          DEALLOCATE( irgconn )
          IF ( icount.GT.0 ) THEN
            WRITE (IOUT,2020)
            CALL USTOP('MULTIPLE DW CONNECTIONS TO THE SAME DW RCHGRP')
          END IF
C
C---------CHECK THAT THE SAME ROUTING APPROACH IS USED FOR ALL
C         REACHES IN A REACH GROUP
          icount = 0
          DO n = 1, NRCHGRP
            irch1       = RCHGRP(n)%REACH(1)
            iroutetype1 = REACH(irch1)%IROUTETYPE
            croutetype1 = REACH(irch1)%CROUTETYPE
            DO nn = 2, RCHGRP(n)%NRGREACH
              irch2       = RCHGRP(n)%REACH(nn)
              iroutetype2 = REACH(irch2)%IROUTETYPE
              croutetype2 = REACH(irch2)%CROUTETYPE
              IF ( croutetype2.NE.croutetype1 ) THEN
C                 WRITE HEADER FOR ERROR                  
                IF ( icount.LT.1 ) THEN
                  WRITE (IOUT,2030)
                END IF
                icount = icount + 1
                WRITE (IOUT,2040) icount, irch2, 
     2                            iroutetype2, iroutetype1
              END IF
            END DO
          END DO
          IF ( icount.GT.0 ) THEN
            WRITE (IOUT,2050)
            CALL USTOP('MULT. ROUTING APPROACHES FOR AT LEAST '//
     2                 'ONE RCHGRP')
          END IF
C
C---------CALCULATE DLEN FOR REACH GROUP
        DO n = 1, NRCHGRP
          DO i = 1, RCHGRP(n)%NRGREACH
            irch = RCHGRP(n)%REACH(i)
            RCHGRP(n)%DLEN = RCHGRP(n)%DLEN + REACH(irch)%DLEN
          END DO
        END DO
C
C---------CLEAN UP TEMPORARY STORAGE
        DEALLOCATE(irgnumlist,drgnumlist)
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_ALLO_RG
C
C-------ALLOCATE MEMORY FOR RCHGRP DATA
      SUBROUTINE SSWR_SET_CONN()
        USE GLOBAL,          ONLY: IOUT, NCOL, NROW
        USE GWFSWRMODULE,    ONLY: IZERO, DZERO, DONE, IRCMRORD, 
     2                             IPC, NLEVELS, IPTFLG, 
     3                             NREACHES, REACH, 
     4                             IRDBND, ISOLVER, NINNER, 
     5                             NRCHGRP, NSOLRG, RCHGRP, JAC
        USE GWFSWRINTERFACE, ONLY: SSWR_SORT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j, n
        INTEGER :: ii, jj, nn
        INTEGER :: nbw, iusercm
        INTEGER :: i0, i1
        INTEGER :: icol
        INTEGER :: ist, ict
        INTEGER :: irg, irgn, irch
        INTEGER :: irchconn, jrchconn
        INTEGER :: istrrch0
        INTEGER :: istr
        INTEGER :: innz
        INTEGER :: ilen
        INTEGER :: ilen2
        INTEGER :: ihbw
        INTEGER :: irchconnrg
        INTEGER :: istrconn, irch2, irchconn2
        INTEGER :: isolrg
        INTEGER, DIMENSION(:), ALLOCATABLE :: irchc, irchn
        INTEGER, DIMENSION(:), ALLOCATABLE :: iimpc
        INTEGER, DIMENSION(:), ALLOCATABLE :: icon
        DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: dcolptr
        INTEGER, DIMENSION(:), ALLOCATABLE :: iadj
        INTEGER, DIMENSION(:), ALLOCATABLE :: jadj
        INTEGER, DIMENSION(:), ALLOCATABLE :: inode
        INTEGER, DIMENSION(:), ALLOCATABLE :: ircm
        INTEGER, DIMENSION(:), ALLOCATABLE :: ircm_inv
        INTEGER, DIMENSION(:), ALLOCATABLE :: iat
        INTEGER, DIMENSION(:), ALLOCATABLE :: jat
        INTEGER, DIMENSION(:), ALLOCATABLE :: ismapt
        INTEGER, DIMENSION(:), ALLOCATABLE :: icmapt
        INTEGER :: nstr, nend
        INTEGER :: iconn
        INTEGER :: ic, nc
        INTEGER :: iwk
        INTEGER :: ijlu, ijw, iwlu
C     + + + FUNCTIONS + + +
        INTEGER :: GSOL_ADJ_PERM_BANDWIDTH
C     + + + OUTPUT FORMATS + + +
C     + + + DATA + + +
C
C---------DEVELOP FULL CONNECTIVITY ON FIRST STRESS PERIOD
        INITIALIZE: IF ( JAC%IALLO.EQ.IZERO ) THEN
C
C-----------DETERMINE THE NUMBER OF CONNECTIONS
          ALLOCATE (irchc(NREACHES),irchn(NREACHES))
          ALLOCATE (iimpc(NREACHES))
          nstr = 0
          nend = 0
          innz = 0
          INITC: DO n = 1, NRCHGRP
            irchc   = IZERO
            irchn   = IZERO
            iimpc   = IZERO
C             EVALUATE FULL REACH CONNECTIVITY FOR RG IN OTHER REACHES
C             - IMPLICIT STARTING 1D CONNECTIONS FOR RG
            FRC: DO i = 1, RCHGRP(n)%NRGREACH  
              irch = RCHGRP(n)%REACH(i)
              irg  = REACH(irch)%IRG
              ICONNECTED: IF (REACH(irch)%NCONN.GT.IZERO) THEN
                DO ii = 1, REACH(irch)%NCONN
                  istrrch0 = REACH(irch)%ICONN(ii)
                  irgn = REACH(istrrch0)%IRG
                  IF (irgn.EQ.irg) CYCLE
                  irchc(istrrch0) = irch
                  iimpc(istrrch0) = 1
                END DO
              END IF ICONNECTED
            END DO FRC
C             REACH GROUP CONNECTIONS
            ilen = SUM(iimpc)
            RCHGRP(n)%NCONN = ilen
C             ADD DIAGONAL + CONNECTIONS TO NNZ
            innz = innz + 1 + ilen
C             ALLOCATE AND FILL FULL STARTING CONNECTIONS        
            IF (RCHGRP(n)%NCONN.GT.IZERO) THEN
              ALLOCATE (RCHGRP(n)%IRCHC(RCHGRP(n)%NCONN))
              ALLOCATE (RCHGRP(n)%IRCHN(RCHGRP(n)%NCONN))
              ALLOCATE (RCHGRP(n)%ISTRRCH(RCHGRP(n)%NCONN))
              ilen = 0
              ilen2 = 0
              DO i = 1, NREACHES
                IF (iimpc(i).GT.IZERO) THEN
                  ilen = ilen + 1
                  RCHGRP(n)%IRCHN(ilen) = irchc(i)
                  RCHGRP(n)%IRCHC(ilen) = i
                END IF
                DO ii = 1, REACH(i)%NCONN
                  jj = REACH(i)%ICONN(ii)
                  IF ( irchc(jj).NE.i ) CYCLE
                  ilen2 = ilen2 + 1
                  REACH(i)%IRGCONN(ii) = ilen2 !1
                END DO
              END DO
            END IF
          END DO INITC
C
C-----------CLEAN UP TEMPORARY STORAGE
          DEALLOCATE(irchc,irchn)
        END IF INITIALIZE
C
C---------DEVELOP BANDED OR FULL STORAGE AND MODIFIED CRS STORAGE FOR JACOBIAN
C         AND MAPPING VECTOR (JAC%ISMAP) FOR RESIDUAL AND STAGE VECTORS
        MODBND: IF ( IRDBND.GT.IZERO ) THEN
          ilen = IZERO
          DO n = 1, NRCHGRP
            IF ( RCHGRP(n)%IRGBND.GT.0 ) ilen = ilen + 1
          END DO
C           DEALLOCATE MEMORY FOR MATRIX STORAGE
          IF ( JAC%IALLO.GT.IZERO ) THEN
            DEALLOCATE (JAC%IA,JAC%JA,JAC%IU,JAC%ICMAP,JAC%ISMAP)
            DEALLOCATE(JAC%FJACC)
            DEALLOCATE(JAC%FSCALE)
            DEALLOCATE(JAC%FSCALE2)
            DEALLOCATE(JAC%JSKM1)
            IF (ISOLVER.EQ.1) THEN
              DEALLOCATE(JAC%FJACU)
              IF ( JAC%IBAND.GT.0 ) THEN
                DEALLOCATE(JAC%FJACL)
              END IF
            ELSE
              DEALLOCATE(JAC%JLU)
              DEALLOCATE(JAC%JW)
              DEALLOCATE(JAC%WLU)
              DEALLOCATE(JAC%MJACC)
            END IF
C             DEALLOCATE TEMPORARY SOLVER STORAGE
            DEALLOCATE (JAC%JPSAV,JAC%JPPH)
            DEALLOCATE (JAC%JH,JAC%JV)
            DEALLOCATE (JAC%PS,JAC%PS0)
            DEALLOCATE (JAC%PINF,JAC%PINF0)
            DEALLOCATE (JAC%S,JAC%S0)
            DEALLOCATE (JAC%R,JAC%XI)
            DEALLOCATE (JAC%XS,JAC%X0)
            DEALLOCATE (JAC%DX,JAC%XSI)
            DEALLOCATE (JAC%F,JAC%F0)
            SELECT CASE ( ISOLVER )
              CASE (1)
                DEALLOCATE (JAC%PINDEXLU,JAC%SLU)
              CASE (2)
                DEALLOCATE (JAC%DBCGS,JAC%DHATBCGS)
                DEALLOCATE (JAC%PBCGS,JAC%PHATBCGS)
                DEALLOCATE (JAC%SBCGS,JAC%SHATBCGS)
                DEALLOCATE (JAC%VBCGS,JAC%TBCGS)
              CASE (3)
                DEALLOCATE (JAC%RGMR,JAC%ZGMR)
                DEALLOCATE (JAC%DGMR,JAC%TGMR)
                DEALLOCATE (JAC%CSGMR,JAC%SNGMR)
                DEALLOCATE (JAC%SGMR,JAC%YGMR)
                DEALLOCATE (JAC%HGMR)
                DEALLOCATE (JAC%VGMR)
            END SELECT
          END IF 
C           CREATE MAPPING ARRAY
          NSOLRG = ilen
          isolrg = MAX( 1, NSOLRG )
          ALLOCATE (JAC%IA(isolrg+1),JAC%IU(isolrg))
          ALLOCATE (JAC%ICMAP(NRCHGRP))
          ALLOCATE (JAC%ISMAP(isolrg))
C           INITIALIZE ARRAYS          
          DO n = 1, isolrg+1
            JAC%IA(n) = IZERO
          END DO
          DO n = 1, isolrg
            JAC%IU(n)    = IZERO
            JAC%ISMAP(n) = IZERO
          END DO
C           INITIALIZE REACH GROUP ARRAYS          
          DO n = 1, NRCHGRP
            JAC%ICMAP(n) = IZERO
          END DO
          ilen = IZERO
          DO n = 1, NRCHGRP
            IF ( RCHGRP(n)%IRGBND.GT.0 ) THEN
              ilen = ilen + 1
              JAC%ISMAP(ilen) = n
              JAC%ICMAP(n)    = ilen
            END IF
          END DO
C           CALCULATE NNZ
          ALLOCATE ( icon(isolrg) )
          DO n = 1, isolrg
            icon(n) = IZERO
          END DO
          innz = IZERO
          DO n = 1, NSOLRG
            irg = JAC%ISMAP(n)
C             FILL IA WITH STARTING POSITION OF SOLUTION ELEMENT
C             IN JA ROW POINTERS - REPRESENTS THE DIAGONAL ELEMENT
            innz = innz + 1
            JAC%IA(n) = innz
            DO i = 1, RCHGRP(irg)%NCONN
              irgn = REACH(RCHGRP(irg)%IRCHC(i))%IRG
              IF ( RCHGRP(irgn)%IRGBND.GT.0 ) THEN
                innz = innz + 1
                icon(n) = icon(n) + 1
              END IF
            END DO
          END DO          
          JAC%NNZ = innz
          innz = MAX( 1, JAC%NNZ )
          JAC%IA(NSOLRG+1) = JAC%NNZ + 1
C
C-----------ALLOCATE, INITIALIZE, AND FILL JA
          ALLOCATE(JAC%JA(innz))
          DO i = 1, innz
            JAC%JA(i) = IZERO
          END DO
C           FILL JA   
          IF ( NSOLRG.GT.0 ) innz = 0
          DO n = 1, NSOLRG
            irg = JAC%ISMAP(n)
            innz = innz + 1
C             ADD DIAGONAL
            JAC%JA(innz) = n
            ilen = icon(n)
            IF (ilen.LT.1) CYCLE
            ALLOCATE (dcolptr(ilen))
            dcolptr = DZERO
            ii = IZERO
            DO i = 1, RCHGRP(irg)%NCONN
              irgn = REACH(RCHGRP(irg)%IRCHC(i))%IRG
              IF ( RCHGRP(irgn)%IRGBND.LT.1 ) CYCLE
              jj = IZERO
              DO j = 1, NSOLRG
                IF ( JAC%ISMAP(j) .EQ. irgn ) THEN
                  jj = j
                  EXIT
                END IF
              END DO
              IF ( jj.EQ.IZERO ) CYCLE
              ii  = ii + 1
              dcolptr(ii) = REAL(jj,8)
            END DO
            CALL SSWR_SORT(dcolptr)
            DO i = 1, ilen
              IF ( dcolptr(i).EQ.DZERO ) CYCLE
              innz = innz + 1
              JAC%JA(innz) = INT(dcolptr(i))
              IF (JAC%JA(innz).GT.n .AND. JAC%IU(n).EQ.IZERO) THEN
                JAC%IU(n) = innz
              END IF
            END DO
C             TEST FOR LOWER RIGHT OF MATRIX WHERE NO UPPER PRESENT
C             SET JAC%IU TO THE LAST ENTRY
            IF (JAC%IU(n).EQ.IZERO) JAC%IU(n) = JAC%IA(n+1) - 1
            DEALLOCATE(dcolptr)
          END DO       
C
C-----------CALCULATE INITIAL BANDWIDTH FOR DIRECT SOLVER AND SET JAC%NHALFB
          DO n = 1, NSOLRG
            irg = JAC%ISMAP(n)
            DO i = 1, RCHGRP(irg)%NCONN
              irgn = REACH( RCHGRP(irg)%IRCHC(i) )%IRG
              IF ( RCHGRP(irgn)%IRGBND.LT.1 ) CYCLE
              ii = JAC%ICMAP(irgn)
              ihbw = ABS( n - ii )
              IF (ihbw.GT.JAC%NHALFB) JAC%NHALFB = ihbw
            END DO
          END DO
          JAC%NBW = 2 * JAC%NHALFB + 1
C
C-----------REVERSE CUTHILL MCKEE REORDERING
          RCMORDL: IF ( NSOLRG.GT.0 .AND. IRCMRORD.NE.0 ) THEN
            ALLOCATE ( iadj(NSOLRG+1), jadj(JAC%NNZ-NSOLRG) )
            ALLOCATE ( inode(NSOLRG), ircm(NSOLRG), ircm_inv(NSOLRG) )
            iadj(1) = 1
            ii = 0
            DO i = 1, NSOLRG
              inode(i)     = i
              ircm(i)      = 0
              ircm_inv(i)  = 0
              i0       = JAC%IA(i)   + 1
              i1       = JAC%IA(i+1) - 1
              iadj(i+1) = iadj(i) + i1 - i0 + 1
              DO j = i0, i1
                ii = ii + 1
                jadj(ii) = JAC%JA(j)
              END DO
            END DO
            CALL GSOL_GENRCM( NSOLRG, JAC%NNZ-NSOLRG, iadj, jadj, ircm )
            CALL GSOL_PERM_INVERSE3( isolrg, ircm, ircm_inv )
C             CALCULATE BANDWIDTH OF THE RCM REORDERED NODES            
            nbw = GSOL_ADJ_PERM_BANDWIDTH( NSOLRG, JAC%NNZ-NSOLRG, 
     2                                     iadj, jadj, ircm, ircm_inv )
            WRITE (IOUT,2000) JAC%NBW, nbw

2000  FORMAT(//,1X,'REVERSE CUTHILL MCKEE REORDERING INFORMATION',
     2        /,1X,'RAW NODE NUMBERING BANDWIDTH:',1X,I10,
     3        /,1X,'RCM REORDERING BANDWIDTH    :',1X,I10,/)
2010  FORMAT(1X,A15,1X,6(I6,1X))
2020  FORMAT(1X,15('-'),1X,6(6('-'),1X))
2030  FORMAT(1X,57('-'),/)
C
C-------------TEST IF RCM REORDERING SHOULD BE USED
            iusercm = 0
            IF (   IRCMRORD.GT.0 .OR. 
     2           ( IRCMRORD.LT.0 .AND. nbw.LT.JAC%NBW ) ) iusercm = 1
C
C-------------FILL TEMPORARY ARRAYS WITH CURRENT VALUES AND
C           RESET IA, JA, ISMAP, AND ICMAP
            RESETIAJA: IF ( iusercm.NE.0 ) THEN
C
C---------------WRITE SUMMARY OF REVERSE CUTHILL MCKEE REORDERING RESULTS TO LST FILE
              IF ( IPTFLG.GT.0 ) THEN
                DO i = 1, NSOLRG, 6
                  WRITE (IOUT,2010) 'REACH GROUP   :',
     2                              (JAC%ISMAP(j),j=i,MIN(i+5,NSOLRG))
                  WRITE (IOUT,2020)
                  WRITE (IOUT,2010) 'ORIGINAL NODE :',
     2                              (inode(j),j=i,MIN(i+5,NSOLRG))
                  WRITE (IOUT,2010) 'RCM MAPPING   :',
     2                              (ircm(j),j=i,MIN(i+5,NSOLRG))
                  WRITE (IOUT,2010) 'RCM NODE      :',
     2                              (ircm_inv(j),j=i,MIN(i+5,NSOLRG))
                  WRITE (IOUT,2030)
                END DO
              END IF
C
C---------------RESET BANDWIDTH AND HALF BANDWIDTH
              JAC%NBW = nbw
              JAC%NHALFB = ( JAC%NBW - 1 ) / 2
              ALLOCATE ( iat(NSOLRG+1), jat(JAC%NNZ) )
              ALLOCATE ( ismapt(NSOLRG), icmapt(NRCHGRP) )
              DO i = 1, NSOLRG+1
                iat(i)    = JAC%IA(i)
                JAC%IA(i) = 0
              END DO
              JAC%IA(1) = 1
              DO j = 1, JAC%NNZ
                jat(j)    = JAC%JA(j)
                JAC%JA(j) = 0
              END DO
              DO i = 1, NSOLRG
                ismapt(i)    = JAC%ISMAP(i)
                JAC%ISMAP(i) = 0
                JAC%IU(i)    = 0
              END DO
              DO i = 1, NRCHGRP
                icmapt(i)    = JAC%ICMAP(i)
                JAC%ICMAP(i) = 0
              END DO
C
C---------------RESET SOLUTION VECTORS
              jj = 0
              DO n = 1, NSOLRG
                nn   = ircm(n)
                i0   = iat(nn)
                i1   = iat(nn+1) - 1
                ilen = i1 - i0 + 1
                JAC%IA(n+1) = JAC%IA(n) + ilen
                jj = jj + 1
                JAC%JA(jj) = n
                ALLOCATE ( dcolptr(ilen - 1) )
                DO i = 1, ilen - 1
                  dcolptr(i) = DZERO
                END DO
                ii = 0
                DO i = i0+1, i1
                  ii   = ii + 1
                  icol = jat(i)
                  dcolptr(ii) = REAL( ircm_inv(icol), 8 )
                END DO
                CALL SSWR_SORT( dcolptr )
                DO i = 1, ilen - 1
                  IF ( dcolptr(i).EQ.DZERO ) CYCLE
                  jj = jj + 1
                  JAC%JA(jj) = INT( dcolptr(i) )
                  IF ( JAC%JA(jj).GT.n .AND. JAC%IU(n).EQ.IZERO ) THEN
                    JAC%IU(n) = jj
                  END IF
                END DO
C                 TEST FOR LOWER RIGHT OF MATRIX WHERE NO UPPER PRESENT
C                 SET JAC%IU TO THE LAST ENTRY
                IF ( JAC%IU(n).EQ.IZERO ) JAC%IU(n) = JAC%IA(n+1) - 1
                DEALLOCATE ( dcolptr )
              END DO
              ii = 0
              DO n = 1, NRCHGRP
                IF ( RCHGRP(n)%IRGBND.GT.0 ) THEN
                  ii  = ii + 1
                  ist = ismapt(ircm(ii))
                  ict = ircm_inv(ii)
                  JAC%ISMAP(ii) = ist
                  JAC%ICMAP(n)  = ict
                END IF
              END DO
C
C-------------DEALLOCATE TEMPORARY STORAGE FOR RESETING IA, JA, ISMAP, AND ICMAP
C             BASED ON RESULTS OF REVERSE CUTHILL MCKEE REORDERING
              DEALLOCATE ( iat, jat )
              DEALLOCATE ( ismapt, icmapt )
            END IF RESETIAJA
C
C-------------DEALLOCATE TEMPORARY STORAGE FOR REVERSE CUTHILL MCKEE REORDERING
            DEALLOCATE ( inode, ircm, ircm_inv )
            DEALLOCATE ( iadj, jadj )
          END IF RCMORDL
C
C           ALLOCATE MEMORY AS NEEDED FOR SOLUTION VECTORS/MATRICES
          ALLOCATE(JAC%FSCALE(isolrg))
          ALLOCATE(JAC%FSCALE2(isolrg))
          DO n = 1, isolrg
            JAC%FSCALE(n)  = DONE
            JAC%FSCALE2(n) = DONE
          END DO
          ALLOCATE(JAC%JSKM1(isolrg))
          IF (ISOLVER.EQ.1) THEN
            IF ( (JAC%NBW * 2).GT.NSOLRG ) THEN
              ALLOCATE(JAC%FJACC(innz))
              ALLOCATE(JAC%FJACU(isolrg,isolrg))
              DO n = 1, innz
                JAC%FJACC(n)  = DZERO
              END DO
              DO n = 1, isolrg
                DO nn = 1, isolrg
                  JAC%FJACU(n,nn)  = DZERO
                END DO
              END DO
              JAC%IBAND  = 0
            ELSE
              ALLOCATE(JAC%FJACC(innz))
              ALLOCATE(JAC%FJACU(isolrg,JAC%NBW))
              ALLOCATE(JAC%FJACL(isolrg,JAC%NHALFB))
              DO n = 1, innz
                JAC%FJACC(n)  = DZERO
              END DO
              DO n = 1, isolrg
                DO nn = 1, JAC%NBW
                  JAC%FJACU(n,nn)  = DZERO
                END DO
                DO nn = 1, JAC%NHALFB
                  JAC%FJACL(n,nn)  = DZERO
                END DO
              END DO
              JAC%IBAND  = 1
            END IF
          ELSE
            ALLOCATE(JAC%FJACC(innz))
C             INITIALIZE            
            DO n = 1, innz
              JAC%FJACC(n) = DZERO
            END DO
          END IF
C
C-----------DETERMINE STORAGE NEEDS FOR ITERATIVE SOLVE PRECONDITIONERS
          IF ( ISOLVER.GT.1 ) THEN
            ijlu      = 1
            ijw       = 1
            iwlu      = 1
            SELECT CASE ( IPC )
              CASE ( 0 )
                JAC%NNZLU = 1
              CASE ( 1 )
                JAC%NRLU  = 1
                JAC%NNZLU = isolrg
              CASE ( 2,3 )
                JAC%NRLU  = 1
                JAC%NNZLU = innz
              CASE ( 4 )
                JAC%NRLU  = isolrg
                iwk       = isolrg + innz * 3 + 1
                CALL ilutsize(isolrg,JAC%NNZ,JAC%JA,JAC%IA,
     2                        NLEVELS,iwk)
                JAC%NNZLU = iwk
                ijlu      = JAC%NNZLU
                ijw       = 2*JAC%NRLU
                iwlu      = JAC%NRLU+1
            END SELECT
            JAC%NJLU = ijlu
            JAC%NJW  = ijw
            JAC%NWLU = iwlu
            ALLOCATE(JAC%JLU(ijlu))
            ALLOCATE(JAC%JW(ijw))
            ALLOCATE(JAC%WLU(iwlu))
            ALLOCATE(JAC%MJACC(JAC%NNZLU))
            DO n = 1, ijlu
              JAC%JLU(n)   = DZERO
            END DO
            DO n = 1, ijw
              JAC%JW(n)   = DZERO
            END DO
            DO n = 1, iwlu
              JAC%WLU(n)  = DZERO
            END DO
            DO n = 1, JAC%NNZLU
              JAC%MJACC(n) = DZERO
            END DO
          END IF
C
C-----------ALLOCATE TEMPORARY SOLVER STORAGE
          ALLOCATE (JAC%JPSAV(NRCHGRP),JAC%JPPH(NRCHGRP))
          ALLOCATE (JAC%JH(NRCHGRP),JAC%JV(NRCHGRP))
          ALLOCATE (JAC%PS(NRCHGRP),JAC%PS0(NRCHGRP))
          ALLOCATE (JAC%PINF(NRCHGRP),JAC%PINF0(NRCHGRP))
          ALLOCATE (JAC%S(NREACHES),JAC%S0(NREACHES))
          ALLOCATE (JAC%R(NRCHGRP),JAC%XI(NRCHGRP))
          ALLOCATE (JAC%XS(isolrg),JAC%X0(isolrg))
          ALLOCATE (JAC%DX(isolrg),JAC%XSI(isolrg))
          ALLOCATE (JAC%F(isolrg),JAC%F0(isolrg))
          SELECT CASE ( ISOLVER )
            CASE (1)
              ALLOCATE (JAC%PINDEXLU(isolrg),JAC%SLU(isolrg))
            CASE (2)
              ALLOCATE (JAC%DBCGS(isolrg),JAC%DHATBCGS(isolrg))
              ALLOCATE (JAC%PBCGS(isolrg),JAC%PHATBCGS(isolrg))
              ALLOCATE (JAC%SBCGS(isolrg),JAC%SHATBCGS(isolrg))
              ALLOCATE (JAC%VBCGS(isolrg),JAC%TBCGS(isolrg))
            CASE (3)
              ALLOCATE (JAC%RGMR(isolrg),JAC%ZGMR(isolrg))
              ALLOCATE (JAC%DGMR(isolrg),JAC%TGMR(isolrg))
              ALLOCATE (JAC%CSGMR(NINNER),JAC%SNGMR(NINNER))
              ALLOCATE (JAC%SGMR(NINNER+1),JAC%YGMR(NINNER+1))
              ALLOCATE (JAC%HGMR(NINNER+1,NINNER))
              ALLOCATE (JAC%VGMR(isolrg,NINNER+1))
          END SELECT
C
C-----------CLEAN UP TEMPORARY STORAGE
          DEALLOCATE(icon)
        END IF MODBND
C
C---------PROCESS STRUCTURES FOR EACH STRESS PERIOD
C         SET REACHES WITH STRUCTURES TO NEGATIVE REACH NUMBERS
        DO irch = 1, NREACHES
          DO ii = 1, REACH(irch)%NCONN
            REACH(irch)%IRGCONN(ii)  = ABS( REACH(irch)%IRGCONN(ii) )
            REACH(irch)%ISTRCONN(ii) = 0
          END DO
        END DO
        DO irch = 1, NREACHES
          IF ( REACH(irch)%NSTRUCT.EQ.0 ) CYCLE
          DO istr = 1, REACH(irch)%NSTRUCT
            istrconn = REACH(irch)%STRUCT(istr)%ISTRCONN
            DO ii = 1, REACH(irch)%NCONN
              irchconn = REACH(irch)%ICONN(ii)
              IF ( istrconn.EQ.irchconn ) THEN
                REACH(irch)%IRGCONN(ii)  = 
     2            -ABS( REACH(irch)%IRGCONN(ii) )
                REACH(irch)%ISTRCONN(ii) = irch
                DO jj = 1, REACH(irchconn)%NCONN
                  jrchconn = REACH(irchconn)%ICONN(jj)
                  IF ( jrchconn.EQ.irch ) THEN
                    REACH(irchconn)%IRGCONN(jj) = 
     2                -ABS( REACH(irchconn)%IRGCONN(jj) )
                    REACH(irchconn)%ISTRCONN(jj) = irch 
                  END IF
                END DO
              END IF
            END DO
          END DO
        END DO

        DO n = 1, NRCHGRP
          IF ( RCHGRP(n)%NCONN.GT.IZERO ) THEN
            RCHGRP(n)%IRCHC(:) = ABS( RCHGRP(n)%IRCHC(:) )
            RCHGRP(n)%ISTRRCH(:) = IZERO
          END IF
        END DO
        DO n = 1, NRCHGRP
          DO i = 1, RCHGRP(n)%NCONN  
            irch     = ABS( RCHGRP(n)%IRCHN(i) )
            irchconn = ABS( RCHGRP(n)%IRCHC(i) )
C             FIND CONNECTIONS THAT ARE STRUCTURE CONTROLLED
            IF (REACH(irch)%NSTRUCT.GT.0) THEN
              irchconnrg = REACH(irchconn)%IRG
              DO ii = 1, REACH(irch)%NSTRUCT
                istrconn = REACH(irch)%STRUCT(ii)%ISTRCONN
                IF ( istrconn.EQ.irchconn ) THEN
                  RCHGRP(n)%IRCHN(i)   = -irch
                  RCHGRP(n)%IRCHC(i)   = -irchconn
                  RCHGRP(n)%ISTRRCH(i) = irch
C                     SET SYMMETRIC CONNECTION FOR STRUCTURE
                    DO nn = 1, RCHGRP(irchconnrg)%NCONN
                      irch2     = ABS( RCHGRP(irchconnrg)%IRCHN(nn) )
                      irchconn2 = ABS( RCHGRP(irchconnrg)%IRCHC(nn) )
                      IF ( irchconn.EQ.irch2 .AND. 
     2                     irch.EQ.irchconn2 ) THEN
                        RCHGRP(irchconnrg)%IRCHN(nn)   = -irch2
                        RCHGRP(irchconnrg)%IRCHC(nn)   = -irchconn2
                        RCHGRP(irchconnrg)%ISTRRCH(nn) = +irch
                        EXIT
                      END IF
                    END DO
                END IF
              END DO
            END IF
          END DO
        END DO
C
C         SET UP SIMPLE CONNECTION INFORMATION FOR
C         UNCONTROLLED CONNECTIONS
        DO n = 1, NRCHGRP
          iconn = RCHGRP(n)%NCONN
          IF ( iconn.LT.1 ) CYCLE 
C-----------ALLOCATE AND INITIALIZE ESTIMATED UNMANAGED FLOWS 
          IF ( JAC%IALLO.LT.1 ) THEN          
            ALLOCATE ( RCHGRP(n)%IN(iconn), 
     2                 RCHGRP(n)%IR(iconn), 
     3                 RCHGRP(n)%ILOC(iconn) ) 
          END IF
          RCHGRP(n)%IN   = IZERO 
          RCHGRP(n)%IR   = IZERO 
          RCHGRP(n)%ILOC = IZERO
C-----------FILL RCHGRP(n)%ir                                                       
          ic = 1 
          IF ( RCHGRP(n)%NCONN.GT.IZERO ) THEN 
            DO nc = 1, RCHGRP(n)%NCONN 
              RCHGRP(n)%IR(ic)   = ABS( RCHGRP(n)%IRCHN(nc) )
              RCHGRP(n)%ILOC(ic) = 0 
              RCHGRP(n)%IN(ic)   = ABS( RCHGRP(n)%IRCHC(nc) ) 
              ic = ic + 1 
            END DO 
          END IF 
        END DO
C
C---------SET FLAG INDICATING JAC HAS BEEN ALLOCATED
        JAC%IALLO = 1
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_SET_CONN
C
C-------CALCULATE REACH OFFSETS FROM STAGE DATA READ FOR 
C       STRESS PERIOD (INPUT ITEM 14)
      SUBROUTINE SSWR_SET_RCHOFF(Istage,Dstage)
        USE GWFSWRMODULE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, DIMENSION(IRDSTG) :: Istage
        DOUBLEPRECISION, DIMENSION(IRDSTG) :: Dstage
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, n
        INTEGER :: irch
        INTEGER :: irg
        INTEGER :: ismin
        INTEGER :: icalcrch
        DOUBLEPRECISION :: smin
        DOUBLEPRECISION :: s0
        DOUBLEPRECISION, DIMENSION(NREACHES) :: s
C       + + + CODE + + +
C---------INITIALIZE OFFSET
        REACH(:)%OFFSET = DZERO
C---------CALCULATE TEMPORARY STAGE FOR CALCULATIONS
C         CURRENT STAGES
        DO irch = 1, NREACHES
          s(irch) = REACH(irch)%STAGE
        END DO
C         FILL WITH UPDATED STAGES
        DO i = 1, IRDSTG
          IF (Istage(i).GT.IZERO) s(Istage(i)) = Dstage(i)
        END DO
C---------RESET OFFSET
        DO n = 1, NRCHGRP
          irg = RCHGRP(n)%IRG
          ismin = 0
          smin = MAXVAL(s) + DONE
C           FIND LOCATION AND VALUE OF MINIMUM STAGE IN REACH GROUP
          DO i = 1, RCHGRP(n)%NRGREACH
            irch = RCHGRP(n)%REACH(i)
            IF (REACH(irch)%CROUTETYPE.EQ.'TP') THEN
              IF (s(i).LE.smin) THEN
                ismin = irch
                smin  = s(i)
              END IF
            ELSE
              ismin = irch
            END IF
          END DO
C           SET LOCATION OF MINIMUM STAGE IN REACH GROUP AND
C             REACH OFFSET
          RCHGRP(n)%ICALCRCH = ismin
          DO i = 1, NREACHES
            IF (REACH(i)%CROUTETYPE.EQ.'TP' .AND. 
     2          REACH(i)%IRG.EQ.irg) THEN
              REACH(i)%OFFSET = s(i) - smin
            END IF
          END DO
        END DO
C
C---------SET OFFSET FOR CONSTANT STAGE REACHES
C         BASED ON STAGE
        DO n = 1, NRCHGRP
          IF ( RCHGRP(n)%CONSTANT ) THEN
            irg = RCHGRP(n)%IRG
            icalcrch = RCHGRP(n)%ICALCRCH
            s0 = REACH(icalcrch)%STAGE
            RCHGRP(n)%STAGE = s0
          END IF
        END DO
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_SET_RCHOFF
C
C-------CALCULATE STAGE-VOLUME-WETTED PERIMETER-SURFACE AREA-CROSS-SECTIONAL AREA 
C       RELATION FOR REACH      
      SUBROUTINE SSWR_CALC_RCHGEODATA(Irch,Igeotype,Igcndop,Ngeopts,
     2        Gmanning,Gwidth,Gbelev,Gsslope,
     3        Gcnd,Glk,Gcndln,Getextd,Geostor)
        USE GWFSWRMODULE
        USE GWFSWRINTERFACE
        USE GLOBAL,       ONLY: IOUT, DELR, DELC, BOTM
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Irch
        INTEGER, INTENT(IN) :: Igeotype, Igcndop, Ngeopts
        DOUBLEPRECISION, INTENT(IN) :: Gmanning,Gwidth,Gbelev,Gsslope
        DOUBLEPRECISION, INTENT(IN) :: Gcnd
        DOUBLEPRECISION, INTENT(IN) :: Glk,Gcndln
        DOUBLEPRECISION, INTENT(IN) :: Getextd
        DOUBLEPRECISION, INTENT(IN), DIMENSION(ngeopts,5) :: Geostor
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j
        INTEGER :: ir, jc, nlen
        INTEGER :: ipos
        DOUBLEPRECISION  :: zg, gzshift
        DOUBLEPRECISION  :: gtelev
        DOUBLEPRECISION  :: width, length, bottom, top, side
        DOUBLEPRECISION  :: dh, dx, dy, dydx, dz
        DOUBLEPRECISION  :: ar, a0, v0, wp0, tw0, sa0, z1, z2, wpc
        DOUBLEPRECISION  :: ac, am
        DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE  :: uz, a, wp, tw, sa
C     + + + INTERFACE + + +
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C         FILL REACH WITH GENERIC IRREGULAR CROSS-SECTION DATA
      REACH(irch)%IGEOTYPE    = Igeotype
      REACH(irch)%IGCNDOP     = Igcndop
      REACH(irch)%NGEOPTS     = Ngeopts
      REACH(irch)%GMANNING    = Gmanning
      REACH(irch)%GWIDTH      = Gwidth
      REACH(irch)%GBELEV      = Gbelev
      REACH(irch)%GSSLOPE     = Gsslope
      REACH(irch)%GCND        = Gcnd
      REACH(irch)%GLK         = Glk
      REACH(irch)%GCNDLN      = Gcndln
C
C       SET GETEXTD FOR REACHES COVERING ENTIRE CELLS
      SELECT CASE (Igeotype)
        CASE (5)
          REACH(irch)%GETEXTD = Getextd
        CASE DEFAULT
          REACH(irch)%GETEXTD = DZERO
      END SELECT
C
C       SET FLAG FOR CALCULATION OF QAQ TERMS     
      SELECT CASE (Igcndop)
        CASE (0)
          IF ( Gcnd.GT.DZERO )  REACH(irch)%ICALCBFLOW = 1
        CASE (1)
          IF ( Glk.GT.DZERO )   REACH(irch)%ICALCBFLOW = 1
        CASE (2,3)
          REACH(irch)%ICALCBFLOW = 1
      END SELECT
      IF (igeotype.EQ.3) THEN
        IF (REACH(irch)%IRRGEO%NGEOPTS.GT.0) THEN
          DEALLOCATE(REACH(irch)%IRRGEO%ELEVB,REACH(irch)%IRRGEO%XB)
        END IF
        REACH(irch)%IRRGEO%NGEOPTS = Ngeopts
        ALLOCATE(REACH(irch)%IRRGEO%ELEVB(Ngeopts),
     2           REACH(irch)%IRRGEO%XB(Ngeopts))
        REACH(irch)%IRRGEO%XB(:)    = Geostor(:,1)
        gzshift = REACH(irch)%GZSHIFT !+ REACH(irch)%GCUMSUB
        REACH(irch)%IRRGEO%ELEVB(:) = Geostor(:,2) + gzshift
      END IF
C      
C         CLEAN UP LEVEL GROUP RATING CURVE DATA
      IF (REACH(irch)%GEO%NGEOPTS.GT.0) THEN
        DEALLOCATE (REACH(irch)%GEO%ELEV,  
     2              REACH(irch)%GEO%XAREA, REACH(irch)%GEO%VOL, 
     3              REACH(irch)%GEO%WETPER,
     4              REACH(irch)%GEO%TOPWID,
     5              REACH(irch)%GEO%SAREA) 
        REACH(irch)%GEO%NGEOPTS = IZERO
      END IF
C
C       REALLOCATE AND FILL REACH GEOMETRY DATA
      nlen    = 0
      ir      = REACH(irch)%IRCH
      jc      = REACH(irch)%JRCH
C       GET SURFACE ELEVATION
      zg      = REAL(BOTM(jc,ir,0),8)
      gtelev  = zg
C       SET SHIFT FOR REACH
      gzshift = REACH(irch)%GZSHIFT !+ REACH(irch)%GCUMSUB
C       CALCULATE CELL AREA
      ac      = DELR(jc) * DELC(ir)
C
C       DETERMINE GWIDTH AND DLEN FOR REACH
      length = REACH(irch)%DLEN
      IF (Igeotype.EQ.1 .OR. Igeotype.EQ.2) THEN
        width = Gwidth
        length = REACH(irch)%DLEN
        bottom = Gbelev + gzshift
        IF (zg.LT.bottom) zg = bottom + 5.0D0
        REACH(irch)%GBELEV = bottom
        REACH(irch)%GTELEV = gtelev
      ELSE IF (Igeotype.EQ.3) THEN
        bottom = MINVAL(Geostor(:,2)) + gzshift
        top    = MIN(gtelev,MAXVAL(Geostor(:,1) + gzshift))
        REACH(irch)%GBELEV = bottom
        REACH(irch)%GTELEV = top
      ELSE IF (Igeotype.EQ.4) THEN
        REACH(irch)%DLEN = DONE
        length = REACH(irch)%DLEN
        bottom = geostor(1,1) + gzshift
        top    = MIN(gtelev,MAXVAL(Geostor(:,1) + gzshift))
        REACH(irch)%GBELEV = bottom
        REACH(irch)%GTELEV = top
      ELSE IF (Igeotype.EQ.5) THEN
        width  = DELR(jc)
        length = DELC(ir)
        bottom = zg
        zg = zg + 5.0D0
C         RESET REACH DLEN TO DELC FOR REACHES COVERING ENTIRE CELLS
        REACH(irch)%DLEN   = length
        REACH(irch)%GWIDTH = width
        REACH(irch)%GBELEV = bottom
        REACH(irch)%GTELEV = gtelev
      END IF
C
C       REALLOCATE
      SELECT CASE (Igeotype)
        CASE (1,2,5)
          nlen = 5 + 9
          dz = DZERO
          IF ( zg.GT.bottom ) THEN
            dz = (zg - bottom + 1.0D-6) / 10.0D0 
          END IF
          ALLOCATE (uz(nlen))
          uz(1) = bottom
          uz(2) = bottom + 1.0D-6
          uz(3) = zg
          uz(4) = zg + 1.0D-6
          uz(5) = uz(3) + 10.0D0
          uz(6) = uz(2) + dz
          DO ipos = 7, (5+9)
            uz(ipos) = uz(ipos-1) + dz
          END DO
          CALL SSWR_UNIQVALUES(uz)
          nlen = SIZE(uz)
        CASE (3)
C           DETERMINE THE NUMBER OF UNIQUE POINTS IN CROSS-SECTION ELEVATIONS
C             ADD TOPOGRAPHY AND MAXIMUM ELEVATION + 10
          nlen = ngeopts + 4 + 9
          ALLOCATE (uz(nlen))
          uz(1:Ngeopts) = REACH(irch)%IRRGEO%ELEVB
          uz(Ngeopts+1) = 
     2      MINVAL(uz(1:Ngeopts)) + 1.0D-6
          uz(Ngeopts+2) = zg
          uz(Ngeopts+3) = zg + 1.0D-6
          uz(Ngeopts+4) = 
     2     MAXVAL(uz(1:Ngeopts+3)) + 10.0D0
          dz = (MAXVAL(uz(1:Ngeopts)) - 
     2       MINVAL(uz(1:Ngeopts)) + 1.0D-6) / 10.0D0
          uz(Ngeopts+5) = 
     2       uz(Ngeopts+1) + dz
          DO ipos = (Ngeopts+6), 
     2              (Ngeopts + 4 + 9)
            uz(ipos) = uz(ipos-1) + dz
          END DO
          CALL SSWR_UNIQVALUES(uz)
          nlen = SIZE(uz)
        CASE (4)
          nlen = Ngeopts
        END SELECT
        REACH(irch)%GEO%NGEOPTS = nlen
        ALLOCATE (REACH(irch)%GEO%ELEV(nlen), 
     2            REACH(irch)%GEO%XAREA(nlen), 
     3            REACH(irch)%GEO%VOL(nlen), 
     4            REACH(irch)%GEO%WETPER(nlen), 
     5            REACH(irch)%GEO%TOPWID(nlen),
     6            REACH(irch)%GEO%SAREA(nlen)) 
C
C         FILL ELEV AND CALCULATE XAREA, VOLUME, WETTED PERIMETER, AND TOP WIDTH
        SELECT CASE (Igeotype)
          CASE (1,2)
C             ELEVATIONS
            REACH(irch)%GEO%ELEV      = uz
C             DATA FOR BOTTOM ELEVATION
            REACH(irch)%GEO%WETPER(1) = DZERO  
            REACH(irch)%GEO%TOPWID(1) = DZERO
            REACH(irch)%GEO%XAREA(1)  = DZERO
            REACH(irch)%GEO%VOL(1)    = DZERO
            REACH(irch)%GEO%SAREA(1)  = DZERO
            DO ipos = 2, nlen
              dx = DZERO
              dz = REACH(irch)%GEO%ELEV(ipos) - 
     2             REACH(irch)%GEO%ELEV(ipos-1)
              side = DZERO
              IF (Gsslope.GT.DZERO) THEN
                dx = dz / Gsslope
              END IF
              wpc = DONE
              IF (Igeotype.EQ.5.OR.
     2            REACH(irch)%GEO%ELEV(ipos).GT.zg) THEN
                wpc = DZERO
              END IF
              side = SQRT(dx*dx + dz*dz)
              a0  = REACH(irch)%GEO%XAREA(ipos-1)
              v0  = REACH(irch)%GEO%VOL(ipos-1)
              wp0 = MAX(REACH(irch)%GEO%WETPER(ipos-1),width)
              tw0 = MAX(REACH(irch)%GEO%TOPWID(ipos-1),width)
              sa0 = MAX(REACH(irch)%GEO%SAREA(ipos-1),width*length)
              ar = (tw0 + wpc * dx) * dz
              REACH(irch)%GEO%XAREA(ipos) = a0 + ar
              REACH(irch)%GEO%VOL(ipos) = (a0 + ar) * length
              REACH(irch)%GEO%WETPER(ipos) = 
     2                                      wp0 + DTWO * wpc * side
              REACH(irch)%GEO%TOPWID(ipos) = tw0 + DTWO * wpc * dx
              REACH(irch)%GEO%SAREA(ipos) = 
     2            sa0 + DTWO * wpc * dx * length
            END DO
C           IRREGULAR CROSS-SECTION
          CASE (3)
            REACH(irch)%GEO%ELEV   = uz
            REACH(irch)%GEO%XAREA  = DZERO
            REACH(irch)%GEO%VOL    = DZERO
            REACH(irch)%GEO%WETPER = DZERO
            REACH(irch)%GEO%TOPWID = DZERO
            REACH(irch)%GEO%SAREA  = DZERO
            ALLOCATE(a(nlen),wp(nlen),tw(nlen),sa(nlen))
            DO j = 2, Ngeopts      
              dy   = ABS(REACH(irch)%IRRGEO%ELEVB(j) - 
     2                   REACH(irch)%IRRGEO%ELEVB(j-1))
              dx   = (REACH(irch)%IRRGEO%XB(j) - 
     2                REACH(irch)%IRRGEO%XB(j-1))
              dydx = DZERO
              IF (ABS(dx).GT.DZERO) dydx = dy / dx
C               INITIALIZE XAREA, PERIMETER, AND TOP_WIDTH 
              a  = DZERO
              wp = DZERO
              tw = DZERO
C             EVALUATE XAREA BELOW EACH UNIQUE DEPTH
              DO i = 1, nlen
                wpc = DONE
                z2 = uz(i)
                z1 = z2
                IF (i.GT.1) z1 = uz(i-1) 
                IF (z1.GT.zg .AND. z2.GT.zg) wpc = DZERO
                IF (j == 2) THEN
                  wpc = wpc * DONE
                END IF
                dh  = DZERO
                a0  = DZERO
                wp0 = DZERO
                tw0 = DZERO
                sa0 = DZERO
                IF (i.GT.1) THEN
                  dh  = uz(i) - uz(i-1)
                  a0  =  a(i-1)
                  wp0 = wp(i-1)
                  tw0 = tw(i-1)
                  sa0 = sa(i-1)
                END IF
C                 DEPTHS BELOW BOTH POINTS
                IF (uz(i).LE.REACH(irch)%IRRGEO%ELEVB(j) .AND. 
     2              uz(i).LE.REACH(irch)%IRRGEO%ELEVB(j-1)) THEN
                  tw(i) = DZERO
                  wp(i) = DZERO
                  a(i)  = DZERO
                  sa(i) = DZERO
C                 DEPTHS ABOVE ONE POINT BUT BELOW THE OTHER POINT
C                   COMBINATION OF RECTANGULAR AND TRIANGULAR AREAS
                ELSE IF (uz(i).LE.REACH(irch)%IRRGEO%ELEVB(j) .OR. 
     2                   uz(i).LE.REACH(irch)%IRRGEO%ELEVB(j-1)) THEN
                  dx = REACH(irch)%IRRGEO%XB(j) - 
     2                 REACH(irch)%IRRGEO%XB(j-1)
                  IF (ABS(dydx).GT.DZERO) dx = dh / dydx
C                   LAST AREA +     RECT. AREA      + TRI. AREA
                  tw(i) = tw0 + dx
                  sa(i) = tw(i) * length
                  wp(i) = wp0 + wpc * SQRT(dx*dx + dh*dh)
                  a(i)  =  a0 + tw0 * dh + dx * dh / DTWO
C                 DEPTHS ABOVE BOTH POINTS - ALL RECTANGULAR AREAS
                ELSE
                  dx = REACH(irch)%IRRGEO%XB(j) - 
     2                 REACH(irch)%IRRGEO%XB(j-1)
C                   LAST AREA + RECT. AREA
                  a(i)  =  a0 + dx * dh
                  IF (wpc.NE.DZERO) THEN
                    wp(i) = SQRT(dx*dx + dy*dy)
                  ELSE
                    wp(i) = wp0
                  END IF
                  tw(i) = dx
                  sa(i) = dx * length
                END IF
              END DO
C               ACCUMULATE GEOMETRY TERMS IN CROSS-SECTION GEOMETRY STRUCTURE
C                 WITHIN CHANNEL
              REACH(irch)%GEO%XAREA(:)  = 
     2          REACH(irch)%GEO%XAREA(:)  + a(:)
              REACH(irch)%GEO%VOL(:)    = 
     2          REACH(irch)%GEO%VOL(:)    + a(:) * length
              REACH(irch)%GEO%WETPER(:) = 
     2          REACH(irch)%GEO%WETPER(:) + wp(:)
              REACH(irch)%GEO%TOPWID(:) = 
     2          REACH(irch)%GEO%TOPWID(:) + tw(:)
              REACH(irch)%GEO%SAREA(:)  = 
     2          REACH(irch)%GEO%SAREA(:)  + sa(:)
            END DO
            uz = uz        
          CASE (4)
            REACH(irch)%GEO%ELEV   = Geostor(:,1) + gzshift
            REACH(irch)%GEO%VOL    = Geostor(:,2)
            REACH(irch)%GEO%WETPER = Geostor(:,3)
            REACH(irch)%GEO%TOPWID = DZERO
            REACH(irch)%GEO%SAREA  = Geostor(:,4)
            REACH(irch)%GEO%XAREA  = Geostor(:,5)
          CASE (5)
C             ELEVATIONS
            REACH(irch)%GEO%ELEV      = uz
C             DATA FOR BOTTOM ELEVATION
            REACH(irch)%GEO%WETPER    = width  
            REACH(irch)%GEO%TOPWID    = width
            REACH(irch)%GEO%XAREA     = DZERO
            REACH(irch)%GEO%VOL       = DZERO
            REACH(irch)%GEO%SAREA     = width * length
            DO ipos = 2, nlen
              z1 = uz(ipos) - bottom
              REACH(irch)%GEO%XAREA(ipos) = width * z1
              REACH(irch)%GEO%VOL(ipos)   = length * width * z1
            END DO
        END SELECT
C
C-----------SET CONSTANT RAINFALL AREA FOR REACH
          am = DZERO
          DO ipos = 2, nlen
            IF ( REACH(irch)%GEO%SAREA(ipos).GT.am ) THEN
              am = REACH(irch)%GEO%SAREA(ipos)
            END IF
          END DO
          IF ( am.GT.ac ) am = ac
          REACH(irch)%RAINAREA = am
C
C-----------CLEANUP TEMPORARY STORAGE
        IF (ALLOCATED(uz)) DEALLOCATE (uz)
        IF (ALLOCATED(a))  DEALLOCATE (a,wp,tw,sa)
C
C---------RETURN        
        RETURN
      END SUBROUTINE SSWR_CALC_RCHGEODATA

      SUBROUTINE SSWR_SET_RCHLAY(irch)         
        USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,BOTM,LBOTM
        USE GWFSWRMODULE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: irch
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: k, kk
        INTEGER :: ir, jc
        INTEGER :: ktop, kbot
        DOUBLEPRECISION :: e
        DOUBLEPRECISION :: rtop, rbot
        DOUBLEPRECISION :: zgtop, zgbot
C     + + + INTERFACE + + +
C     + + + FUNCTIONS + + +
C     + + + DATA + + +
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
02000 FORMAT(//1X,'KRCH MUST BE SET TO 1 FOR REACH',1X,I5,
     &         1X,'WHICH IS SPECIFIED WITH IGEOTYPE =',1X,I5,
     &        /1X,'TERMINATING SIMULATION.')
C     + + + CODE + + +
      e = EPSILON(DZERO)
      IF (REACH(irch)%KRCH.GT.0) THEN
        REACH(irch)%LAYSTR   = REACH(irch)%KRCH
        REACH(irch)%LAYEND   = REACH(irch)%KRCH
      ELSE IF (REACH(irch)%IGEOTYPE.EQ.5) THEN
        IF ( REACH(irch)%KRCH.LT.1 ) THEN
          WRITE (IOUT,2000) irch, REACH(irch)%IGEOTYPE
          CALL USTOP('KRCH MUST BE SET TO 1 IF IGEOTYPE = 5')
        END IF
      ELSE IF (NLAY.EQ.1) THEN
        REACH(irch)%LAYSTR   = 1
        REACH(irch)%LAYEND   = 1
      ELSE
        ir = REACH(irch)%IRCH
        jc = REACH(irch)%JRCH
        rtop = REACH(irch)%GTELEV
        rbot = REACH(irch)%GBELEV
        DO k = 1, NLAY
          kk = LBOTM(k)
          zgtop = REAL(BOTM(jc,ir,kk-1),8)
          zgbot = REAL(BOTM(jc,ir,kk),8)
          IF (zgtop.GE.rtop .AND. zgbot.LE.rtop) THEN
            ktop = kk
          END IF
          IF (zgtop.GE.rbot .AND. zgbot.LE.rbot) THEN
            kbot = kk
          END IF
        END DO
        REACH(irch)%LAYSTR   = ktop
        REACH(irch)%LAYEND   = kbot
      END IF
C
C---------RETURN        
        RETURN
      END SUBROUTINE SSWR_SET_RCHLAY

      SUBROUTINE SSWR_CALC_RATCURVE()
        USE GWFSWRMODULE
        USE GWFSWRINTERFACE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j
        INTEGER :: n
        INTEGER :: nlen
        INTEGER :: irch
        INTEGER :: ipos
        DOUBLEPRECISION  :: s, v
        DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE  :: uz
C     + + + INTERFACE + + +
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C         CLEAN UP REACH GROUP RATING CURVE DATA
        DO n = 1, NRCHGRP
          IF (RCHGRP(n)%IRGUPDATE.LT.1) CYCLE
          IF (ALLOCATED(RCHGRP(n)%RGELEV)) THEN
            DEALLOCATE (RCHGRP(n)%RGELEV, RCHGRP(n)%RGVOL)
          END IF
        END DO
C
C         REALLOCATE LEVEL GROUP RATING CURVE DATA
        DO n = 1, NRCHGRP
C           SKIP IF RCHGRP NOT UPDATED
          IF (RCHGRP(n)%IRGUPDATE.LT.1) CYCLE
C           CALCULATE NUMBER OF DATA POINTS PER REACH
          nlen = 0
          DO i = 1, RCHGRP(n)%NRGREACH
            irch = RCHGRP(n)%REACH(i)
            nlen = nlen + REACH(irch)%GEO%NGEOPTS
          END DO
          ALLOCATE (uz(nlen))
C           FILL WITH VALUES
          ipos = 0
          DO i = 1, RCHGRP(n)%NRGREACH
            irch = RCHGRP(n)%REACH(i)
            do j = 1, REACH(irch)%GEO%NGEOPTS
              ipos = ipos + 1
              uz(ipos) = REACH(irch)%GEO%ELEV(j)
            END DO
          END DO
          CALL SSWR_UNIQVALUES(uz)
          ALLOCATE(RCHGRP(n)%RGELEV(SIZE(uz)),
     3             RCHGRP(n)%RGVOL(SIZE(uz)))
          RCHGRP(n)%RGELEV(:) = uz(:)
          RCHGRP(n)%RGVOL(:)  = DZERO
          DO i = 1, RCHGRP(n)%NRGREACH
            irch = RCHGRP(n)%REACH(i)
            DO j = 1, SIZE(uz)
              s = uz(j) + REACH(irch)%OFFSET
              v = SSWR_LININT(REACH(irch)%GEO%ELEV,
     2                        REACH(irch)%GEO%VOL,s)
              RCHGRP(n)%RGVOL(j) = RCHGRP(n)%RGVOL(j) + v
            END DO
          END DO
C-----------CLEANUP TEMPORARY STORAGE
          DEALLOCATE (uz)
        END DO
C
C---------RETURN        
        RETURN
      END SUBROUTINE SSWR_CALC_RATCURVE
      
      SUBROUTINE SSWR_VOL2STG_UPDATE()
        USE GLOBAL,      ONLY:IOUT
        USE GWFSWRMODULE
        USE GWFSWRINTERFACE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j
        INTEGER :: irch
        DOUBLEPRECISION :: e
        DOUBLEPRECISION :: s
        DOUBLEPRECISION :: v
        DOUBLEPRECISION :: vm
        DOUBLEPRECISION :: ps
        DOUBLEPRECISION :: pe
        DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE  :: vp
        DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE  :: vpc
        DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE  :: vr
C     + + + INTERFACE + + +
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_RG_STG
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(/1X,'WARNING -- ',
     2  'STAGE ADJUSTMENT AT BEGINNING OF STRESS-PERIOD RESULTING FROM',
     3  /1X,11X,'CALCULATED REACH OFFSET AND INITIAL/PREVIOUS STAGE',
     4   1X,'HAS RESULTED IN',
     5  /1X,11X,'A VOLUME ERROR EXCEEDING 0.1%.',
     6  /1X,11X,'THE STAGE FOR THIS TIME STEP SHOULD BE ADJUSTED!!!',
     7 //1X,'   RCHGRP',1X,'   INIT. VOLUME',
     8   1X,'     NEW VOLUME',1X,'   VOLUME DIFF.',1X,'  PERCENT ERROR',
     9  /1X,74('-'))
2010  FORMAT(1X,I10,4(1X,E15.7))
C     + + + CODE + + +
C---------ALLOCATE LOCAL STORAGE
      ALLOCATE ( vp(NRCHGRP)  )
      ALLOCATE ( vpc(NRCHGRP) )
      ALLOCATE ( vr(NREACHES) )
C---------CALCULATE VOLUME IN EACH REACH AND 
C         IN EACH REACH GROUP
        vr = DZERO
        vp = DZERO
        DO i = 1, NRCHGRP
          IF (RCHGRP(i)%CONSTANT) CYCLE
          IF (RCHGRP(i)%IRGUPDATE.LT.1) CYCLE
          DO j = 1, RCHGRP(i)%NRGREACH
            irch = RCHGRP(i)%REACH(j)
            s = REACH(irch)%STAGE
            vr(irch) = SSWR_LININT(REACH(irch)%GEO%ELEV,
     2                             REACH(irch)%GEO%VOL,s)
            vp(i) = vp(i) + vr(irch)
          END DO
        END DO
C
C---------CALCULATE GROUP STAGE BASED ON CALCULATED VOLUME 
C         IN EACH GROUP (I.E., REACH GROUP)
C         ALSO UPDATE STAGE FOR EACH REACH IN GROUP
        DO i = 1, NRCHGRP
          IF (RCHGRP(i)%CONSTANT) CYCLE
          IF (RCHGRP(i)%IRGUPDATE.LT.1) CYCLE
          ps = SSWR_RG_STG(RCHGRP(i),vp(i))
          DO j = 1, RCHGRP(i)%NRGREACH  
            irch = RCHGRP(i)%REACH(j)
            REACH(irch)%STAGE = ps + REACH(irch)%OFFSET
          END DO
        END DO
C
C---------PERFORM CHECK OF ORIGINAL AND REVISED GROUP VOLUME
C         CALCULATED FROM UPDATED REACH STAGES TO DETERMINE IF
C         WATER IS CREATED OR DESTROYED BECAUSE OF SPECIFIED WSSLOPE.
C         ISSUE WARNING IF CONDITION OCCURS
        vr  = DZERO
        vpc = DZERO
        e   = EPSILON(DZERO)
        DO i = 1, NRCHGRP
          IF (RCHGRP(i)%CONSTANT) CYCLE
          IF (RCHGRP(i)%IRGUPDATE.LT.1) CYCLE
          DO j = 1, RCHGRP(i)%NRGREACH  
            irch = RCHGRP(i)%REACH(j)
            s = REACH(irch)%STAGE
            vr(irch) = SSWR_LININT(REACH(irch)%GEO%ELEV,
     2                             REACH(irch)%GEO%VOL,s)
            vpc(i) = vpc(i) + vr(irch)
          END DO
        END DO
        vm = DZERO
        DO i = 1, NRCHGRP
          IF (RCHGRP(i)%IRGUPDATE.LT.1) CYCLE
          IF ( ABS( vp(i) ).GT.DZERO ) THEN
            v = ABS( (vpc(i)-vp(i)) / vp(i) )
            IF ( v.GT.vm ) vm = v
          END IF
        END DO
        IF ( ( 1.0D+02 * vm ).GT.1.0D-03 ) THEN
          WRITE (IOUT,2000)
          DO i = 1, NRCHGRP
            IF (RCHGRP(i)%IRGUPDATE.LT.1) CYCLE
            pe = 1.0D+02 * (vpc(i)-vp(i)) / vp(i)
            IF ( ABS(pe).LE.1.0D-03 ) CYCLE
            WRITE (IOUT,2010) i, vp(i), vpc(i), vp(i)-vpc(i), pe
          END DO
        END IF
C
C---------DEALLOCATE LOCAL STORAGE
      DEALLOCATE ( vp )
      DEALLOCATE ( vpc )
      DEALLOCATE ( vr )
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_VOL2STG_UPDATE
C
C-------CALCULATE DISTANCE WEIGHTED PARAMETERS FOR REACH GROUP
      SUBROUTINE SSWR_CALCA_UFLOW(Ice,Ps,B,A,P,R)
        USE GWFSWRMODULE
        USE GWFSWRINTERFACE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Ice
        DOUBLEPRECISION, INTENT(IN)    :: Ps
        DOUBLEPRECISION, INTENT(INOUT) :: B
        DOUBLEPRECISION, INTENT(INOUT) :: A
        DOUBLEPRECISION, INTENT(INOUT) :: P
        DOUBLEPRECISION, INTENT(INOUT) :: R
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, n
        DOUBLEPRECISION :: bot, s
        DOUBLEPRECISION :: celength, dlen, fact
        DOUBLEPRECISION :: rb, ra, rp, rr
C     + + + INTERFACE + + +
C     + + + FUNCTIONS + + +
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
C     + + + CODE + + +
        celength = RCHGRP(Ice)%DLEN
        DO i = 1, RCHGRP(Ice)%NRGREACH  
          n    = RCHGRP(Ice)%REACH(i)
          dlen = REACH(n)%DLEN
          fact = dlen / celength
          bot  = REACH(n)%GBELEV
          s    = Ps + REACH(n)%OFFSET
          rb   = MAX(DZERO,s-bot)
          ra   = SSWR_LININT(REACH(n)%GEO%ELEV,REACH(n)%GEO%XAREA,s)
          rp   = SSWR_LININT(REACH(n)%GEO%ELEV,REACH(n)%GEO%WETPER,s)
          rr   = REACH(n)%GMANNING
          B    = B + fact * rb
          A    = A + fact * ra
          P    = P + fact * rp
          R    = R + fact * rr
        END DO
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_CALCA_UFLOW
C
C
C
C-------WRAPPER FOR CONSOLIDATED OUTER ITERATIONS (NON-LINEAR NEWTON STEP)
C       AND ALL LINEAR SOLVERS 
      SUBROUTINE SSWR_GSOLWRP(Iouter,Isoln,X,Check)
        USE GLOBAL,       ONLY: IOUT
        USE GWFSWRMODULE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(INOUT) :: Iouter
        INTEGER, INTENT(INOUT) :: Isoln
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(INOUT) :: X
        LOGICAL, INTENT(INOUT) :: Check
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: icnvg
        INTEGER :: n
        INTEGER :: ots, its
        INTEGER :: ia
        DOUBLEPRECISION :: deltaf, deltafi, deltax
        DOUBLEPRECISION :: fn, fn0
        DOUBLEPRECISION :: fni
        DOUBLEPRECISION :: eta
        DOUBLEPRECISION :: damp
        DOUBLEPRECISION :: e
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: GSOL_L2NORM
C     + + + CODE + + +
        Check  = .FALSE.
        Iouter = 0
        e = SQRT( EPSILON( DZERO ) )
        IF ( NSOLRG.LT.1 ) RETURN
        DO n = 1, NSOLRG
          JAC%DX(n) = DONE
        END DO
C---------SET APPROPRIATE DAMPENING FACTOR
        damp = DAMPTR
        IF ( ISWRSS.GT.0 ) THEN
          damp = ABS( DAMPSS )
        END IF
C---------CALCULATE EXPLICIT INVARIANT QM
        IF ( ICIQM.GT.0 ) THEN
          CALL SSWR_EST_EXPLICIT_SFM(X)
        END IF
C---------CALCULATE RESIDUAL FOR CURRENT VALUE OF X
        CALL SSWR_CALC_RGRESI(X,JAC%R)
        CALL SSWRC2S(JAC%R,JAC%F)
        CALL SSWRC2S(X,JAC%XS)
C---------TEST FOR INITIAL GUESS BEING A SOLUTION
!!        IF ( MAXVAL(ABS(JAC%F(:))).LT.(DONEPERCENT*TOLR) ) RETURN
        IF ( INEXCTNWT.GT.0 ) THEN
          deltaf  = DZERO
          DO n = 1, NSOLRG
            IF ( ABS(JAC%F(n)).GT.deltaf ) THEN 
              deltaf  = ABS(JAC%F(n))
            END IF
          END DO
          IF ( deltaf.LT.(DONEPERCENT*TOLR) ) RETURN
        END IF
C---------SET INITIAL VALUES
        DO n = 1, NRCHGRP
          JAC%XI(n)   = X(n)
        END DO
        DO n = 1, NSOLRG
          JAC%X0(n)   = JAC%XS(n)
          JAC%F0(n)   = JAC%F(n)
        END DO
        eta  = DZERO
        IF ( INEXCTNWT.GT.0 ) eta = DONEHALF
        fni  = GSOL_L2NORM(NSOLRG, JAC%F)
C---------OUTER ITERATION
        OUTER: DO ots = 1, NOUTER
          Iouter = ots
          its    = 0
C-----------CALCULATE EXPLICIT INVARIANT QM AND UPDATE THE RESIDUAL
          IF ( ICIQM.GT.0 ) THEN
            IF ( ots.GT.1 ) THEN
              CALL SSWR_EST_EXPLICIT_SFM(X)
              CALL SSWR_CALC_RGRESI(X,JAC%R)
            END IF
          END IF
C-----------CALCULATE COMPRESSED JACOBIAN
          CALL SSWR_FDJACC(X,JAC%R,JAC%XS,JAC%FJACC)
C-----------CHECK FOR ZERO ELEMENTS ALONG DIAGONAL
          DO n = 1, NSOLRG
            ia = JAC%IA(n)
            IF ( ABS(JAC%FJACC(ia)).EQ.DZERO ) THEN
              JAC%FJACC(ia) = DONE
              JAC%F(n)      = JAC%F(n) + DONE
            END IF
          END DO
C-----------CALCULATE THE MATRIX VECTOR PRODUCT J s(k-1)
         IF ( ISOLSTG.NE.0 ) THEN
           CALL GSOL_CMATVEC(NSOLRG,JAC%NNZ,JAC%IA,JAC%JA,
     2                       JAC%FJACC,JAC%XS,JAC%JSKM1)
         END IF
C-----------FILL RHS AND X EQUIVALENT
          DO n = 1, NSOLRG
            IF ( ISOLSTG.NE.0 ) THEN
              JAC%F(n)   = -JAC%F(n) + JAC%JSKM1(n)  !RHS EQUIVALENT
              JAC%DX(n)  =  JAC%XS(n)                !X EQUIVALENT
              JAC%XSI(n) =  JAC%XS(n)                !INITIAL X EQUIVALENT
            ELSE
              JAC%F(n)   = -JAC%F(n)                 !RHS EQUIVALENT
              JAC%DX(n)  =  DZERO                    !X EQUIVALENT
              JAC%XSI(n) =  DZERO                    !INITIAL X EQUIVALENT
            END IF
          END DO
C-----------SCALE COMPRESSED JACOBIAN, AND RESIDUAL
          IF ( IJACSCL.GT.0 ) THEN
            CALL GSOL_SCALE(0,IJACSCL,NSOLRG,JAC%NNZ,
     2                      JAC%IA,JAC%JA,JAC%FJACC,JAC%DX,JAC%F,
     3                      JAC%FSCALE,JAC%FSCALE2)
          END IF
C-----------PRECONDITIONER FOR JACOBIAN
          IF ( ISOLVER.GT.1 ) THEN
            CALL GSOL_PCU(IPC,NSOLRG,JAC%NNZ,
     2                    JAC%IA,JAC%JA,JAC%FJACC,
     3                    JAC%NRLU,JAC%NNZLU,
     4                    JAC%NJLU,JAC%NJW,JAC%NWLU,
     5                    JAC%JLU,JAC%IU,
     6                    JAC%JW,JAC%WLU,JAC%MJACC,
     7                    NLEVELS,DROPTOL)
          END IF
C-----------CALL APPROPRIATE SOLVER
          SELECT CASE ( ISOLVER )
C             LU DECOMPOSITION
            CASE (1)
              IF (JAC%IBAND.LT.1) THEN
                CALL GSOL_FULL_LUDCAP(icnvg,its,NOUTER,
     2            NSOLRG,JAC%NNZ,damp,
     3            JAC%IA,JAC%JA,
     4            JAC%FJACC,JAC%FJACU,JAC%DX,JAC%F,JAC%XSI,
     5            JAC%PINDEXLU,JAC%SLU)
              ELSE
                CALL GSOL_BAND_LUDCAP(icnvg,its,NOUTER,
     2            NSOLRG,JAC%NNZ,JAC%NBW,JAC%NHALFB,damp,
     3            JAC%IA,JAC%JA,
     4            JAC%FJACC,JAC%FJACU,JAC%FJACL,JAC%DX,JAC%F,JAC%XSI,
     5            JAC%PINDEXLU,JAC%SLU)
              END IF
C             BICONJUGATE GRADIENT STABILIZED
            CASE (2)
              CALL GSOL_BCGSAP(IPC,icnvg,its,NOUTER,NINNER,
     2                         TOLS,TOLR,eta,NSOLRG,JAC%NNZ,damp,
     3                         JAC%IA,JAC%JA,JAC%FJACC,
     4                         JAC%NRLU,JAC%NNZLU,
     5                         JAC%NJLU,JAC%NJW,JAC%NWLU,
     6                         JAC%JLU,JAC%IU,
     7                         JAC%JW,JAC%WLU,JAC%MJACC,
     8                         JAC%DX,JAC%F,JAC%XSI,
     9                         JAC%FSCALE,JAC%FSCALE2,
     X                         JAC%DBCGS,JAC%DHATBCGS,
     1                         JAC%PBCGS,JAC%PHATBCGS,
     2                         JAC%SBCGS,JAC%SHATBCGS,
     3                         JAC%VBCGS,JAC%TBCGS)
C             RESTARTED GENERALIZED MINIMUM RESIDUAL                        
            CASE (3)
              CALL GSOL_GMRMAP(IPC,icnvg,its,NOUTER,NINNER,
     2                         TOLR,eta,NSOLRG,JAC%NNZ,damp,
     3                         JAC%IA,JAC%JA,JAC%FJACC,
     4                         JAC%NRLU,JAC%NNZLU,
     5                         JAC%NJLU,JAC%NJW,JAC%NWLU,
     6                         JAC%JLU,JAC%IU,
     7                         JAC%JW,JAC%WLU,JAC%MJACC,
     8                         JAC%DX,JAC%F,JAC%XSI,
     9                         JAC%FSCALE,JAC%FSCALE2,
     X                         JAC%RGMR,JAC%ZGMR,JAC%DGMR,JAC%TGMR,
     1                         JAC%CSGMR,JAC%SNGMR,JAC%SGMR,JAC%YGMR,
     2                         JAC%HGMR,JAC%VGMR)
          END SELECT 
C-----------INCREMENT COUNTER FOR TOTAL NUMBER OF INNER ITERATIONS
          Isoln = Isoln + its         
C-----------UNSCALE DX
          IF ( IJACSCL.GT.0 ) THEN
            CALL GSOL_SCALE(1,IJACSCL,NSOLRG,JAC%NNZ,
     2                      JAC%IA,JAC%JA,JAC%FJACC,JAC%DX,JAC%F,
     3                      JAC%FSCALE,JAC%FSCALE2)
          END IF
C-----------UPDATE f WITH CURRENT X ITERATE
          DO n = 1, NSOLRG
            IF ( ISOLSTG.NE.0 ) THEN
              JAC%XS(n) = JAC%DX(n)
            ELSE
              JAC%XS(n) = JAC%XS(n) + JAC%DX(n)
            END IF
          END DO
C-----------MAP SOLUTION TO ALL REACH GROUPS
          CALL SSWRS2C(JAC%XS,X)
          CALL SSWR_CALC_RGRESI(X,JAC%R)
          CALL SSWRC2S(JAC%R,JAC%F)
          fn = GSOL_L2NORM(NSOLRG, JAC%F)
C-----------BACKTRACKING - 
C           ONLY APPLIED WHEN THERE HAS BEEN NO IMPROVEMENT IN THE L2NORM
          IF ( IBT.GT.1 .AND. fn.GT.fni .AND. ISWRSS.EQ.0 ) THEN
!          IF ( IBT.GT.1 .AND. fn.GT.fni ) THEN
            fn0 = fn
            CALL SSWRBTBM(ots,fn0,fn,JAC%R,JAC%XI,X)
            CALL SSWRC2S(X,JAC%XS)
            CALL SSWRC2S(JAC%R,JAC%F)
            DO n = 1, NSOLRG
              JAC%DX(n) = JAC%XS(n) - JAC%X0(n)
            END DO
          END IF
C-----------CHECK FOR ABSOLUTE MINIMIZATION OF f
          deltaf  = DZERO
          deltafi = DZERO
          deltax  = DZERO
          DO n = 1, NSOLRG
            deltaf  = MAX( deltaf,  ABS(JAC%F(n)) )
            deltafi = MAX( deltafi, ABS(JAC%F(n) - JAC%F0(n)) )
            deltax  = MAX( deltax,  ABS(JAC%DX(n)) )
          END DO
          IF ( deltaf.LT.TOLR .AND. deltafi.LT.TOLR ) THEN
!          IF ( deltaf .LT.TOLR  .AND. 
!     2         deltafi.LT.TOLR .AND. 
!     3         deltax .LT.TOLS ) THEN
!            IF ( its.EQ.1 .AND. ots.GT.1 ) RETURN  !this check greatly increases the run time of SWRTestSimulation06
            IF ( ots.GT.1 ) RETURN
          END IF
C-----------SAVE CURRENT OUTER ITERATES
           DO n = 1, NRCHGRP
             JAC%XI(n)  = X(n)
           END DO
           DO n = 1, NSOLRG
             JAC%X0(n)  = JAC%XS(n)
             JAC%DX(n)  = DZERO
             JAC%F0(n)  = JAC%F(n)
           END DO
           fni = fn
        END DO OUTER
C---------SET Check TO .TRUE. TO INDICATE CONVERGENCE NOT ACHIEVED
        Check = .TRUE.
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_GSOLWRP
C
C     FORWARD DIFFERENCE JACOBIAN - MODIFIED CRS FORMAT
      SUBROUTINE SSWR_FDJACC(P,Fvec,Vs,Df)
        USE GWFSWRMODULE, ONLY: DZERO, DONE, DTWO, NEARZERO, TIMECONV, 
     2                          NREACHES, NRCHGRP, NSOLRG, JAC
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(INOUT)  :: P
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN)     :: Fvec
        DOUBLEPRECISION, DIMENSION(NSOLRG),  INTENT(IN)     :: Vs
        DOUBLEPRECISION, DIMENSION(JAC%NNZ),  INTENT(INOUT) :: Df
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: ic0, ic1
        INTEGER :: i, n
        INTEGER :: ii, nn
        DOUBLEPRECISION :: f
        DOUBLEPRECISION :: dc
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_RGTERMS
        DOUBLEPRECISION :: SSWR_KELLEY_PERTB
C     + + + CODE + + +
        f     = DZERO
        DO n = 1, NRCHGRP
          JAC%JV(n)    = DZERO
          JAC%JPSAV(n) = P(n)
        END DO
        DO n = 1, JAC%NNZ
          Df(n) = DZERO
        END DO
C         PERTURBATION BASED ON KELLY (2003)
        CALL SSWRS2C(Vs,JAC%JV)
        DO n = 1, NRCHGRP
          JAC%JH(n)   = SSWR_KELLEY_PERTB( JAC%JV(n) )
          JAC%JPPH(n) = JAC%JPSAV(n) + JAC%JH(n)
        END DO
        DO n = 1, NSOLRG
          nn    = JAC%ISMAP(n)
          P(nn) = JAC%JPPH(nn)
          ic0   = JAC%IA(n)
          ic1   = JAC%IA(n+1) - 1
          DO i = ic0, ic1
            ii = JAC%ISMAP(JAC%JA(i))
            f = SSWR_CALC_RGTERMS(ii,P)
C            dc = (f - Fvec(ii))/ e           !JONES AND WOODWARD (2001)
            dc = (f - Fvec(ii))/ JAC%JH(nn)  !KELLY (2003)
            IF ( ISNAN(dc) ) dc = DZERO
            Df(i) = dc
          END DO
          P(nn)  = JAC%JPSAV(nn)
        END DO
!C---------
!        DO n = 1, NSOLRG
!          nn   = JAC%ISMAP(n)
!          ic0 = JAC%IA(n)
!          ic1 = JAC%IA(n+1) - 1
!          DO i = ic0, ic1
!            WRITE (666,'(I10,1X,I10,1X,G16.7)') n, JAC%JA(i), Df(i)
!          END DO
!        END DO
!        STOP
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_FDJACC


      SUBROUTINE SSWR_CALC_RGRESI(P,F)
        USE GWFSWRMODULE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN)    :: P
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(INOUT) :: F
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i 
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_RGTERMS
C     + + + CODE + + +
        F = DZERO
C
C---------CALCULATE F
        DO i = 1, NRCHGRP
          F(i) = SSWR_CALC_RGTERMS(i,P)
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_CALC_RGRESI

      DOUBLEPRECISION FUNCTION SSWR_CALC_RGTERMS(Irg,P) RESULT(value)
        USE GWFSWRMODULE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Irg
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN) :: P
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: in
        DOUBLEPRECISION :: r, e, b, qm, ex, bc, c, v
        DOUBLEPRECISION :: v0
        DOUBLEPRECISION :: ps
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_RG_QLI
        DOUBLEPRECISION :: SSWR_RG_QPR
        DOUBLEPRECISION :: SSWR_RG_QEV
        DOUBLEPRECISION :: SSWR_RG_QAQ
        DOUBLEPRECISION :: SSWR_RG_VOL
        DOUBLEPRECISION :: SSWR_RG_QM
        DOUBLEPRECISION :: SSWR_RG_QBC
        DOUBLEPRECISION :: SSWR_RG_QCS
C     + + + CODE + + +
        value = DZERO
        in    = DZERO
        r     = DZERO
        e     = DZERO
        b     = DZERO
        qm    = DZERO
        ex    = DZERO
        bc    = DZERO
        c     = DZERO
        v     = DZERO
        v0    = DZERO
C
C---------CALCULATE INITIAL VOLUME          
C         TRANSIENT
        IF (ISWRSS.LT.1) THEN
          v0 = SSWR_RG_VOL(RCHGRP(Irg),GSTAGE(Irg,ISWRDT-1))
          v0 = v0 / SWRDT
          IF ( RCHGRP(Irg)%CONSTANT ) THEN
            v0 = SSWR_RG_VOL(RCHGRP(Irg),GSTAGE(Irg,0))
            v0 = v0 / SWRDT
          END IF
C         FOR STEADY STATE - EVALUATED USING PSUEDO-TRANSIENT CONTINUATION APPROACH
        ELSE
C           NO STORAGE OPTION
          IF ( ISWRSS.EQ.1 ) THEN
            v0 = DZERO
C           PSUEDO-TRANSIENT CONTINUATION APPROACH
          ELSE IF ( ISWRSS.EQ.2 ) THEN
            v0 = SSWR_RG_VOL(RCHGRP(Irg),GSTAGE(Irg,0))
            v0 = v0 / SWRDT
          END IF
C           UPDATE INITIAL VOLUME BASED ON CHANGES IN CONSTANT STAGES
          IF ( RCHGRP(Irg)%CONSTANT ) THEN
            v0 = SSWR_RG_VOL(RCHGRP(Irg),GSTAGE(Irg,0))
            v0 = v0 / SWRDT
          END IF
        END IF
C
C---------GET ESTIMATE OF CURRENT TERMS
        ps = P(Irg)
        in = SSWR_RG_QLI(RCHGRP(Irg))
        r  = SSWR_RG_QPR(RCHGRP(Irg))
        e  = SSWR_RG_QEV(RCHGRP(Irg),ps)
        b  = SSWR_RG_QAQ(RCHGRP(Irg),ps)
        qm = SSWR_RG_QM(RCHGRP(Irg),P)
        bc = SSWR_RG_QBC(RCHGRP(Irg),P) 
        c  = SSWR_RG_QCS(RCHGRP(Irg),in,r,e,b,qm,bc)
C         TRANSIENT
        IF (ISWRSS.LT.1) THEN
          v  = SSWR_RG_VOL(RCHGRP(Irg),ps) / SWRDT
C         FOR STEADY STATE - EVALUATED USING PSUEDO-TRANSIENT CONTINUATION APPROACH
        ELSE
C           NO STORAGE OPTION
          IF ( ISWRSS.EQ.1 ) THEN
            v = DZERO
C           PSUEDO-TRANSIENT CONTINUATION APPROACH
          ELSE IF ( ISWRSS.EQ.2 ) THEN
            v = SSWR_RG_VOL(RCHGRP(Irg),ps) / SWRDT
          END IF

        END IF
C           ELIMINATE VOLUME CHANGE FOR CONSTANT STAGE REACH GROUPS
C           FROM RESIDUAL CALCULATION 
          IF ( RCHGRP(Irg)%CONSTANT ) v = v0
C---------BACKWARD DIFFERENCE - ALL TERMS ARE RATES
        value = in + r + e + b + qm + bc + c + v0 - v
C---------SCALE RESIDUAL FROM CONSISTENT UNITS TO CUBIC METERS PER SECOND
        value = value * QSCALE
C---------RETURN
        RETURN
      END FUNCTION SSWR_CALC_RGTERMS

      SUBROUTINE SSWR_SET_TEMPRCHSTG(Ps,S)
        USE GWFSWRMODULE, ONLY: DZERO, NREACHES, REACH,
     2                          NRCHGRP, RCHGRP
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN)    :: Ps
        DOUBLEPRECISION, DIMENSION(NREACHES), INTENT(INOUT) :: S
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j
        INTEGER :: irch
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        S = DZERO
        DO i = 1, NRCHGRP
          DO j = 1, RCHGRP(i)%NRGREACH  
            irch = RCHGRP(i)%REACH(j)
            S(irch) = Ps(i) + REACH(irch)%OFFSET
          END DO
        END DO
        RETURN
      END SUBROUTINE SSWR_SET_TEMPRCHSTG

      SUBROUTINE SSWR_SET_INITVOL()
        USE GWFSWRMODULE
        USE GWFSWRINTERFACE, ONLY: SSWR_LININT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i
        INTEGER :: n
        INTEGER :: irch
        INTEGER :: icalcrch
        DOUBLEPRECISION :: ps, s
        DOUBLEPRECISION :: v
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO n = 1, NRCHGRP
          RCHGRP(n)%CURRENT%VOLUME = 0.0D0
        END DO
        DO n = 1, NRCHGRP
          icalcrch = RCHGRP(n)%ICALCRCH
          ps = REACH(icalcrch)%STAGE
          RCHGRP(n)%STAGE = ps
          DO i = 1, RCHGRP(n)%NRGREACH  
            irch =   RCHGRP(n)%REACH(i)
            s  = ps + REACH(irch)%OFFSET
            v = SSWR_LININT(REACH(irch)%GEO%ELEV,
     2                      REACH(irch)%GEO%VOL,s)
            REACH(irch)%CURRENT%VOLUME = v
            RCHGRP(n)%CURRENT%VOLUME = RCHGRP(n)%CURRENT%VOLUME + v
          END DO
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_SET_INITVOL

      SUBROUTINE SSWR_RG_UPDATE
     2           (Rg,V,Rainfall,Evap,Qaqflow,Qbcflow,Crflow)
        USE GWFSWRMODULE, ONLY: DZERO, REACH,
     2                          TRCHGRP, !QUZFLOW, QEXTFLOW,
     3                          ISWRSS, ISWRDT, SWRDT,
     4                          RSTAGE, GSTAGE
        USE GWFSWRINTERFACE, ONLY: SSWR_LININT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        TYPE (TRCHGRP), INTENT(INOUT)  :: Rg
        DOUBLEPRECISION, INTENT(IN)    :: V
        DOUBLEPRECISION, INTENT(IN)    :: Rainfall
        DOUBLEPRECISION, INTENT(IN)    :: Evap
        DOUBLEPRECISION, INTENT(IN)    :: Qaqflow
        DOUBLEPRECISION, INTENT(IN)    :: Qbcflow
        DOUBLEPRECISION, INTENT(IN)    :: Crflow
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: nr
        INTEGER :: irch, irg
        DOUBLEPRECISION :: linf, uinf, einf
        DOUBLEPRECISION :: qpos, qneg
        DOUBLEPRECISION :: v0
        DOUBLEPRECISION :: ps
        DOUBLEPRECISION :: cv
        DOUBLEPRECISION :: rs
        DOUBLEPRECISION :: rv, rv0
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_RG_VOL
        DOUBLEPRECISION :: SSWR_CALC_VOL
        DOUBLEPRECISION :: SSWR_RG_STG
C     + + + CODE + + +
C
C---------FILL RCHGRP WITH CURRENT ESTIMATES
        Rg%CURRENT%VOLUME   = V
C---------DVOLUME SAVED AS A RATE - [L3/T]
C         TRANSIENT
        IF (ISWRSS.LT.1) THEN
          irg = Rg%IRG
          v0 = SSWR_RG_VOL(Rg,GSTAGE(irg,ISWRDT-1))
          Rg%CURRENT%DVOLUME  = (v0 - V) / SWRDT
C         STEADY STATE - ISWRSS > 0
        ELSE
          Rg%CURRENT%DVOLUME = DZERO
        END IF
C---------CALCULATE REACH TERMS
        linf = DZERO
        uinf = DZERO
        einf = DZERO
        qpos = DZERO
        qneg = DZERO
        DO nr = 1, Rg%NRGREACH  
          irch  = Rg%REACH(nr)
C-----------ADD LATERAL INFLOW: [RATE]
          linf = linf + REACH(irch)%QLATFLOW
C-----------ADD LATERAL INFLOW: [RATE]
          uinf = uinf + REACH(irch)%QUZFLOW
C-----------ADD EXTERNAL INFLOW: [RATE]
          einf = einf + REACH(irch)%QEXTFLOW
C-----------UNCONTROLLED AND CONTROLLED INFLOW
          qpos = qpos + REACH(irch)%RPOSFLOW
C-----------UNCONTROLLED AND CONTROLLED OUTFLOW
          qneg = qneg + REACH(irch)%RNEGFLOW
        END DO
        Rg%CURRENT%RAIN       = Rainfall
        Rg%CURRENT%EVAP       = Evap
        Rg%CURRENT%QAQFLOW    = Qaqflow
        Rg%CURRENT%QLATFLOW   = linf
        Rg%CURRENT%QUZFLOW    = uinf
        Rg%CURRENT%QMPOSFLOW  = qpos        
        Rg%CURRENT%QMNEGFLOW  = qneg
        Rg%CURRENT%QEXTFLOW   = einf        
        Rg%CURRENT%QBCFLOW    = Qbcflow        
C---------BOUNDARY FLOW RESULTING FROM CHANGE IN CONSTANT STAGES
        cv = DZERO
        IF ( Rg%CONSTANT ) THEN
          cv = -Rg%CURRENT%DVOLUME
        END IF
        Rg%CURRENT%QCRFLOW    = Crflow + cv
C        
C---------GET RCHGRP STAGE
        ps = SSWR_RG_STG(Rg, Rg%CURRENT%VOLUME)
        Rg%STAGE = ps
C        
C---------UPDATE REACH DATA FOR CURRENT SWR TIMESTEP
C         BASED ON NEW VOLUME
        DO nr = 1, Rg%NRGREACH  
          irch = Rg%REACH(nr)
C           UPDATE STAGE
          IF (.NOT.Rg%CONSTANT) THEN
            REACH(irch)%STAGE = ps + REACH(irch)%OFFSET
          END IF
C           UPDATE VOLUME
          rs = ps + REACH(irch)%OFFSET
          rv  = SSWR_LININT(REACH(irch)%GEO%ELEV,
     2                      REACH(irch)%GEO%VOL,rs)
          REACH(irch)%CURRENT%VOLUME = rv
          rv0 = SSWR_CALC_VOL(irch,RSTAGE(irch,ISWRDT-1))
C           UPDATE DVOLUME
          IF ( ISWRSS.LT.1 ) THEN
            REACH(irch)%CURRENT%DVOLUME = (rv0 - rv) / SWRDT
C-------------BOUNDARY FLOW RESULTING FROM CHANGE IN CONSTANT STAGES
            IF ( Rg%CONSTANT ) THEN
              REACH(irch)%CURRENT%QCRFLOW = 
     2          REACH(irch)%CURRENT%QCRFLOW + (rv - rv0) / SWRDT
            END IF
          ELSE
            REACH(irch)%CURRENT%DVOLUME = DZERO
          END IF
        END DO
C---------RETURN        
        RETURN
      END SUBROUTINE SSWR_RG_UPDATE

C
C-------SUBROUTINE TO CALCULATE INCREMENTAL AND CUMULATIVE 
C       SURFACE WATER ROUTING BUDGET FOR MODEL
      SUBROUTINE SSWRBUDGET(Kper,Kstp,
     2                      Qaqcumin,Qaqincin,Qaqcumout,Qaqincout)
        USE GLOBAL,       ONLY: IOUT, ISSFLG, NCOL, NROW, NLAY, IBOUND
        USE GWFBASMODULE, ONLY: DELT, TOTIM, IBUDFL
        USE GWFSWRMODULE, ONLY: IZERO, RZERO, DZERO, DONE, 
     &                          ISWRONLY, NUMTIME, ISWRDT,
     &                          REACH, 
     &                          NREACHES, NRCHGRP, RCHGRP,
     &                          NBDITEMS, INCBD, CUMBD, 
     &                          CUMQAQBD, INCQAQBD, 
     &                          SWRTIME,
     &                          BFCNV, 
     &                          RSTAGE, GSTAGE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Kper
        INTEGER, INTENT(IN) :: Kstp
        REAL, INTENT(IN)    :: Qaqcumin
        REAL, INTENT(IN)    :: Qaqincin
        REAL, INTENT(IN)    :: Qaqcumout
        REAL, INTENT(IN)    :: Qaqincout
C     + + + LOCAL DEFINITIONS + + +
        CHARACTER (LEN=16), DIMENSION(9) :: bdtext
        CHARACTER (LEN=17)   :: val1, val2
        INTEGER              :: i, n
        INTEGER              :: ia
        INTEGER              :: irch, irg
        INTEGER              :: iacc
        INTEGER              :: ir, jc, kl
        INTEGER              :: kact, kk
        DOUBLEPRECISION      :: q
        DOUBLEPRECISION      :: dtscale
        DOUBLEPRECISION      :: rs
        DOUBLEPRECISION      :: rs0
        DOUBLEPRECISION      :: v
        DOUBLEPRECISION      :: v0
        DOUBLEPRECISION      :: dv
        DOUBLEPRECISION      :: cv
        DOUBLEPRECISION      :: incscl
        DOUBLEPRECISION      :: ratin,ratout
        REAL                 :: incin,incout
        DOUBLEPRECISION      :: incavg,incp
        REAL                 :: cumin,cumout,cumavg,cump
        REAL                 :: cumswr1, incswr1
        REAL                 :: cummf, incmf
        REAL                 :: cumiaswr1, inciaswr1
        DOUBLEPRECISION, DIMENSION(NBDITEMS) :: incrate, tsrate
        DOUBLEPRECISION, DIMENSION(NBDITEMS) :: rratin, rratout
        DOUBLEPRECISION :: qaqrate, b
        REAL :: qaqratin, qaqratout
C     + + + FUNCTIONS + + +
        CHARACTER (LEN=17) :: SSWR_BDCHAR
        DOUBLEPRECISION :: SSWR_CALC_QM
        DOUBLEPRECISION :: SSWR_CALC_VOL
        DOUBLEPRECISION :: SSWR_CALC_QLI
        DOUBLEPRECISION :: SSWR_CALC_QUZ
        DOUBLEPRECISION :: SSWR_CALC_QPR
        DOUBLEPRECISION :: SSWR_CALC_QEV
        DOUBLEPRECISION :: SSWR_CALC_QAQ
        DOUBLEPRECISION :: SSWR_CALC_QEX
        DOUBLEPRECISION :: SSWR_CALC_QBC
        DOUBLEPRECISION :: SSWR_CALC_QCS
C     + + + DATA + + +
      DATA bdtext /'   LATERAL FLOW',
     2             '  UNSAT. RUNOFF',
     3             '       RAINFALL',
     4             '    EVAPORATION',
     5             '  REACH-AQ FLOW',
     6             '  EXTERNAL FLOW',
     7             '  BOUNDARY FLOW',
     8             '  CONSTANT FLOW',
     9             '        STORAGE'/
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(//20X,'VOLUMETRIC SURFACE WATER BUDGET FOR ENTIRE MODEL',
     2       /20X,'AT END OF TIME STEP',1X,I5,' IN STRESS PERIOD',1X,I4,
     3       /2X,83('-'))
2010  FORMAT(1X,/8X,'CUMULATIVE VOLUMES',6X,'L**3',7X,
     2       'RATES FOR THIS TIME STEP',6X,'L**3/T',/,
     3       8X,18('-'),17X,24('-'),//,
     4       14X,'IN:',38X,'IN:'/14X,'---',38X,'---')
2020  FORMAT(1X,6X,A16,' =',A17,6X,A16,' =',A17)
2030  FORMAT(1X,/15X,'TOTAL IN =',A,14X,'TOTAL IN =',A)
2040  FORMAT(1X,/13X,'OUT:',37X,'OUT:'/13X,4('-'),37X,4('-'))
2050  FORMAT(1X,/14X,'TOTAL OUT =',A,13X,'TOTAL OUT =',A)
2060  FORMAT(1X,/15X,'IN - OUT =',A,14X,'IN - OUT =',A)
2070  FORMAT(1X,/4X,'PERCENT DISCREPANCY =',F15.2,
     2       5X,'PERCENT DISCREPANCY =',F15.2,/)
2080  FORMAT(/19X,
     2       'DISCREPANCY BETWEEN MODFLOW AND SWR1 AQUIFER-REACH',
     3       /18X,
     4       '             TERMS FOR THE ENTIRE MODEL               ',
     5       /02X,83('-'),
     6       /07X,'CUMULATIVE VOLUMES',7X,'L**3',8X,
     7       'RATES FOR THIS TIME STEP',7X,'L**3/T',/,
     8        07X,18('-'),19X,24('-'))
2090  FORMAT(2X,3X,'           MODFLOW =',A17,
     2          6X,'           MODFLOW =',A17)
2100  FORMAT(2X,3X,'              SWR1 =',A17,
     2          6X,'              SWR1 =',A17)
2110  FORMAT(2X,'  PERCENT DISCREPANCY =',F15.2,
     2       5X,'  PERCENT DISCREPANCY =',F15.2,
     3      /2X,'SWR1 INACTIVE MODFLOW =',A17,
     4       3X,'SWR1 INACTIVE MODFLOW =',A17,
     5      /2X,83('-'),/14X,
     6         'PERCENT DISCREPANCY = 100 x (SWR1 - MODFLOW) / MODFLOW',
     7      ///)
C     + + + CODE + + +
C---------INITIALIZE INACTIVE CELL ACCUMULATOR
        ia         = IZERO
C---------ZERO OUT INCREMENTAL TERMS
        INCBD(:,:) = RZERO
C---------ZERO OUT SUMMATION TERMS
        incrate    = DZERO
        tsrate     = DZERO
        rratin     = DZERO
        rratout    = DZERO
        incin      = RZERO
        incout     = RZERO
        cumin      = RZERO
        cumout     = RZERO
        qaqratout  = RZERO
        qaqratin   = RZERO
C---------ASSEMBLE BUDGET TERMS        
        incscl = 1.0  / REAL( NUMTIME, 8 )
        REACHES: DO irch = 1, NREACHES
          ir      = REACH(irch)%IRCH
          jc      = REACH(irch)%JRCH
          tsrate  = DZERO
          qaqrate = DZERO
          irg = REACH(irch)%IRG
C           SKIP CONNECTION IF CONNECTED REACH IS INACTIVE
          IF ( REACH(irch)%ISWRBND.EQ.0 ) CYCLE
          TIMES: DO n = 1, NUMTIME
            ISWRDT = n

            CALL SWR_SET_STRVALS(irch,n)
            q       = SSWR_CALC_QM(irch,GSTAGE(1,n))
            rs      = RSTAGE(irch,n)
            v       = SSWR_CALC_VOL(irch,rs)
            rs0 = RSTAGE(irch,n-1)
            v0  = SSWR_CALC_VOL(irch,rs0)
            dv  = ( v0 - v ) / SWRTIME(n)%SWRDT
C             CALCULATE CONSTANT STAGE REACH CONSTANT FLOW CHANGE
C             DUE TO CHANGES IN CONSTANT STAGES
            cv  = DZERO
            IF ( REACH(irch)%ISWRBND.LT.0 ) cv = -dv

            incrate(1) = SSWR_CALC_QLI(irch)
            incrate(2) = SSWR_CALC_QUZ(irch)
            incrate(3) = SSWR_CALC_QPR(irch)
            incrate(4) = SSWR_CALC_QEV(irch,rs)
            incrate(5) = SSWR_CALC_QAQ(REACH(irch),rs)
            incrate(6) = SSWR_CALC_QEX(irch)
            incrate(7) = SSWR_CALC_QBC(irch,GSTAGE(1,n))
            incrate(8) = SSWR_CALC_QCS(irch) + cv
            incrate(9) = dv
C
C------------ ACCUMULATE BUDGET ITEMS IN ACCUMULATOR FOR THE CURRENT
C             MODFLOW TIME STEP
            dtscale = SWRTIME(n)%SWRDT / REAL( DELT, 8 )
            DO iacc = 1, NBDITEMS
              tsrate(iacc) = tsrate(iacc) + incrate(iacc) * dtscale
            END DO
C
C-------------PROCESS ACCUMULATED QAQFLOW DATA FOR ACTIVE MODFLOW CELLS
C             FOR USE IN COMPARISON OF SWR1 AND MODFLOW QAQ TERMS
            kact = REACH(irch)%LAYACT
            IF ( kact.GT.NLAY ) THEN
              ia = ia + 1
              CYCLE TIMES
            END IF
            LAYERS: DO kl = REACH(irch)%LAYSTR, REACH(irch)%LAYEND
              kk = MAX( kl, REACH(irch)%LAYACT )
              IF( IBOUND(jc,ir,kk).EQ.0 ) THEN
                ia = ia + 1
                CYCLE LAYERS  
              END IF
              b = REACH(irch)%CURRENTQAQ(kl)%QAQFLOW
              qaqrate = qaqrate - b * dtscale
            END DO LAYERS
          END DO TIMES
C-----------FILL POSITIVE AND NEGATIVE BUDGET ITEMS
          DO iacc = 1, NBDITEMS
            IF (REAL(tsrate(iacc),4).LT.RZERO) THEN
              rratout(iacc) = rratout(iacc) - tsrate(iacc)  !* incscl
            ELSE
              rratin(iacc)  = rratin(iacc)  + tsrate(iacc)  !* incscl
            END IF
          END DO
          IF ( REAL( qaqrate, 4 ).LT.RZERO ) THEN
            qaqratout = qaqratout - qaqrate
          ELSE
            qaqratin  = qaqratin  + qaqrate
          END IF
        END DO REACHES
C---------ADD INDIVIDUAL BUDGET TERMS TO BUDGET ACCUMULATORS
        DO iacc = 1, NBDITEMS
          ratin  = rratin(iacc)
          ratout = rratout(iacc)
          INCBD(1,iacc) = ratin
          INCBD(2,iacc) = ratout
          CUMBD(1,iacc) = CUMBD(1,iacc) + ratin  * DELT
          CUMBD(2,iacc) = CUMBD(2,iacc) + ratout * DELT
        END DO
C---------QAQ COMPARISON TERMS
        INCQAQBD(1) = qaqratin
        INCQAQBD(2) = qaqratout
        CUMQAQBD(1) = CUMQAQBD(1) + qaqratin  * DELT
        CUMQAQBD(2) = CUMQAQBD(2) + qaqratout * DELT
C
C---------WRITE RESULTS
        IF (IBUDFL.NE.0) THEN
          incin  = SUM(INCBD(1,:))
          incout = SUM(INCBD(2,:))
          cumin  = SUM(CUMBD(1,:))
          cumout = SUM(CUMBD(2,:))
          WRITE (IOUT,2000) Kstp,Kper
C-----------INFLOW ITEMS          
          WRITE (IOUT,2010)
          DO i = 1, NBDITEMS
            val1 = SSWR_BDCHAR(CUMBD(1,i))
            val2 = SSWR_BDCHAR(INCBD(1,i))
            WRITE (IOUT,2020) bdtext(i),val1,bdtext(i),val2
          END DO
          val1 = SSWR_BDCHAR(cumin)
          val2 = SSWR_BDCHAR(incin)
          WRITE (IOUT,2030) val1, val2
C-----------OUTFLOW ITEMS          
          WRITE (IOUT,2040)
          DO i = 1, NBDITEMS
            val1 = SSWR_BDCHAR(CUMBD(2,i))
            val2 = SSWR_BDCHAR(INCBD(2,i))
            WRITE (IOUT,2020) bdtext(i),val1,bdtext(i),val2
          END DO
          val1 = SSWR_BDCHAR(cumout)
          val2 = SSWR_BDCHAR(incout)
          WRITE (IOUT,2050) val1, val2
C------------SUMMARY ITEMS
C         IN - OUT
          val1 = SSWR_BDCHAR(cumin-cumout)
          val2 = SSWR_BDCHAR(incin-incout)
          WRITE (IOUT,2060) val1, val2
C           PERCENT DIFFERENCE
          incavg = (incin + incout) / 2.0
          cumavg = (cumin + cumout) / 2.0
          cump = RZERO
          IF (cumavg.NE.RZERO) cump = 100.0 * (cumin-cumout) / cumavg
          incp = RZERO
          IF (incavg.NE.RZERO) incp = 100.0 * (incin-incout) / incavg
          WRITE (IOUT,2070) cump, incp
C
C-----------WRITE DISCREPANCY BETWEEN MODFLOW AND SWR1 QAQFLOW
          QAQDIS: IF ( ISWRONLY.LT.1 ) THEN
            cumswr1   = CUMQAQBD(2) - CUMQAQBD(1)
            incswr1   = INCQAQBD(2) - INCQAQBD(1)
            cummf     = Qaqcumout   - Qaqcumin
            incmf     = Qaqincout   - Qaqincin
            IF ( ia.GT.0 ) THEN
              cumiaswr1 = ( CUMBD(2,5) - CUMBD(1,5) ) - cumswr1
              inciaswr1 = ( INCBD(2,5) - INCBD(1,5) ) - incswr1
            ELSE
              cumiaswr1 = RZERO
              inciaswr1 = RZERO
            END IF
            WRITE (IOUT,2080)
C             MODFLOW
            val1 = SSWR_BDCHAR(cummf)
            val2 = SSWR_BDCHAR(incmf)
            WRITE (IOUT,2090) val1, val2
C             SWR
            val1 = SSWR_BDCHAR(cumswr1)
            val2 = SSWR_BDCHAR(incswr1)
            WRITE (IOUT,2100) val1, val2
C             QAQFLOW PERCENT DIFFERENCE
C               RELATIVE TO CUMULATIVE AND INCREMENTAL VALUES FOR SWR1 BUDGET
            val1 = SSWR_BDCHAR(cumiaswr1)
            val2 = SSWR_BDCHAR(inciaswr1)
            cump = RZERO
            IF (cummf.NE.RZERO) cump = 100.0 * (cumswr1-cummf) / cummf
            incp = RZERO
            IF (incmf.NE.RZERO) incp = 100.0 * (incswr1-incmf) / incmf
            WRITE (IOUT,2110) cump, incp, val1, val2
          END IF QAQDIS
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SSWRBUDGET
C
C-------SUBROUTINE TO OUTPUT SUMMARY GROUP DATA TO ISWRPRGF
      SUBROUTINE SSWR_P_RGFLOW(Kper,Kstp)
        USE GWFBASMODULE, ONLY: DELT, TOTIM
        USE GLOBAL,       ONLY: ISSFLG
        USE GWFSWRMODULE, ONLY: RZERO, DZERO, DONE, 
     2                          ISWRSAVG, RTPRN, NPMAX, 
     3                          REACH, NRCHGRP, RCHGRP, SWRDT, ISWRDT,
     4                          NUMTIME, ISWRPRGF, SWRHEADER, SWRDT, 
     5                          ISWRDT, SWRTIME, RSTAGE, GSTAGE, GSTAGEP
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Kper
        INTEGER, INTENT(IN) :: Kstp
C     + + + LOCAL DEFINITIONS + + +
        CHARACTER (LEN=224)  :: dataline
        CHARACTER (LEN=5128) :: line
        CHARACTER (LEN=5)    :: cval
        INTEGER              :: i, n
        INTEGER              :: iu
        INTEGER              :: irg
        INTEGER              :: ic
        INTEGER              :: irch
        INTEGER              :: ip
        DOUBLEPRECISION      :: swrtot, dt
        DOUBLEPRECISION      :: pfact
        DOUBLEPRECISION      :: s, vol
        DOUBLEPRECISION      :: inf,latinf,uzfinf,r,e,b,bc,c,dv,f
        DOUBLEPRECISION      :: ext
        DOUBLEPRECISION      :: qm,qpos,qneg
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_RG_QPR
        DOUBLEPRECISION :: SSWR_RG_QEV
        DOUBLEPRECISION :: SSWR_RG_QAQ
        DOUBLEPRECISION :: SSWR_RG_QM
        DOUBLEPRECISION :: SSWR_RG_QLI
        DOUBLEPRECISION :: SSWR_RG_QBC
        DOUBLEPRECISION :: SSWR_RG_QCS
        DOUBLEPRECISION :: SSWR_RG_VOL
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000    FORMAT(I5.5)
2010    FORMAT(A)
2020    FORMAT(2(E10.3,','),3(I10,','))
2030    FORMAT(E15.8,',',13(E15.8,','))
2040    FORMAT(2(E10.3,','),4(I10,','),E15.8,',',13(E15.8,','))
C     + + + CODE + + +
        iu = ABS(ISWRPRGF)
C
C---------WRITE FILE HEADER IF FIRST CALL TO SWR BUDGET SUBROUTINE
        IF ( SWRHEADER.EQ.0 ) THEN
          IF ( ISWRPRGF.GT.0 ) THEN
            line = 'TOTTIME,SWRDT,KPER,KSTP,' //
     2           'KSWR,RCHGRP,' //
     3          'STAGE,QPFLOW,QLATFLOW,QUZFLOW,RAIN,EVAP,QAQFLOW,' //
     4          'QNFLOW,QEXTFLOW,QBCFLOW,QCRFLOW,DV,INF-OUT,VOLUME'
!            DO i = 1, NRCHGRP
!              WRITE (cval,2000) i
!              line = trim(adjustl(line)) //
!     2          'STAGE' // cval // ',' //  
!     3          'QPFLOW ' // cval // ',' //  
!     4          'QLATFLOW' // cval // ',' //  
!     5          'QUZFLOW' // cval // ',' //  
!     6          'RAIN' // cval // ',' //  
!     7          'EVAP' // cval // ',' //  
!     8          'QAQFLOW' // cval // ',' //
!     9          'QNFLOW' // cval // ',' //
!     X          'QEXTFLOW' // cval // ',' //
!     1          'QBCFLOW' // cval // ',' //
!     2          'QCRFLOW' // cval // ',' //
!     3          'DV' // cval // ',' //
!     4          'INF-OUT' // cval // ',' //
!     5          'VOLUME' // cval // ','
!            END DO
            WRITE (iu,2010) TRIM(ADJUSTL(line))
          ELSE
            WRITE (iu) NRCHGRP
          END IF
        END IF
C---------FILL PRINT ARRAYS
        pfact = DONE
        IF ( RTPRN.NE.RZERO ) THEN
          pfact = DONE / DBLE( RTPRN )
        ELSE
          pfact = DONE / DBLE( DELT )
        END IF
        ip = 1
        PTIME: DO n = 1, NUMTIME
          ISWRDT = n
          dt = SWRTIME(n)%SWRDT
C           UPDATE STRUCTURE VALUES FOR CURRENT TIME       
          DO irg = 1, NRCHGRP
            DO ic = 1, RCHGRP(irg)%NCONN
              irch = RCHGRP(irg)%IN(ic)
              CALL SWR_SET_STRVALS(irch,n)
            END DO
          END DO
          DO i = 1, NRCHGRP
C             UPDATE CURRENT REACH VALUES
            r   = SSWR_RG_QPR(RCHGRP(i))
            e   = SSWR_RG_QEV(RCHGRP(i),GSTAGE(i,n))
            b   = SSWR_RG_QAQ(RCHGRP(i),GSTAGE(i,n))
            qm  = SSWR_RG_QM(RCHGRP(i),GSTAGE(1,n))
            inf = SSWR_RG_QLI(RCHGRP(i))
            bc  = SSWR_RG_QBC(RCHGRP(i),GSTAGE(1,n))
            c   = SSWR_RG_QCS(RCHGRP(i),inf,r,e,b,qm,bc)
            vol = SSWR_RG_VOL(RCHGRP(i),GSTAGE(i,n))
            CALL SSWR_RG_UPDATE(RCHGRP(i),vol,r,e,b,bc,c)
C             SET TEMPORARY REACH GROUP VALUES
            s      = GSTAGE(i,n)
            qpos   = RCHGRP(i)%CURRENT%QMPOSFLOW
            latinf = RCHGRP(i)%CURRENT%QLATFLOW
            uzfinf = RCHGRP(i)%CURRENT%QUZFLOW
            r      = RCHGRP(i)%CURRENT%RAIN
            e      = RCHGRP(i)%CURRENT%EVAP
            b      = RCHGRP(i)%CURRENT%QAQFLOW
            qneg   = RCHGRP(i)%CURRENT%QMNEGFLOW
            ext    = RCHGRP(i)%CURRENT%QEXTFLOW
            bc     = RCHGRP(i)%CURRENT%QBCFLOW
            c      = RCHGRP(i)%CURRENT%QCRFLOW
            dv     = RCHGRP(i)%CURRENT%DVOLUME
            vol    = RCHGRP(i)%CURRENT%VOLUME
            IF ( ISWRSAVG.NE.0 ) THEN
              GSTAGEP(i,ip)                  = 
     2            GSTAGEP(i,ip)                 + s      * dt * pfact
              RCHGRP(i)%RSLTS(ip)%QMPOSFLOW = 
     2            RCHGRP(i)%RSLTS(ip)%QMPOSFLOW + qpos   * dt * pfact
              RCHGRP(i)%RSLTS(ip)%QLATFLOW  = 
     2            RCHGRP(i)%RSLTS(ip)%QLATFLOW  + latinf * dt * pfact
              RCHGRP(i)%RSLTS(ip)%QUZFLOW   = 
     2            RCHGRP(i)%RSLTS(ip)%QUZFLOW   + uzfinf * dt * pfact
              RCHGRP(i)%RSLTS(ip)%RAIN      = 
     2            RCHGRP(i)%RSLTS(ip)%RAIN      + r      * dt * pfact
              RCHGRP(i)%RSLTS(ip)%EVAP      = 
     2            RCHGRP(i)%RSLTS(ip)%EVAP      + e      * dt * pfact
              RCHGRP(i)%RSLTS(ip)%QAQFLOW   = 
     2            RCHGRP(i)%RSLTS(ip)%QAQFLOW   + b      * dt * pfact
              RCHGRP(i)%RSLTS(ip)%QMNEGFLOW =
     2            RCHGRP(i)%RSLTS(ip)%QMNEGFLOW + qneg   * dt * pfact
              RCHGRP(i)%RSLTS(ip)%QEXTFLOW  =
     2            RCHGRP(i)%RSLTS(ip)%QEXTFLOW  + ext    * dt * pfact
              RCHGRP(i)%RSLTS(ip)%QBCFLOW   = 
     2            RCHGRP(i)%RSLTS(ip)%QBCFLOW   + bc     * dt * pfact
              RCHGRP(i)%RSLTS(ip)%QCRFLOW   = 
     2            RCHGRP(i)%RSLTS(ip)%QCRFLOW   + c      * dt * pfact
              RCHGRP(i)%RSLTS(ip)%DVOLUME   = 
     2            RCHGRP(i)%RSLTS(ip)%DVOLUME   + dv     * dt * pfact
              RCHGRP(i)%RSLTS(ip)%VOLUME    = 
     2            RCHGRP(i)%RSLTS(ip)%VOLUME    + vol    * dt * pfact
            ELSE IF ( SWRTIME(n)%ITPRN.GT.0 ) THEN
              GSTAGEP(i,ip)                 = s
              RCHGRP(i)%RSLTS(ip)%QMPOSFLOW = qpos
              RCHGRP(i)%RSLTS(ip)%QMPOSFLOW = qpos
              RCHGRP(i)%RSLTS(ip)%QLATFLOW  = latinf
              RCHGRP(i)%RSLTS(ip)%QUZFLOW   = uzfinf
              RCHGRP(i)%RSLTS(ip)%RAIN      = r
              RCHGRP(i)%RSLTS(ip)%EVAP      = e
              RCHGRP(i)%RSLTS(ip)%QAQFLOW   = b
              RCHGRP(i)%RSLTS(ip)%QMNEGFLOW = qneg
              RCHGRP(i)%RSLTS(ip)%QEXTFLOW  = ext
              RCHGRP(i)%RSLTS(ip)%QBCFLOW   = bc
              RCHGRP(i)%RSLTS(ip)%QCRFLOW   = c
              RCHGRP(i)%RSLTS(ip)%DVOLUME   = dv
              RCHGRP(i)%RSLTS(ip)%VOLUME    = vol
            END IF
          END DO
          IF ( SWRTIME(n)%ITPRN.LT.1 ) CYCLE PTIME
          ip = ip + 1
          IF ( ip.GT.NPMAX ) EXIT PTIME !HACK
        END DO PTIME
C---------WRITE GROUP DATA FOR EACH SWR TIMESTEP
        ip = 1
        swrtot = DBLE( TOTIM - DELT )
        OTIME: DO n = 1, NUMTIME
          dt = SWRTIME(n)%SWRDT
          swrtot = swrtot + dt
          IF ( SWRTIME(n)%ITPRN.LT.1 ) CYCLE
          IF ( ISWRPRGF.GT.0 ) THEN
!            WRITE (line,2020) swrtot, dt, Kper, Kstp, n
          ELSE
            WRITE (iu)  swrtot, dt, Kper, Kstp, n
          END IF
          DO i = 1, NRCHGRP
            irg    = REACH(RCHGRP(i)%REACH(1))%IRG
            qpos   = RCHGRP(i)%RSLTS(ip)%QMPOSFLOW
            latinf = RCHGRP(i)%RSLTS(ip)%QLATFLOW
            uzfinf = RCHGRP(i)%RSLTS(ip)%QUZFLOW
            r      = RCHGRP(i)%RSLTS(ip)%RAIN
            e      = RCHGRP(i)%RSLTS(ip)%EVAP
            b      = RCHGRP(i)%RSLTS(ip)%QAQFLOW
            qneg   = RCHGRP(i)%RSLTS(ip)%QMNEGFLOW
            ext    = RCHGRP(i)%RSLTS(ip)%QEXTFLOW
            bc     = RCHGRP(i)%RSLTS(ip)%QBCFLOW
            c      = RCHGRP(i)%RSLTS(ip)%QCRFLOW
            dv     = RCHGRP(i)%RSLTS(ip)%DVOLUME
            f = qpos + latinf + uzfinf + r + e + b + qneg + ext +
     2          bc + c + dv
            IF ( ISWRPRGF.GT.0 ) THEN
              WRITE (iu,2040) swrtot, dt, Kper, Kstp, n, i, 
     2                        GSTAGEP(i,ip),
     3                        qpos, latinf, uzfinf, 
     4                        r, e, b, qneg, ext, bc, c, 
     5                        dv, f, RCHGRP(irg)%RSLTS(ip)%VOLUME
!              WRITE (dataline,2030) GSTAGEP(i,ip),
!     2                              qpos, latinf, uzfinf, 
!     3                              r, e, b, qneg, ext, bc, c, 
!     4                              dv, f, RCHGRP(irg)%RSLTS(ip)%VOLUME
!              line = TRIM(ADJUSTL(line))//TRIM(ADJUSTL(dataline))
            ELSE
              WRITE (iu) GSTAGEP(i,ip),
     2                   qpos, latinf, uzfinf,  
     3                   r, e, b, qneg, ext, bc, c, 
     4                   dv, f, RCHGRP(irg)%RSLTS(ip)%VOLUME
            END IF
C             RESET PRINT DATA FOR REACH GROUP AFTER PRINTING IF 
C             DATA IS BEING AVERAGED
            IF ( ISWRSAVG.NE.0 ) THEN
              GSTAGEP(i,ip)                 = DZERO
              RCHGRP(i)%RSLTS(ip)%QMPOSFLOW = DZERO
              RCHGRP(i)%RSLTS(ip)%QMPOSFLOW = DZERO
              RCHGRP(i)%RSLTS(ip)%QLATFLOW  = DZERO
              RCHGRP(i)%RSLTS(ip)%QUZFLOW   = DZERO
              RCHGRP(i)%RSLTS(ip)%RAIN      = DZERO
              RCHGRP(i)%RSLTS(ip)%EVAP      = DZERO
              RCHGRP(i)%RSLTS(ip)%QAQFLOW   = DZERO
              RCHGRP(i)%RSLTS(ip)%QMNEGFLOW = DZERO
              RCHGRP(i)%RSLTS(ip)%QEXTFLOW  = DZERO
              RCHGRP(i)%RSLTS(ip)%QBCFLOW   = DZERO
              RCHGRP(i)%RSLTS(ip)%QCRFLOW   = DZERO
              RCHGRP(i)%RSLTS(ip)%DVOLUME   = DZERO
              RCHGRP(i)%RSLTS(ip)%VOLUME    = DZERO
            END IF
          END DO
!          IF ( ISWRPRGF.GT.0 ) WRITE (iu,2010) TRIM(ADJUSTL(line))
C           INCREMENT PRINT COUNTER
          ip = ip + 1
          IF ( ip.GT.NPMAX ) EXIT OTIME !HACK
        END DO OTIME 
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_P_RGFLOW
C
C-------SUBROUTINE TO OUTPUT SUMMARY REACH STAGE DATA TO ISWRPSTG
      SUBROUTINE SSWR_P_RCHSTG(Kper,Kstp)
        USE GWFBASMODULE, ONLY: DELT, TOTIM
        USE GLOBAL,       ONLY: ISSFLG
        USE GWFSWRMODULE, ONLY: IZERO, RZERO, DZERO, DONE,
     2                          ISWRSAVG, RTPRN, NPMAX, NREACHES, REACH,
     3                          ISWRPSTG, ISWRPQAQ, ISWRSRIV, ISWRPSTR,
     4                          NUMTIME, SWRHEADER, SWRDT, 
     5                          ISWRDT, SWRTIME, RSTAGE, RSTAGEP
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Kper
        INTEGER, INTENT(IN) :: Kstp
C     + + + LOCAL DEFINITIONS + + +
        INTEGER              :: iu
        INTEGER              :: i, n
        INTEGER              :: ip
        DOUBLEPRECISION      :: swrtot, dt
        DOUBLEPRECISION      :: pfact
C     + + + FUNCTIONS + + +
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000    FORMAT('TOTIME         ,SWRDT          ,KPER      ,KSTP      ,',
     2         'KSWR      ,',100(10(:'STAGE',I10.10,',')))
2010    FORMAT(2(E15.7,','),3(I10,','),100(10(E15.7,',')))
C     + + + CODE + + +
        iu = ABS(ISWRPSTG)
C
C---------WRITE FILE HEADER IF FIRST CALL TO SWR BUDGET SUBROUTINE
        IF ( SWRHEADER.EQ.0 ) THEN
          IF ( ISWRPSTG.GT.IZERO ) THEN
            WRITE (iu,2000) (i,i=1,NREACHES)
          ELSE
            WRITE (iu) NREACHES
          END IF
        END IF
C---------FILL PRINT ARRAYS
        pfact = DONE
        IF ( RTPRN.NE.RZERO ) THEN
          pfact = DONE / DBLE( RTPRN )
        ELSE
          pfact = DONE / DBLE( DELT )
        END IF
        ip = 1
        PTIME: DO n = 1, NUMTIME
          dt = SWRTIME(n)%SWRDT
          DO i = 1, NREACHES
            IF ( ISWRSAVG.NE.0 ) THEN
              RSTAGEP(i,ip) = RSTAGEP(i,ip) + RSTAGE(i,n) * dt * pfact
            ELSE IF ( SWRTIME(n)%ITPRN.GT.0 ) THEN
              RSTAGEP(i,ip) = RSTAGE(i,n)
            END IF
          END DO
          IF ( SWRTIME(n)%ITPRN.LT.1 ) CYCLE PTIME
          ip = ip + 1
          IF ( ip.GT.NPMAX ) EXIT PTIME !HACK
        END DO PTIME
C---------WRITE REACH STAGE DATA FOR EACH SWR TIMESTEP
        ip = 1
        swrtot = DBLE( TOTIM - DELT )
        OTIME: DO n = 1, NUMTIME
          dt = SWRTIME(n)%SWRDT
          swrtot = swrtot + dt
          IF ( SWRTIME(n)%ITPRN.LT.1 ) CYCLE
          IF ( ISWRPSTG.GT.IZERO ) THEN
            WRITE (iu,2010) swrtot, dt, Kper, Kstp, n,
     2            (RSTAGEP(i,ip),i=1,NREACHES)
          ELSE
            WRITE (iu) swrtot, dt, Kper, Kstp, n,
     2            (RSTAGEP(i,ip),i=1,NREACHES)
          END IF
C           RESET PRINT DATA FOR REACH AFTER PRINTING IF 
C           DATA IS BEING AVERAGED
          IF ( ISWRSAVG.NE.0 ) THEN
            IF ( ISWRPQAQ.EQ.0 .AND. ISWRSRIV.EQ.0 .AND. 
     2           ISWRPSTR.EQ.0 ) THEN
              DO i = 1, NREACHES
                RSTAGEP(i,ip) = DZERO
              END DO
            END IF
          END IF
C           INCREMENT PRINT COUNTER
          ip = ip + 1
          IF ( ip.GT.NPMAX ) EXIT OTIME !HACK
        END DO OTIME
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_P_RCHSTG

C
C-------SUBROUTINE TO OUTPUT SUMMARY QAQFLOW DATA TO ISWRPQAQ
      SUBROUTINE SSWR_P_QAQFLOW(Kper,Kstp)
        USE GWFBASMODULE, ONLY: DELT, TOTIM
        USE GLOBAL,       ONLY: NLAY, IFREFM, ISSFLG, HNEW, HOLD
        USE GWFSWRMODULE, ONLY: IZERO, RZERO, DZERO, DONE, 
     2                          ISWRSRIV, ISWRALLRIV, ISWRSAVG, 
     3                          RTPRN, NPMAX, NREACHES, REACH, RCHGRP,
     4                          ISWRPSTG, ISWRPQAQ, ISWRPSTR,  
     5                          NUMTIME, SWRHEADER, ISWRDT, SWRDT,
     6                          SWRTIME, RSTAGE, RSTAGEP
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Kper
        INTEGER, INTENT(IN) :: Kstp
C     + + + LOCAL DEFINITIONS + + +
        INTEGER              :: iu
        INTEGER              :: iriv
        INTEGER              :: i, n
        INTEGER              :: ip
        INTEGER              :: irch
        INTEGER              :: irg
        INTEGER              :: kl, ir, jc
        INTEGER              :: mxactr, itmp
        DOUBLEPRECISION      :: swrtot, dt
        DOUBLEPRECISION      :: pfact
        DOUBLEPRECISION      :: q, rs
        DOUBLEPRECISION      :: h0, h1, h
        DOUBLEPRECISION      :: fdelt
        DOUBLEPRECISION      :: c
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_QAQ
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000    FORMAT('TOTIME    ,SWRDT     ,KPER      ,KSTP      ,',
     2         'KSWR      ,REACH     ,LAYER     ,GBELEV    ,',
     3         'STAGE     ,DEPTH     ,HEAD      ,WETPERM   ,',
     4         'CONDUCT   ,HEADDIFF  ,QAQFLOW')
2010    FORMAT(2(E10.3,','),5(I10,','),8(E10.3,','))
2020    FORMAT(3(1X,I9),F10.3,F10.3,F10.3,10X,'REACH',1X,I6.6)
2025    FORMAT(3(1X,I9),3(1X,G15.7),10X,'REACH',1X,I6.6)
2030    FORMAT(2(I10),10X,
     2         'STRESS PERIOD',1X,I5,1X,
     3         'TIME STEP',1X,I5,1X,
     4         'SWR1 TIME STEP',1X,I5,1X,
     5         'TOTIM',1X,G10.3)
C     + + + CODE + + +
        iu   = ABS( ISWRPQAQ )
        iriv = ABS( ISWRSRIV ) 
C---------WRITE FILE HEADER IF FIRST CALL TO SWR BUDGET SUBROUTINE
        IF ( SWRHEADER.EQ.0 ) THEN
          IF ( ISWRPQAQ.GT.IZERO ) THEN
            WRITE (iu,2000)
          ELSE IF ( ISWRPQAQ.LT.IZERO ) THEN
            WRITE (iu) NREACHES
          END IF
C-----------CALCULATE THE MAXIMUM NUMBER OF RIV CELLS
          IF ( iriv.GT.0 ) THEN
            mxactr = 0
            DO irch = 1, NREACHES
              IF ( REACH(irch)%KRCH.LT.0 ) THEN
                mxactr = mxactr + NLAY
              ELSE
                mxactr = mxactr + 1
              END IF
            END DO
          END IF        
C-----------WRITE DATASET 0 AND 2 TO THE MODFLOW RIV PACKAGE 
C           GENERATED BY SWR1
          IF ( ISWRSRIV.GT.0 ) THEN
            WRITE (iriv,'(A)') '#MODFLOW RIV PACKAGE CREATED BY SWR1'
            WRITE (iriv,2020) mxactr, 0
          END IF
        END IF
C---------DETERMINE THE NUMBER OF MODFLOW RIV CELLS FOR THIS STRESS PERIOD
        IF ( iriv.GT.0 ) THEN
          itmp = 0
          DO irch = 1, NREACHES
            IF ( REACH(irch)%ISWRBND.EQ.0 .AND. ISWRALLRIV.EQ.0 ) CYCLE
            DO kl = REACH(irch)%LAYSTR, REACH(irch)%LAYEND
              itmp = itmp + 1
            END DO
          END DO
        END IF
C---------FILL PRINT ARRAYS
        pfact = DONE
        IF ( RTPRN.NE.RZERO ) THEN
          pfact = DONE / DBLE( RTPRN )
        ELSE
          pfact = DONE / DBLE( DELT )
        END IF
        ip = 1
        PTIME: DO n = 1, NUMTIME
          dt     = SWRTIME(n)%SWRDT
          ISWRDT = n
          DO irch = 1, NREACHES
            ir=REACH(irch)%IRCH
            jc=REACH(irch)%JRCH
C-------------CALCULATE BASEFLOW FOR THIS TIME
            irg = REACH(irch)%IRG
            rs  = RSTAGE(irch,n)
            q   = SSWR_CALC_QAQ(REACH(irch),rs)
C-------------LOOP THROUGH EACH LAYER
            PLAYERS: DO kl = REACH(irch)%LAYSTR, REACH(irch)%LAYEND
              IF ( ISWRSAVG.NE.0 ) THEN
                REACH(irch)%QAQRSLTS(ip,kl)%WETTEDPERIMETER = 
     2            REACH(irch)%QAQRSLTS(ip,kl)%WETTEDPERIMETER + 
     3            REACH(irch)%CURRENTQAQ(kl)%WETTEDPERIMETER * dt*pfact
                REACH(irch)%QAQRSLTS(ip,kl)%CONDUCTANCE     = 
     2            REACH(irch)%QAQRSLTS(ip,kl)%CONDUCTANCE     + 
     3            REACH(irch)%CURRENTQAQ(kl)%CONDUCTANCE     * dt*pfact
                REACH(irch)%QAQRSLTS(ip,kl)%HEADDIFFERENCE  = 
     2            REACH(irch)%QAQRSLTS(ip,kl)%HEADDIFFERENCE  + 
     3            REACH(irch)%CURRENTQAQ(kl)%HEADDIFFERENCE  * dt*pfact
                REACH(irch)%QAQRSLTS(ip,kl)%QAQFLOW         = 
     2            REACH(irch)%QAQRSLTS(ip,kl)%QAQFLOW         + 
     3            REACH(irch)%CURRENTQAQ(kl)%QAQFLOW         * dt*pfact
              ELSE IF ( SWRTIME(n)%ITPRN.GT.0 ) THEN
                REACH(irch)%QAQRSLTS(ip,kl)%WETTEDPERIMETER = 
     2            REACH(irch)%CURRENTQAQ(kl)%WETTEDPERIMETER
                REACH(irch)%QAQRSLTS(ip,kl)%CONDUCTANCE     = 
     2            REACH(irch)%CURRENTQAQ(kl)%CONDUCTANCE
                REACH(irch)%QAQRSLTS(ip,kl)%HEADDIFFERENCE  = 
     2            REACH(irch)%CURRENTQAQ(kl)%HEADDIFFERENCE
                REACH(irch)%QAQRSLTS(ip,kl)%QAQFLOW         = 
     2            REACH(irch)%CURRENTQAQ(kl)%QAQFLOW
              END IF
            END DO PLAYERS
          END DO
          IF ( SWRTIME(n)%ITPRN.LT.1 ) CYCLE PTIME
          ip = ip + 1
          IF ( ip.GT.NPMAX ) EXIT PTIME !HACK
        END DO PTIME
C---------WRITE REACH QAQFLOW DATA FOR EACH SWR TIMESTEP
        ip = 1
        swrtot = DBLE( TOTIM - DELT )
        OTIME: DO n = 1, NUMTIME
          dt = SWRTIME(n)%SWRDT
          swrtot = swrtot + dt
          IF ( SWRTIME(n)%ITPRN.LT.1 ) CYCLE
          IF ( ISWRPQAQ.LT.IZERO ) THEN
C             WRITE HEADER THAT DEFINES THE NUMBER OF LAYERS
C             QAQ DATA IS WRITTEN FOR EACH REACH
            DO irch = 1, NREACHES
              kl = REACH(irch)%LAYEND - REACH(irch)%LAYSTR + 1
              WRITE (iu) kl
            END DO
            WRITE (iu) swrtot, dt, Kper, Kstp, n
          END IF
C           WRITE DATASET 5 FOR MODFLOW RIV PACKAGE
          IF ( ISWRSRIV.GT.0 ) THEN
            WRITE (iriv,2030) itmp, 0, Kper, Kstp, n, swrtot
          END IF
          DO irch = 1, NREACHES
            ir=REACH(irch)%IRCH
            jc=REACH(irch)%JRCH
!            IF ( ISWRPQAQ.LT.IZERO ) THEN
!              WRITE (iu) REACH(irch)%LAYEND - REACH(irch)%LAYSTR + 1
!            END IF
            LAYERS: DO kl = REACH(irch)%LAYSTR, REACH(irch)%LAYEND
              h0    = REAL( HOLD(jc,ir,kl), 8 )
              h1    = HNEW(jc,ir,kl)
              fdelt = SWRTIME(n)%FDELT
              h     = ( DONE - fdelt ) * h0 + fdelt * h1
              IF ( ISWRPQAQ.GT.IZERO ) THEN
                WRITE (iu,2010) swrtot, dt, Kper, Kstp,
     2             n, irch, kl, REACH(irch)%GBELEV, 
     3             RSTAGEP(irch,ip),
     3             RSTAGEP(irch,ip) - REACH(irch)%GBELEV, h,
     4             REACH(irch)%QAQRSLTS(ip,kl)%WETTEDPERIMETER,
     5             REACH(irch)%QAQRSLTS(ip,kl)%CONDUCTANCE,
     6             REACH(irch)%QAQRSLTS(ip,kl)%HEADDIFFERENCE,
     7             REACH(irch)%QAQRSLTS(ip,kl)%QAQFLOW
              ELSE IF ( ISWRPQAQ.LT.IZERO ) THEN
                WRITE (iu) kl, REACH(irch)%GBELEV, 
     2             RSTAGEP(irch,ip),
     3             RSTAGEP(irch,ip) - REACH(irch)%GBELEV, h,
     4             REACH(irch)%QAQRSLTS(ip,kl)%WETTEDPERIMETER,
     5             REACH(irch)%QAQRSLTS(ip,kl)%CONDUCTANCE,
     6             REACH(irch)%QAQRSLTS(ip,kl)%HEADDIFFERENCE,
     7             REACH(irch)%QAQRSLTS(ip,kl)%QAQFLOW
              END IF
C               WRITE DATASET 6 FOR MODFLOW RIV PACKAGE
              IF ( ISWRSRIV.GT.0 ) THEN
                c = REACH(irch)%QAQRSLTS(ip,kl)%CONDUCTANCE
                IF ( REACH(irch)%ISWRBND.EQ.0 .AND. 
     2               ISWRALLRIV.NE.0 ) THEN
                  c = DZERO
                END IF
                IF ( REACH(irch)%ISWRBND.NE.0 .OR.
     2               ( REACH(irch)%ISWRBND.EQ.0 .AND.
     3                 ISWRALLRIV.NE.0 ) ) THEN
                  IF ( IFREFM.EQ.0 ) THEN
                    WRITE (iriv,2020) kl, ir, jc, 
     2                RSTAGEP(irch,ip),
     3                c, REACH(irch)%GBELEV, irch
                  ELSE
                    WRITE (iriv,2025) kl, ir, jc, 
     2                RSTAGEP(irch,ip),
     3                c, REACH(irch)%GBELEV, irch
                  END IF
                END IF
              END IF
C               RESET PRINT DATA FOR REACH AFTER PRINTING IF 
C               DATA IS BEING AVERAGED
              IF ( ISWRSAVG.NE.0 ) THEN
                IF ( ISWRPSTR.EQ.0 ) THEN
                  RSTAGEP(irch,ip) = DZERO
                END IF
                REACH(irch)%QAQRSLTS(ip,kl)%WETTEDPERIMETER = DZERO
                REACH(irch)%QAQRSLTS(ip,kl)%CONDUCTANCE     = DZERO
                REACH(irch)%QAQRSLTS(ip,kl)%HEADDIFFERENCE  = DZERO
                REACH(irch)%QAQRSLTS(ip,kl)%QAQFLOW         = DZERO
              END IF
            END DO LAYERS
          END DO
C           INCREMENT PRINT COUNTER
          ip = ip + 1
          IF ( ip.GT.NPMAX ) EXIT OTIME !HACK
        END DO OTIME 
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_P_QAQFLOW
C
C-------SUBROUTINE TO OUTPUT LATERAL FLOW DATA TO ISWRPQM
      SUBROUTINE SSWR_P_QMFLOW(Kper,Kstp)
        USE GWFBASMODULE, ONLY: DELT, TOTIM
        USE GLOBAL,       ONLY: ISSFLG
        USE GWFSWRMODULE, ONLY: IZERO, RZERO, DZERO, DONE, 
     2                          ISWRSAVG, RTPRN, NPMAX, NRCHGRP, RCHGRP,
     3                          NUMTIME, ISWRPQM, SWRHEADER, SWRDT, 
     4                          ISWRDT, SWRTIME, GSTAGE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Kper
        INTEGER, INTENT(IN) :: Kstp
C     + + + LOCAL DEFINITIONS + + +
        INTEGER              :: iu
        INTEGER              :: n
        INTEGER              :: irg, ic
        INTEGER              :: irch
        INTEGER              :: nconn, nrgout, ntconn
        INTEGER              :: ip
        DOUBLEPRECISION      :: swrtot, dt
        DOUBLEPRECISION      :: qm
        DOUBLEPRECISION      :: pfact
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION      :: SSWR_RG_QM

C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000    FORMAT('TOTIME         ,SWRDT          ,KPER      ,KSTP      ,',
     2         'KSWR      ,RCHGRP    ,REACHC    ,CONNREACH ,',
     3         'QLATFLOW       ,VLATFLOW        ')
2010    FORMAT(2(E15.7,','),6(I10,','),E15.7,',',E15.7)
C     + + + CODE + + +
        iu = ABS( ISWRPQM )
C
C---------WRITE FILE HEADER IF FIRST CALL TO SWR BUDGET SUBROUTINE
        IF ( SWRHEADER.EQ.0 ) THEN
          IF ( ISWRPQM.GT.IZERO ) THEN
            WRITE (iu,2000)
          ELSE
             nrgout = IZERO
             ntconn = IZERO
             DO irg = 1, NRCHGRP
               nconn = RCHGRP(irg)%NCONN
               IF ( nconn.GT.0 ) THEN
                 nrgout = nrgout + 1
                 ntconn = ntconn + nconn
               END IF
             END DO
            WRITE (iu) nrgout, ntconn
             DO irg = 1, NRCHGRP
               nconn = RCHGRP(irg)%NCONN
               IF ( nconn.GT.0 ) THEN
                 WRITE(iu) nconn
                 DO ic = 1, nconn
                    WRITE (iu)
     2               ABS( RCHGRP(irg)%IRCHN(ic) ),
     3               ABS( RCHGRP(irg)%IRCHC(ic) )
                 END DO
                 nrgout = nrgout + 1
                 ntconn = ntconn + nconn
               END IF
             END DO
            
          END IF
        END IF
C---------FILL PRINT ARRAYS
        pfact = DONE
        IF ( RTPRN.NE.RZERO ) THEN
          pfact = DONE / DBLE( RTPRN )
        ELSE
          pfact = DONE / DBLE( DELT )
        END IF
        ip = 1
        PTIME: DO n = 1, NUMTIME
          ISWRDT = n
          dt     = SWRTIME(n)%SWRDT
C             UPDATE STRUCTURE VALUES          
          DO irg = 1, NRCHGRP
            DO ic = 1, RCHGRP(irg)%NCONN
              irch = RCHGRP(irg)%IN(ic)
              CALL SWR_SET_STRVALS(irch,n)
            END DO
          END DO
          DO irg = 1, NRCHGRP
C             UPDATE CURRENT QM VALUES
            qm  = SSWR_RG_QM(RCHGRP(irg),GSTAGE(1,n))
            DO ic = 1, RCHGRP(irg)%NCONN
              IF ( ISWRSAVG.NE.0 ) THEN
                RCHGRP(irg)%QCONNP(ic,ip)%FLOW = 
     2            RCHGRP(irg)%QCONNP(ic,ip)%FLOW + 
     3            RCHGRP(irg)%QCONN(ic)%FLOW * dt * pfact
                RCHGRP(irg)%QCONNP(ic,ip)%VELOCITY = 
     2            RCHGRP(irg)%QCONNP(ic,ip)%VELOCITY + 
     3            RCHGRP(irg)%QCONN(ic)%VELOCITY * dt * pfact
              ELSE IF ( SWRTIME(n)%ITPRN.GT.0 ) THEN
                RCHGRP(irg)%QCONNP(ic,ip)%FLOW = 
     2            RCHGRP(irg)%QCONN(ic)%FLOW
                RCHGRP(irg)%QCONNP(ic,ip)%VELOCITY = 
     2            RCHGRP(irg)%QCONN(ic)%VELOCITY
              END IF
            END DO
          END DO
          IF ( SWRTIME(n)%ITPRN.LT.1 ) CYCLE PTIME
          ip = ip + 1
          IF ( ip.GT.NPMAX ) EXIT PTIME !HACK
        END DO PTIME
C---------WRITE LATERAL FLOW DATA FOR EACH SWR TIMESTEP
        ip = 1
        swrtot = DBLE( TOTIM - DELT )
        VTIME: DO n = 1, NUMTIME
          dt = SWRTIME(n)%SWRDT
          swrtot = swrtot + dt
          IF ( SWRTIME(n)%ITPRN.LT.1 ) CYCLE VTIME
          IF ( ISWRPQM.LT.IZERO ) THEN
            WRITE (iu) swrtot, dt, Kper, Kstp, n
          END IF
          EACHGRP: DO irg = 1, NRCHGRP
            IF ( RCHGRP(irg)%NCONN.LT.1 ) CYCLE EACHGRP
            EACHCONN: DO ic = 1, RCHGRP(irg)%NCONN
              IF ( ISWRPQM.GT.IZERO ) THEN
                WRITE (iu,2010) swrtot, dt, Kper, Kstp, n, irg,
     2               ABS( RCHGRP(irg)%IRCHN(ic) ), 
     3               ABS( RCHGRP(irg)%IRCHC(ic) ),
     4               RCHGRP(irg)%QCONNP(ic,ip)%FLOW,
     5               RCHGRP(irg)%QCONNP(ic,ip)%VELOCITY
              ELSE
                WRITE (iu) 
     2               RCHGRP(irg)%QCONNP(ic,ip)%FLOW,
     3               RCHGRP(irg)%QCONNP(ic,ip)%VELOCITY
              END IF
C               RESET PRINT DATA FOR REACH GROUP AFTER PRINTING IF 
C               DATA IS BEING AVERAGED
              IF ( ISWRSAVG.NE.0 ) THEN
                RCHGRP(irg)%QCONNP(ic,ip)%FLOW     = DZERO
                RCHGRP(irg)%QCONNP(ic,ip)%VELOCITY = DZERO
              END IF
            END DO EACHCONN
          END DO EACHGRP
C           INCREMENT PRINT COUNTER
          ip = ip + 1
          IF ( ip.GT.NPMAX ) EXIT VTIME !HACK
        END DO VTIME
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_P_QMFLOW
C
C-------SUBROUTINE TO OUTPUT SUMMARY STRUCTURE FLOW DATA TO ISWRPSTR
      SUBROUTINE SSWR_P_STRFLOW(Kper,Kstp)
        USE GWFBASMODULE, ONLY: DELT, TOTIM
        USE GWFSWRMODULE, ONLY: IZERO, RZERO, DZERO, DONE, DMISSING,
     2                          ISWRSAVG, RTPRN, NPMAX, NREACHES, REACH,
     3                          ISWRPSTG, ISWRPQAQ, ISWRPSTR,  
     4                          NUMTIME, SWRHEADER, SWRDT,
     5                          ISWRDT, SWRTIME, RSTAGE, GSTAGE, RSTAGEP
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Kper
        INTEGER, INTENT(IN) :: Kstp
C     + + + LOCAL DEFINITIONS + + +
        INTEGER              :: iu
        INTEGER              :: i, n
        INTEGER              :: ip
        INTEGER              :: irch
        INTEGER              :: istr
        INTEGER              :: istrtype
        INTEGER              :: istrconn
        DOUBLEPRECISION      :: swrtot, dt
        DOUBLEPRECISION      :: pfact
        DOUBLEPRECISION      :: t, b, o
        DOUBLEPRECISION      :: sus, sds, val, q
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION      :: SSWR_CALC_QM
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000    FORMAT('TOTIME    ,SWRDT     ,KPER      ,KSTP      ,',
     2         'KSWR      ,REACH     ,STRUCTURE ,USSTAGE   ,',
     3         'DSSTAGE   ,GATEELEV  ,OPENING   ,STRFLOW    ')
2010    FORMAT(2(E10.3,','),5(I10,','),8(E10.3,','))
C     + + + CODE + + +
        iu = ABS( ISWRPSTR )
C---------WRITE FILE HEADER IF FIRST CALL TO SWR BUDGET SUBROUTINE
        IF ( SWRHEADER.EQ.0 ) THEN
          IF ( ISWRPSTR.GT.IZERO ) THEN
            WRITE (iu,2000)
          ELSE
            WRITE (iu) NREACHES
!            DO irch = 1, NREACHES
!              WRITE (iu) REACH(irch)%NSTRUCT
!            END DO
          END IF
        END IF
C---------FILL PRINT ARRAYS
        pfact = DONE
        IF ( RTPRN.NE.RZERO ) THEN
          pfact = DONE / DBLE( RTPRN )
        ELSE
          pfact = DONE / DBLE( DELT )
        END IF
        ip = 1
        PTIME: DO n = 1, NUMTIME
          ISWRDT = n
          dt = SWRTIME(n)%SWRDT
          DO irch = 1, NREACHES
            CALL SWR_SET_STRVALS(irch,n)
            q = SSWR_CALC_QM(irch,GSTAGE(1,n))
            DO istr = 1, REACH(irch)%NSTRUCT
              istrtype = REACH(irch)%STRUCT(istr)%ISTRTYPE
              t = DZERO
              b = DZERO
              o = DZERO
              SELECT CASE ( ABS(istrtype) )
                CASE ( 1 )
                CASE ( 2 )
                CASE ( 3 )
                CASE ( 4 )
                CASE ( 5 )
                CASE ( 6 )
                  t = REACH(irch)%STRUCT(istr)%STRINV
                  b = t
                CASE ( 7 )
                  t = REACH(irch)%STRUCT(istr)%STRTOP
                  b = REACH(irch)%STRUCT(istr)%STRINV
                  o   = t - b
                CASE ( 8 )
                  t = REACH(irch)%STRUCT(istr)%CURRENT%BOTTOM
                  b = t
                CASE ( 9,10 )
                  t = REACH(irch)%STRUCT(istr)%CURRENT%TOP
                  b = REACH(irch)%STRUCT(istr)%STRINV
                  o = t - b
              END SELECT
              IF ( ISWRSAVG.NE.0 ) THEN
                REACH(irch)%STRUCT(istr)%RSLTS(ip)%TOP     =
     2            REACH(irch)%STRUCT(istr)%RSLTS(ip)%TOP     + 
     3            t * dt * pfact
                REACH(irch)%STRUCT(istr)%RSLTS(ip)%BOTTOM  =
     2            REACH(irch)%STRUCT(istr)%RSLTS(ip)%BOTTOM  + 
     3            b * dt * pfact
                REACH(irch)%STRUCT(istr)%RSLTS(ip)%OPENING =
     2            REACH(irch)%STRUCT(istr)%RSLTS(ip)%OPENING + 
     3            o * dt * pfact
                REACH(irch)%STRUCT(istr)%RSLTS(ip)%FLOW    =
     2            REACH(irch)%STRUCT(istr)%RSLTS(ip)%FLOW    + 
     3            REACH(irch)%STRUCT(istr)%CURRENT%FLOW * dt * pfact
              ELSE IF ( SWRTIME(n)%ITPRN.GT.0 ) THEN
                REACH(irch)%STRUCT(istr)%RSLTS(ip)%TOP     = t
                REACH(irch)%STRUCT(istr)%RSLTS(ip)%BOTTOM  =
     2            REACH(irch)%STRUCT(istr)%RSLTS(ip)%BOTTOM  + b
                REACH(irch)%STRUCT(istr)%RSLTS(ip)%OPENING = o
                REACH(irch)%STRUCT(istr)%RSLTS(ip)%FLOW    =
     2            REACH(irch)%STRUCT(istr)%CURRENT%FLOW
              END IF
            END DO
          END DO
          IF ( SWRTIME(n)%ITPRN.LT.1 ) CYCLE PTIME
          ip = ip + 1
          IF ( ip.GT.NPMAX ) EXIT PTIME !HACK
        END DO PTIME
C---------WRITE STRUCTURE FLOW FOR EACH SWR TIMESTEP
        ip = 1
        swrtot = DBLE( TOTIM - DELT )
        OTIME: DO n = 1, NUMTIME
          dt = SWRTIME(n)%SWRDT
          swrtot = swrtot + dt
          IF ( SWRTIME(n)%ITPRN.LT.1 ) CYCLE
          IF ( ISWRPSTR.LT.IZERO ) THEN
C             WRITE HEADER THAT DEFINES THE NUMBER OF STRUCTURES
C             DATA THAT ARE WRITTEN FOR EACH REACH
            DO irch = 1, NREACHES
              WRITE (iu) REACH(irch)%NSTRUCT
            END DO
            WRITE (iu) swrtot, dt, Kper, Kstp, n
          END IF
          DO irch = 1, NREACHES
            DO istr = 1, REACH(irch)%NSTRUCT
              istrtype = REACH(irch)%STRUCT(istr)%ISTRTYPE
              istrconn = REACH(irch)%STRUCT(istr)%ISTRCONN
              sus = RSTAGEP(irch,ip)
              IF ( istrconn.GT.0 ) THEN
                sds = RSTAGEP(istrconn,ip)
              ELSE
                sds = DMISSING
              END IF
              val = DZERO
              o   = DZERO
              SELECT CASE ( ABS(istrtype) )
                CASE ( 1 )
                CASE ( 2 )
                CASE ( 3 )
                CASE ( 4 )
                CASE ( 5 )
                CASE ( 6,8 )
                  val = REACH(irch)%STRUCT(istr)%RSLTS(ip)%BOTTOM
                CASE ( 7,9,10 )
                  val = REACH(irch)%STRUCT(istr)%RSLTS(ip)%TOP
                  o   = REACH(irch)%STRUCT(istr)%RSLTS(ip)%OPENING
              END SELECT
              q = REACH(irch)%STRUCT(istr)%RSLTS(ip)%FLOW
              IF ( ISWRPSTR.GT.IZERO ) THEN
                WRITE (iu,2010) swrtot, dt, Kper, Kstp,
     2             n, irch, istr, sus, sds, val, o, q
              ELSE
                WRITE (iu) sus, sds, val, o, q
              END IF
            END DO
          END DO
C           RESET PRINT DATA FOR REACH AFTER PRINTING IF 
C           DATA IS BEING AVERAGED
          IF ( ISWRSAVG.NE.0 ) THEN
            DO i = 1, NREACHES
              RSTAGEP(i,ip) = DZERO
            END DO
          END IF
C           INCREMENT PRINT COUNTER
          ip = ip + 1
          IF ( ip.GT.NPMAX ) EXIT OTIME !HACK
        END DO OTIME  
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_P_STRFLOW
C
C-------SUBROUTINE TO OUTPUT SUMMARY FROUDE NUMBER DATA TO IOUT
      SUBROUTINE SSWR_P_FROUDE(Kper,Kstp)
        USE GWFBASMODULE, ONLY: DELT, TOTIM
        USE GLOBAL,       ONLY: IOUT,NPER,NSTP
        USE GWFSWRMODULE, ONLY: IZERO, DZERO, DONE, GRAVITY,
     2                          DLENCONV, TIMECONV, NUMTIME,
     3                          REACH, NRCHGRP, RCHGRP,
     4                          ISWRDT, SWRTIME, FROUDE, GSTAGE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Kper
        INTEGER, INTENT(IN) :: Kstp
C     + + + LOCAL DEFINITIONS + + +
        INTEGER              :: i, j, n
        INTEGER              :: irg
        INTEGER              :: jc
        INTEGER              :: locdfr
        INTEGER              :: locdfrdt
        INTEGER              :: locdfrdl
        DOUBLEPRECISION      :: dt
        DOUBLEPRECISION      :: qm
        DOUBLEPRECISION      :: g,v,v0,b,b0
        DOUBLEPRECISION      :: frmax
        DOUBLEPRECISION      :: dfrdt, dfrdtmax
        DOUBLEPRECISION      :: frpos, frneg
        DOUBLEPRECISION      :: dfrdl, dfrdlmax
        DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: fr0, fr
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION      :: SSWR_RG_QM
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000    FORMAT(//,29X,'FROUDE NUMBER SUMMARY',/
     2            15X,'FOR STRESS PERIOD',I10,1X,'TIME STEP',I10,/,
     3             1X,'  SWR STEP',
     4             1X,'    MAX FR',1X,'   FR RGRP',
     5             1X,' MAX DFRDX',1X,'DFRDX RGRP',
     6             1X,' MAX DFRDT',1X,'DFRDT RGRP',/,
     7             1X,7(10('-'),1X)) 
2010    FORMAT(1X,I10,
     2         1X,G10.3,1X,I10,
     3         1X,G10.3,1X,I10,
     4         1X,G10.3,1X,I10)
2020    FORMAT(//,18X,'FROUDE NUMBER SUMMARY FOR ENTIRE SIMULATION',/,
     2        11X,1X,'    MAX FR',1X,'   FR RGRP',
     3            1X,' MAX DFRDX',1X,'DFRDX RGRP',
     4            1X,' MAX DFRDT',1X,'DFRDT RGRP',/,
     5        11X,1X,6(10('-'),1X)) 
2030    FORMAT(1X,10X,
     2         1X,G10.3,1X,I10,
     3         1X,G10.3,1X,I10,
     4         1X,G10.3,1X,I10)
C     + + + CODE + + +
        g = GRAVITY * DLENCONV * TIMECONV * TIMECONV
        WRITE (IOUT,2000) Kper,Kstp
        TME: DO n = 1, NUMTIME
          frmax    = DZERO
          dfrdlmax = DZERO
          dfrdtmax = DZERO
          dfrdt    = DZERO
          dfrdl    = DZERO
          locdfr   = IZERO
          locdfrdt = IZERO
          locdfrdl = IZERO
          ISWRDT = n
          dt     = SWRTIME(n)%SWRDT
          RG: DO i = 1, NRCHGRP
            IF ( RCHGRP(i)%CONSTANT ) CYCLE RG
            IF ( .NOT.RCHGRP(i)%DIFFUSIVE ) CYCLE RG
            irg = REACH(RCHGRP(i)%REACH(1))%IRG
            IF ( RCHGRP(irg)%NCONN.LT.1 ) CYCLE RG
C             ALLOCATE TEMPORARY STORAGE FOR REACH GROUP
            ALLOCATE ( fr0(RCHGRP(irg)%NCONN) )
            ALLOCATE (  fr(RCHGRP(irg)%NCONN) )
            fr0 = DZERO
            fr  = DZERO
C             UPDATE CURRENT QM VALUES TO PREVIOUS VALUES
            qm  = SSWR_RG_QM(RCHGRP(irg),GSTAGE(1,n-1))
            DO j = 1, RCHGRP(irg)%NCONN
              v0 = RCHGRP(irg)%QCONN(j)%VELOCITY
              b0 = RCHGRP(irg)%QCONN(j)%DEPTH
              IF ( b0.GT.DZERO ) THEN
                fr0(j) = ABS( v0 ) / SQRT( g * b0 )
              END IF
            END DO
C             UPDATE CURRENT QM VALUES
            qm  = SSWR_RG_QM(RCHGRP(irg),GSTAGE(1,n))
            DO j = 1, RCHGRP(irg)%NCONN
              jc  = RCHGRP(irg)%IRCHN(j) 
              IF ( jc.LT.1 ) CYCLE  !STRUCTURES
              v = RCHGRP(irg)%QCONN(j)%VELOCITY
              b = RCHGRP(irg)%QCONN(j)%DEPTH
              IF ( b.GT.DZERO ) THEN
                fr(j)  = ABS( v )  / SQRT( g * b  )
              END IF
            END DO
            DO j = 1, RCHGRP(irg)%NCONN
C               DETERMINE IF MAX FROUDE NUMBER              
              IF ( fr(j).GT.frmax ) THEN
                frmax  = fr(j)
                locdfr = irg
              END IF
              IF ( fr(j).GT.FROUDE%VAL ) THEN
                FROUDE%VAL    = fr(j)
                FROUDE%LOCVAL = irg
              END IF
C               DETERMINE IF MAX CHANGE IN FROUDE NUMBER WITH TIME
              dfrdt = ABS( fr(j) - fr0(j) )
              IF ( fr(j).GT.DZERO .AND. fr0(j).GT.DZERO ) THEN
                IF ( dfrdt.GT.dfrdtmax ) THEN
                  dfrdtmax = dfrdt
                  locdfrdt = irg
                END IF
                IF ( dfrdt.GT.FROUDE%DVALDT ) THEN
                  FROUDE%DVALDT    = dfrdt
                  FROUDE%LOCDVALDT = irg
                END IF
              END IF
            END DO
C             FIND MAXIMUM CHANGE IN FROUDE NUMBER BETWEEN CONNECTIONS
            frpos = DZERO
            frneg = DZERO
            DO j = 1, RCHGRP(irg)%NCONN
              IF ( RCHGRP(irg)%QCONN(j)%FLOW.GT.DZERO ) THEN
                frpos = MAX( frpos, fr(j) )
              ELSE
                frneg = MAX( frneg, fr(j) )
              END IF
            END DO
            dfrdl = ABS( frpos - frneg )
            IF ( dfrdl.GT.dfrdlmax ) THEN
              dfrdlmax  = dfrdl
              locdfrdl  = irg
            END IF
            IF ( dfrdl.GT.FROUDE%DVALDL ) THEN
              FROUDE%DVALDL    = dfrdl
              FROUDE%LOCDVALDL = irg
            END IF
C             CLEAN UP TEMPORARY STORAGE FOR REACH GROUP
            DEALLOCATE(  fr )
            DEALLOCATE( fr0 )
          END DO RG
C-----------WRITE SUMMARY FOR TIMESTEP
          WRITE (IOUT,2010) n,
     2      frmax,locdfr,
     3      dfrdlmax,locdfrdl,
     4      dfrdtmax,locdfrdt
        END DO TME 
C
C---------WRITE SUMMARY FOR ENTIRE SIMULATION        
        IF ( Kper.EQ.NPER ) THEN
          IF ( Kstp.EQ.NSTP(NPER) ) THEN
            n = 0
            WRITE (IOUT,2020)
            WRITE (IOUT,2030) 
     2        FROUDE%VAL,FROUDE%LOCVAL,
     3        FROUDE%DVALDL,FROUDE%LOCDVALDL,
     4        FROUDE%DVALDT,FROUDE%LOCDVALDT
          END IF
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_P_FROUDE
C
C  SURFACE-WATER ROUTING CALCULATION FUNCTIONS
C
C-------PASSED VOLUME WILL ALWAYS RETURN STAGE AT DEFINED "DOWNSTREAM" 
C       REACH OF REACH GROUP
      DOUBLEPRECISION FUNCTION SSWR_RG_STG(Rg,V) RESULT(s)
        USE GWFSWRMODULE, ONLY: IZERO, DZERO, DTWO, TRCHGRP, REACH
        USE GWFSWRINTERFACE, ONLY: SSWR_LININT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        TYPE (TRCHGRP), INTENT(IN)  :: Rg
        DOUBLEPRECISION, INTENT(IN) :: V
C     + + + LOCAL DEFINITIONS + + +
        INTEGER          :: i
        INTEGER          :: nr
        INTEGER          :: irch
        INTEGER          :: ibs
        DOUBLEPRECISION  :: e
        DOUBLEPRECISION  :: mins, maxs, maxoff
        DOUBLEPRECISION  :: ru, rl, r
        DOUBLEPRECISION  :: ts, ts0, tv
        DOUBLEPRECISION  :: gsu, gsl
        DOUBLEPRECISION  :: vu,  vl
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION  :: SSWR_RG_VOL
C     + + + CODE + + +
        s      = DZERO
        e      = EPSILON(DZERO)
        mins   =  1.0D9
        maxs   = -1.0D9
        maxoff = -1.0D9
        ts0    =  1.0D9
C---------FIND MINIMUM STAGE, MAXIMUM STAGE, AND MAXIMUM OFFSET
        DO nr = 1, Rg%NRGREACH  
          irch = Rg%REACH(nr)
          IF (REACH(irch)%OFFSET.GT.maxoff) THEN
            maxoff = REACH(irch)%OFFSET
          END IF
          DO i = 1, REACH(irch)%GEO%NGEOPTS
            IF (REACH(irch)%GEO%ELEV(i).LT.mins) THEN
              mins = REACH(irch)%GEO%ELEV(i)
            END IF
            IF (REACH(irch)%GEO%ELEV(i).GT.maxs) THEN
              maxs = REACH(irch)%GEO%ELEV(i)
            END IF
          END DO
        END DO
        gsu = maxs + maxoff
        vu  = SSWR_RG_VOL(Rg,gsu)
        ru  = V - vu
        IF (ABS(ru).LT.e) THEN
          s = gsu
          RETURN
        END IF
        gsl = mins
        vl  = SSWR_RG_VOL(Rg,gsl)
        rl  = V-vl
        IF (ABS(rl).LT.e) THEN
          s = gsl
          RETURN
        END IF
C---------USE BI-SECTION TO FIND GROUP STAGE
        ibs = IZERO
        DO
          ibs = ibs + 1
          ts = gsu + (gsl - gsu) / DTWO
          tv = SSWR_RG_VOL(Rg,ts)
          r  = V - tv
          IF (ABS(r).LT.e) THEN
            s = ts
            RETURN
          END IF
C           CHECK THAT PROGRESS IS BEING MADE ON STAGE
          IF (ABS(ts-ts0).LT.e) THEN
            s = ts
            RETURN
          END IF
C           CHECK FOR INFINITE LOOP
          IF (ibs.GT.100) THEN
            s = ts
            RETURN
          END IF          
          IF ( (rl * r).GT.DZERO ) THEN
            gsl = ts
            vl  = tv
            rl  = r
          ELSE
            gsu = ts
            vu  = tv
            ru  = r
          END IF
          ts0 = ts
        END DO
C--------RETURN
        RETURN
      END FUNCTION SSWR_RG_STG
C
C-------PASSED STAGE WILL ALWAYS BE STAGE AT DOWNSTREAM END OF GROUP
      DOUBLEPRECISION FUNCTION SSWR_RG_VOL(Rg,St) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, TRCHGRP, REACH
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        TYPE (TRCHGRP), INTENT(IN)   :: Rg
        DOUBLEPRECISION, INTENT(IN)  :: St
C     + + + LOCAL DEFINITIONS + + +
        integer (kind=4) :: nr
        integer (kind=4) :: irch
        DOUBLEPRECISION  :: rs
        DOUBLEPRECISION  :: vr
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION  :: SSWR_CALC_VOL
C     + + + CODE + + +
        value = DZERO
        DO nr = 1, Rg%NRGREACH  
          irch  = Rg%REACH(nr)
          rs    = St + REACH(irch)%OFFSET
          vr    = SSWR_CALC_VOL(irch,rs)
          value = value + vr
        END DO
C---------RETURN
        RETURN
      END FUNCTION SSWR_RG_VOL

      DOUBLEPRECISION FUNCTION SSWR_CALC_VOL(Irch,Rs) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, REACH
        USE GWFSWRINTERFACE, ONLY: SSWR_LININT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN)  :: Irch
        DOUBLEPRECISION, INTENT(IN)  :: Rs
C     + + + LOCAL DEFINITIONS + + +
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        value = SSWR_LININT(REACH(Irch)%GEO%ELEV,
     2                      REACH(Irch)%GEO%VOL,Rs)
C---------RETURN
        RETURN
      END FUNCTION SSWR_CALC_VOL
C
C-------INFLOW IS SPECIFIED LATERAL INFLOW (QLATFLOW), INFLOW FROM THE UZF 
C       PACKAGE (QUZFLOW), AND INFLOW FROM EXTERNAL SOURCES (QEXTFLOW).
      DOUBLEPRECISION FUNCTION SSWR_RG_QLI(Rg) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, REACH, TRCHGRP !, QUZFLOW, QEXTFLOW
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        TYPE (TRCHGRP), INTENT(IN) :: Rg
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: nr
        INTEGER :: irch
        DOUBLEPRECISION :: latinf, uzfinf, extinf
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_QLI
        DOUBLEPRECISION :: SSWR_CALC_QUZ
        DOUBLEPRECISION :: SSWR_CALC_QEX
C     + + + CODE + + +
        value = DZERO
C         FILL WITH CURRENT ESTIMATE
        DO nr = 1, Rg%NRGREACH  
          latinf = DZERO
          uzfinf = DZERO
          irch   = Rg%REACH(nr)
          latinf = SSWR_CALC_QLI(irch)
          uzfinf = SSWR_CALC_QUZ(irch)
          extinf = SSWR_CALC_QEX(irch)
          value  = value + latinf + uzfinf + extinf
        END DO
C---------RETURN
        RETURN
      END FUNCTION SSWR_RG_QLI

      DOUBLEPRECISION FUNCTION SSWR_CALC_QLI(Irch) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, REACH
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Irch
C     + + + LOCAL DEFINITIONS + + +
C     + + + CODE + + +
        value = DZERO
C         DO NOT EVALUATE INACTIVE REACHES
        IF ( REACH(irch)%ISWRBND.EQ.0 ) GOTO 9999
C
        value = REACH(irch)%QLATFLOW
C---------REACH DATA FOR SWR TIMESTEP          
        REACH(Irch)%CURRENT%QLATFLOW = value
C---------RETURN
09999   RETURN
      END FUNCTION SSWR_CALC_QLI

      DOUBLEPRECISION FUNCTION SSWR_CALC_QUZ(Irch) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, REACH
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Irch
C     + + + LOCAL DEFINITIONS + + +
C     + + + CODE + + +
        value = DZERO
C         DO NOT EVALUATE INACTIVE REACHES
        IF ( REACH(irch)%ISWRBND.EQ.0 ) GOTO 9999
C
        value = REACH(irch)%QUZFLOW
C---------REACH DATA FOR SWR TIMESTEP          
        REACH(Irch)%CURRENT%QUZFLOW = value
C---------RETURN
09999   RETURN
      END FUNCTION SSWR_CALC_QUZ

      DOUBLEPRECISION FUNCTION SSWR_CALC_QEX(Irch) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, REACH
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Irch
C     + + + LOCAL DEFINITIONS + + +
C     + + + CODE + + +
        value = DZERO
C         DO NOT EVALUATE INACTIVE REACHES
        IF ( REACH(irch)%ISWRBND.EQ.0 ) GOTO 9999
C
        value = REACH(irch)%QEXTFLOW
C---------REACH DATA FOR SWR TIMESTEP          
        REACH(Irch)%CURRENT%QEXTFLOW = value
C---------RETURN
09999   RETURN
      END FUNCTION SSWR_CALC_QEX
    
      DOUBLEPRECISION FUNCTION SSWR_RG_QPR(Rg) result(value)
        USE GWFSWRMODULE, ONLY: DZERO, NEARZERO, REACH, TRCHGRP
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        TYPE (TRCHGRP), INTENT(IN)  :: Rg
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: nr
        INTEGER :: irch
        DOUBLEPRECISION :: r
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_QPR
C     + + + CODE + + +
        value = DZERO
C         DO NOT EVALUATE INACTIVE REACH GROUPS
        IF ( Rg%INACTIVE ) RETURN
        DO nr = 1, Rg%NRGREACH  
          irch = Rg%REACH(nr)
          r    = SSWR_CALC_QPR(irch)
C-----------RATE
          value = value + r
        END DO
C---------RETURN
        RETURN
      END FUNCTION SSWR_RG_QPR

      DOUBLEPRECISION FUNCTION SSWR_CALC_QPR(Irch) result(value)
        USE GWFSWRMODULE, ONLY: DZERO, NEARZERO, REACH
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN)         :: Irch
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: sa, r
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        value = DZERO
!        r     = DZERO
C         DO NOT EVALUATE INACTIVE REACHES
        IF ( REACH(Irch)%ISWRBND.EQ.0 ) GOTO 9999
C
       r     = REACH(Irch)%RAIN
        REACH(Irch)%CURRENT%RAIN = DZERO
C         DO NOT EVALUATE REACHES WITH ZERO RAINFALL RATES
        IF ( r.LT.NEARZERO ) GOTO 9999
C---------SURFACE AREA
        sa    = REACH(Irch)%RAINAREA
        r     = sa * r
C---------REACH DATA FOR SWR TIMESTEP          
        REACH(Irch)%CURRENT%RAIN = r
C---------RATE
        value = r
C---------RETURN
09999   RETURN
      END FUNCTION SSWR_CALC_QPR

      DOUBLEPRECISION FUNCTION SSWR_RG_QEV(Rg,Gs) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, NEARZERO, REACH, TRCHGRP, 
     2                          DMINDPTH, DUPDPTH
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        TYPE (TRCHGRP), INTENT(IN)  :: Rg
        DOUBLEPRECISION, INTENT(IN) :: Gs
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: nr
        INTEGER :: irch
        DOUBLEPRECISION :: rs
        DOUBLEPRECISION :: e
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_QEV
C     + + + CODE + + +
        value = DZERO
C         DO NOT EVALUATE INACTIVE REACH GROUPS
        IF ( Rg%INACTIVE ) RETURN
        DO nr = 1, Rg%NRGREACH  
          irch   = Rg%REACH(nr)
          rs     = Gs + REACH(irch)%OFFSET
          e      = SSWR_CALC_QEV(irch,rs)
C-----------RATE
          value = value + e
        END DO
C---------RETURN
        RETURN
      END FUNCTION SSWR_RG_QEV

      DOUBLEPRECISION FUNCTION SSWR_CALC_QEV(Irch,Rs) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, NEARZERO, REACH,
     2                          DMINDPTH, DUPDPTH
        USE GWFSWRINTERFACE, ONLY: SSWR_LININT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN)         :: Irch
        DOUBLEPRECISION, INTENT(IN) :: Rs
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: rbot, d
        DOUBLEPRECISION :: sa, e, swe
        DOUBLEPRECISION :: fact
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_DPTHFACT
C     + + + CODE + + +
        value = DZERO
!        swe   = DZERO
!        e     = DZERO
C         DO NOT EVALUATE INACTIVE REACHES
        IF ( REACH(Irch)%ISWRBND.EQ.0 ) GOTO 9999
C
        swe    = REACH(Irch)%EVAP
        e      = swe
        REACH(Irch)%CURRENT%EVAP = DZERO
C         DO NOT EVALUATE REACHES WITH ZERO EVAPORATION RATES
        IF ( e.LT.NEARZERO ) GOTO 9999
        rbot   = REACH(Irch)%GEO%ELEV(1)
        d      = Rs - rbot
        e      = MIN( d, e )
C---------EVALUATE IF DEPTH (d) IS LESS THAN DUPDPTH
        IF ( d.LT.DUPDPTH ) THEN
          fact = SSWR_CALC_DPTHFACT( ABS(d), DUPDPTH, DMINDPTH )
          e    = e * fact
        END IF
C---------SURFACE AREA
        sa  = SSWR_LININT(REACH(Irch)%GEO%ELEV,
     2                    REACH(Irch)%GEO%SAREA,Rs)
        e   = -sa * e
        swe = -sa * swe
C---------REACH DATA FOR SWR TIMESTEP          
        REACH(Irch)%CURRENT%EVAP     = e
        REACH(Irch)%CURRENT%QPOTGWET = MIN( DZERO, swe - e )
C---------RATE
        value = e
C---------RETURN
09999   RETURN
      END FUNCTION SSWR_CALC_QEV

C-------REACH GROUP AQUIFER-REACH FLOW GROUNDWATER HEAD      
      SUBROUTINE SSWR_UPDATE_QAQHGW()    
        USE GLOBAL,       ONLY: NCOL,NROW,NLAY,HNEW
        USE GWFSWRMODULE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: irch
        INTEGER :: kl
        INTEGER :: irow, jcol
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO irch = 1, NREACHES
          irow  = REACH(irch)%IRCH
          jcol  = REACH(irch)%JRCH
          DO kl = 1, NLAY
            REACH(irch)%CURRENTQAQ(kl)%HGW = HNEW(jcol,irow,kl)
          END DO
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_UPDATE_QAQHGW    
c
C-------REACH GROUP AQUIFER-REACH FLOW ESTIMATE      
      DOUBLEPRECISION FUNCTION SSWR_RG_QAQ(Rg,Gs) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, DONE, NEARZERO,
     2                          DMINDPTH, DUPDPTH,
     3                          REACH, 
     4                          TRCHGRP, ISWRDT, SWRDT, HK, 
     5                          ISWRSS, 
     6                          INWTCORR, INWTCNT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        TYPE (TRCHGRP), INTENT(IN)  :: Rg
        DOUBLEPRECISION, INTENT(IN) :: Gs
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: nr
        INTEGER :: irch
        DOUBLEPRECISION :: rs
        DOUBLEPRECISION :: q
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_QAQ
C     + + + CODE + + +
        value = DZERO
C         DO NOT EVALUATE INACTIVE REACH GROUPS
        IF ( Rg%INACTIVE ) RETURN
        RCHGRPL: DO nr = 1, Rg%NRGREACH
          irch = Rg%REACH(nr)
          rs   = Gs + REACH(irch)%OFFSET
          q    = SSWR_CALC_QAQ(REACH(irch),rs)
C-----------RATE
C           RELATIVE TO GROUP
C           POSITIVE IS A SOURCE FOR GROUP
C           NEGATIVE IS A SINK   FOR GROUP
          value = value + q
        END DO RCHGRPL
C---------RETURN
        RETURN
      END FUNCTION SSWR_RG_QAQ    

C-------REACH AQUIFER-REACH FLOW ESTIMATE - SWR1      
      DOUBLEPRECISION FUNCTION SSWR_CALC_QAQ(Rch,Rs) RESULT(value)
        USE GLOBAL,       ONLY: NCOL,NROW,NLAY,HNEW,HOLD,
     2                          IBOUND,BOTM,LBOTM
        USE GWFBASMODULE, ONLY: HDRY
        USE GWFSWRMODULE, ONLY: DZERO, DONE, NEARZERO,
     2                          DMINDPTH, DUPDPTH,
     3                          TREACH, ISWRDT, SWRDT, HK, 
     4                          ISWRSS, 
     5                          INWTUNIT, INWTCORR, INWTCNT,
     6                          SWRTIME, KMFITER, SWRHEPS
        USE GWFSWRINTERFACE, ONLY: SSWR_LININT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        TYPE (TREACH), INTENT(INOUT)  :: Rch
        DOUBLEPRECISION, INTENT(IN)   :: Rs
C     + + + LOCAL DEFINITIONS + + +
        LOGICAL :: rhsonly
        INTEGER :: k,kk,kact
        INTEGER :: irow, jcol  !ROW, COLUMN
        DOUBLEPRECISION :: dh
        DOUBLEPRECISION :: trs, wps
        DOUBLEPRECISION :: twp
        DOUBLEPRECISION :: length
        DOUBLEPRECISION :: hcond
        DOUBLEPRECISION :: gcndln
        DOUBLEPRECISION :: cond
        DOUBLEPRECISION :: rbot
        DOUBLEPRECISION :: h0, h1, h, hmin, hd
        DOUBLEPRECISION :: fdelt
        DOUBLEPRECISION :: zgtop, zgbot
        DOUBLEPRECISION :: wptop, wpbot
        DOUBLEPRECISION :: zfact, ztot
        DOUBLEPRECISION :: fact, d
        DOUBLEPRECISION :: rh
        DOUBLEPRECISION :: hc
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_DPTHFACT
C     + + + CODE + + +
        value = DZERO
C         DO NOT EVALUATE INACTIVE REACHES
        IF ( Rch%ISWRBND.EQ.0 )    GOTO 9999
C---------GROUNDWATER DATA
        irow  = Rch%IRCH
        jcol  = Rch%JRCH
        kact  = NLAY + 1
C---------FIND UPPERMOST ACTIVE GROUNDWATER CELL
        TOPLAYER: DO k = 1, NLAY
          IF( IBOUND(jcol,irow,k).EQ.0 ) CYCLE TOPLAYER
          kact = k
          EXIT TOPLAYER
        END DO TOPLAYER
        Rch%LAYACT = kact
C         SET dh FOR GROUNDWATER FLOW NEWTON ITERATIONS
        dh   = DZERO
        IF ( INWTCNT.EQ.1 ) dh = SWRHEPS
C
        Rch%CURRENT%QAQFLOW = DZERO
C---------REACH DATA
        trs      = Rs
        length   = Rch%DLEN
        rbot     = Rch%GEO%ELEV(1)
        gcndln   = Rch%GCNDLN
        IF ( trs.LT.rbot ) THEN
          trs = rbot
        END IF
C---------CALCULATE QAQFLOW FOR REACH
        LAYERS: DO k = Rch%LAYSTR, Rch%LAYEND
          Rch%CURRENTQAQ(k)%QAQFLOW        = DZERO
          Rch%CURRENTQAQ(k)%CONDUCTANCE    = DZERO
          Rch%CURRENTQAQ(k)%HEADDIFFERENCE = DZERO
C           DO NOT EVALUATE REACHES WITH ZERO CONDUCTANCES 
          IF ( Rch%ICALCBFLOW.LT.1 ) CYCLE LAYERS
C           DO NOT EVALUATE REACHES WITH NO UNDERLYING ACTIVE CELLS
          IF ( kact.GT.NLAY ) CYCLE LAYERS
C           CALCULATE QAQ FOR LAYER
          kk    = LBOTM(k)
          kact  = MAX( k, Rch%LAYACT )
          zgtop = MIN(BOTM(jcol,irow,kk-1),Rch%GTELEV)
          zgbot = MAX(BOTM(jcol,irow,kk),Rch%GBELEV)
          hcond = HK(jcol,irow,k)
          h0    = REAL( HOLD(jcol,irow,kact), 8 )
          h1    = Rch%CURRENTQAQ(kact)%HGW
          fdelt = SWRTIME(ISWRDT)%FDELT
          hmin  = ( DONE - fdelt ) * h0 + fdelt * h1
          h     = hmin + dh
C---------SKIP IF STAGE (trs) AND GROUNDWATER HEAD (h) IS BELOW REACH BOTTOM (rbot)
          IF ( trs.EQ.rbot  .AND. h-rbot.LT.DZERO ) CYCLE LAYERS
C-----------CALCULATE MAXIMUM INCREMENTAL WETTED PERIMETER FOR CURRENT HEAD
          wps = MAX(h,trs)
          IF (k.EQ.1) zgtop = MAX(wps,zgtop)
          wptop = SSWR_LININT(Rch%GEO%ELEV,
     2                        Rch%GEO%WETPER,zgtop)
            
          wpbot = SSWR_LININT(Rch%GEO%ELEV,
     2                        Rch%GEO%WETPER,zgbot)
          twp = wptop
          IF (Rch%IGEOTYPE.NE.5) twp = wptop - wpbot
          Rch%CURRENTQAQ(k)%WETTEDPERIMETER     = twp
C-----------CALCULATE DYNAMIC CONDUCTANCE
          IF (Rch%IGCNDOP.GT.0) THEN
            SELECT CASE (Rch%IGCNDOP)
              CASE (1)
                cond = twp * length * Rch%GLK
              CASE (2)
                cond = twp * length * hcond / gcndln
              CASE (3)
                cond = (gcndln / (twp * length * hcond)) +
     2                 (DONE / (twp * length * Rch%GLK))
                cond = DONE / cond
            END SELECT
C-----------USER SPECIFIED CONDUCTANCE
          ELSE
            zfact = DONE
            IF (Rch%LAYEND.GT.Rch%LAYSTR) THEN
              ztot  = (Rch%GTELEV - Rch%GBELEV)
              zfact = (zgtop - zgbot) / ztot
            END IF
            cond = Rch%GCND * zfact
          END IF
C-----------EVALUATE IF SEEPAGE LOSSES WILL OCCUR AND IF
C           DEPTH IS IN RANGE OF DUPDPTH AND DMINDPTH
C           IF TRUE SCALE CONDUCTANCE
          hd = trs - h
          d  = trs - rbot
          IF ( Rch%ISWRBND.GT.0 .AND.
     2         hd.GT.DZERO .AND. d.LT.DUPDPTH ) THEN
            fact = SSWR_CALC_DPTHFACT( ABS(d), DUPDPTH, DMINDPTH )
            cond = cond * fact
          END IF
C-----------DETERMINE IF GROUNDWATER HEAD IS BELOW STREAM BOTTOM
          IF (h.GT.rbot) THEN
            hd = trs - h
          ELSE
            hd = MAX(trs - rbot, DZERO)
          END IF
          rhsonly = .FALSE.
          IF ( hmin.LT.rbot ) rhsonly = .TRUE.
C-----------RATE
C           RELATIVE TO GROUP
C           POSITIVE IS A SOURCE FOR GROUP
C           NEGATIVE IS A SINK   FOR GROUP
          value = value - cond * hd
C-----------RATE - WITH SIGN CHANGE
C           USING MODFLOW CONVENTION
C           POSITIVE IS A SOURCE FOR MODFLOW
C           NEGATIVE IS A SINK   FOR MODFLOW
C-----------REACH DATA FOR CURRENT SWR TIMESTEP          
          Rch%CURRENTQAQ(k)%QAQFLOW        = cond * hd
          Rch%CURRENTQAQ(k)%CONDUCTANCE    = cond
          Rch%CURRENTQAQ(k)%HEADDIFFERENCE = hd
C-----------REACH DATA FOR SWR TIMESTEP          
          Rch%CURRENT%QAQFLOW = 
     2      Rch%CURRENT%QAQFLOW - cond * hd
C---------FILL NEWTON CORRECTION TERMS
          IF ( INWTCORR.NE.0 ) THEN
            IF ( rhsonly ) THEN
              rh = cond * hd
              hc = DZERO
            ELSE
              rh = cond * trs
              hc = cond
            END IF
            IF ( INWTCNT.EQ.1 ) THEN
              Rch%QAQNWT(ISWRDT,k)%RHS1   = rh
              Rch%QAQNWT(ISWRDT,k)%CONDN1 = hc
            ELSE
              Rch%QAQNWT(ISWRDT,k)%RHS2   = rh
              Rch%QAQNWT(ISWRDT,k)%CONDN2 = hc
            END IF
          END IF     
        END DO LAYERS
C---------RETURN
09999   RETURN
      END FUNCTION SSWR_CALC_QAQ    
C
C-------REACH AQUIFER-REACH FLOW ESTIMATE - MODFLOW      
      DOUBLEPRECISION FUNCTION SSWR_CALC_MFQAQ(Rch,Rs) RESULT(value)
        USE GLOBAL,       ONLY: NCOL,NROW,NLAY,HNEW,HOLD,
     2                          IBOUND,BOTM,LBOTM
        USE GWFBASMODULE, ONLY: HDRY
        USE GWFSWRMODULE, ONLY: DZERO, DONE, NEARZERO,
     2                          DMINDPTH, DUPDPTH,
     3                          TREACH, ISWRDT, SWRDT, HK, 
     4                          ISWRSS, 
     5                          INWTUNIT, INWTCORR, INWTCNT,
     6                          SWRTIME, KMFITER, SWRHEPS
        USE GWFSWRINTERFACE, ONLY: SSWR_LININT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        TYPE (TREACH), INTENT(INOUT)  :: Rch
        DOUBLEPRECISION, INTENT(IN)   :: Rs
C     + + + LOCAL DEFINITIONS + + +
        LOGICAL :: rhsonly
        INTEGER :: k,kk,kact
        INTEGER :: irow, jcol  !ROW, COLUMN
        DOUBLEPRECISION :: trs, wps
        DOUBLEPRECISION :: twp
        DOUBLEPRECISION :: length
        DOUBLEPRECISION :: hcond
        DOUBLEPRECISION :: gcndln
        DOUBLEPRECISION :: cond
        DOUBLEPRECISION :: rbot
        DOUBLEPRECISION :: h0, h1, h, hd
        DOUBLEPRECISION :: fdelt
        DOUBLEPRECISION :: zgtop, zgbot
        DOUBLEPRECISION :: wptop, wpbot
        DOUBLEPRECISION :: zfact, ztot
        DOUBLEPRECISION :: fact, d
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_DPTHFACT
C     + + + CODE + + +
        value = DZERO
C         DO NOT EVALUATE INACTIVE REACHES
        IF ( Rch%ISWRBND.EQ.0 )    GOTO 9999
C---------GROUNDWATER DATA
        irow  = Rch%IRCH
        jcol  = Rch%JRCH
        kact  = NLAY + 1
C---------FIND UPPERMOST ACTIVE GROUNDWATER CELL
        TOPLAYER: DO k = 1, NLAY
          IF( IBOUND(jcol,irow,k).EQ.0 ) CYCLE TOPLAYER
          kact = k
          EXIT TOPLAYER
        END DO TOPLAYER
        Rch%LAYACT          = kact
C
        Rch%CURRENT%QAQFLOW = DZERO
C---------REACH DATA
        trs      = Rs
        length   = Rch%DLEN
        rbot     = Rch%GEO%ELEV(1)
        gcndln   = Rch%GCNDLN
        IF ( trs.LT.rbot ) THEN
          trs = rbot
        END IF
C---------CALCULATE QAQFLOW FOR REACH
        LAYERS: DO k = Rch%LAYSTR, Rch%LAYEND
          Rch%CURRENTQAQ(k)%QAQFLOW        = DZERO
          Rch%CURRENTQAQ(k)%CONDUCTANCE    = DZERO
          Rch%CURRENTQAQ(k)%HEADDIFFERENCE = DZERO
C           DO NOT EVALUATE REACHES WITH ZERO CONDUCTANCES 
          IF ( Rch%ICALCBFLOW.LT.1 ) CYCLE LAYERS
C           DO NOT EVALUATE REACHES WITH NO UNDERLYING ACTIVE CELLS
          IF ( kact.GT.NLAY ) CYCLE LAYERS
C           CALCULATE QAQ FOR LAYER
          kk    = LBOTM(k)
          kact  = MAX( k, Rch%LAYACT )
          zgtop = MIN(BOTM(jcol,irow,kk-1),Rch%GTELEV)
          zgbot = MAX(BOTM(jcol,irow,kk),Rch%GBELEV)
          hcond = HK(jcol,irow,k)
          h0    = REAL( HOLD(jcol,irow,kact), 8 )
          h1    = HNEW(jcol,irow,kact)
          fdelt = SWRTIME(ISWRDT)%FDELT
          h     = ( DONE - fdelt ) * h0 + fdelt * h1
C---------SKIP IF STAGE (trs) AND GROUNDWATER HEAD (h) IS BELOW REACH BOTTOM (rbot)
          IF ( trs.EQ.rbot  .AND. h-rbot.LT.DZERO ) CYCLE LAYERS
C-----------CALCULATE MAXIMUM INCREMENTAL WETTED PERIMETER FOR CURRENT HEAD
          wps = MAX(h,trs)
          IF (k.EQ.1) zgtop = MAX(wps,zgtop)
          wptop = SSWR_LININT(Rch%GEO%ELEV,
     2                        Rch%GEO%WETPER,zgtop)
            
          wpbot = SSWR_LININT(Rch%GEO%ELEV,
     2                        Rch%GEO%WETPER,zgbot)
          twp = wptop
          IF (Rch%IGEOTYPE.NE.5) twp = wptop - wpbot
          Rch%CURRENTQAQ(k)%WETTEDPERIMETER     = twp
C-----------CALCULATE DYNAMIC CONDUCTANCE
          IF (Rch%IGCNDOP.GT.0) THEN
            SELECT CASE (Rch%IGCNDOP)
              CASE (1)
                cond = twp * length * Rch%GLK
              CASE (2)
                cond = twp * length * hcond / gcndln
              CASE (3)
                cond = (gcndln / (twp * length * hcond)) +
     2                 (DONE / (twp * length * Rch%GLK))
                cond = DONE / cond
            END SELECT
C-----------USER SPECIFIED CONDUCTANCE
          ELSE
            zfact = DONE
            IF (Rch%LAYEND.GT.Rch%LAYSTR) THEN
              ztot  = (Rch%GTELEV - Rch%GBELEV)
              zfact = (zgtop - zgbot) / ztot
            END IF
            cond = Rch%GCND * zfact
          END IF
C-----------EVALUATE IF SEEPAGE LOSSES WILL OCCUR AND IF
C           DEPTH IS IN RANGE OF DUPDPTH AND DMINDPTH
C           IF TRUE SCALE CONDUCTANCE
          hd = trs - h
          d  = trs - rbot
          IF ( Rch%ISWRBND.GT.0 .AND.
     2         hd.GT.DZERO .AND. d.LT.DUPDPTH ) THEN
            fact = SSWR_CALC_DPTHFACT( ABS(d), DUPDPTH, DMINDPTH )
            cond = cond * fact
          END IF
C-----------DETERMINE IF GROUNDWATER HEAD IS BELOW STREAM BOTTOM
          IF ( h.GT.rbot ) THEN
            hd = trs - h
          ELSE
            hd = MAX(trs - rbot, DZERO)
          END IF
          rhsonly = .FALSE.
          IF ( h.LT.rbot ) rhsonly = .TRUE.
C-----------RATE
C           RELATIVE TO GROUP
C           POSITIVE IS A SOURCE FOR GROUP
C           NEGATIVE IS A SINK   FOR GROUP
          value = value - cond * hd
C-----------RATE - WITH SIGN CHANGE
C           USING MODFLOW CONVENTION
C           POSITIVE IS A SOURCE FOR MODFLOW
C           NEGATIVE IS A SINK   FOR MODFLOW
C-----------REACH DATA FOR CURRENT SWR TIMESTEP          
          Rch%CURRENTQAQ(k)%QAQFLOW        = cond * hd
          Rch%CURRENTQAQ(k)%CONDUCTANCE    = cond
          Rch%CURRENTQAQ(k)%HEADDIFFERENCE = hd
        END DO LAYERS
C---------RETURN
09999   RETURN
      END FUNCTION SSWR_CALC_MFQAQ    

C-------REACH STRUCTURE FLOW CALCULATION
      DOUBLEPRECISION FUNCTION SSWR_CALC_SFLOW(Rch,Irch,Idsrch,P) 
     2                         RESULT(value)
        USE GWFSWRMODULE
        USE GWFSWRINTERFACE, ONLY: SSWR_LININT
        USE GWFSFRMODULE, ONLY: SEG
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        TYPE (TREACH), INTENT(INOUT) :: Rch
        INTEGER, INTENT(IN) :: Irch
        INTEGER, INTENT(IN) :: Idsrch
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN)   :: P
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: ns
        INTEGER :: istrconn, istrtype
        INTEGER :: isfr, iseg
        DOUBLEPRECISION :: q, qt
        DOUBLEPRECISION :: qsfr
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_SFLOW
C     + + + CODE + + +
        isfr  = IZERO
        value = DZERO
        q     = DZERO
        qt    = DZERO
        qsfr  = DZERO
C---------RETURN IF NO STRUCTURES IN CURRENT REACH
        IF ( Rch%NSTRUCT.LT.1 ) RETURN
C---------EVALUATE EVERY REACH IN REACH GROUP
        DO ns = 1, Rch%NSTRUCT
          istrconn = Rch%STRUCT(ns)%ISTRCONN
          istrtype = Rch%STRUCT(ns)%ISTRTYPE
C           CHECK THAT THIS IS THE CORRECT STRUCTURE
          IF ( istrconn.NE.Idsrch ) CYCLE
C           EVALUATE STRUCTURE FLOW
          q = SSWR_SFLOW(Rch%STRUCT(ns),Irch,P)
          qt = qt + q
          IF ( ISWRPSTR.NE.0 ) THEN
            Rch%STRUCT(ns)%CURRENT%FLOW = q
          END IF
C           TOTALIZER FOR SFR CONNECTIONS
          IF ( Rch%STRUCT(ns)%ISFRRCH.GT.0 .AND.
     2         istrtype.NE.11 ) THEN
            isfr = 1
            iseg = Rch%STRUCT(ns)%ISFRSEG
            qsfr = qsfr + q
          END IF
        END DO
        value = qt
C---------FILL SFR ARRAYS
        IF ( isfr.GT.0 ) THEN
          SEG(2,iseg) = qsfr
        END IF
C---------RETURN        
        RETURN
      END FUNCTION SSWR_CALC_SFLOW

      DOUBLEPRECISION FUNCTION SWR_STR_DIRCORR(Istrdir,Q) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, DONE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN)           :: Istrdir
        DOUBLEPRECISION, INTENT(IN)   :: Q
C     + + + LOCAL DEFINITIONS + + +
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C---------INITIALIZE VARIABLES
        value       = DONE
        SELECT CASE (Istrdir)
C           U/S FLOW - ONLY NEGATIVE VALUES ALLOWED                
          CASE (:-1)
            IF ( Q.GT.DZERO ) VALUE = DZERO
          CASE (0)
            value = DONE
C           D/S FLOW - ONLY POSITIVE VALUES ALLOWED                
          CASE (1:)
            IF ( Q.LT.DZERO ) VALUE = DZERO
        END SELECT
C
C---------RETURN
        RETURN
      END FUNCTION SWR_STR_DIRCORR
      
      DOUBLEPRECISION FUNCTION SSWR_CALC_CFLOW(Ustage,Dstage,Str) 
     2                         RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, DONEHALF, DONETHIRD, DTWOTHIRDS, 
     2                          DTHREEHALVES, DONE, DTWO, DTWOPI,
     3                          NEARZERO, 
     4                          TSTRUCT, 
     5                          GRAVITY, DLENCONV, TIMECONV
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, INTENT(IN)   :: Ustage
        DOUBLEPRECISION, INTENT(IN)   :: Dstage
        TYPE (TSTRUCT),  INTENT(IN)   :: Str
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: hmax, hmin
        DOUBLEPRECISION :: dmax, dmin
        DOUBLEPRECISION :: hcrit
        DOUBLEPRECISION :: r, d, dhc, v, theta
        DOUBLEPRECISION :: hrc, ac, wp
        DOUBLEPRECISION :: mult, cw, co, cl, cm, ce
        DOUBLEPRECISION :: g, q
        DOUBLEPRECISION :: c2
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C---------INITIALIZE VARIABLES
        value       = DZERO
C
C---------CALCULATE HEADWATER AND TAILWATER ELEVATIONS
        hmax  = MAX(Ustage, Dstage)
        hmin  = MIN(Ustage, Dstage)
        IF ( Ustage.GT.Dstage ) THEN
          dmax = hmax - Str%STRINV
          dmin = hmin - Str%STRINV2
          mult = +DONE
        ELSE
          dmax = hmax - Str%STRINV2
          dmin = hmin - Str%STRINV
          mult = -DONE
        END IF
C
C---------CHECK IF MAXIMUM DEPTH ABOVE CULVERT INVERT IS LESS THAN NEARZERO
        IF ( dmax.LT.NEARZERO ) RETURN
C
C---------CALCULATE CULVERT AREA
C         RECTANGULAR CULVERT
        IF ( Str%STRWID2.GT.DZERO ) THEN
          d   = Str%STRWID2
          dhc = MIN( dmax, d )
          ac  = dhc * Str%STRWID
          wp  = Str%STRWID + dhc * DTWO
          IF ( dhc.EQ.d ) wp = wp + Str%STRWID
          hrc = ac / wp
C         CIRCULAR CULVERT
        ELSE
          d   = Str%STRWID
          r   = d / DTWO
          dhc = MIN( dmax, d )
          v   = SQRT( r**DTWO - ( r - dhc )**DTWO )
          v   = v / ( r - dhc )
          theta = DTWO * ATAN( v )
          IF ( dhc.GE.r ) THEN
            theta = DTWOPI + theta
          END IF
          hrc = ( d / 4.0D0 ) * ( DONE - ( SIN(theta) / theta ) )
          ac  = ( d * d / 8.0D0 ) * ( theta - SIN( theta ) )
        END IF
C
C---------GRAVITY
        g  = DTWO * GRAVITY * DLENCONV * TIMECONV * TIMECONV
        cw = Str%STRCD
        co = Str%STRCD2
C
C---------CALCULATE RATIO OF MAXIMUM DEPTH TO CULVERT DIAMETER (hcrit)
        hcrit = dmax / d
C
C---------UNSUBMERGED CULVERT FLOW
        IF ( hcrit.LT.1.2D0 ) THEN
          q = cw * ac * SQRT( g * dmax )
C---------SUBMERGED CULVERT FLOW
        ELSE
C           INLET CONTROLLED
          IF ( dmin.LT.d ) THEN
            q = co * ac * SQRT( g * ( dmax - co * d ) )
C           OUTLET CONTROLLED
          ELSE
            cl = Str%STRLEN
            cm = Str%STRMAN
            c2 = ( (DLENCONV**DONETHIRD) * TIMECONV )**2.0D0
            ce = 4.0D0 / 3.0D0
            v  = SQRT( DONE + co + ( g*cm*cm*cl / ( c2 * hrc**ce ) ) )
            q  = ac * SQRT( g * ( hmax - hmin ) ) / v
          END IF
        END IF
        value = mult * q
C
C---------RETURN
        RETURN
      END FUNCTION SSWR_CALC_CFLOW
      
      DOUBLEPRECISION FUNCTION SSWR_CALC_OSFLOW(Ustage,Dstage,
     2                                          Gelev,Strinv,Strwid,
     3                                          Cw,Co,Se) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, DONEHALF, DTWOTHIRDS, 
     2                          DTHREEHALVES, DONE, DTWO,
     3                          NEARZERO,
     5                          ISWRDT, NTMAX,
     6                          GRAVITY, DLENCONV, TIMECONV
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, INTENT(IN)   :: Ustage
        DOUBLEPRECISION, INTENT(IN)   :: Dstage
        DOUBLEPRECISION, INTENT(IN)   :: Gelev
        DOUBLEPRECISION, INTENT(IN)   :: Strinv
        DOUBLEPRECISION, INTENT(IN)   :: Strwid
        DOUBLEPRECISION, INTENT(IN)   :: Cw
        DOUBLEPRECISION, INTENT(IN)   :: Co
        DOUBLEPRECISION, INTENT(IN)   :: Se
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: hus, hds
        DOUBLEPRECISION :: dus, dds
        DOUBLEPRECISION :: dgo
        DOUBLEPRECISION :: mult
        DOUBLEPRECISION :: dcrit, gcrit
        DOUBLEPRECISION :: f
        DOUBLEPRECISION :: dh
        DOUBLEPRECISION :: b
        DOUBLEPRECISION :: dcdh
        DOUBLEPRECISION :: g, c
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C---------INITIALIZE VARIABLES
        value       = DZERO
        mult        = DONE  !DTWOTHIRDS
        dcrit       = DZERO
        gcrit       = DONE
        f           = DONE
        g           = DTWO * GRAVITY * DLENCONV * TIMECONV * TIMECONV
C
C---------CALCULATE HEADWATER AND TAILWATER ELEVATIONS
        hus = MAX( Ustage, Dstage )
        hds = MIN( Ustage, Dstage )
        dus = hus - Strinv
        dds = MAX( hds - Strinv, DZERO )
C
C---------CHECK IF MAXIMUM DEPTH ABOVE THE INVERT ELEVATION IS LESS THAN NEARZERO
        IF ( dus.LT.NEARZERO ) RETURN
C
C---------CALCULATE FLOW DIRECTION
C         FLOW FROM DOWNSTREAM TO UPSTREAM = NEGATIVE
        IF ( Ustage.LT.Dstage ) mult = -mult
C
C---------CALCULATE RATIO OF DOWNSTREAM TO UPSTREAM DEPTH (dcrit)
        dcrit = dds / dus
C
C---------SET CONTROLLING HEAD DIFFERENCE BASED ON SUBMERGENCE
        IF ( dcrit.GT.DZERO ) THEN
          dh = dus - dds
          f  = ( DONE - dcrit )**Se
        ELSE
          dh = dus
          f  = DONE
        END IF
C
C---------CHECK FOR CONTROLLED CONDITIONS
        dgo = Gelev - Strinv
        IF ( dgo.LT.NEARZERO ) RETURN
        gcrit = dgo / dus
C         ORIFICE EQUATION
        IF ( gcrit.LT.0.8D0 ) THEN
          b = dgo
          c = Co
        ELSE
C           LINEAR SCALING BETWEEN Cw AND Co FROM 0.8 >= gcrit < 1.0
          IF ( gcrit.LT.DONE ) THEN
            b = dgo
            dcdh = ( Cw - Co ) / ( 0.2D0 * dus )
            c = Co + dcdh * ( b - 0.8D0 * dus )
C           WEIR EQUATION
          ELSE
            b = dus
            c = Cw
          END IF
        END IF
C
C---------CALCULATE FLOW
        value = f * mult * c * Strwid * b * SQRT( g * dus )
C
C---------RETURN
        RETURN
      END FUNCTION SSWR_CALC_OSFLOW

C
C-------GENERALIZED SPILLWAY EQUATION
C       BASED ON THE EQUATION OF ANSAR, M., AND CHEN, Z. (2009)
C       JOURNAL OF HYDRAULIC ENGINEERING
C       DOI: 10.1061/(ASCE)0733-9429(2009)135:7(602)
      DOUBLEPRECISION FUNCTION SSWR_CALC_GSFLOW(Ustage,Dstage,
     2                                          Gelev,Strinv,Strwid,
     3                                          C1,C2,C3) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, DONETHIRD, DONEHALF, DTWOTHIRDS, 
     2                          DONE, DTHREE,
     3                          NEARZERO,
     4                          GRAVITY, DLENCONV, TIMECONV
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, INTENT(IN)   :: Ustage
        DOUBLEPRECISION, INTENT(IN)   :: Dstage
        DOUBLEPRECISION, INTENT(IN)   :: Gelev
        DOUBLEPRECISION, INTENT(IN)   :: Strinv
        DOUBLEPRECISION, INTENT(IN)   :: Strwid
        DOUBLEPRECISION, INTENT(IN)   :: C1
        DOUBLEPRECISION, INTENT(IN)   :: C2
        DOUBLEPRECISION, INTENT(IN)   :: C3
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: hus, hds
        DOUBLEPRECISION :: dus, dds
        DOUBLEPRECISION :: d, yc
        DOUBLEPRECISION :: dgo
        DOUBLEPRECISION :: dcrit, gcrit
        DOUBLEPRECISION :: mult
        DOUBLEPRECISION :: g
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C---------INITIALIZE VARIABLES
        value       = DZERO
        mult        = DONE
        d           = DZERO
        yc          = DZERO
        dcrit       = DZERO
        gcrit       = DZERO
C
C---------CALCULATE HEADWATER AND TAILWATER ELEVATIONS
        hus = MAX( Ustage, Dstage )
        hds = MIN( Ustage, Dstage )
        dus = hus - Strinv
        dds = hds - Strinv
C
C---------CALCULATE GATE OPENING (dgo) AND MINIMUM OF GATE OPENING OR
C         UPSTREAM DEPTH ABOVE THE SPILLWAY GATE INVERT ELEVATION
        dgo = MAX( Gelev - Strinv, DZERO )
        d   = MIN( dgo, dus )
C
C---------CHECK IF CONTROLLING DEPTH (d) IS LESS THAN NEARZERO
        IF ( d.LT.NEARZERO ) RETURN
C
C---------CALCULATE FLOW DIRECTION
C         FLOW FROM DOWNSTREAM TO UPSTREAM = NEGATIVE
        IF ( Ustage.LT.Dstage ) mult = -mult
C
C---------CALCULATE RATIO OF DOWNSTREAM TO UPSTREAM DEPTH (dcrit) AND
C         RATIO OF d AND UPSTREAM DEPTH (gcrit)
        dcrit = dds / dus
        gcrit = d   / dus
C
C---------CALCULATE THE CRITICAL DEPTH (yc)
        yc = ( d ** DTWOTHIRDS ) * ( dus ** DONETHIRD ) *
     2       ( C1 + C2 * gcrit ) *
     3       ( ( 1 - dcrit ) ** C3 )    
C
C---------CONVERT GRAVITY TO CORRECT UNITS
        g = GRAVITY * DLENCONV * TIMECONV * TIMECONV
C
C---------CALCULATE SPILLWAY FLOW
        value = mult * Strwid * SQRT( g * ( yc ** DTHREE) )
C
C---------RETURN
        RETURN
      END FUNCTION SSWR_CALC_GSFLOW


      SUBROUTINE SSWR_RESET_RTMIN()
        USE GLOBAL,       ONLY: IOUT
        USE GWFSWRMODULE, ONLY: ITABTIME, TABDATA, RTMIN
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        INTEGER :: itab
        REAL    :: r, rmin
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        rmin   = 1.0E+09
        itab   = ITABTIME
        DO n = 1, TABDATA(itab)%TSDATA%NDATA
          r = TABDATA(ITAB)%TSDATA%V(n)
          IF ( r.LT.rmin ) rmin = r
        END DO
        RTMIN = rmin
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_RESET_RTMIN

      SUBROUTINE SSWR_GETDT(Rtime0,Rdt)
        USE GLOBAL,       ONLY: IOUT
        USE GWFSWRMODULE, ONLY: ITABTIME, RTMIN, TABDATA
        USE GWFSWRINTERFACE, ONLY: SSWR_RLININT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        REAL, INTENT(IN)    :: Rtime0
        REAL, INTENT(INOUT) :: Rdt
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        REAL :: rtime1
        REAL :: t
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C---------UPDATE RTIME WHICH IS USED TO DEFINE SWRDT
        n      = ITABTIME
        SELECT CASE ( TABDATA(n)%TSDATA%INTP )
C           NONE AND AVERAGE            
          CASE (1,2)
            rtime1 = Rtime0 + RTMIN
            t      = TABDATA(n)%TSDATA%NEXTTIME
            Rdt    = TABDATA(n)%TSDATA%DVAL
C             EVALUATE IF DATA NEEDS TO BE RETRIEVED FROM MEMORY
            IF ( t.GT.Rtime1 ) RETURN
C             UPDATE DVAL
            CALL SSWRGTTS(Rtime0,rtime1,
     2                    TABDATA(n)%TSDATA)
C           INTERPOLATE
          CASE (3)
            TABDATA(n)%TSDATA%DVAL = 
     2        SSWR_RLININT(TABDATA(n)%TSDATA%T(:),
     2                     TABDATA(n)%TSDATA%V(:),Rtime0)
        END SELECT
        Rdt = TABDATA(n)%TSDATA%DVAL
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_GETDT

      SUBROUTINE SSWR_GET_TABS(Rtime0,Rtime1)
        USE GLOBAL,       ONLY: IOUT
        USE GWFSWRMODULE, ONLY: IZERO, DZERO, DONE, 
     2                          NTABS, NREACHES, TABDATA, REACH,
     3                          RCHGRP
        USE GWFSWRINTERFACE, ONLY: SSWR_RLININT, SSWR_LININT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        REAL, INTENT(IN)   :: Rtime0
        REAL, INTENT(IN)   :: Rtime1
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n, nr
        INTEGER :: irch
        REAL :: t
        DOUBLEPRECISION :: v
        DOUBLEPRECISION :: s, gbelv
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C---------UPDATE TABULAR DATA FROM EXTERNAL FILES
        LTAB: DO n = 1, NTABS
C           SKIP TABULAR DATA SPECIFYING SWRDT
          IF ( TABDATA(n)%ITABTYPE.LT.1 ) CYCLE LTAB
C           SKIP TABULAR DATA USING SWR STAGE RESULTS
          IF ( TABDATA(n)%ISTGRES.NE.0 ) CYCLE LTAB
          SELECT CASE ( TABDATA(n)%TSDATA%INTP )
C             NONE AND AVERAGE            
            CASE (1,2)
              t  = TABDATA(n)%TSDATA%NEXTTIME
C               EVALUATE IF DATA NEEDS TO BE RETRIEVED FROM MEMORY
              CALL SSWRGTTS(Rtime0,Rtime1,
     2                      TABDATA(n)%TSDATA)
C             INTERPOLATE
            CASE (3)
              TABDATA(n)%TSDATA%DVAL = 
     2          SSWR_RLININT(TABDATA(n)%TSDATA%T(:),
     3                       TABDATA(n)%TSDATA%V(:),Rtime1)
          END SELECT
C-----------SET TEMPORARY VALUE v
          v = TABDATA(n)%TSDATA%DVAL
C-----------EVALUATE EACH REACH USING CURRENT TABULAR DATA ITEM
          LREACHES: DO nr = 1, TABDATA(n)%NTABRCH  
            irch   = TABDATA(n)%ITABRCH(nr)
            SELECT CASE ( TABDATA(n)%ITABTYPE )
C               RAINFALL
              CASE (1)
                REACH(irch)%RAIN     = v
C               EVAPORATION
              CASE (2)
                REACH(irch)%EVAP     = v
C               LATERAL INFLOW
              CASE (3)
                REACH(irch)%QLATFLOW = v
C               STAGE - ONLY ADJUST CONSTANT STAGE REACHES
              CASE (4)
                IF ( REACH(irch)%ISWRBND.LT.0 ) THEN
                  s     = v
                  gbelv = REACH(irch)%GBELEV
                  IF ( v.LT.gbelv ) s = gbelv
                  REACH(irch)%STAGE  = s
                END IF
            END SELECT
          END DO LREACHES
        END DO LTAB
C
C---------UPDATE TABULAR DATA FROM SWR STAGE RESULTS FILES
        LSTGRES: DO n = 1, NTABS
C           SKIP TABULAR DATA NOT USING SWR STAGE RESULTS
          IF ( TABDATA(n)%ISTGRES.LT.1 ) CYCLE LSTGRES
          LSTGRCH: DO nr = 1, TABDATA(n)%NTABRCH  
            irch   = TABDATA(n)%ITABRCH(nr)
            SELECT CASE ( TABDATA(n)%STGRES(nr)%INTP )
C             NONE AND AVERAGE            
            CASE (1,2)
              t  = TABDATA(n)%STGRES(nr)%NEXTTIME
C               EVALUATE IF DATA NEEDS TO BE RETRIEVED FROM MEMORY
              CALL SSWRGTTS(Rtime0,Rtime1,
     2                      TABDATA(n)%STGRES(nr))
C               INTERPOLATE
              CASE (3)
                TABDATA(n)%STGRES(nr)%DVAL = 
     2            SSWR_RLININT(TABDATA(n)%STGRES(nr)%T(:),
     3                         TABDATA(n)%STGRES(nr)%V(:),Rtime1)
            END SELECT
C-------------SET TEMPORARY VALUE v
            v = TABDATA(n)%STGRES(nr)%DVAL
C             STAGE - ONLY ADJUST CONSTANT STAGE REACHES
            IF ( REACH(irch)%ISWRBND.LT.0 ) THEN
              s     = v
              gbelv = REACH(irch)%GBELEV
              IF ( v.LT.gbelv ) s = gbelv
              REACH(irch)%STAGE  = s
            END IF
          END DO LSTGRCH
        END DO LSTGRES
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_GET_TABS

      SUBROUTINE SSWR_GET_STRGATE(Rtime0,Rtime1) 
        USE GLOBAL,       ONLY: IOUT
        USE GWFSWRMODULE, ONLY: IZERO, DZERO, DONE, 
     2                          NREACHES, NRCHGRP, REACH, RCHGRP,  
     3                          ISWRDT, SWRDT, IADTIME,
     4                          NTABS, TABDATA
        USE GWFSWRINTERFACE, ONLY: SSWR_RLININT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        REAL, INTENT(IN)   :: Rtime0
        REAL, INTENT(IN)   :: Rtime1
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n, nn, nr
        INTEGER :: irch
        INTEGER :: istrtype
        INTEGER :: itab
        INTEGER :: istrtstype
        REAL    :: t
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C
C---------UPDATE STRUCTURE DATA FROM TIMESERIES FILES
        LRG: DO n = 1, NRCHGRP
C-----------DO NOT EVALUATE INACTIVE REACH GROUPS
          IF ( RCHGRP(n)%INACTIVE ) CYCLE LRG
C-----------DO NOT EVALUATE IF NO STRUCTURES IN CURRENT REACH GROUP
          IF ( RCHGRP(n)%NSTRUCT.LT.1 ) CYCLE LRG
C-----------EVALUATE EACH REACH IN THE REACH GROUP
          LREACHES: DO nr = 1, RCHGRP(n)%NRGREACH  
            irch   = RCHGRP(n)%REACH(nr)
C             DO NOT EVALUATE INACTIVE REACHES
            IF ( ABS( REACH(irch)%ISWRBND ).LT.1 ) CYCLE LREACHES
            LSTRUCTURE: DO nn = 1, REACH(irch)%NSTRUCT
              istrtype   = REACH(irch)%STRUCT(nn)%ISTRTYPE
              istrtstype = REACH(irch)%STRUCT(nn)%ISTRTSTYPE
C               DO NOT EVALUATE STRUCTURES THAT ARE NOT OPERATED USING 
C               TIMESERIES DATA
              IF ( istrtstype.LT.1 ) CYCLE LSTRUCTURE
              itab = REACH(irch)%STRUCT(nn)%ISTRTAB
              SELECT CASE ( TABDATA(itab)%TSDATA%INTP )
C                 NONE AND AVERAGE            
                CASE (1,2)
                  t  = TABDATA(itab)%TSDATA%NEXTTIME
C                   EVALUATE IF DATA NEEDS TO BE RETRIEVED FROM MEMORY
                  IF ( t.GT.Rtime1 ) CYCLE LSTRUCTURE
                  CALL SSWRGTTS(Rtime0,Rtime1,
     2                          TABDATA(itab)%TSDATA)
C                 INTERPOLATE
                CASE (3)
                  TABDATA(itab)%TSDATA%DVAL = 
     2              SSWR_RLININT(TABDATA(itab)%TSDATA%T(:),
     2                           TABDATA(itab)%TSDATA%V(:),Rtime1)
              END SELECT
              REACH(irch)%STRUCT(nn)%STRVAL = TABDATA(itab)%TSDATA%DVAL
              SELECT CASE ( istrtstype )
C                 STRCRIT
                CASE (1)
                  REACH(irch)%STRUCT(nn)%STRCRIT = 
     2              REACH(irch)%STRUCT(nn)%STRVAL
C                 STRVAL
                CASE (2)
                  !NOTHING TO DO
                CASE DEFAULT
                  CALL USTOP('PROGRAMMING ERROR: UNKNOWN '//
     2                       'ISTRTSTYPE IN SSWR_GET_STRGATE')
              END SELECT
            END DO LSTRUCTURE
          END DO LREACHES
        END DO LRG
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWR_GET_STRGATE

C       UPDATE OPERABLE STRUCTURE DISCHARGE (PUMP) RATES AND GATE
C       LEVELS FOR ALL OPERABLE STRUCTURES
      SUBROUTINE SWR_CALC_STRVALS(P) 
        USE GWFSWRMODULE, ONLY: IZERO, DZERO, DONE, 
     2                          ISWRPSTR, ILAGSTROPR,
     3                          NREACHES, NRCHGRP, REACH, RCHGRP,  
     4                          ISWRDT, SWRDT, RSTAGE, GSTAGE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN)   :: P
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n, nn, nr
        INTEGER :: irch, icrch
        INTEGER :: irg, iconn
        INTEGER :: istrtype
        INTEGER :: istrlo
        INTEGER :: istrostg, irg2
        INTEGER :: istrtstype
        DOUBLEPRECISION :: strcrit, strcrito, strinv
        DOUBLEPRECISION :: strrt, strmax
        DOUBLEPRECISION :: ps, psc
        DOUBLEPRECISION :: stage
        DOUBLEPRECISION :: cval
        DOUBLEPRECISION :: qm
        DOUBLEPRECISION :: cl1, cl2, cr1, cr2
        DOUBLEPRECISION :: gt0
        DOUBLEPRECISION :: gb0
        DOUBLEPRECISION :: dgo
        DOUBLEPRECISION :: gt
        DOUBLEPRECISION :: gb
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_RG_QM
C     + + + CODE + + +
        LRG: DO n = 1, NRCHGRP
C-----------DO NOT EVALUATE INACTIVE REACH GROUPS
          IF ( RCHGRP(n)%INACTIVE ) CYCLE LRG
C-----------RETURN IF NO STRUCTURES IN CURRENT REACH GROUP
          IF ( RCHGRP(n)%NSTRUCT.LT.1 ) CYCLE LRG
C-----------SET ps TO RCHGRP STAGE
          ps = P(RCHGRP(n)%IRG)  
          LREACHES: DO nr = 1, RCHGRP(n)%NRGREACH  
            irch   = RCHGRP(n)%REACH(nr)
C             DO NOT EVALUATE INACTIVE REACHES
            IF ( ABS( REACH(irch)%ISWRBND ).LT.1 ) CYCLE LREACHES
C             DO NOT EVALUATE REACHES WITHOUT OPERABLE STRUCTURES
            IF ( REACH(irch)%NOPR.LT.1 ) CYCLE LREACHES            
            stage  = ps + REACH(irch)%OFFSET
            LSTRUCTURE: DO nn = 1, REACH(irch)%NSTRUCT
              istrtype = REACH(irch)%STRUCT(nn)%ISTRTYPE
C               CHECK IF CURRENT STRUCTURE IS AN OPERABLE STRUCTURE
              IF ( istrtype.NE.3 .AND. istrtype.NE.8 .AND.
     2             istrtype.NE.9 .AND. istrtype.NE.10 ) CYCLE LSTRUCTURE
              strinv     = REACH(irch)%STRUCT(nn)%STRINV
              istrtstype = REACH(irch)%STRUCT(nn)%ISTRTSTYPE
              istrlo     = REACH(irch)%STRUCT(nn)%ISTRLO
              icrch      = REACH(irch)%STRUCT(nn)%ISTRORCH
              istrostg   = REACH(irch)%STRUCT(nn)%ISTROSTG
              IF ( istrostg.NE.0 ) THEN
                irg2       = REACH(istrostg)%IRG
                strcrit    = P(irg2) + REACH(istrostg)%OFFSET
                strcrito   = strcrit + REACH(irch)%STRUCT(nn)%STRCRITC
              ELSE
                strcrit    = REACH(irch)%STRUCT(nn)%STRCRIT
                strcrito   = strcrit + REACH(irch)%STRUCT(nn)%STRCRITC
              END IF
              strrt      = REACH(irch)%STRUCT(nn)%STRRT
              strmax     = REACH(irch)%STRUCT(nn)%STRMAX
              SELECT CASE (REACH(irch)%STRUCT(nn)%ISTROVAL)
C                 STAGE
                CASE (1)
                  IF ( ILAGSTROPR.EQ.0 ) THEN
                    psc    = P(REACH(icrch)%IRG)
                    cval   = psc + REACH(icrch)%OFFSET
                  ELSE
                    IF ( ISWRDT.NE.1 ) THEN
                      cval = RSTAGE(icrch,ISWRDT-1)
                    ELSE
                      cval = RSTAGE(icrch,0)
                    END IF
                  END IF
C                 FLOW
                CASE (2)
                  irg      = REACH(icrch)%IRG
                  iconn    = REACH(irch)%STRUCT(nn)%ISTROQCON
                  IF ( ILAGSTROPR.EQ.0 ) THEN
C                     ESTIMATE CURRENT CONNECTION FLOWS
C                     REQUIRED FOR SURFACE WATER ONLY SIMULATIONS THAT
C                     ONLY MAKE ONE CALL TO CALL TO GWF2SWR7FM
                    cval   = SSWR_RG_QM(RCHGRP(irg),P)
C                     SET cval TO CURRENT ESTIMATE OF QCONN FOR
C                     SPECIFIED REACH GROUP CONNECTION
                    cval   = RCHGRP(irg)%QCONN(iconn)%FLOW
                  ELSE
C                    CALCULATE FLOW FOR LAST SWR TIME STEP
                    cval   = SSWR_RG_QM(RCHGRP(irg),GSTAGE(1,ISWRDT-1))
                    cval   = RCHGRP(irg)%QCONN(iconn)%FLOW
C                     RESET QM TO VALUE FOR CURRENT REACH GROUP STAGE
                    qm     = SSWR_RG_QM(RCHGRP(irg),P)
                  END IF
              END SELECT
C               INITIAL GATE OPENING
              IF ( ISWRDT.EQ.1 ) THEN
                gt0 = REACH(irch)%STRUCT(nn)%STRTOPINI
                gb0 = REACH(irch)%STRUCT(nn)%STRBOTINI
              ELSE
                gt0 = REACH(irch)%STRUCT(nn)%STRTOP0
                gb0 = REACH(irch)%STRUCT(nn)%STRBOT0
              END IF
C               CALCULATE NEW GATE OPENING
              QISOPRSTR: IF ( istrtype.EQ.3 .OR. istrtype.EQ.8 .OR.
     2                        istrtype.EQ.9 .OR. istrtype.EQ.10 ) THEN
C                 USE TIMESERIES DATA TO SET GATE-LEVEL
                QUSETS: IF ( istrtstype.EQ.2 ) THEN
                  SELECT CASE ( istrtype )
C                     SPECIFIED DISCHARGE (PUMP)
                    CASE (3)
                      gt = REACH(irch)%STRUCT(nn)%STRVAL
                      gb = gt
C                     MOVABLE CREST WEIR
                    CASE (8)
                      gt = MAX( stage, gb0 ) + 100.0D0
                      gb = REACH(irch)%STRUCT(nn)%STRVAL + strinv
C                     OPERABLE GATED SPILLWAY
                    CASE (9,10)
                      gt = REACH(irch)%STRUCT(nn)%STRVAL + strinv
                      gb = strinv
                    CASE DEFAULT
                      CALL USTOP('PROGRAM ERROR: UNKNOWN ISTRTYPE IN '//
     2                           'SWR_CALC_STRVALS')
                  END SELECT
C                 OPERATE GATE USING STRCRIT AND PHYSICAL STRUCTURE PARAMETERS
                ELSE
C                   MAXIMUM GATE CHANGE OVER SWR TIMESTEP
                  dgo = strrt * SWRDT
C                   SET VALUES FOR THE LEFT AND RIGHT SIDES OF THE EVALUATION
                  SELECT CASE ( istrlo )
                    CASE (1)
                      cl1 = cval
                      cr1 = strcrit
                      cl2 = cval 
                      cr2 = strcrito
                    CASE (2)
                      cl1 = strcrit
                      cr1 = cval
                      cl2 = strcrito
                      cr2 = cval
                  END SELECT
                  gt = gt0
                  gb = gb0
C                   SPECIFIED DISCHARGE (PUMP)
                  IF ( istrtype.EQ.3 ) THEN
C                     OPERATE PUMP
                    IF ( cl1.LT.cr1 ) THEN
C                       TURN ON PUMP
                      gt  = MIN( gt0 + dgo, strmax )
                    ELSE IF ( cl2.GE.cr2 ) THEN
C                       TURN OFF PUMP
                      gt  = MAX( gt0 - dgo, DZERO )
                    END IF
                    gb = gt
C                   MOVABLE CREST WEIR
                  ELSE IF ( istrtype.EQ.8 ) THEN
                    gt = MAX( stage, gb0 ) + 100.0D0
C                     OPERATE WEIR
                    IF ( cl1.LT.cr1 ) THEN
C                       LOWER WEIR
                      gb  = MAX( gb0 - dgo, strinv )
                    ELSE IF ( cl2.GE.cr2 ) THEN
C                       RAISE WEIR
                      gb  = MIN( gb0 + dgo, strinv + strmax )
                    END IF
C                   OPERABLE GATED SPILLWAY
                  ELSE IF ( istrtype.EQ.9 .OR. istrtype.EQ.10 ) THEN
                    gb = strinv
C                     OPERATE GATE
                    IF ( cl1.LT.cr1 ) THEN
C                       OPEN GATE
                      IF ( gt.LT.( strinv + strmax ) ) THEN
                        gt  = MIN( gt0 + dgo, strinv + strmax )
                      END IF
                    ELSE IF ( cl2.GE.cr2 ) THEN
C                       CLOSE GATE
                      IF ( gt.GT.strinv ) THEN
                        gt  = MAX( gt0 - dgo, strinv )
                      END IF
                    ELSE
                    END IF
                  END IF
                END IF QUSETS
C                 SET PREVIOUS TOP AND BOTTOM TO TOP AND BOTTOM FROM THE LAST SWR TIMESTEP
                REACH(irch)%STRUCT(nn)%STRTOP0 = 
     2            REACH(irch)%STRUCT(nn)%STRTOP
                REACH(irch)%STRUCT(nn)%STRBOT0 = 
     2            REACH(irch)%STRUCT(nn)%STRBOT
C                 SET CURRENT TOP AND BOTTOM TO CALCULATED TOP AND BOTTOM
                REACH(irch)%STRUCT(nn)%STRTOP = gt
                REACH(irch)%STRUCT(nn)%STRBOT = gb
C                 FILL STRUCTURE TYPE
                SELECT CASE ( istrtype )
C                   SPECIFIED DISCHARGE (PUMP)
                  CASE (3)
                    REACH(irch)%STRUCT(nn)%CURRENT%TOP = 
     2                REACH(irch)%STRUCT(nn)%STRTOP
                    REACH(irch)%STRUCT(nn)%OPRVAL(ISWRDT) = 
     2                REACH(irch)%STRUCT(nn)%STRTOP
C                   MOVABLE CREST WEIR
                  CASE (8)
                    REACH(irch)%STRUCT(nn)%CURRENT%BOTTOM = 
     2                REACH(irch)%STRUCT(nn)%STRBOT
                    REACH(irch)%STRUCT(nn)%OPRVAL(ISWRDT) = 
     2                REACH(irch)%STRUCT(nn)%STRBOT
C                   OPERABLE GATED SPILLWAY
                  CASE (9,10)
                    REACH(irch)%STRUCT(nn)%CURRENT%TOP = 
     2                REACH(irch)%STRUCT(nn)%STRTOP
                    REACH(irch)%STRUCT(nn)%CURRENT%BOTTOM = 
     2                REACH(irch)%STRUCT(nn)%STRBOT
                    REACH(irch)%STRUCT(nn)%CURRENT%OPENING = 
     2                REACH(irch)%STRUCT(nn)%STRTOP - 
     3                REACH(irch)%STRUCT(nn)%STRBOT
                    REACH(irch)%STRUCT(nn)%OPRVAL(ISWRDT) = 
     2                REACH(irch)%STRUCT(nn)%STRTOP
                END SELECT
              END IF QISOPRSTR

            END DO LSTRUCTURE
          END DO LREACHES
        END DO LRG
C
C---------RETURN
        RETURN
      END SUBROUTINE SWR_CALC_STRVALS

C       SET OPERABLE STRUCTURE DISCHARGE (PUMP) RATES AND GATE
C       LEVELS FOR ALL OPERABLE STRUCTURES TO VALUE SAVED FOR SWR1
C       TIMESTEP Nt
      SUBROUTINE SWR_SET_STRVALS(Irch,Nt) 
        USE GWFSWRMODULE, ONLY: IZERO, DZERO, DONE, 
     2                          ISWRPSTR, ILAGSTROPR,
     3                          NREACHES, NRCHGRP, REACH, RCHGRP,  
     4                          ISWRDT, SWRDT, RSTAGE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN)   :: Irch
        INTEGER, INTENT(IN)   :: Nt
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: nn
        INTEGER :: istrtype
        DOUBLEPRECISION :: rs
        DOUBLEPRECISION :: rb
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C         DO NOT EVALUATE INACTIVE REACHES
        IF ( ABS( REACH(Irch)%ISWRBND ).LT.1 ) GOTO 9999
C         DO NOT EVALUATE REACHES WITHOUT OPERABLE STRUCTURES
        IF ( REACH(Irch)%NOPR.LT.1 ) GOTO 9999            
C         SET CURRENT VALUE FOR STRUCTURE TO VALUE SAVED FOR TIME Nt
        LSTRUCTURE: DO nn = 1, REACH(Irch)%NSTRUCT
          istrtype = REACH(irch)%STRUCT(nn)%ISTRTYPE
C           CHECK IF CURRENT STRUCTURE IS AN OPERABLE STRUCTURE
          IF ( istrtype.NE.3 .AND. istrtype.NE.8 .AND.
     2         istrtype.NE.9 .AND. istrtype.NE.10 ) CYCLE LSTRUCTURE
C
          rs = RSTAGE(Irch,Nt)
          rb = REACH(Irch)%STRUCT(nn)%STRINV
          SELECT CASE( istrtype )
            CASE (3)
             REACH(Irch)%STRUCT(nn)%STRTOP =
     2         REACH(Irch)%STRUCT(nn)%OPRVAL(Nt)
             REACH(Irch)%STRUCT(nn)%STRBOT =
     2         REACH(Irch)%STRUCT(nn)%OPRVAL(Nt)
           CASE (8)
             REACH(Irch)%STRUCT(nn)%STRTOP =
     2         MAX( rs, rb ) + 100.0D0
             REACH(Irch)%STRUCT(nn)%STRBOT =
     2         REACH(Irch)%STRUCT(nn)%OPRVAL(Nt)
            CASE (9,10)
             REACH(Irch)%STRUCT(nn)%STRTOP =
     2         REACH(Irch)%STRUCT(nn)%OPRVAL(Nt)
             REACH(Irch)%STRUCT(nn)%STRBOT =
     2         rb
          END SELECT
        END DO LSTRUCTURE
C
C---------RETURN
09999    RETURN
      END SUBROUTINE SWR_SET_STRVALS

C-------INFLOW STRUCTURE USING FLOW FROM SFR
      DOUBLEPRECISION FUNCTION SSWR_CALC_SFRFLOW(Istrm) RESULT(value)
        USE GWFSWRMODULE, ONLY: IZERO, DZERO, DONE, NEARZERO,
     2                          REACH, RCHGRP, DMINDPTH, DUPDPTH, SWRDT
        USE GWFSFRMODULE, ONLY: STRM
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Istrm
C       + + + LOCAL DEFINITIONS + + +
C       + + + FUNCTIONS + + +
C       + + + CODE + + +
C
C---------FILL DISCHARGE WITH OUTFLOW FOR THE SELECTED SFR STREAM REACH
        value = -STRM(9,Istrm)
C
C---------RETURN
        RETURN
      END FUNCTION SSWR_CALC_SFRFLOW
C
C-------CALCULATE REACH TO REACH LATERAL FLOWS
C       COMBINED UNCONTROLLED AND CONTROLLED FLOWS
      DOUBLEPRECISION FUNCTION SSWR_RG_QM(Rg,Gs) RESULT(value)
        USE GLOBAL,       ONLY: DELR, DELC
        USE GWFSWRMODULE
        USE GWFSWRINTERFACE, ONLY: SSWR_LININT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        TYPE (TRCHGRP), INTENT(INOUT) :: Rg
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN)   :: Gs
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: nr
        INTEGER :: irch
        DOUBLEPRECISION :: q
        DOUBLEPRECISION :: qpos, qneg
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_QM
C     + + + CODE + + +
        value = DZERO
C---------DO NOT EVALUATE INACTIVE REACH GROUPS
        IF ( Rg%INACTIVE ) RETURN
C---------MAKE SURE REACH GROUP IS CONNECTED TO ADJACENT REACH GROUPS
        IF ( Rg%NCONN.LT.1 ) RETURN
C
C---------EVALUATE QM FLOW FOR EACH REACH GROUP AS A RATE        
        qpos = DZERO
        qneg = DZERO
        REACHINRCHGRP: DO nr = 1, Rg%NRGREACH
          irch  = Rg%REACH(nr)
          q = SSWR_CALC_QM(irch,Gs)
          qpos = qpos + REACH(irch)%CURRENT%QMPOSFLOW
          qneg = qneg + REACH(irch)%CURRENT%QMNEGFLOW
        END DO REACHINRCHGRP
C        
C---------ADD ESTIMATE OF UNMANAGED AND MANAGED FLOW TO RCHGRP VALUE
        Rg%CURRENT%QMPOSFLOW = qpos
        Rg%CURRENT%QMNEGFLOW = qneg
        value = qpos + qneg
C---------RETURN        
        RETURN
      END FUNCTION SSWR_RG_QM

      DOUBLEPRECISION FUNCTION SSWR_CALC_QM(Irch,Gs) 
     2  RESULT(value)
        USE GWFSWRMODULE
        USE GWFSWRINTERFACE, ONLY: SSWR_LININT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Irch
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN)   :: Gs
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: nc
        INTEGER :: ic
        INTEGER :: imult
        INTEGER :: i, j
        INTEGER :: ia, ja
        INTEGER :: istrrch, jstrrch
        INTEGER :: irowi, irowj
        INTEGER :: jcoli, jcolj
        INTEGER :: irgi, irgj
        DOUBLEPRECISION :: mult
        DOUBLEPRECISION :: si, sj
        DOUBLEPRECISION :: q
        DOUBLEPRECISION :: depth, area, velocity
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_SFLOW
        DOUBLEPRECISION :: SSWR_CALC_UFLOW
C     + + + CODE + + +
        value = DZERO
C
C---------EVALUATE QM FLOW FOR EACH REACH CONNECTION AS A RATE        
        REACH(Irch)%RPOSFLOW          = DZERO
        REACH(Irch)%CURRENT%QMPOSFLOW = DZERO
        REACH(Irch)%RNEGFLOW          = DZERO
        REACH(Irch)%CURRENT%QMNEGFLOW = DZERO
C---------DO NOT EVALUATE INACTIVE REACHES
        IF ( REACH(Irch)%ISWRBND.EQ.0 ) RETURN
C---------LOOP THROUGH EACH CONNECTION IN REACH
        EACHRCHCONN: DO ic = 1, REACH(Irch)%NCONN
          q     = DZERO
          imult = REACH(Irch)%IRGCONN(ic)
          IF ( imult.EQ.0 ) CYCLE
          nc    = ABS ( imult )
          imult = imult / nc
          ia    = Irch
          ja    = REACH(Irch)%ICONN(ic)
          i     = imult * ia
          j     = imult * ja 
C           SKIP CONNECTION IF CONNECTED REACH IS INACTIVE
          IF ( REACH(ja)%ISWRBND.EQ.0 ) CYCLE EACHRCHCONN
C           CALCULATE PARAMETERS NEEDED FOR BOTH METHODS
          irgi    = REACH(ia)%IRG
          irgj    = REACH(ja)%IRG
          irowi   = REACH(ia)%IRCH
          jcoli   = REACH(ia)%JRCH
          irowj   = REACH(ja)%IRCH
          jcolj   = REACH(ja)%JRCH
C           STAGES          
          si  = Gs(irgi) + REACH(ia)%OFFSET
          sj  = Gs(irgj) + REACH(ja)%OFFSET
C           STRUCTURE-CONTROLED FLOW
          IF ( i.LT.1 ) THEN
            istrrch = REACH(ia)%ISTRCONN(ic)
C             SEND REACH TO STRUCTURE CALL TO RETURN TOTAL FLOW FOR CONNECTION
            jstrrch = ja
            mult    = -DONE
C             CHECK IF EVALUATING STRUCTURE FLOW IN REACH WITH STRUCTURE (ia) 
C             OR REACH ON THE DOWNSTREAM END OF STRUCTURE (ja)
            IF ( istrrch.NE.ia ) THEN
              jstrrch = ia
              mult    = DONE
            END IF
C             CALCULATE FLOW FOR STRUCTURE
            q  = SSWR_CALC_SFLOW(REACH(istrrch),istrrch,jstrrch,Gs)
            q  = mult * q
C           DIFFUSIVE-WAVE APPROXIMATION
          ELSE
C             NOT DIFFUSIVE WAVE REACH - SKIP
            IF ( REACH(i)%CROUTETYPE.NE.'DW' ) CYCLE EACHRCHCONN
C             UNCONTROLLED FLOW ONLY FOR DIFFUSIVE-WAVE APPROXIMATION CONNECTION
            q = SSWR_CALC_UFLOW(i,j,irowi,jcoli,irowj,jcolj,
     2                          irgi,irgj,si,sj,
     3                          Gs)
          END IF
C           POSITIVE FLOW - INFLOW TO REACH
          IF ( q.GT.DZERO ) THEN 
            REACH(ia)%RPOSFLOW          = REACH(ia)%RPOSFLOW + q
            REACH(ia)%CURRENT%QMPOSFLOW = 
     2        REACH(ia)%CURRENT%QMPOSFLOW + q
C           NEGATIVE FLOW
          ELSE
            REACH(ia)%RNEGFLOW          = REACH(ia)%RNEGFLOW + q
            REACH(ia)%CURRENT%QMNEGFLOW = 
     2        REACH(ia)%CURRENT%QMNEGFLOW + q
          END IF
C           CALCULATE DEPTH AND CROSS-SECTIONAL AREA FOR CONNECTION
          CALL SSWR_CALC_DEPTHAREA(ia,ja,irowi,jcoli,irowj,jcolj,
     2                             irgi,irgj,si,sj,depth,area)
C           CALCULATE AVERAGE VELOCITY FOR CONNECTION
          velocity = DZERO
          IF ( area.GT.DZERO ) velocity = q / area
C           FILL QCONN
          RCHGRP(irgi)%QCONN(nc)%FLOW     = q
          RCHGRP(irgi)%QCONN(nc)%AREA     = area
          RCHGRP(irgi)%QCONN(nc)%DEPTH    = depth
          RCHGRP(irgi)%QCONN(nc)%VELOCITY = velocity
        END DO EACHRCHCONN

        value = REACH(Irch)%CURRENT%QMPOSFLOW +
     2          REACH(Irch)%CURRENT%QMNEGFLOW
C---------RETURN        
        RETURN
      END FUNCTION SSWR_CALC_QM

      DOUBLEPRECISION FUNCTION SSWR_RG_QBC(Rg,Gs) 
     2  RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, DONE, NRCHGRP, TRCHGRP, REACH
        !USE GWFSWRMODULE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        TYPE (TRCHGRP), INTENT(IN)  :: Rg
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN)   :: Gs
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: nr
        INTEGER :: irch
        DOUBLEPRECISION :: q, qt
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_QBC
C     + + + CODE + + +
        value = DZERO
        qt    = DZERO
C---------DO NOT EVALUATE QBC FOR INACTIVE REACH GROUPS
        IF ( Rg%INACTIVE ) RETURN
C---------DO NOT EVALUATE QBC FOR REACH GROUPS WITHOUT STRUCTURES
        IF ( Rg%BCSTRUCT.LT.1 ) RETURN
C---------CALCULATE QBC FOR REACH GROUP
        DO nr = 1, Rg%NRGREACH
          irch  = Rg%REACH(nr)
          q  = SSWR_CALC_QBC(irch,Gs)
          qt = qt + q
        END DO
        value = qt
C---------RETURN
        RETURN
      END FUNCTION SSWR_RG_QBC

      DOUBLEPRECISION FUNCTION SSWR_CALC_QBC(Irch,Gs) 
     2  RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, DONE, NRCHGRP, REACH
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Irch
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN)   :: Gs
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: idsrch
        DOUBLEPRECISION :: q
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_SFLOW
C     + + + CODE + + +
        value  = DZERO
        q      = DZERO
C         DO NOT EVALUATE INACTIVE REACHES
        IF ( REACH(Irch)%ISWRBND.EQ.0 ) GOTO 9999
        idsrch = 0
        q      = SSWR_CALC_SFLOW(REACH(Irch),Irch,idsrch,Gs)
        q      = -q
C---------REACH DATA FOR SWR TIMESTEP          
09999   REACH(irch)%CURRENT%QBCFLOW = q
        value  = q
C---------RETURN
        RETURN
      END FUNCTION SSWR_CALC_QBC

      DOUBLEPRECISION FUNCTION SSWR_RG_QCS(Rg,In,R,E,B,Qm,Bc) 
     2  RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, DONE, TRCHGRP, REACH
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        TYPE (TRCHGRP), INTENT(IN)  :: Rg
        DOUBLEPRECISION, INTENT(IN) :: In, R, E, B, Qm, Bc
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: nr
        INTEGER :: irch
        DOUBLEPRECISION :: q
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_QCS
C     + + + CODE + + +
        value = DZERO
C---------DO NOT EVALUATE INACTIVE REACH GROUPS
        IF ( Rg%INACTIVE ) RETURN
C---------CONSTANT HEAD FLOW IS THE SUM OF RAINFALL, EVAPORATION, QAQFLOW, AND OUTFLOW
C           OPPOSITE SIGN OF TERMS BECAUSE CONSTANT HEAD PROVIDE FLOW TO COMPENSATE FOR
C           DEFICIT IN FLOW TERMS.
        IF (Rg%CONSTANT) THEN
          value = -DONE * (In + R + E + B + Qm + Bc)
C           EVALUATE EACH REACH
          DO nr = 1, Rg%NRGREACH  
            irch  = Rg%REACH(nr)
            q     = SSWR_CALC_QCS(irch)
          END DO
        END IF
C---------RETURN
        RETURN
      END FUNCTION SSWR_RG_QCS

      DOUBLEPRECISION FUNCTION SSWR_CALC_QCS(Irch) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, DONE, REACH
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: irch
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: ri, rr, re, rb, rqm, rbc
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        value = DZERO
C---------DO NOT EVALUATE INACTIVE OR ACTIVE REACHES
        IF ( REACH(Irch)%ISWRBND.LT.0 ) THEN
C-----------REACH DATA FOR SWR TIMESTEP
          ri    = REACH(Irch)%CURRENT%QLATFLOW +
     2            REACH(Irch)%CURRENT%QUZFLOW  +
     3            REACH(Irch)%CURRENT%QEXTFLOW
          rr    = REACH(Irch)%CURRENT%RAIN
          re    = REACH(Irch)%CURRENT%EVAP
          rb    = REACH(Irch)%CURRENT%QAQFLOW
          rqm   = REACH(Irch)%CURRENT%QMPOSFLOW + 
     2            REACH(Irch)%CURRENT%QMNEGFLOW
          rbc   = REACH(Irch)%CURRENT%QBCFLOW
          value = -DONE * (ri+rr+re+rb+rqm+rbc)
        END IF
C---------SET CONSTANT REACH FLOW FOR REACH
        REACH(Irch)%CURRENT%QCRFLOW = value
C---------RETURN
        RETURN
      END FUNCTION SSWR_CALC_QCS
C
C-------INFLOW IS SPECIFIED INFLOW, UPSTREAM INFLOW, DOWNSTREAM INFLOW, 
C       AND LATERAL INFLOW
      DOUBLEPRECISION FUNCTION SSWR_RG_QINF(Rg) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, REACH, TRCHGRP
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        TYPE (TRCHGRP), INTENT(IN) :: Rg
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: nr
        INTEGER :: irch
        DOUBLEPRECISION :: r
        DOUBLEPRECISION :: qpos, latinf, uzfinf, extinf
        DOUBLEPRECISION :: qaq
        DOUBLEPRECISION :: qneg
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        value = DZERO
C---------DO NOT EVALUATE INACTIVE REACH GROUPS
        IF ( Rg%INACTIVE ) RETURN
C--------FILL WITH CURRENT ESTIMATE
        DO nr = 1, Rg%NRGREACH  
          irch   = Rg%REACH(nr)
C           ONLY CALCULATE FOR ACTIVE REACHES
          IF ( REACH(irch)%ISWRBND.LT.1 ) CYCLE
          qpos   = MAX( DZERO, ( REACH(irch)%CURRENT%QMPOSFLOW ) )
          latinf = MAX( DZERO,   REACH(irch)%CURRENT%QLATFLOW )
          uzfinf = MAX( DZERO,   REACH(irch)%CURRENT%QUZFLOW )
          extinf = MAX( DZERO,   REACH(irch)%CURRENT%QEXTFLOW )
          r      = REACH(irch)%CURRENT%RAIN
          qaq    = MAX( DZERO,   REACH(irch)%CURRENT%QAQFLOW )
          qneg   = MAX( DZERO, ( REACH(irch)%CURRENT%QMNEGFLOW ) )
          value  = value + 
     2             qpos + latinf + uzfinf + extinf + r + qaq + qneg
        END DO
C---------RETURN
        RETURN
      END FUNCTION SSWR_RG_QINF

      DOUBLEPRECISION FUNCTION SSWR_CALC_UFLOW(I,J,
     2                                         Irowi,Jcoli,Irowj,Jcolj,
     3                                         Icei,Icej,
     4                                         Si,Sj,
     5                                         Gs)  RESULT(value)
        USE GLOBAL,       ONLY: DELR, DELC
        USE GWFSWRMODULE
        USE GWFSWRINTERFACE, ONLY: SSWR_LININT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: I, J
        INTEGER, INTENT(IN) :: Irowi, Irowj
        INTEGER, INTENT(IN) :: Jcoli, Jcolj
        INTEGER, INTENT(IN) :: Icei, Icej
        DOUBLEPRECISION, INTENT(IN) :: Si, Sj
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN) :: Gs
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: e, t, c
        DOUBLEPRECISION :: ss
        DOUBLEPRECISION :: psi, psj
        DOUBLEPRECISION :: alphai, alphaj
        DOUBLEPRECISION :: b, bi, bj, bmin
        DOUBLEPRECISION :: a, ai, aj
        DOUBLEPRECISION :: p, pi, pj
        DOUBLEPRECISION :: r, ri, rj
        DOUBLEPRECISION :: h, gsm, sf
        DOUBLEPRECISION :: dl, dli, dlj
        DOUBLEPRECISION :: hr
        DOUBLEPRECISION :: g
        DOUBLEPRECISION :: sfm, sfmi
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_DPTHFACT
        DOUBLEPRECISION :: SSWR_GET_MAGQM
        DOUBLEPRECISION :: SSWR_CALC_MAGQM
        DOUBLEPRECISION :: SSWR_CALC_BILINMAGQM
C     + + + CODE + + +
        value = DZERO
        e     = EPSILON(DZERO)
        t     = TINY(DZERO)
        c     = (DLENCONV**DONETHIRD) * TIMECONV
        dli   = DZERO
        bi    = DZERO
        ai    = DZERO
        pi    = DZERO
        ri    = DZERO
        dlj   = DZERO
        bj    = DZERO
        aj    = DZERO
        pj    = DZERO
        rj    = DZERO
        dl    = DZERO
        gsm   = DZERO
        IF (REACH(I)%IGEOTYPE.EQ.5) THEN
          IF (Irowi.NE.Irowj) dli = DELC(Irowi) * DONEHALF
          IF (Jcoli.NE.Jcolj) dli = DELR(Jcoli) * DONEHALF
        ELSE
          IF (Icei.GT.IZERO) THEN
            dli = RCHGRP(Icei)%DLEN * DONEHALF
          ELSE
            dli = REACH(I)%DLEN * DONEHALF
          END IF
        END IF
        IF (J.GT.IZERO) THEN
          IF (REACH(J)%IGEOTYPE.EQ.5) THEN
            IF (Irowi.NE.Irowj) dlj = DELC(Irowj) * DONEHALF
            IF (Jcoli.NE.Jcolj) dlj = DELR(Jcolj) * DONEHALF
          ELSE
            IF (Icei.GT.IZERO) THEN
              dlj = RCHGRP(Icej)%DLEN * DONEHALF
            ELSE
              dlj = REACH(J)%DLEN * DONEHALF
            END IF
          END IF
        END IF
        alphai = dli / (dli + dlj)
        alphaj = DONE - alphai
        psi    = Si - REACH(I)%OFFSET
        psj    = Sj
        IF (Icei.GT.IZERO) THEN
          CALL SSWR_CALCA_UFLOW(Icei,psi,bi,ai,pi,ri)
        ELSE
          bi  = Si - REACH(I)%GEO%ELEV(1)
          ai  = SSWR_LININT(REACH(I)%GEO%ELEV,REACH(I)%GEO%XAREA,Si)
          pi  = SSWR_LININT(REACH(I)%GEO%ELEV,REACH(I)%GEO%WETPER,Si)
          ri  = REACH(I)%GMANNING
        END IF
        IF ( J.GT.IZERO ) THEN
          IF ( Icej.GT.IZERO ) THEN
            psj    = Sj - REACH(J)%OFFSET
            CALL SSWR_CALCA_UFLOW(Icej,psj,bj,aj,pj,rj)
          ELSE
            bj  = Sj - REACH(J)%GEO%ELEV(1)
            aj  = SSWR_LININT(REACH(J)%GEO%ELEV,REACH(J)%GEO%XAREA,Sj)
            pj  = SSWR_LININT(REACH(J)%GEO%ELEV,REACH(J)%GEO%WETPER,Sj)
            rj  = REACH(J)%GMANNING
          END IF
        END IF
        b     = alphai * bi + alphaj * bj
        a     = alphai * ai + alphaj * aj
        p     = alphai * pi + alphaj * pj
        r     = alphai * ri + alphaj * rj
        h     = psj - psi
        dl    = dli + dlj
        sf    = ABS(h) / dl
C
        sfm   = sf
        IF ( ICIQM.NE.0 ) THEN
          IF ( J.NE.IZERO ) THEN
            IF ( REACH(I)%IGEOTYPE.EQ.5 .AND. 
     2           REACH(J)%IGEOTYPE.EQ.5 ) THEN
C               EXPLICIT QM MAGNITUDE
              IF ( ICIQM.GT.0 ) THEN
                sfm = SSWR_GET_MAGQM(I,J,sf)
C               IMPLICIT QM MAGNITUDE
              ELSE
                IF ( ICIBL.NE.0 ) THEN
                  sfm = SSWR_CALC_BILINMAGQM(I,J,psi,psj,dl,Gs)
                ELSE
                  sfm = SSWR_CALC_MAGQM(I,J,psi,psj,dl,Gs)
                END IF
              END IF
            END IF
          END IF
        END IF
C         DO NOT PROCEED WITH QM FLOW CALCULATION IF WATER SURFACE GRADIENT
C         IS LESS THAN USER SPECIFIED MINIMUM WATER SURFACE GRADIENT
        IF ( sfm.LT.DMINGRAD .OR. (sfm-NEARZERO).LT.DZERO ) RETURN
C         UPSTREAM WEIGHTING OF UNCONTROLLED FLOW BETWEEN CONNECTED REACHES
        IF ( ISWRUPWT.NE.0 .AND. J.NE.IZERO ) THEN
          IF ( psj.GT.psi ) THEN 
            a = aj
            p = pj
          ELSE 
            a = ai
            p = pi
          END IF 
        END IF 
C         DO NOT PROCEED WITH QM FLOW CALCULATION IF WETTED PERIMETER
C         IS LESS THAN OR EQUAL TO DZERO
        IF ( p.LE.DZERO ) RETURN
C         
        sfmi = DONE / SQRT( sfm )
C         GAUCKLER-MANNING-STRICKLER COEFFICIENT
        gsm   = ( DONE / r )
C         SCALE GAUCKLER-MANNING-STRICKLER COEFFICIENT
        bmin = b
        IF (bi.LT.bmin .AND. alphai.GT.DZERO .AND. h.LT.DZERO) THEN
          bmin = bi
        END IF
        IF (bj.LT.bmin .AND. alphaj.GT.DZERO .AND. h.GT.DZERO) THEN
          bmin = bj
        END IF
C         SCALE OF BED RESISTANCE (gsm)
        IF ( bmin.LT.DUPDPTH ) THEN
          ss  = SSWR_CALC_DPTHFACT( bmin, DUPDPTH, DMINDPTH )
          gsm = gsm * ss
        END IF
        hr    = ( a / p )
        IF ( h.LT.DZERO    ) c = -c
        value = c * p * gsm * ( hr**DFIVETHIRDS ) * sfmi * sf
        IF ( J.LT.1 ) THEN
          IF ( psj.EQ.DMISSING ) THEN
            g = GRAVITY * DLENCONV * TIMECONV * TIMECONV
            c = -1.0D0
            value = c * a * SQRT( g * hr ) 
          END IF
        END IF
C       
C---------RETURN
        RETURN
      END FUNCTION SSWR_CALC_UFLOW
      
      CHARACTER (LEN=17) FUNCTION SSWR_BDCHAR(R) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        REAL, INTENT(IN) :: R
C     + + + LOCAL DEFINITIONS + + +
        REAL :: t
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        t = ABS(R)
        IF(t.NE.0.0 .AND.
     1    (t.GE.9.99999E11 .OR. t.LT.0.1) ) THEN
          WRITE(value,'(1PE17.4)') R
        ELSE
          WRITE(value,'(F17.4)') R
        END IF
C---------RETURN
        RETURN
      END FUNCTION SSWR_BDCHAR
C
C
      CHARACTER (LEN=10) FUNCTION SSWR_SCCHAR(R) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        REAL, INTENT(IN) :: R
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: t
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        t = ABS(R)
        IF( t.GT.9.99999D5 .OR. t.LT.0.01 ) THEN
          WRITE(value,'(1PE10.3)') R
        ELSE
          WRITE(value,'(F10.3)') R
        END IF
C---------RETURN
        RETURN
      END FUNCTION SSWR_SCCHAR
C
C
      DOUBLEPRECISION FUNCTION SSWRMXRN() RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, DONE, NREACHES, DMAXRAI, REACH
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i
        DOUBLEPRECISION :: rmax
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        value = DONE
        rmax  = DZERO
        DO i = 1, NREACHES
          IF ( REACH(i)%RAIN.GT.rmax ) rmax = REACH(i)%RAIN
        END DO
        value = rmax / DMAXRAI  
C---------RETURN
        RETURN
      END FUNCTION SSWRMXRN
C
C-------CALCULATE SMALL DEPTH SCALING FACTOR FOR DIFFUSIVE WAVE
C       FLOW, QAQFLOW, AND EVAPORATION 
C       VALUE OF 1.0 (UNSCALED) ABOVE DUPDPTH
C       S-CURVE FUNCTION BETWEEN DUPDPTH AND DMINDPTH
C       VALUE OF 0.0 BELOW DMINDPTH
      DOUBLEPRECISION FUNCTION SSWR_CALC_DPTHFACT(D,U,L) RESULT(value)
        USE GWFSWRMODULE, ONLY: IZERO, DZERO, DONE, 
     2                          DMINDPTH, DUPDPTH, IDPTHSCL
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, INTENT(IN) :: D
        DOUBLEPRECISION, INTENT(IN) :: U
        DOUBLEPRECISION, INTENT(IN) :: L
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: dr
        DOUBLEPRECISION :: dt
        DOUBLEPRECISION :: tu
        DOUBLEPRECISION :: tl
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        value = DONE
        IF ( IDPTHSCL.EQ.0 ) RETURN
        tu = U
        tl = L
        IF ( D.GT.tl ) THEN
          IF ( D.GT.tu ) THEN
            value = DONE
          ELSE
            dt = tu - tl
            dr = ( D - tl ) / dt
C             SIGMOID SCALING
            IF ( IDPTHSCL.EQ.1 ) THEN
              value = DONE + EXP( 12.0D0 * ( 1 - dr ) - 6.0D0 )
              value = DONE / value
C             LINEAR SCALING
            ELSE IF ( IDPTHSCL.EQ.2 ) THEN
              value = dr
            END IF
          END IF
        END IF
C---------RETURN
        RETURN
      END FUNCTION SSWR_CALC_DPTHFACT
C
C + + + SOLVER SUBROUTINES AND FUNCTIONS + + +
C                                                                       
C-------BACKTRACKING BASED ON THE L2NORM OF THE RESIDUAL
C       MINIMIZATION USING BRENT'S METHOD
      SUBROUTINE SSWRBTBM(I,Rn0,Rn,R,X0,X) 
        USE GLOBAL,       ONLY: IOUT
        USE GWFSWRMODULE, ONLY: SMALL, IZERO, 
     2                          DZERO, DONE, DTWO, DONEPERCENT,
     3                          NRCHGRP, NSOLRG,
     4                          TOLR, IBT, IBTPRT
        IMPLICIT NONE 
C     + + + DUMMY ARGUMENTS + + +                                       
        INTEGER, INTENT(IN)            :: I 
        DOUBLEPRECISION, INTENT(IN)    :: Rn0 
        DOUBLEPRECISION, INTENT(INOUT) :: Rn 
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(INOUT) :: R 
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN)    :: X0 
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(INOUT) :: X 
C     + + + LOCAL DEFINITIONS + + +      
        DOUBLEPRECISION, PARAMETER :: dgold = 0.3819660D0
        INTEGER :: ib, ibtc 
        DOUBLEPRECISION :: bepsilon
        DOUBLEPRECISION :: pa, pb, pd
        DOUBLEPRECISION :: lu, lv, lw, lx
        DOUBLEPRECISION :: pm
        DOUBLEPRECISION :: pp, pq, pr
        DOUBLEPRECISION :: pu, pv, pw, px
        DOUBLEPRECISION :: pe, petemp
        DOUBLEPRECISION :: tol1, tol2
        DOUBLEPRECISION :: alpha 
        DOUBLEPRECISION, DIMENSION(NRCHGRP) :: dx 
        DOUBLEPRECISION, DIMENSION(NRCHGRP) :: rt, xt 
        DOUBLEPRECISION, DIMENSION(NSOLRG)  :: f, ft 
C     + + + FUNCTIONS + + +                                             
        DOUBLEPRECISION :: GSOL_L2NORM 
C     + + + INPUT FORMATS + + +                                         
C     + + + OUTPUT FORMATS + + +                                        
 2000   FORMAT(1X,'OUTER ITER.: ',I5,1X,'BACKTRACK ITERS.: ',I5,
     2         1X,'INIT. L2NORM: ',G10.3,                       
     3         1X,'FIN. L2NORM: ',G10.3,                        
     4         1X,'FIN. ALPHA: ',G10.3,1X,'UPDATE PERFORMED')           
C     + + + CODE + + +                                                  
        ibtc     = IZERO
        bepsilon = 1.0D-03 * EPSILON(pa) 
        xt       = DZERO 
        dx       = X - X0 
        alpha    = DONE 
        CALL SSWRC2S(R,f) 
        Rn       = GSOL_L2NORM(NSOLRG,f) 
        ib       = 0
        IF ( Rn.LT.( TOLR/DTWO ) ) RETURN
C---------INITIALIZE POLYNOMIAL POINTS
        pa = 0.0001D0
        pb = 1.0000D0
        pv = pb
        pw = pv
        px = pv
        pe = DZERO
C----------INITIALIZE RESIDUALS CORRESPONDING TO POLYNOMIAL POINTS
        lx    = Rn0
        lv    = lx
        lw    = lx
C---------MINIMIZE RESIDUAL
        BT: DO ib = 1, IBT
          ibtc  = ibtc + 1
          pm    = 0.5D0 * ( pa + pb )
          tol1  = SMALL * ABS( px ) + bepsilon
          tol2  = DTWO * tol1
          IF ( ABS( px - pm ).LT.1.0D-02 ) THEN
            alpha = px
            EXIT BT
          END IF
C           CONSTRUCT A TRIAL PARABOLIC FIT
          IF ( ABS( pe ).GT.tol1 ) THEN
            pr = ( px - pw ) * ( lx - lv )
            pq = ( px - pv ) * ( lx - lw )
            pp = ( px - pv ) * pq - ( px - pw ) * pr
            pq = 2.0D0 * ( pq - pr )
            IF ( pq.GT.DZERO ) pq = -pq
            pq = ABS( pq )
            petemp = pe
            pe = pd  !INITIALIZED ??
            IF ( ABS( pp ).GE.ABS( 0.5D0 * pq * petemp ) .OR. 
     2           pp.LE.( pq * ( pa - px ) ) .OR.              
     3           pp.GE.( pq * ( pb - px ) ) ) THEN
              pe = MERGE( ( pa - px ), ( pb - px ), px.GE.pm )
              pd = dgold * pe
            ELSE
              pd = pp / pq
              pu = px + pd
              IF ( ( pu - pa ).LT.tol2 .OR. 
     2             ( pb - pu ).LT.tol2 ) pd = SIGN( tol1, ( pm - px ) )
            END IF
          ELSE
            pe = MERGE( ( pa - px ), ( pb - px ), px.GE.pm )
            pd = dgold * pe
          END IF
          pu = MERGE( ( px + pd ), 
     2                ( px + SIGN( tol1, pd ) ), ABS( pd ).GE.tol1 )
C           EVALUATE RESIDUAL AT pd. PARABOLIC FIT OR GOLDEN SECTION USED
C           TO CALCULATE pd.
          xt    = X0 + ( pu * dx ) 
          CALL SSWR_CALC_RGRESI(xt,rt)
          CALL SSWRC2S(rt,ft)
          lu    = GSOL_L2NORM(NSOLRG,ft)
C           EVALUATE HOW TO PROCEED
          IF ( lu.LE.lx ) THEN
            IF ( pu.GE.px ) THEN
              pa = px
            ELSE
              pb = px
            END IF
            CALL SHIFT4(pv,pw,px,pu)
            CALL SHIFT4(lv,lw,lx,lu)
          ELSE
            IF ( pu.LT.px ) THEN
              pa = pu
            ELSE
              pb = pu
            END IF
            IF ( lu.LE.lw .OR. lw.EQ.lx ) THEN
              pv = pw
              lv = lw
              pw = pu
              lw = lu
            ELSE IF ( lu.LE.lv .OR. lv.EQ.lx .OR. lv.EQ.lw ) THEN
              pv = pu
              lv = lu
            END IF
          END IF
        END DO BT
C-----------UPDATE RESIDUAL AND L2 NORM       
        X = X0 + (alpha * dx) 
        CALL SSWR_CALC_RGRESI(X,R) 
        CALL SSWRC2S(R,ft) 
        Rn = GSOL_L2NORM(NSOLRG,ft) 
        IF ( ibtc.GT.IZERO .AND. IBTPRT.GT.IZERO ) THEN 
          IF ( MOD(I,IBTPRT).EQ.DZERO ) THEN 
            WRITE (IOUT,2000) I, ibtc, Rn0, Rn, alpha 
          END IF 
        END IF 
C---------RETURN                                                        
        RETURN 
      END SUBROUTINE SSWRBTBM
      
      SUBROUTINE SHIFT4(A,B,C,D)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +                                       
        DOUBLEPRECISION, INTENT(OUT)   :: A
        DOUBLEPRECISION, INTENT(INOUT) :: B
        DOUBLEPRECISION, INTENT(INOUT) :: C
        DOUBLEPRECISION, INTENT(IN)    :: D
C     + + + CODE + + +                                       
        A = B
        B = C
        C = D
C---------RETURN                                                        
        RETURN 
      END SUBROUTINE SHIFT4
C
C-------MAP GENERIC SOLUTION (NSOLRG) VECTOR TO GENERIC 
C       FULL COMPUTATION ELEMENT (NRCHGRP) VECTOR
      SUBROUTINE SSWRS2C(S,F)
        USE GWFSWRMODULE, ONLY: NRCHGRP, NSOLRG, JAC
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, DIMENSION(NSOLRG),  INTENT(IN)    :: S
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(INOUT) :: F
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO i = 1, NSOLRG
          j = JAC%ISMAP(i)
          F(j) = S(i)
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SSWRS2C
C
C-------MAP GENERIC FULL COMPUTATION ELEMENT (NRCHGRP) VECTOR
C       TO GENERIC SOLUTION (NSOLRG) VECTOR
      SUBROUTINE SSWRC2S(F,S)
        USE GWFSWRMODULE, ONLY: IZERO, NRCHGRP, NSOLRG, JAC
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN)    :: F
        DOUBLEPRECISION, DIMENSION(NSOLRG),  INTENT(INOUT) :: S
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO i = 1, NRCHGRP
          j = JAC%ICMAP(i)
          IF ( j.LT.1 ) CYCLE
          S(j) = F(i)
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SSWRC2S
      
      SUBROUTINE SSWREXTRD(Iswr,Iout,Iut,Iclose)
C     ******************************************************************
C     Determine if data should be processed from an internal, external,
C     or temporary file.
C     ******************************************************************
        USE GWFSWRMODULE, ONLY: NREACHES, IPTFLG
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN)    :: Iswr
        INTEGER, INTENT(IN)    :: Iout
        INTEGER, INTENT(INOUT) :: Iut
        INTEGER, INTENT(INOUT) :: Iclose
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: nunopn
        INTEGER :: i
        INTEGER :: istart, istop
        INTEGER :: lloc
        REAL    :: r
        CHARACTER (LEN=200) :: line, fname
C     + + + FUNCTIONS + + +
C     + + + DATA + + +
        DATA nunopn/9999/
C     + + + INCLUDE STATEMENTS + + +
        INCLUDE 'mf_openspec.inc'
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000    FORMAT(1X,'Reading SWR1 Process data on unit ',I4)
2005    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
C     + + + CODE + + +
C     ------------------------------------------------------------------
C
C---------Check for and decode EXTERNAL and OPEN/CLOSE records.
        IF ( Iclose.EQ.0 ) THEN
          Iut = Iswr
          Iclose = 0
          CALL URDCOM(Iut,IOUT,line)
          lloc=1
          CALL URWORD(line,lloc,istart,istop,1,i,r,Iout,Iut)
          IF( line(ISTART:ISTOP).EQ.'EXTERNAL' ) THEN
             CALL URWORD(line,lloc,istart,istop,2,i,r,Iout,Iut)
             Iut = i
             IF ( IPTFLG.EQ.1 ) WRITE(Iout,2000) Iut
          ELSE IF( line(istart:istop).EQ.'INTERNAL' ) THEN
             Iut = Iswr
          ELSE IF( line(istart:istop).EQ.'OPEN/CLOSE' ) THEN
             CALL URWORD(line,lloc,istart,istop,0,i,r,Iout,Iut)
             fname=line(istart:istop)
             Iut = nunopn
             IF( IPTFLG.EQ.1 ) WRITE(Iout,2005) Iut, fname
             OPEN(UNIT=Iut,FILE=fname,ACTION=ACTION(1))
             Iclose = 1
          ELSE
            BACKSPACE(Iut)
          END IF
          CALL SSWR_RD_COMM(Iut)
        ELSE
          CLOSE( UNIT=Iut )
        END IF
C
        RETURN
      END SUBROUTINE SSWREXTRD

      SUBROUTINE SSWRLSTRD(Iswr,Iout,Ldim,Mxlist,Rlist,Naux)
C     ******************************************************************
C     Read and print a list.  NAUX of the values in the list are
C     optional -- auxiliary data.
C     ******************************************************************
        USE GWFSWRMODULE, ONLY: NREACHES, IPTFLG
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Iswr
        INTEGER, INTENT(IN) :: Iout
        INTEGER, INTENT(IN) :: Ldim
        INTEGER, INTENT(IN) :: Mxlist
        REAL, INTENT(INOUT) :: Rlist(Ldim,Mxlist)
        INTEGER, INTENT(IN) :: Naux
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: nunopn
        INTEGER :: in
        INTEGER :: i
        INTEGER :: istart, istop
        INTEGER :: lloc
        INTEGER :: iclose
        INTEGER :: nread
        INTEGER :: ii, jj
        REAL :: r
        REAL :: gfac
        CHARACTER (LEN=200) :: line, fname
C     + + + FUNCTIONS + + +
C     + + + DATA + + +
        DATA nunopn/99/
C     + + + INCLUDE STATEMENTS + + +
        INCLUDE 'mf_openspec.inc'
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000    FORMAT(1X,'Reading list on unit ',I4)
2005    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
2010    FORMAT(1X,'LIST ADJUSTMENT FACTOR=',1PG12.5)
2015    FORMAT(1X,'LIST    SCALING FACTOR=',1PG12.5)
2020    FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELD',I2,')')
2025    FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELDS',I2,
     2            '-',I2,')')
C     + + + CODE + + +
C     ------------------------------------------------------------------
C
C1--------If the list is empty, return.
        IF (Mxlist.EQ.0) RETURN
C
C2--------Check for and decode EXTERNAL and OPEN/CLOSE records.
        in = ISWR
        Iclose=0
        READ(In,'(A)') line
        gfac=1.
        lloc=1
        CALL URWORD(line,lloc,istart,istop,1,i,r,Iout,in)
        IF( line(ISTART:ISTOP).EQ.'EXTERNAL' ) THEN
           CALL URWORD(line,lloc,istart,istop,2,i,r,Iout,in)
           in=i
           IF ( IPTFLG.EQ.1 ) WRITE(Iout,2000) in
        ELSE IF( line(istart:istop).EQ.'INTERNAL' ) THEN
           in = Iswr
        ELSE IF( line(istart:istop).EQ.'OPEN/CLOSE' ) THEN
           CALL URWORD(line,lloc,istart,istop,0,i,r,Iout,in)
           fname=line(istart:istop)
           in=nunopn
           IF( IPTFLG.EQ.1 ) WRITE(Iout,2005) in, fname
           OPEN(UNIT=in,FILE=fname,ACTION=ACTION(1))
           iclose=1
        ELSE
          BACKSPACE(in)
        END IF
C
C3--------Setup indirgs for reading the list
        nread = Ldim - Naux
C3A-------Advance to first       
        CALL SSWR_RD_COMM(in)
C
C4--------Read the list.
        DO ii = 1, Mxlist
C
C4A---------Read a line into the buffer.  (The first line has already been
C4A---------read to scan for EXTERNAL and gfac records.)
          READ(in,'(A)') line
C
C4B---------Get the non-optional values from the line.
          lloc = 1
          DO jj = 1, nread
            CALL URWORD(line,lloc,istart,istop,3,i,Rlist(jj,ii),Iout,in)
          END DO
C
C4E-------Get the optional values from the line
          IF(NAUX.GT.0) THEN
            DO JJ = nread+1, Ldim
            CALL URWORD(line,lloc,istart,istop,3,i,Rlist(jj,ii),Iout,in)
            END DO
          END IF
        END DO
C
C5--------Done reading the list.  If file is open/close, close it.
        IF(iclose.NE.0) CLOSE(UNIT=in)
C
        RETURN
      END SUBROUTINE SSWRLSTRD
C
C
      SUBROUTINE SSWR_ALLO_SOLN(L)
C     ******************************************************************
C     Allocate space for solution data sized using NTMAX
C       Callable subroutine to allow for expansion of data by
C       adaptive time step algorithm
C     ******************************************************************
        USE GLOBAL,       ONLY: IOUT, NLAY
        USE GWFSWRMODULE, ONLY: TQAQNWT,
     2                          TSWRTIME,
     3                          DZERO,
     4                          INWTCORR, NREACHES, REACH,
     5                          NRCHGRP, RCHGRP, 
     6                          NTMAX, NPMAX, SWRTIME,
     8                          RSTAGE, GSTAGE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: L
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, k, n
        INTEGER :: id
        TYPE tsoln
          TYPE (TQAQNWT),   DIMENSION(:,:), ALLOCATABLE :: qaqnwt
        END TYPE tsoln
        TYPE (tsoln), ALLOCATABLE, DIMENSION(:)    :: r
        TYPE (tsoln), ALLOCATABLE, DIMENSION(:)    :: rg
        TYPE(TSWRTIME), ALLOCATABLE,DIMENSION(:)   :: s
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:,:) :: rs
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:,:) :: gs
C     + + + FUNCTIONS + + +
C     + + + DATA + + +
C     + + + INCLUDE STATEMENTS + + +
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
C     + + + CODE + + +
        id = 0
        IF (ASSOCIATED(SWRTIME)) THEN
          WRITE (IOUT,'(//1X,A,1X,I10,1X,A,1X,I10)') 
     2      'REALLOCATING SOLUTION SPACE:', NTMAX, 'TO', L
          id = 1
          ALLOCATE( r(NREACHES) )
          ALLOCATE( rg(NRCHGRP) )
          ALLOCATE( s(L) )
          ALLOCATE( rs(NREACHES,0:L) )
          ALLOCATE( gs(NRCHGRP,0:L) )
          IF ( INWTCORR.NE.0 ) THEN
            DO i = 1, NREACHES
               ALLOCATE( r(i)%qaqnwt(L,NLAY) )
            END DO
          END IF
          DO n = 0, NTMAX
            IF ( n > 0 ) THEN
              s(n) = SWRTIME(n)
              IF ( INWTCORR.NE.0 ) THEN
                DO i = 1, NREACHES
                  DO k = 1, NLAY
                      r(i)%qaqnwt(n,k) = REACH(i)%QAQNWT(n,k)
                  END DO
                END DO
              END IF
            END IF
            DO i = 1, NREACHES
              rs(i,n) = RSTAGE(i,n)
            END DO
            DO i = 1, NRCHGRP
              gs(i,n) = GSTAGE(i,n)
            END DO
          END DO
          DEALLOCATE(SWRTIME)
          DO i = 1, NREACHES
            DEALLOCATE(REACH(i)%CURRENTQAQ)
            IF ( INWTCORR.NE.0 ) THEN
              DEALLOCATE(REACH(i)%QAQNWT)
            END IF
          END DO
          DEALLOCATE( RSTAGE )
          DEALLOCATE( GSTAGE )
C           RESET NTMAX
          NTMAX = L
        END IF
C
C---------ALLOCATE OR REALLOCATE MEMORY
        ALLOCATE(SWRTIME(NTMAX))
        DO i = 1, NREACHES
          ALLOCATE(REACH(i)%CURRENTQAQ(NLAY))
          IF ( INWTCORR.NE.0 ) THEN
            ALLOCATE(REACH(i)%QAQNWT(NTMAX,NLAY))
          END IF
          IF ( id.EQ.0 ) THEN
            ALLOCATE(REACH(i)%QAQRSLTS(NPMAX,NLAY))
          END IF
        END DO
        DO i = 1, NRCHGRP
          IF ( id.EQ.0 ) THEN
            ALLOCATE(RCHGRP(i)%RSLTS(NPMAX))
          END IF
        END DO
        ALLOCATE( RSTAGE(NREACHES,0:NTMAX) )
        ALLOCATE( GSTAGE(NRCHGRP,0:NTMAX)  )
C
C---------FILL WITH DATA IN TEMPORARY ARRAYS, IF NECESSARY
        IF ( id.GT.0 ) THEN
          SWRTIME(:) = s(:)
          IF ( INWTCORR.NE.0 ) THEN
            DO n = 1, NTMAX
              DO i = 1, NREACHES
                DO k = 1, NLAY
                  REACH(i)%QAQNWT(n,k)   = r(i)%qaqnwt(n,k)
                END DO
              END DO
            END DO
          END IF
          DO n = 0, NTMAX
            DO i = 1, NREACHES
              RSTAGE(i,n) = rs(i,n)
            END DO
            DO i = 1, NRCHGRP
              GSTAGE(i,n) = gs(i,n)
            END DO
          END DO
C---------DEALLOCATE TEMPORARY STORAGE
          DEALLOCATE(s)
          IF ( INWTCORR.NE.0 ) THEN
            DO i = 1, NREACHES
               DEALLOCATE( r(i)%qaqnwt )
            END DO
          END IF
          DEALLOCATE(r)
          DEALLOCATE(rg)

          DEALLOCATE( rs )
          DEALLOCATE( gs )

        END IF
C
C---------RETURN
        RETURN      
      END SUBROUTINE SSWR_ALLO_SOLN

!      SUBROUTINE SSWR_SET_QCONN()
!C     ******************************************************************
!C     Allocate space for solution data sized using NTMAX
!C       Callable subroutine to allow for expansion of data by
!C       adaptive time step algorithm
!C     ******************************************************************
!        USE GLOBAL,       ONLY: IOUT, NLAY
!        USE GWFSWRMODULE, ONLY: TQCONN, NRCHGRP, RCHGRP, NTMAX, NPMAX
!        IMPLICIT NONE
!C     + + + DUMMY ARGUMENTS + + +
!C     + + + LOCAL DEFINITIONS + + +
!        INTEGER :: i, j, n 
!        INTEGER :: lold
!        INTEGER :: nc
!        INTEGER :: id
!        TYPE tcf
!          TYPE (TQCONN), DIMENSION(:),   ALLOCATABLE :: cf
!          TYPE (TQCONN), DIMENSION(:,:), ALLOCATABLE :: cfp
!        END TYPE tcf
!        TYPE (tcf), ALLOCATABLE, DIMENSION(:) :: c
!C     + + + FUNCTIONS + + +
!C     + + + DATA + + +
!C     + + + INCLUDE STATEMENTS + + +
!C     + + + INPUT FORMATS + + +
!C     + + + OUTPUT FORMATS + + +
!C     + + + CODE + + +
!        id = 0
!        IF ( ALLOCATED(RCHGRP(1)%QCONN) ) THEN
!          id = 1
!          ALLOCATE(c(NRCHGRP))
!          DO n = 1, NRCHGRP
!            nc = RCHGRP(n)%NCONN
!            IF ( nc.GT.0 ) THEN
!              ALLOCATE(c(n)%cf(nc))
!              ALLOCATE(c(n)%cfp(nc,NPMAX))
!            END IF
!          END DO
!          lold = 1
!          DO n = 1, NRCHGRP
!            DO j = 1, RCHGRP(n)%NCONN
!              c(n)%cf(j) = RCHGRP(n)%QCONN(j)
!              DO i = 1, NPMAX
!                c(n)%cfp(j,i) = RCHGRP(n)%QCONNP(j,i)
!              END DO
!            END DO
!          END DO
!          DO n = 1, NRCHGRP
!            nc = RCHGRP(n)%NCONN
!            IF ( nc.GT.0 ) THEN
!              DEALLOCATE(RCHGRP(n)%QCONN)
!              DEALLOCATE(RCHGRP(n)%QCONNP)
!            END IF
!          END DO
!        END IF
!C
!C---------ALLOCATE OR REALLOCATE MEMORY
!        DO n = 1, NRCHGRP
!          nc = RCHGRP(n)%NCONN
!          IF ( nc.GT.0 ) THEN
!            ALLOCATE(RCHGRP(n)%QCONN(nc))
!            ALLOCATE(RCHGRP(n)%QCONNP(nc,NPMAX))
!          END IF
!        END DO
!C
!C---------FILL WITH DATA IN TEMPORARY ARRAYS, IF NECESSARY
!        IF ( id.GT.0 ) THEN
!          DO n = 1, NRCHGRP
!            DO j = 1, RCHGRP(n)%NCONN
!              RCHGRP(n)%QCONN(j) = c(n)%cf(j)
!              DO i = 1, NPMAX
!                RCHGRP(n)%QCONNP(j,i) = c(n)%cfp(j,i)
!              END DO
!            END DO
!          END DO
!C---------DEALLOCATE TEMPORARY STORAGE
!          DO n = 1, NRCHGRP
!            DO j = 1, RCHGRP(n)%NCONN
!              IF (ALLOCATED(c(n)%cf))  DEALLOCATE(c(n)%cf)
!              IF (ALLOCATED(c(n)%cfp)) DEALLOCATE(c(n)%cfp)
!            END DO
!          END DO
!          DEALLOCATE(c)
!        END IF
!C
!C---------RETURN
!        RETURN      
!      END SUBROUTINE SSWR_SET_QCONN

      SUBROUTINE SSWR_SET_SOLSUM(Is,Ds)
C     ******************************************************************
C     Allocate space for solution data sized using solution summary
C       Callable subroutine to allow for expansion of data by
C       adaptive time step algorithm
C     ******************************************************************
        USE GWFSWRMODULE, ONLY: IZERO, DZERO, NTMAX
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: Is
        DOUBLEPRECISION, INTENT(INOUT), ALLOCATABLE, 
     +    DIMENSION(:,:) :: Ds
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: nold
        INTEGER :: id
        INTEGER, ALLOCATABLE, DIMENSION(:,:) :: it
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:,:) :: dt
C     + + + FUNCTIONS + + +
C     + + + DATA + + +
C     + + + INCLUDE STATEMENTS + + +
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
C     + + + CODE + + +
C
      id = 0
      IF ( ALLOCATED(Is) ) THEN
        id = 1
C---------ALLOCATE TEMPORARY STORAGE
        ALLOCATE(it(3,NTMAX))
        ALLOCATE(dt(2,NTMAX))
        it = 0
        dt = DZERO
        nold = SIZE(Is,DIM=2)
        WRITE (*,*) nold, NTMAX
        it(:,1:nold) = Is(:,1:nold)
        dt(:,1:nold) = Ds(:,1:nold)
C---------DEALLOCATE SOLUTION STORAGE
        DEALLOCATE(Is,Ds)
      END IF
C---------ALLOCATE AND INITIALIZE SOLUTION STORAGE
      ALLOCATE(Is(3,NTMAX),Ds(2,NTMAX))
      Is = IZERO
      Ds = DZERO
C-------FILL SOLUTION STORAGE WITH NON-ZERO VALUES, IF REQUIRED
      IF ( id.GT.0 ) THEN
C---------FILL SOLUTION STORAGE
        Is = it
        Ds = dt
C---------DEALLOCATE TEMPORARY STORAGE
        DEALLOCATE(it,dt)
      END IF
C
C---------RETURN
        RETURN      
      END SUBROUTINE SSWR_SET_SOLSUM

      SUBROUTINE SSWRVADJ(DVZ)
        USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY
        USE GWFSWRMODULE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        REAL, DIMENSION(NCOL,NROW,NLAY), INTENT(IN) :: DVZ
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: ir, jc, kl
        INTEGER :: irch, irg
        INTEGER :: np, ns, nt
        INTEGER :: ifirst
        INTEGER :: istrtype
        DOUBLEPRECISION :: z
C     + + + FUNCTIONS + + +
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000    FORMAT(//9X,'             SUBSIDENCE PACKAGE             ',
     2          /9X,'          VERTICAL REACH ADJUSTMENT         ',
     3          /1X,'     REACH',2X,'     INCR. SUB.',
     4           2X,'CUMULATIVE SUB.',2X,'   BOTTOM ELEV.',
     5          /1X,10('-'),2X,15('-'),2X,15('-'),2X,15('-'))
2010    FORMAT(1X,I10,2X,G15.7,2X,G15.7,2X,G15.7)
2020    FORMAT(1X,61('-'))
C     + + + CODE + + +
C
        ifirst = 1
        RCHGRP(:)%IRGUPDATE = 0
        DO irch = 1, NREACHES
          irg = REACH(irch)%IRG
          ir  = REACH(irch)%IRCH
          jc  = REACH(irch)%JRCH
          kl  = REACH(irch)%LAYSTR
          z   = DVZ(jc,ir,kl)
C           CALCULATE CUMULATIVE SUBSIDANCE FOR THE REACH
          REACH(irch)%GCUMSUB = REACH(irch)%GCUMSUB - z
          IF ( ABS(z).GT.DZERO ) THEN
C             ADJUST CURRENT STAGE FOR THE REACH          
            REACH(irch)%STAGE = REACH(irch)%STAGE - z
C             ADJUST GEOMETRY FOR THE REACH
            RCHGRP(irg)%IRGUPDATE = 1
            DO np = 1, REACH(irch)%NGEOPTS
              REACH(irch)%GEO%ELEV(np) = REACH(irch)%GEO%ELEV(np) - z
            END DO
            REACH(irch)%GTELEV = REACH(irch)%GTELEV - z
            REACH(irch)%GBELEV = REACH(irch)%GBELEV - z
C             RESET STARTING AND ENDING LAYER
            CALL SSWR_SET_RCHLAY(irch)         
C             ADJUST STRUCTURE DATA FOR REACH
            DO ns = 1, REACH(irch)%NSTRUCT
              istrtype =  REACH(irch)%STRUCT(ns)%ISTRTYPE
C               ADJUST STRUCTURE INVERT
              REACH(irch)%STRUCT(ns)%STRINV = 
     2          REACH(irch)%STRUCT(ns)%STRINV - z
C               ADJUST OTHER VERTICALLY TRANSLATABLE ELEVATIONS
              SELECT CASE ( istrtype )
                CASE (4)
                  DO nt = 1, REACH(irch)%STRUCT(ns)%NSTRPTS
                    REACH(irch)%STRUCT(ns)%STRELEV(nt) = 
     2                REACH(irch)%STRUCT(ns)%STRELEV(nt) - z
                  END DO
                CASE (5)
                  REACH(irch)%STRUCT(ns)%STRINV2 = 
     2              REACH(irch)%STRUCT(ns)%STRINV2 - z
                CASE (6,7,8,9,10)
                  REACH(irch)%STRUCT(ns)%STRTOP =
     2              REACH(irch)%STRUCT(ns)%STRTOP - z
                  REACH(irch)%STRUCT(ns)%STRBOT =
     2              REACH(irch)%STRUCT(ns)%STRBOT - z
              END SELECT
            END DO
          END IF
C           PRINT VERTICAL ADJUSTMENT FOR THE REACH
          IF ( IPTFLG.GT.0 ) THEN
            IF ( ifirst.GT.0 ) THEN
              WRITE(IOUT,2000)
              ifirst = 0
            END IF
            WRITE(IOUT,2010) irch, -z, 
     2                       REACH(irch)%GCUMSUB, REACH(irch)%GBELEV
          END IF
        END DO
        IF ( ifirst.EQ.0 ) WRITE(IOUT,2020)
C
C---------RESET CURRENT REACH GROUP STAGES
        DO irg = 1, NRCHGRP
          irch = RCHGRP(irg)%ICALCRCH
          RCHGRP(irg)%STAGE = REACH(irch)%STAGE
        END DO
C
C---------RECALCULATE STAGE-VOLUME RELATION FOR EACH REACH GROUP
        CALL SSWR_CALC_RATCURVE()
C
C---------RETURN
        RETURN      
      END SUBROUTINE SSWRVADJ
      
      DOUBLEPRECISION FUNCTION SSWR_SFLOW(Str,Irch,P) 
     2  RESULT(value)
        USE GWFSWRMODULE
        USE GWFSWRINTERFACE, ONLY: SSWR_LININT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        TYPE (TSTRUCT), INTENT(INOUT) :: Str
        INTEGER, INTENT(IN) :: Irch
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN)   :: P
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: istrtype, istrdir, istrbc
        INTEGER :: isfrstrm
        DOUBLEPRECISION :: strtop, strbot
        DOUBLEPRECISION :: strcd, strcd2, strcd3
        DOUBLEPRECISION :: strinv, strwid
        DOUBLEPRECISION :: strval, strcrit
        DOUBLEPRECISION :: strmax
        INTEGER :: idsrch
        INTEGER :: irchrg, idsrchrg
        INTEGER :: irowi, irowj
        INTEGER :: jcoli, jcolj
        DOUBLEPRECISION :: rbot
        DOUBLEPRECISION :: stage, dstage
        DOUBLEPRECISION :: v, vt, dv
        DOUBLEPRECISION :: q
        DOUBLEPRECISION :: fact
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_RG_VOL
        DOUBLEPRECISION :: SSWR_CALC_CFLOW
        DOUBLEPRECISION :: SSWR_CALC_OSFLOW
        DOUBLEPRECISION :: SSWR_CALC_UFLOW
        DOUBLEPRECISION :: SSWR_CALC_GSFLOW
        DOUBLEPRECISION :: SSWR_CALC_SFRFLOW
        DOUBLEPRECISION :: SWR_STR_DIRCORR
C     + + + CODE + + +
        value = DZERO
        idsrch    = Str%ISTRCONN
        istrtype  = Str%ISTRTYPE
        strtop    = Str%STRTOP
        strbot    = Str%STRBOT
        strcd     = Str%STRCD
        strcd2    = Str%STRCD2
        strcd3    = Str%STRCD3
        strinv    = Str%STRINV
        strwid    = Str%STRWID
        istrdir   = Str%ISTRDIR
        strval    = Str%STRVAL
        strcrit   = Str%STRCRIT
        strmax    = Str%STRMAX
        dstage    = strinv
        istrbc    = Str%ISTRBC
        irchrg    = REACH(Irch)%IRG
        idsrchrg  = 0
        isfrstrm  = Str%ISFRSTRM
        IF ( idsrch.GT.0 ) idsrchrg  = REACH(Idsrch)%IRG
        stage = P(irchrg) + REACH(Irch)%OFFSET  
C         RESET dstage FOR CULVERTS TO DOWNSTREAM CULVERT
C         INVERT TO ENSURE INLET CONTROL IF CULVERT SPECIFIED
C         IN A REACH WITHOUT A DOWNSTREAM CONNECTION
        IF ( Str%ISTRTYPE.EQ.5 ) THEN
          dstage = Str%STRINV2
        END IF
        IF ( Idsrch.GT.0 ) THEN
          dstage = P(idsrchrg) + REACH(Idsrch)%OFFSET  
        END IF
C         CALCULATE STRUCTURE FLOW FOR EACH STRUCTURE TYPE
        SELECT CASE ( istrtype )
C           NO STRUCTURE
          CASE (0)
            RETURN
C           SPECIFIED STAGE - LIMITED TO EXCESS VOLUME 
          CASE (1)
            IF ( stage.GT.strcrit ) THEN
              v  = SSWR_RG_VOL(RCHGRP(irchrg),P(irchrg))
              v  = v / SWRDT
              vt = SSWR_RG_VOL(RCHGRP(irchrg),strcrit)
              vt = vt / SWRDT
              dv = v - vt
              q  = MIN( strmax, dv )
            END IF
C             FREE CONNECTION
          CASE (2)
            irowi   = REACH(Irch)%IRCH
            jcoli   = REACH(Irch)%JRCH
            IF ( Idsrch.GT.IZERO ) THEN
              irowj   = REACH(idsrch)%IRCH
              jcolj   = REACH(idsrch)%JRCH
            ELSE
              IF ( REACH(Irch)%IGEOTYPE.EQ.5 ) THEN
                IF ( istrdir.EQ.1 ) THEN
                  irowj = irowi
                  jcolj = jcoli - 1
                ELSE IF ( istrdir.EQ.2 ) THEN
                  irowj = irowi
                  jcolj = jcoli + 1
                ELSE IF ( istrdir.EQ.3 ) THEN
                  irowj = irowi + 1
                  jcolj = jcoli
                ELSE IF ( istrdir.EQ.4 ) THEN
                  irowj = irowi - 1
                  jcolj = jcoli
                END IF
              ELSE
                rbot   = REACH(Irch)%GEO%ELEV(1)
                dstage = strinv + ( stage - rbot )
C                   CRITICAL DEPTH BOUNDARY
                IF ( istrbc.EQ.3 ) dstage = DMISSING
              END IF
            END IF
C               RETURNED VALUE IS RELATIVE TO REACH
            q = SSWR_CALC_UFLOW(Irch,idsrch,
     2                          irowi,jcoli,irowj,jcolj,
     3                          -irchrg,0,stage,dstage,
     4                          P)
C               NOTE: SIGN CONVENTION OF q FROM SSWR_CALC_UFLOW IS OPPOSITE
C                     FROM OTHER STRUCTURE FLOWS - +VE = OUTFLOW, -VE = INFLOW
            q  = -q
C             SPECIFIED DISCHARGE - LIMITED TO AVAILABLE VOLUME
          CASE (3)
            q = strtop
C               SPECIFIED H-Q RELATION
          CASE (4)
            q = SSWR_LININT(Str%STRELEV,
     2                      Str%STRQ,stage)
C             CULVERT
          CASE (5)
            q = SSWR_CALC_CFLOW(stage,dstage,
     2                            Str)
C               APPLY DIRECTIONAL CORRECTION
            fact = SWR_STR_DIRCORR(istrdir,q)
            q = q * fact
C               FIXED CREST WEIR FORMULA
          CASE (6)
            strtop = MAX( stage, dstage ) + 100.0D0
            q = SSWR_CALC_OSFLOW(stage,dstage,
     2              strtop,strinv,strwid,
     3              strcd,strcd2,strcd3)
C                 APPLY DIRECTIONAL CORRECTION                
            fact = SWR_STR_DIRCORR(istrdir,q)
            q = q * fact
C             SPECIFIED GATE LEVEL GATED SPILLWAY
          CASE (7)
            strtop = strinv + strval
            q = SSWR_CALC_OSFLOW(stage,dstage,
     2              strtop,strinv,
     3              strwid,strcd,strcd2,strcd3)
C                 APPLY DIRECTIONAL CORRECTION                
            fact = SWR_STR_DIRCORR(istrdir,q)
            q = q * fact
C               MOVABLE CREST WEIR FORMULA
          CASE (8)
            q = SSWR_CALC_OSFLOW(stage,dstage,
     2              strtop,strbot,
     3              strwid,strcd,strcd2,strcd3)
C               APPLY DIRECTIONAL CORRECTION                
            fact = SWR_STR_DIRCORR(istrdir,q)
            q = q * fact
C             GATED SPILLWAY
          CASE (9)
            q = SSWR_CALC_OSFLOW(stage,dstage,
     2              strtop,strbot,
     3              strwid,strcd,strcd2,strcd3)
C                 APPLY DIRECTIONAL CORRECTION                
            fact = SWR_STR_DIRCORR(istrdir,q)
            q = q * fact
C             GENERALIZED SPILLWAY EQUATION
          CASE (10)
            q = SSWR_CALC_GSFLOW(stage,dstage,
     2              strtop,strbot,strwid,
     3              strcd,strcd2,strcd3)
C                 APPLY DIRECTIONAL CORRECTION                
            fact = SWR_STR_DIRCORR(istrdir,q)
            q = q * fact
C             SFR INFLOW STRUCTURE
          CASE (11)
            q = SSWR_CALC_SFRFLOW(isfrstrm)
C                 APPLY DIRECTIONAL CORRECTION                
        END SELECT
        value = q
C
C---------RETURN
        RETURN      
      END FUNCTION SSWR_SFLOW

      SUBROUTINE SSWR_CALC_DEPTHAREA(I,J,
     2                               Irowi,Jcoli,Irowj,Jcolj,
     3                               Icei,Icej,
     4                               Si,Sj,
     5                               B, A)
        USE GLOBAL,       ONLY: DELR, DELC
        USE GWFSWRMODULE
        USE GWFSWRINTERFACE, ONLY: SSWR_LININT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: I, J
        INTEGER, INTENT(IN) :: Irowi, Irowj
        INTEGER, INTENT(IN) :: Jcoli, Jcolj
        INTEGER, INTENT(IN) :: Icei, Icej
        DOUBLEPRECISION, INTENT(IN) :: Si, Sj
        DOUBLEPRECISION, INTENT(INOUT) :: B, A
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: dli, dlj
        DOUBLEPRECISION :: psi, psj
        DOUBLEPRECISION :: alphai, alphaj
        DOUBLEPRECISION :: bi, bj
        DOUBLEPRECISION :: ai, aj
        DOUBLEPRECISION :: p
        DOUBLEPRECISION :: r
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        dli   = DZERO
        bi    = DZERO
        ai    = DZERO
        dlj   = DZERO
        bj    = DZERO
        aj    = DZERO
        IF (REACH(I)%IGEOTYPE.EQ.5) THEN
          IF (Irowi.NE.Irowj) dli = DELC(Irowi) * DONEHALF
          IF (Jcoli.NE.Jcolj) dli = DELR(Jcoli) * DONEHALF
        ELSE
          IF (Icei.GT.IZERO) THEN
            dli = RCHGRP(Icei)%DLEN * DONEHALF
          ELSE
            dli = REACH(I)%DLEN * DONEHALF
          END IF
        END IF
        IF (REACH(J)%IGEOTYPE.EQ.5) THEN
          IF (Irowi.NE.Irowj) dlj = DELC(Irowj) * DONEHALF
          IF (Jcoli.NE.Jcolj) dlj = DELR(Jcolj) * DONEHALF
        ELSE
          IF (Icei.GT.IZERO) THEN
            dlj = RCHGRP(Icej)%DLEN * DONEHALF
          ELSE
            dlj = REACH(J)%DLEN * DONEHALF
          END IF
        END IF
        alphai = dli / (dli + dlj)
        alphaj = DONE - alphai
        psi    = Si - REACH(I)%OFFSET
        psj    = Sj
        CALL SSWR_CALCA_UFLOW(Icei,psi,bi,ai,p,r)
        psj    = Sj - REACH(J)%OFFSET
        CALL SSWR_CALCA_UFLOW(Icej,psj,bj,aj,p,r)
        B     = alphai * bi + alphaj * bj
        A     = alphai * ai + alphaj * aj
C
C---------RETURN
        RETURN      
      END SUBROUTINE SSWR_CALC_DEPTHAREA
      
      
C-------READ SWR STAGE DATA INTO MEMORY
      SUBROUTINE SSWR_RDSTGRES(Iu,Totim,TABDATA)
        USE GLOBAL,       ONLY: IOUT
        USE GWFSWRMODULE, ONLY: RZERO, TTABS, NREACHES
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Iu
        REAL, INTENT(IN)    :: Totim
        TYPE(TTABS), INTENT(INOUT) :: TABDATA
C     + + + LOCAL DEFINITIONS + + +
        CHARACTER (LEN=200) :: line
        INTEGER :: iut
        INTEGER :: itmp
        INTEGER :: i, n
        INTEGER :: ios
        INTEGER :: kkper, kkstp, kkswr
        INTEGER :: idata
        INTEGER :: irch
        REAL    :: r
        DOUBLEPRECISION    :: swrt, swrdt
        DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: s
C     + + + FUNCTIONS + + +
C     + + + OUTPUT FORMATS + + +
02000   FORMAT(//1X,'NUMBER OF REACHES IN SWR1 STAGE FILE (',I10,')',
     2           1X,'INCONSISTENT WITH NUMER OF SWR1 REACHES (',I10,')',
     3          /1X,'SPECIFIED IN SWR1 DATASET 1')
02010   FORMAT(//1X,'NO DATA READ FROM SPECIFIED SWR1 STAGE FILE',
     2           1X,'(UNIT',I10,')')
C     + + + CODE + + +
        i  = 0
        iut = ABS( Iu )
C---------REWIND THE FILE
        REWIND(iut)
        IF ( Iu.GT.0 ) THEN
          READ (iut,'(A200)') line
        ELSE
          READ (iut) itmp
          IF ( itmp.NE.NREACHES ) THEN
            WRITE (IOUT,2000) itmp, NREACHES
            CALL USTOP('SWR STAGE FILE NOT '//
     2                 'CONSISTENT WITH NREACHES')
          END IF
        END IF
        ALLOCATE( s(NREACHES) )
C---------DETERMINE THE NUMBER OF ENTRIES
        DO
          IF ( Iu.GT.0 ) THEN
            READ ( iut,*,IOSTAT=ios ) swrt, swrdt, kkper, kkstp, kkswr,
     2                                (s(n),n=1,NREACHES)       
          ELSE
            READ ( iut,IOSTAT=ios ) swrt, swrdt, kkper, kkstp, kkswr,
     2                              (s(n),n=1,NREACHES)
          END IF
          IF ( ios.NE.0 ) EXIT
          i = i + 1
        END DO
C
C---------MAKE SURE SOME DATA HAS BEEN READ
        IF ( i.LT.1 ) THEN
          WRITE (IOUT,2010) iut
          CALL USTOP('NO DATA READ FROM SPECIFIED SWR1 STAGE FILE')
        END IF
C
C---------REWIND THE FILE, ALLOCATE MEMORY, AND READ DATA
        REWIND (iut)
        IF ( Iu.GT.0 ) THEN
          READ (iut,'(A200)') line
        ELSE
          READ (iut) itmp
        END IF
        idata = i + 2
        DO n = 1, TABDATA%NTABRCH
          ALLOCATE ( TABDATA%STGRES(n)%T(idata) )
          ALLOCATE ( TABDATA%STGRES(n)%V(idata) )
          TABDATA%STGRES(n)%NDATA = idata
          TABDATA%STGRES(n)%IPOS  = 1
        END DO
        DO i = 2, idata - 1
          IF ( Iu.GT.0 ) THEN
            READ ( iut,* ) swrt, swrdt, kkper, kkstp, kkswr,
     2                     (s(n),n=1,NREACHES)       
          ELSE
            READ ( iut ) swrt, swrdt, kkper, kkstp, kkswr,
     2                   (s(n),n=1,NREACHES)
          END IF
          DO n = 1, TABDATA%NTABRCH
            irch = TABDATA%ITABRCH(n)
            TABDATA%STGRES(n)%T(i) = REAL( swrt,    4 )
            TABDATA%STGRES(n)%V(i) = REAL( s(irch), 4 )
          END DO
        END DO
C
C---------FILL FIRST AND LAST VALUE
          DO n = 1, TABDATA%NTABRCH
            r = MIN( 0.0, TABDATA%STGRES(n)%T(2) - 1.0 )
            TABDATA%STGRES(n)%T(1) = r
            TABDATA%STGRES(n)%V(1) = TABDATA%STGRES(n)%V(2)
            i = idata - 1
            r = MAX( Totim, TABDATA%STGRES(n)%T(i) + 1.0 )
            TABDATA%STGRES(n)%T(i+1) = r
            TABDATA%STGRES(n)%V(i+1) = TABDATA%STGRES(n)%V(i)
          END DO
C
C---------CLEAN UP TEMPORARY STORAGE
        DEALLOCATE( s )
C
C---------RETURN
        RETURN      
      END SUBROUTINE SSWR_RDSTGRES

C-------READ TABULAR DATA INTO MEMORY
      SUBROUTINE SSWR_RDTABDATA(Iu,Totim,TSDATA)
        USE GLOBAL,       ONLY: IOUT
        USE GWFSWRMODULE, ONLY: RZERO, RONE, TTSDATA
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Iu
        REAL, INTENT(IN)    :: Totim
        TYPE(TTSDATA), INTENT(INOUT) :: TSDATA
C     + + + LOCAL DEFINITIONS + + +
        CHARACTER (LEN=200) :: line
        INTEGER :: iut
        INTEGER :: lloc, istart, istop
        INTEGER :: iv
        INTEGER :: i
        INTEGER :: kwargs
        REAL    :: rv, rt
        REAL    :: toffset, tscale
        REAL    :: offset, scale
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        i       = 0
        kwargs  = 0
        toffset = RZERO
        tscale  = RONE
        offset  = RZERO
        scale   = RONE
        iut     = ABS( Iu )
C---------REWIND THE FILE
C         STRUCTURE CRITERIA (STRCRIT) OR STRUCTURE
C         VALUE (STRVAL) MAY BE READ AGAIN IF NEW STRUCTURE
C         SPECIFIED AND USING A TIME SERIES FILE USED PREVIOUSLY
        REWIND(iut)
C---------LOOK FOR KEYWORDS IN ASCII FILES
        IF ( Iu.GT.0 ) THEN
          CALL SSWR_RD_COMM(iut)
          lloc = 1
          CALL URDCOM(iut,IOUT,line)
          DO 
            CALL URWORD(line,lloc,istart,istop,0,iv,rv,IOUT,Iu)
            IF ( istart.GE.200 ) EXIT
            SELECT CASE (line(istart:istop))
              CASE ('TIMEOFFSET')
                CALL URWORD(line,lloc,istart,istop,3,iv,rv,IOUT,Iu)
                toffset = rv
                kwargs  = 1
              CASE ('TIMESCALE')
                CALL URWORD(line,lloc,istart,istop,3,iv,rv,IOUT,Iu)
                tscale  = rv
                kwargs  = 1
              CASE ('OFFSET')
                CALL URWORD(line,lloc,istart,istop,3,iv,rv,IOUT,Iu)
                offset  = rv
                kwargs  = 1
              CASE ('SCALE')
                CALL URWORD(line,lloc,istart,istop,3,iv,rv,IOUT,Iu)
                scale   = rv
                kwargs  = 1
              CASE DEFAULT
                EXIT
            END SELECT
          END DO
        END IF
C---------BACKSPACE FILE IF KEYWORD ARGUMENTS ARE NOT PRESENT
        IF ( kwargs.NE.1 ) BACKSPACE( Iu ) 
        IF ( Iu.GT.0 ) CALL SSWR_RD_COMM(iut)
C---------DETERMINE THE NUMBER OF ENTRIES
        DO
          IF ( Iu.GT.0 ) THEN
            lloc = 1
            CALL URDCOM(iut,IOUT,line)
            CALL URWORD(line,lloc,istart,istop,3,iv,rv,IOUT,Iu)
          ELSE
            READ ( iut ) rv, rt
          END IF
          rv = ( rv + toffset ) * tscale
          IF ( rv.LT.RZERO ) THEN
            i = 1
          ELSE IF ( rv.GE.RZERO .AND. rv.LE.Totim ) THEN
            i = i + 1
          ELSE IF ( rv.GT.Totim ) THEN
            i = i + 1
            EXIT
          END IF
        END DO
C---------READ DATA FROM FILE
        REWIND (iut)
C---------SKIP KEYWORDS IN ASCII FILES
        IF ( Iu.GT.0 .AND. kwargs.NE.0 ) THEN
          CALL SSWR_RD_COMM(iut)
          lloc = 1
          CALL URDCOM(iut,IOUT,line)
          CALL URWORD(line,lloc,istart,istop,0,iv,rv,IOUT,Iu)
        END IF
        IF ( Iu.GT.0 ) CALL SSWR_RD_COMM(iut)
C---------SKIP
        ALLOCATE ( TSDATA%T(i) )
        ALLOCATE ( TSDATA%V(i) )
        TSDATA%NDATA = i
        TSDATA%IPOS  = 1
        i  = 0
        DO
          IF ( Iu.GT.0 ) THEN
            lloc = 1
            CALL URDCOM(iut,IOUT,line)
            CALL URWORD(line,lloc,istart,istop,3,iv,rv,IOUT,Iu)
            CALL URWORD(line,lloc,istart,istop,3,iv,rt,IOUT,Iu)
          ELSE
            READ ( iut ) rv, rt
          END IF
          rv = ( rv + toffset ) * tscale
          rt = ( rt + offset  ) * scale
          IF ( rv.LT.RZERO ) THEN
             i = 1
             TSDATA%T(i) = rv
             TSDATA%V(i) = rt
          ELSE IF ( rv.GE.RZERO ) THEN
             i = i + 1
             TSDATA%T(i) = rv
             TSDATA%V(i) = rt
          END IF
          IF ( i.EQ.TSDATA%NDATA ) EXIT
        END DO
C
C---------RETURN
        RETURN      
      END SUBROUTINE SSWR_RDTABDATA

      SUBROUTINE SSWRGTTS(Rtime0,Rtime1,TSDATA)
        USE GLOBAL,       ONLY: IOUT
        USE GWFSWRMODULE, ONLY: RZERO, DZERO, TTSDATA
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        REAL, INTENT(IN) :: Rtime0
        REAL, INTENT(IN) :: Rtime1
        TYPE(TTSDATA), INTENT(INOUT)   :: TSDATA
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: intp
        INTEGER :: ipos
        REAL            :: dt, tinc
        REAL            :: rv
        REAL            :: t0, t1, t
        DOUBLEPRECISION :: v0, v1, v, vinc
        DOUBLEPRECISION :: vc
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        intp = TSDATA%INTP
        ipos = TSDATA%IPOS
        vc   = TSDATA%DVAL
        t    = TSDATA%T(ipos)
        t1   = t
        rv   = TSDATA%V(ipos)
        v1   = DBLE( rv )
        IF ( t.GT.Rtime0 .AND. ipos.GT.1 ) THEN
          BKSPTS: DO
            ipos = ipos - 1
            t    = TSDATA%T(ipos)
            IF ( t.LE.Rtime0 ) THEN
              t1 = t
              rv = TSDATA%V(ipos)
              v1 = DBLE( rv )
              EXIT BKSPTS
            END IF
          END DO BKSPTS
        END IF
        dt   = Rtime1 - Rtime0
        tinc = RZERO
        vinc = DZERO
        GETTS: DO
          t    = TSDATA%T(ipos)
          rv   = TSDATA%V(ipos)
          v    = DBLE( rv )
          t0   = t1
          v0   = v1
          t1   = t
          v1   = v
          SELECT CASE ( intp )
            CASE (1)
              IF ( t.GE.Rtime1 ) THEN
                TSDATA%DVAL = v1
                EXIT GETTS
              END IF
            CASE (2)
              IF ( t.GE.Rtime0 ) THEN
                IF ( t.LT.Rtime1 ) THEN
                  tinc = t1 - MAX( Rtime0, t0 )
                  vinc = vinc + v0 * DBLE( tinc )
                ELSE
                  tinc        = Rtime1 - MAX( Rtime0, t0 )
                  vinc        = vinc +  v0 * DBLE( tinc )
                  TSDATA%DVAL = vinc / DBLE( dt )
                  EXIT GETTS
                END IF
              END IF
          END SELECT
          ipos = ipos + 1
        END DO GETTS
C
C       
        TSDATA%IPOS     = ipos
        TSDATA%NEXTTIME = t1
C
C---------RETURN
        RETURN      
      END SUBROUTINE SSWRGTTS
      
      !CALL SSWR_SETSFR_STRM(REACH(istrrch)%STRUCT(istrnum))
      SUBROUTINE SSWR_SETSFR_STRM(Str)
        USE GWFSWRMODULE, ONLY: IZERO, TSTRUCT
        USE GWFSFRMODULE, ONLY: NSTRM, ISTRM
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        TYPE (TSTRUCT), INTENT(INOUT) :: Str
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        INTEGER :: ifound
        INTEGER :: isfrseg, iseg
        INTEGER :: isfrrch, irch
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        ifound = IZERO
        isfrseg = Str%ISFRSEG
        isfrrch = Str%ISFRRCH
        FINDSTRM: DO n = 1, NSTRM
          iseg = ISTRM(4,n)
          irch = ISTRM(5,n)
          IF ( iseg.EQ.isfrseg .AND. irch.EQ.isfrrch ) THEN
            ifound = 1
            Str%ISFRSTRM = n
            EXIT FINDSTRM
          END IF
        END DO FINDSTRM
C---------RESET ISFRRCH IF isfrseg AND isfrrch ARE NOT FOUND
        IF ( ifound.LT.1 ) Str%ISFRRCH = IZERO
C
C---------RETURN
        RETURN      
      END SUBROUTINE SSWR_SETSFR_STRM

      SUBROUTINE SSWR_PCNVG(Kkper,Kkstp)
        USE GLOBAL,       ONLY:IOUT, NSTP
        USE GWFSWRMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Kkper
        INTEGER, INTENT(IN) :: Kkstp
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, n
        INTEGER :: irch
        INTEGER :: iprncnvg
        INTEGER :: nfail, ipos
        INTEGER :: iloc
        DOUBLEPRECISION :: swrin, swrout
        DOUBLEPRECISION :: mfin, mfout
        DOUBLEPRECISION :: swrtot, mftot
        DOUBLEPRECISION :: mfdiff
        DOUBLEPRECISION :: bfdiff, bfdiffmax
        DOUBLEPRECISION :: bc, bavg
        DOUBLEPRECISION :: fmax
C     + + + FUNCTIONS + + +
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(/1X,'WARNING:',1X,A,1X,'SOLVER',/,10X,
     2      'DID NOT CONVERGE FOR EACH',1X,
     3      'SURFACE WATER ROUTING TIMESTEP',/,10X,
     4      'TOLR EXCEEDED',1X,I5,1X,
     5      'TIMES IN STRESS PERIOD',1X,I5,1X,
     6      'AND TIME STEP',1X,I5,/,10X,
     7      'MAXIMUM ABSOLUTE ERROR (',G15.7,') IN RCHGRP',1X,I5,/,10X,
     8      'DURING SURFACE WATER ROUTING TIMESTEP',1X,I5,/)
2010  FORMAT(1X,/,1X,'MAXIMUM RESIDUAL FOR EACH SWR TIMESTEP',
     2       1X,'(NEG. ISWRDT VALUES INDICATE REL. TOLR EXCEEDED)',/,
     3       1X,3('ISWRDT,ITER.,MAX.RESID., LOC.',1X),/,
     4       1X,89('-'))
2020  FORMAT(1X,3(:'(',I5,',',I5,',',G10.3,',',I5,')'))
2030  FORMAT(1X,/,1X,'SWR TIME STEP SUMMARY',/,
     2       1X,5('ISWRDT,     SWRDT '),/,
     3       1X,89('-'))
2040  FORMAT(1X,5(:'(',I5,',',G10.3,')'))
2050  FORMAT(1X,89('-'))
2060  FORMAT(/1X,
     2       /1X,27X,'ITERATIONS FOR EACH SWR TIMESTEP',
     3       /1X,14(I5,1X),100000(:/1X,14(I5,1X)))
2070  FORMAT(1X,84('-'))
2080  FORMAT(1X,14(I5,1X))
2090  FORMAT(/1X,'QAQFLOW CONVERGENCE CRITERIA',
     2       /1X,'MAX. REACH     ',1X,'MAX. REACH',1X,'ALL REACHES    ',
     3       /1X,'DIFFERENCE     ',1X,'LOCATION  ',1X,'TOTAL DIFF.    ')
2100  FORMAT(1X,42('-'))
2110  FORMAT(1X,1PG15.6,1X,I10,1X,1PG15.6)
2120  FORMAT(/1X,2X,'PERCENT DISCREPANCY =',1X,1PG15.6)
C     + + + CODE + + +
C
C-------PRINT RESIDUAL EACH ITERATION IF PRINTOUT INTERVAL IS REACHED
        iprncnvg = MOD(Kkstp,IPRSWR)
        IF ( Kkstp.EQ.NSTP(Kkper) ) iprncnvg = IZERO
        IF ( ( MUTSWR.EQ.0 .AND. iprncnvg.EQ.0 ) .OR. 
     2       ( MUTSWR.EQ.3 .AND. ISWRCNVG.LT.1 ) ) THEN
C-----------PRINT ITERATION SUMMARY
          WRITE (IOUT,2010)
          DO n = 1, NUMTIME, 3
            WRITE (IOUT,2020) (SWRTIME(i)%ICNVG,SWRTIME(i)%ITER,
     2        SWRTIME(i)%FMAX,
     3        SWRTIME(i)%ILOCFMAX,i=n,MIN((n+2),NUMTIME))
          END DO
          WRITE (IOUT,2050)
C-----------PRINT SWRDT SUMMARY
          WRITE (IOUT,2030) 
          DO n = 1, NUMTIME, 5
            WRITE (IOUT,2040) (SWRTIME(i)%ICNVG,
     2        SWRTIME(i)%SWRDT,i=n,MIN((n+4),NUMTIME))
          END DO
          WRITE (IOUT,2050)
C-----------PRINT SUMMARY OF BASEFLOW DIFFERENCES
          IF ( TOLA.GT.DZERO ) THEN
            swrin     = DZERO
            swrout    = DZERO
            mfin      = DZERO
            mfout     = DZERO
            bfdiffmax = DZERO
            iloc      = IZERO
            DO irch = 1, NREACHES
              swrin  = swrin  + BFCNV(1,irch)
              swrout = swrout + BFCNV(2,irch)
              mfin   = mfin   + BFCNV(3,irch)
              mfout  = mfout  + BFCNV(4,irch)
              swrtot = BFCNV(1,irch) - BFCNV(2,irch)
              mftot  = BFCNV(3,irch) - BFCNV(4,irch)
              mfdiff = ABS( swrtot - mftot )
              IF ( mfdiff.GT.bfdiffmax ) THEN
                iloc      = irch
                bfdiffmax = mfdiff
              END IF
            END DO
            bfdiff = ABS( swrtot - mftot )
            WRITE (IOUT,2090)
            WRITE (IOUT,2100)
            WRITE (IOUT,2110) bfdiffmax, 
     2                        iloc,
     3                        bfdiff
C
            bavg = ( swrtot + mftot ) / DTWO
            bc   = DZERO
            IF ( ABS(bavg).GT.DZERO ) THEN
              bc = 100.0D+00 * ( swrtot - mftot ) / bavg
            END IF
            WRITE (IOUT,2120) bc
C
            WRITE (IOUT,2100)
          END IF
C---------WRITE ITERATION SUMMARY
        ELSE IF ( MUTSWR.EQ.1 .AND. iprncnvg.EQ.0 ) THEN
          WRITE (IOUT,2060) (n,n=1,NUMTIME)
          WRITE (IOUT,2070)
          DO n = 1, NUMTIME, 14
            WRITE (IOUT,2080) (SWRTIME(i)%ITER,i=n,MIN((n+13),NUMTIME))
          END DO
          WRITE (IOUT,2080)
        ELSE
C-----------PRINT SUMMARY OF MAXIMUM ERROR IF TOLR EXCEEDED DURING THIS
C           MODFLOW TIMESTEP
          IF ( ISWRCNVG.LT.1 ) THEN
            nfail  = 0
            ipos   = 1
            fmax   = DZERO
            DO n = 1, NUMTIME
              IF ( SWRTIME(n)%ICNVG.LT.0 ) nfail = nfail + 1
              IF ( ABS( SWRTIME(n)%FMAX ).GT. fmax ) THEN
                fmax = ABS( SWRTIME(n)%FMAX )
                ipos = n
              END IF
            END DO
            WRITE(IOUT,2000) TRIM(ADJUSTL(SOLVERTEXT)),
     2        nfail,Kkper,Kkstp,
     3        SWRTIME(ipos)%FMAX,SWRTIME(ipos)%ILOCFMAX,
     4        ABS(SWRTIME(ipos)%ICNVG)
          END IF
        END IF
C
C---------RETURN
        RETURN      
      END SUBROUTINE SSWR_PCNVG

C
C-------TIMER FOR SWR CALCULATIONS
      SUBROUTINE SSWRTIMER(It,T1,T2,Ts)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: It
        DOUBLEPRECISION, INTENT(INOUT) :: T1
        DOUBLEPRECISION, INTENT(INOUT) :: T2
        DOUBLEPRECISION, INTENT(INOUT) :: Ts
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: dt
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        IF ( It.EQ.0 ) THEN
          call CPU_TIME(T1)
        ELSE
          call CPU_TIME(T2)
          dt = T2 - T1
          Ts = Ts + dt
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SSWRTIMER
C
C-------SUBROUTINE TO OUTPUT INFORMATION ABOUT THE CURRENT
C       SWR1 PROCESS DATASET BEING READ
      SUBROUTINE SSWRDSOUT(C)
        USE GLOBAL,       ONLY:IOUT
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        CHARACTER (LEN=*), INTENT(IN) :: C
C     + + + LOCAL DEFINITIONS + + +
C     + + + FUNCTIONS + + +
C     + + + OUTPUT FORMAT + + +
02000   FORMAT(/1X,'...READING SWR1 PROCESS DATASET',1X,A)
C     + + + CODE + + +
        WRITE (IOUT,2000) C
C---------RETURN
        RETURN
      END SUBROUTINE SSWRDSOUT

      SUBROUTINE SSWRNWT(n,kl,irch,h,hc)
        USE GWFSWRMODULE
        USE GWFBASMODULE, ONLY: DELT
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: n
        INTEGER, INTENT(IN) :: irch
        INTEGER, INTENT(IN) :: kl
        DOUBLEPRECISION, INTENT(IN) :: h
        DOUBLEPRECISION, INTENT(INOUT) :: hc
C       + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: hepsinv
        DOUBLEPRECISION :: dtscale
        DOUBLEPRECISION :: rhs1, rhs2, drhs
        DOUBLEPRECISION :: cond1, cond2, dcond
C     + + + FUNCTIONS + + +
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
C     + + + CODE + + +
C
C---------CALCULATE NEWTON CORRECTIONS TO HCOF AND RHS
        hepsinv = DONE / SWRHEPS
        dtscale = SWRTIME(n)%SWRDT / REAL( DELT, 8 )
C---------CALCULATE DERIVATIVE AND INSERT IN THE NEWTON A MATRIX
        rhs1  = REACH(irch)%QAQNWT(n,kl)%RHS1   * dtscale
        rhs2  = REACH(irch)%QAQNWT(n,kl)%RHS2   * dtscale
        cond1 = REACH(irch)%QAQNWT(n,kl)%CONDN1 * dtscale
        cond2 = REACH(irch)%QAQNWT(n,kl)%CONDN2 * dtscale
        drhs  = ( rhs1  - rhs2  ) * hepsinv
        dcond = ( cond1 - cond2 ) * hepsinv
C---------ACCUMULATE THE NEWTON CORRECTION TERMS THAT WILL BE APPLIED TO 
C         HCOF IF A MODFLOW-2005 MODEL OR A MATRIX IF A MODFLOW-NWT MODEL.
        hc = hc + dcond * h - drhs
C
C---------RETURN
9999    RETURN
      END SUBROUTINE SSWRNWT

      DOUBLEPRECISION FUNCTION SSWR_MFQAQ(Rch) RESULT(value)
        USE GLOBAL,       ONLY: NCOL,NROW,NLAY,HNEW,HOLD,
     2                          IBOUND,BOTM,LBOTM
        USE GWFBASMODULE, ONLY: DELT
        USE GWFSWRMODULE, ONLY: DZERO, RZERO, DONE,
     2                          TREACH, ISWRDT, SWRDT,
     3                          SWRTIME, KMFITER, NUMTIME,
     4                          RSTAGE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        TYPE (TREACH), INTENT(IN) :: Rch
C       + + + LOCAL DEFINITIONS + + +
        LOGICAL :: rhsonly
        INTEGER :: n, kl, kk
        INTEGER :: ir, jc
        INTEGER :: kact
        INTEGER :: irch
        REAL :: rate
        DOUBLEPRECISION :: rrate
        DOUBLEPRECISION :: rbot
        DOUBLEPRECISION :: dtscale
        DOUBLEPRECISION :: h, h0, h1
        DOUBLEPRECISION :: fdelt
        DOUBLEPRECISION :: b, s
        DOUBLEPRECISION :: rs, q
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_MFQAQ
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
C     + + + CODE + + +
        value   = DZERO
C---------REACH DATA
        rbot    = Rch%GBELEV
C---------GET ROW & COLUMN OF CELL CONTAINING REACH.
        ir      = Rch%IRCH
        jc      = Rch%JRCH
C---------DO NOT EVALUATE REACHES WITH ZERO CONDUCTANCES 
        IF ( Rch%ICALCBFLOW.LT.1 ) RETURN
C---------EVALUATE EACH TIME
        TIMES: DO n = 1, NUMTIME
C-----------SET UPPER MOST ACTIVE LAYER FOR REACH
          kact = Rch%LAYACT
C-----------IF NO ACTIVE CELLS UNDERLYING REACH MOVE ON TO NEXT TIME FOR REACH.
          IF( kact.GT.NLAY ) CYCLE TIMES
          dtscale = SWRTIME(n)%SWRDT / REAL( DELT, 8 )
C-----------CALCULATE BASEFLOW FOR THIS TIME
          ISWRDT = n
          irch   = Rch%IREACH
          rs     = RSTAGE(irch,n)
          q      = SSWR_CALC_MFQAQ(Rch,rs)
C-----------PROCESS           
          LAYERS: DO kl = Rch%LAYSTR, Rch%LAYEND
C-------------SET RATE
            rrate = DZERO
            rate  = RZERO
C-------------SET GROUNDWATER LAYER TO USE
            kk    = MAX( kl, kact )
C-------------IF CELL IS CONSTANT-HEAD MOVE ON TO NEXT LAYER FOR REACH.
            IF( IBOUND(jc,ir,kk).LT.0 ) CYCLE LAYERS  
            h0    = REAL( HOLD(jc,ir,kk), 8 )
            h1    = HNEW(jc,ir,kk)
            fdelt = SWRTIME(n)%FDELT
            h     = ( DONE - fdelt ) * h0 + fdelt * h1
C-------------DETERMINE SWR1 AQUIFER-REACH EXCHANGE APPROACH FOR REACH
C             DEFAULT CONDITION IS AQUIFER-REACH EXCHANGE ADDED TO HCOF+RHS
            rhsonly = .FALSE.
            IF ( h.LT.rbot ) THEN
              h = rbot
              rhsonly = .TRUE.
            END IF
C-------------CALCULATE AQUIFER-REACH EXCHANGE USING APPROPRIATE METHOD
            IF ( rhsonly ) THEN
              b     = Rch%CURRENTQAQ(kl)%QAQFLOW
            ELSE
              s     = RSTAGE(irch,n)
              b     = Rch%CURRENTQAQ(kl)%CONDUCTANCE * (s - h)
            END IF
            rrate       = b * dtscale
            value       = value + rrate
          END DO LAYERS
        END DO TIMES      
C---------RETURN
9999    RETURN
      END FUNCTION SSWR_MFQAQ
      
      DOUBLEPRECISION FUNCTION SSWR_KELLEY_PERTB(V) RESULT(value)
        USE GWFSWRMODULE, ONLY: DZERO, DONE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, INTENT(IN) :: V
C       + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: e
C        DOUBLEPRECISION :: sgn
C       + + + FUNCTIONS + + +
C       + + + INPUT FORMATS + + +
C       + + + OUTPUT FORMATS + + +
C       + + + CODE + + +
        value = DZERO
        e     = SQRT( EPSILON(DZERO) )
C        sgn   = DONE
C        IF ( ABS(V).GT.DZERO ) sgn = V / ABS( V )
C        value = MAX( e * ABS( V ) , e ) * sgn
        value = MAX( e * ABS( V ) , e )
C---------RETURN
9999    RETURN
      END FUNCTION SSWR_KELLEY_PERTB
      
      DOUBLEPRECISION FUNCTION SSWR_CALC_MAGQM(I,J,Gsi,Gsj,Dl,P) 
     2  RESULT(value)
        USE GWFSWRMODULE
        USE GLOBAL,       ONLY: NCOL, NROW, DELR, DELC
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN)         :: I
        INTEGER, INTENT(IN)         :: J
        DOUBLEPRECISION, INTENT(IN) :: Gsi
        DOUBLEPRECISION, INTENT(IN) :: Gsj
        DOUBLEPRECISION, INTENT(IN) :: Dl
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN) :: P
C       + + + LOCAL DEFINITIONS + + +
        INTEGER         :: irow, icol
        INTEGER         :: jrow, jcol
        INTEGER         :: ixcon
        INTEGER         :: ieval
        INTEGER         :: isgn, isgnm1, isgnp1
        INTEGER         :: irchrow, irchcol
        INTEGER         :: jrowm1, jrowp1
        INTEGER         :: jcolm1, jcolp1
        INTEGER         :: jrchm1, jrchp1
        INTEGER         :: jc
        INTEGER         :: jrch
        INTEGER         :: jrgm1, jrgp1
        DOUBLEPRECISION :: sfmag
        DOUBLEPRECISION :: ds
        DOUBLEPRECISION :: sf
        DOUBLEPRECISION :: peval
        DOUBLEPRECISION :: sfeval
        DOUBLEPRECISION :: d
        DOUBLEPRECISION :: psm1, psp1
        DOUBLEPRECISION :: dsm1, dsp1
        DOUBLEPRECISION :: dlm1, dlp1
        DOUBLEPRECISION :: sfm1, sfp1
        DOUBLEPRECISION :: sfmagm1, sfmagp1
C       + + + INPUT FORMATS + + +
C       + + + OUTPUT FORMATS + + +
C       + + + CODE + + +
C         CALCULATE STAGE DIFFERENCE FOR FACE
        ds      = (Gsj - Gsi)
        sf      = ds / Dl
        sfmag   = ABS( sf )
        value   = sfmag
C         RETURN IF NO STAGE DIFFERENCE
        IF ( ABS(ds).LT.NEARZERO ) GOTO 9999
C         DETERMINE IF CONNECTION IS ALONG A ROW OR A COLUMN
        irow    = REACH(I)%IRCH
        icol    = REACH(I)%JRCH
        jrow    = REACH(J)%IRCH
        jcol    = REACH(J)%JRCH
C         I-J CONNECTION IN Y-DIRECTION - ALONG A COLUMN
        IF ( irow.NE.jrow ) THEN
          ixcon = 0
C         I-J CONNECTION IN X-DIRECTION - ALONG A ROW
        ELSE
          ixcon = 1
        END IF
C         DETERMINE REACH TO USE TO DETERMINE THE GRADIENT 
C         FOR FACE(IJ)
        IF ( sf.LT.DZERO ) THEN
          ieval  = I
          peval  = Gsi
          sfeval = sf
        ELSE
          ieval  = J
          peval  = Gsj
          sfeval = -sf
        END IF
        isgn = 1
        IF ( sfeval.LT.DZERO ) isgn = -1
C         DETERMINE WHICH TWO CELLS TO LOOK IN TO DETERMINE THE
C         GRADIENT FOR FACE(IJ)
        irchrow = REACH(ieval)%IRCH
        irchcol = REACH(ieval)%JRCH
        jrowm1  = irchrow
        jrowp1  = irchrow
        jcolm1  = irchcol
        jcolp1  = irchcol
C         I-J CONNECTION IN X-DIRECTION - EVALUATE ADJACENT ROWS
        IF ( ixcon.NE.0 ) THEN
          jrowm1 = jrowm1 - 1
          jrowp1 = jrowp1 + 1
C         I-J CONNECTION IN Y-DIRECTION - EVALUATE ADJACENT COLUMNS
        ELSE
          jcolm1 = jcolm1 - 1
          jcolp1 = jcolp1 + 1
        END IF
        jrchm1  = 0
        jrchp1  = 0
C         DETERMINE THE REACH NUMBERS FOR THE TWO CELLS USED TO 
C         DETERMINE THE GRADIENT FOR FACE(IJ)
        IF ( jrowm1.GE.1 .AND. jcolm1.GE.1 ) THEN
          jrchm1 = IQMCI(jcolm1,jrowm1)
        END IF
        IF ( jrowp1.LE.NROW .AND. jcolp1.LE.NCOL ) THEN
          jrchp1 = IQMCI(jcolp1,jrowp1)
        END IF
C         DETERMINE IF THE TWO CELLS USED TO DETERMINE THE
C         GRADIENT FOR FACE(IJ) ARE STRUCTURE CONTROLLED.
C         ONE-DIMENSIONAL (NOT MODEL-BASED) REACHES HAVE
C         jrchm1 AND/OR jrchp1 VALUES EQUAL TO IZERO
        DO jc = 1, REACH(ieval)%NCONN
          jrch  = REACH(ieval)%ICONN(jc)
          IF ( ABS(jrch).EQ.jrchm1 ) THEN
            IF ( jrch.LT.0 ) jrchm1 = 0
          END IF
          IF ( ABS(jrch).EQ.jrchp1 ) THEN
            IF ( jrch.LT.0 ) jrchp1 = 0
          END IF
        END DO
C         CALCULATE THE GRADIENT ACROSS THE TWO CELLS USED TO DETERMINE THE
C         GRADIENT FOR FACE(IJ)
        d       = DZERO
        sfmagm1 = DZERO
        sfmagp1 = DZERO
        IF ( jrchm1.GT.0 ) THEN
          jrgm1  = REACH(jrchm1)%IRG
          psm1   = P(jrgm1) + REACH(jrchm1)%OFFSET
          dsm1   = psm1 - peval
C           I-J CONNECTION IN X-DIRECTION - EVALUATE ADJACENT ROWS
          IF ( ixcon.NE.0 ) THEN
            dlm1 = ( DELC(irchrow) + DELC(jrowm1) ) * DONEHALF !/ DTWO
C           I-J CONNECTION IN Y-DIRECTION - EVALUATE ADJACENT COLUMNS
          ELSE
            dlm1 = ( DELR(irchcol) + DELC(jcolm1) ) * DONEHALF !/ DTWO
          END IF
          sfm1   = dsm1 / dlm1
          isgnm1 = 1
          IF ( sfm1.LT.DZERO ) isgnm1 = -1
          IF ( isgnm1.NE.isgn ) THEN
            sfmagm1 = DZERO
          ELSE
            sfmagm1 = SQRT( sfeval*sfeval + sfm1*sfm1 )
            d       = d + DONE
          END IF
        END IF
        IF ( jrchp1.GT.0 ) THEN
          jrgp1 = REACH(jrchp1)%IRG
          psp1  = P(jrgp1) + REACH(jrchp1)%OFFSET
          dsp1  = psp1 - peval
C           I-J CONNECTION IN X-DIRECTION - EVALUATE ADJACENT ROWS
          IF ( ixcon.NE.0 ) THEN
            dlp1  = ( DELC(irchrow) + DELC(jrowp1) ) * DONEHALF !/ DTWO
C           I-J CONNECTION IN Y-DIRECTION - EVALUATE ADJACENT COLUMNS
          ELSE
            dlp1  = ( DELR(irchcol) + DELC(jcolp1) ) * DONEHALF !/ DTWO
          END IF
          sfp1  = dsp1 / dlp1
          isgnp1 = 1
          IF ( sfp1.LT.DZERO ) isgnp1 = -1
          IF ( isgnp1.NE.isgn ) THEN
            sfmagp1 = DZERO
          ELSE
            sfmagp1 = SQRT( sfeval*sfeval + sfp1*sfp1 )
            d       = d + DONE
          END IF
        END IF
        IF ( d.GT.DZERO ) THEN
          sfmag = ( sfmagm1 + sfmagp1 ) / d
        END IF
        value = sfmag
C---------RETURN
9999    RETURN
      END FUNCTION SSWR_CALC_MAGQM

      DOUBLEPRECISION FUNCTION SSWR_CALC_BILINMAGQM(I,J,Gsi,Gsj,Dl,P) 
     2  RESULT(value)
        USE GWFSWRMODULE
        USE GLOBAL,       ONLY: NCOL, NROW, DELR, DELC
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN)         :: I
        INTEGER, INTENT(IN)         :: J
        DOUBLEPRECISION, INTENT(IN) :: Gsi
        DOUBLEPRECISION, INTENT(IN) :: Gsj
        DOUBLEPRECISION, INTENT(IN) :: Dl
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN) :: P
C       + + + LOCAL DEFINITIONS + + +
        INTEGER         :: irow, icol
        INTEGER         :: jrow, jcol
        INTEGER         :: iirow, iicol
        INTEGER         :: irch43, irch12
        INTEGER         :: irch41, irch32
        INTEGER         :: iirow1, iirow2
        INTEGER         :: iicol1, iicol2
        INTEGER         :: irchconn
        INTEGER         :: irg
        DOUBLEPRECISION :: ds, sf
        DOUBLEPRECISION :: sfdir2, sfmag
        DOUBLEPRECISION :: s1, s2
        DOUBLEPRECISION :: d1, d2, d3, d4
        DOUBLEPRECISION :: g1, g2, g3, g4
        DOUBLEPRECISION :: dx1, dx2, dy1, dy2
        DOUBLEPRECISION :: dxt, dyt
        DOUBLEPRECISION :: t, u
C       + + + INPUT FORMATS + + +
C       + + + OUTPUT FORMATS + + +
C       + + + CODE + + +
C         CALCULATE STAGE DIFFERENCE FOR FACE
        ds      = (Gsj - Gsi)
        sf      = ds / Dl
        sfmag   = ABS( sf )
        value   = sfmag
C         RETURN IF NO STAGE DIFFERENCE
        IF ( ABS(ds).LT.NEARZERO ) GOTO 9999
C         DETERMINE IF CONNECTION IS ALONG A ROW OR A COLUMN
        irow    = REACH(I)%IRCH
        icol    = REACH(I)%JRCH
        jrow    = REACH(J)%IRCH
        jcol    = REACH(J)%JRCH
        d1      = DZERO
        d2      = DZERO
        d3      = DZERO
        d4      = DZERO
        g1      = DZERO
        g2      = DZERO
        g3      = DZERO
        g4      = DZERO
C         I-J CONNECTION IN Y-DIRECTION - ALONG A COLUMN
        IF ( irow.NE.jrow ) THEN
C           LOWER FACE CONNECTION
          IF ( irow.LT.jrow ) THEN
            irch43 = I
            irch12 = J
            iirow1 = irow
            iirow2 = jrow
C           UPPER FACE CONNECTION
          ELSE
            irch43 = J
            irch12 = I
            iirow1 = jrow
            iirow2 = irow
          END IF
          iicol = icol
          iirow = irow
C          CALCULATE TERMS
          d4    = DELR(iicol) * DONEHALF
          IF ( icol-1.GT.0 ) THEN
            irchconn = IQMCI(iicol-1,iirow1)
            IF ( irchconn.GT.0 ) THEN
              irg = REACH(irchconn)%IRG
              s1  = P(irg) + REACH(irchconn)%OFFSET
              irg = REACH(irch43)%IRG
              s2  = P(irg) + REACH(irch43)%OFFSET
              d4  = d4 + DELR(iicol-1) * DONEHALF
              g4  = ( s1 - s2 ) / d4
            END IF
          END IF
          d3    = DELR(iicol) * DONEHALF
          IF ( iicol+1.LE.NCOL ) THEN
            irchconn = IQMCI(iicol+1,iirow1)
            IF ( irchconn.GT.0 ) THEN
              irg = REACH(irch43)%IRG
              s1  = P(irg) + REACH(irch43)%OFFSET
              irg = REACH(irchconn)%IRG
              s2  = P(irg) + REACH(irchconn)%OFFSET
              d3  = d3 + DELR(iicol+1) * DONEHALF
              g3  = ( s1 - s2 ) / d3
            END IF
          END IF
          d1    = DELR(iicol) * DONEHALF
          IF ( iicol-1.GT.0 ) THEN
            irchconn = IQMCI(iicol-1,iirow2)
            IF ( irchconn.GT.0 ) THEN
              irg = REACH(irchconn)%IRG
              s1  = P(irg) + REACH(irchconn)%OFFSET
              irg = REACH(irch12)%IRG
              s2  = P(irg) + REACH(irch12)%OFFSET
              d1  = d1 + DELR(iicol-1) * DONEHALF
              g1  = ( s1 - s2 ) / d1
            END IF
          END IF
          d2    = DELR(iicol) * DONEHALF
          IF ( iicol+1.LE.NCOL ) THEN
            irchconn = IQMCI(iicol+1,iirow2)
            IF ( irchconn.GT.0 ) THEN
              irg = REACH(irch12)%IRG
              s1  = P(irg) + REACH(irch12)%OFFSET
              irg = REACH(irchconn)%IRG
              s2  = P(irg) + REACH(irchconn)%OFFSET
              d2 = d2 + DELR(iicol+1) * DONEHALF
              g2 = ( s1 - s2 ) / d2
            END IF
          END IF
          dx1 = DELR( REACH(irch43)%JRCH ) * DONEHALF
          dx2 = DELR( REACH(irch12)%JRCH ) * DONEHALF
          dxt = dx1 + dx2
          t   = dx1 / dxt
          dy2 = DELC( REACH(irch43)%IRCH ) * DONEHALF
          dy1 = DELC( REACH(irch12)%IRCH ) * DONEHALF
          dyt = dy1 + dy2
          u   = dy1 / dyt
C         I-J CONNECTION IN X-DIRECTION - ALONG A ROW
        ELSE
C           LEFT FACE CONNECTION
          IF ( icol.LT.jcol ) THEN
            irch41 = I
            irch32 = J
            iicol1 = icol
            iicol2 = jcol
C           RIGHT FACE CONNECTION
          ELSE
            irch41 = J
            irch32 = I
            iicol1 = jcol
            iicol2 = icol
          END IF
          iicol = icol
          iirow = irow
C          CALCULATE TERMS
          d4    = DELC(iirow) * DONEHALF
          IF ( iirow-1.GT.0 ) THEN
            irchconn = IQMCI(iicol1,iirow-1)
            IF ( irchconn.GT.0 ) THEN
              irg = REACH(irch41)%IRG
              s1  = P(irg) + REACH(irch41)%OFFSET
              irg = REACH(irchconn)%IRG
              s2  = P(irg) + REACH(irchconn)%OFFSET
              d4  = d4 + DELC(iirow-1) * DONEHALF
              g4  = ( s1 - s2 ) / d4
            END IF
          END IF
          d3    = DELC(iirow) * DONEHALF
          IF ( iirow-1.GT.0 ) THEN
            irchconn = IQMCI(iicol2,iirow-1)
            IF ( irchconn.GT.0 ) THEN
              irg = REACH(irch32)%IRG
              s1  = P(irg) + REACH(irch32)%OFFSET
              irg = REACH(irchconn)%IRG
              s2  = P(irg) + REACH(irchconn)%OFFSET
              d3  = d3 + DELC(iirow-1) * DONEHALF
              g3  = ( s1 - s2 ) / d3
            END IF
          END IF
          d1    = DELC(iirow) * DONEHALF
          IF ( iirow+1.LE.NCOL ) THEN
            irchconn = IQMCI(iicol1,iirow+1)
            IF ( irchconn.GT.0 ) THEN
              irg = REACH(irchconn)%IRG
              s1  = P(irg) + REACH(irchconn)%OFFSET
              irg = REACH(irch41)%IRG
              s2  = P(irg) + REACH(irch41)%OFFSET
              d1  = d1 + DELC(iirow+1) * DONEHALF
              g1  = ( s1 - s2 ) / d1
            END IF
          END IF
          d2    = DELR(iirow) * DONEHALF
          IF ( iirow+1.LE.NCOL ) THEN
            irchconn = IQMCI(iicol2,iirow+1)
            IF ( irchconn.GT.0 ) THEN
              irg = REACH(irchconn)%IRG
              s1  = P(irg) + REACH(irchconn)%OFFSET
              irg = REACH(irch32)%IRG
              s2  = P(irg) + REACH(irch32)%OFFSET
              d2  = d2 + DELR(iirow+1) * DONEHALF
              g2  = ( s1 - s2 ) / d2
            END IF
          END IF
          dx1 = DELR( REACH(irch41)%JRCH ) * DONEHALF
          dx2 = DELR( REACH(irch32)%JRCH ) * DONEHALF
          dxt = dx1 + dx2
          t   = dx1 / dxt
          dy1 = DELC( REACH(irch41)%IRCH ) * DONEHALF
          dy2 = DELC( REACH(irch32)%IRCH ) * DONEHALF
          dyt = dy1 + dy2
          u   = dy1 / dyt
        END IF
        sfdir2 = ( DONE - t ) * ( DONE - u ) * g1 +
     2           t * ( DONE - u ) * g2 +
     3           t * u * g3 +
     4           ( DONE - t ) * u * g4
        sfmag = SQRT( sfdir2 * sfdir2 + sf * sf )
        value = sfmag
C---------RETURN
9999    RETURN
      END FUNCTION SSWR_CALC_BILINMAGQM

      DOUBLEPRECISION FUNCTION SSWR_GET_MAGQM(I,J,Sf) RESULT(value)
        USE GWFSWRMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN)         :: I
        INTEGER, INTENT(IN)         :: J
        DOUBLEPRECISION, INTENT(IN) :: Sf
C       + + + LOCAL DEFINITIONS + + +
        INTEGER         :: ic
        INTEGER         :: jrch
C       + + + INPUT FORMATS + + +
C       + + + OUTPUT FORMATS + + +
C       + + + CODE + + +
C
C---------GET CALCULATED STAGE GARDIENT MAGNITUDE
        value = Sf
        DO ic = 1, REACH(I)%NCONN
          jrch  = REACH(I)%ICONN(ic)
          IF ( ABS(jrch).EQ.J ) THEN
            IF ( jrch.GT.0 ) THEN
              value = REACH(I)%SFM(ic)
            END IF
            EXIT
          END IF
        END DO
C---------RETURN
9999    RETURN
      END FUNCTION SSWR_GET_MAGQM
      
      SUBROUTINE SSWR_EST_EXPLICIT_SFM(Ps)
        USE GWFSWRMODULE
        USE GLOBAL,       ONLY: NCOL, NROW, DELR, DELC
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        DOUBLEPRECISION, DIMENSION(NRCHGRP), INTENT(IN) :: Ps
C       + + + LOCAL DEFINITIONS + + +
        INTEGER         :: i, j
        INTEGER         :: ic
        INTEGER         :: irgi, irgj
        INTEGER         :: irowi, jcoli
        INTEGER         :: irowj, jcolj
        DOUBLEPRECISION :: dli, dlj, dl
        DOUBLEPRECISION :: si, sj
        DOUBLEPRECISION :: sfm
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SSWR_CALC_MAGQM
        DOUBLEPRECISION :: SSWR_CALC_BILINMAGQM
C       + + + INPUT FORMATS + + +
C       + + + OUTPUT FORMATS + + +
C       + + + CODE + + +
        DO i = 1, NREACHES
          dli   = DZERO
          IF ( REACH(i)%IGEOTYPE.NE.5 ) CYCLE
          irgi    = REACH(i)%IRG
          irowi   = REACH(i)%IRCH
          jcoli   = REACH(i)%JRCH
          si      = Ps(irgi) + REACH(i)%OFFSET
          DO ic = 1, REACH(i)%NCONN
            j = REACH(i)%ICONN(ic)
            IF ( REACH(j)%IGEOTYPE.NE.5 ) CYCLE
            irgj    = REACH(j)%IRG
            irowj   = REACH(j)%IRCH
            jcolj   = REACH(j)%JRCH
            dli     = DZERO
            dlj     = DZERO
            IF (irowi.NE.irowj) dli = DELC(irowi) * DONEHALF
            IF (jcoli.NE.jcolj) dli = DELR(jcoli) * DONEHALF
            IF (irowi.NE.irowj) dlj = DELC(irowj) * DONEHALF
            IF (jcoli.NE.jcolj) dlj = DELR(jcolj) * DONEHALF
            dl    = dli + dlj
            sj    = Ps(irgj) + REACH(j)%OFFSET
            IF ( ICIBL.NE.0 ) THEN
              sfm   = SSWR_CALC_BILINMAGQM(i,j,si,sj,dl,Ps)
            ELSE
              sfm   = SSWR_CALC_MAGQM(i,j,si,sj,dl,Ps)
            END IF
            REACH(i)%SFM(ic) = sfm
          END DO
        END DO
C---------RETURN
9999    RETURN
      END SUBROUTINE SSWR_EST_EXPLICIT_SFM

C     + + + DUMMY ARGUMENTS + + +
C     + + + LOCAL DEFINITIONS + + +
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
