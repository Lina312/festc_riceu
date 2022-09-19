      PROGRAM epic_sumlayers_weighted_crf

C**********************************************************************************
C
C  FUNCTION: (1) Read 3D FILEIN CONC
C            (2) Just add the column together       
C            (3) Write to FILEOUT
C  PRECONDITIONS: None
C
C  CONDITIONS REQUIRED IF YOU NEED THIS PROGRAM PORTABLE:
C               (1) "setenv"s for output file and output file,
C               (2) link to libraries of m3io and netCDF
C               (3) from a directory containing PARMS3.EXT, FDESC3.EXT, 
C                   IODECL3.EXT
C               (4)LINA LUO Revision:
C                  only write NINX,NINO,NIN2,HONO,DENO,DEN2,TONO,TON2,TONX,
C              &   AVOL,FNO,FNO3.FNH3
C              &   ID (42-54)
C              &   UNIT: kgN
C              &   CRF cal for NINO,DENO, TONO,TONX,HONO
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: collapx.F, locate.F from CONC3.0
C
C  REVISION HISTORY:  May 16, 2011 - Rui ZHANG
C  REVISION HISTORY:  May 26, 2021 - Lina LUO
C**********************************************************************************

      IMPLICIT NONE 

C..INCLUDES:
      
      INCLUDE 'PARMS3.EXT'   ! I/O parameters definitions
      INCLUDE 'FDESC3.EXT'   ! file header data structure
      INCLUDE 'IODECL3.EXT'  ! I/O definitions and declarations 

C..PARAMETERS:

c     INTEGER    XSTAT0,     XSTAT1  
c     PARAMETER (XSTAT0 = 0, XSTAT1 = 1) ! Status code

      real, dimension(:,:,:), allocatable::CONC    !buffer
      real, dimension(:,:,:), allocatable::CROPPERC !buffer 
      real, dimension(:,:,:,:,:), allocatable::CONCIN
      real, dimension(:,:,:,:,:), allocatable::CROPF
      real, dimension(:,:,:,:,:), allocatable::CONCOUT
      real, dimension(:,:,:,:,:), allocatable::CRF
      real, dimension(:,:,:),allocatable::CRFPERC

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
C
      CHARACTER*16    PNAME   ! Program name 
      CHARACTER*20    GDNAME
      CHARACTER*16    FILEIN  ! Input 3D FILEIN file
      CHARACTER*50    FILEIN1 !Crop fraction file
      CHARACTER*50    FILEIN2 !CRF file
      CHARACTER*16    FILEOUT ! Output
      CHARACTER*16    VNAMEIN(MXVARS3) ! Variable names from FILEONE
      CHARACTER*200   VDESCIN(MXVARS3) ! Variable description from FILEONE
      CHARACTER*20    VUNITIN(MXVARS3) ! Variable unit from FILEONE
      CHARACTER*16    VNAM_UI ! Variable names from user input

      INTEGER         NARGS   ! Number of command line arguements
      INTEGER         LOGDEV  ! FORTRAN unit number for log file
      INTEGER         JDATE   ! current model date, coded YYYYDDD
      INTEGER         JTIME   ! midpoint model time, coded HHMMSS
      INTEGER         NVARS   !  number of vbles to be totaled
      INTEGER         SDATE   ! start date, file one
      INTEGER         STIME   ! start time, file one
      INTEGER         TSTEP   ! size of time step
      INTEGER         MXREC   ! Number of records on file
      INTEGER         NTHIK   ! Boundary cell thickness
      INTEGER         NSTEPS  ! Number of time steps to process
      INTEGER         REPDEV  ! Unit number of report file
      INTEGER         STATUS  ! Status code
      
      INTEGER         FTYPE
      INTEGER         GDTYP
      REAL            P_ALP
      REAL            P_BET
      REAL            P_GAM
      REAL            XCENT
      REAL            YCENT
      REAL            XORIG
      REAL            YORIG
      INTEGER         XCELL
      INTEGER         YCELL
      INTEGER         VGTYP
      INTEGER         VGTOP

      INTEGER         COL0    ! Lower bound subdomain (column)
      INTEGER         COL1    ! Upper bound subdomain FileOne (column)
      INTEGER         COL2    ! Upper bound subdomain FileTwo (column)
      INTEGER         ROW0    ! Lower bound subdomain (row)
      INTEGER         ROW1    ! Upper bound subdomain FileOne (row)
      INTEGER         ROW2    ! Upper bound subdomain FileTwo (row)
      INTEGER         LAY0    ! Lower bound subdomain (layer)
      INTEGER         LAY1    ! Upper bound subdomain FileOne (layer)
      INTEGER         LAY2    ! Upper bound subdomain FileTwo (layer)
C
      INTEGER     S     ! FILEONE Variable loop index
      INTEGER     N     ! FILETWO Variable loop index
      INTEGER     M     ! TMPLFILE Variable loop index
      INTEGER     C     ! Column loop index
      INTEGER     R     ! Row loop index
      INTEGER     L     ! Layer loop index
      INTEGER     II, J, K, T, I, SS
c
      INTEGER DAYBEG, TIMBEG, TBEG
c
      REAL, DIMENSION (:), ALLOCATABLE :: VGLVS
      REAL    TEMP
C**********************************************************************

      DATA FILEIN  /   'EXTFILEONE'  /
c
      DATA FILEIN1 /   'EXTFILETWO'  /
c
      DATA FILEIN2 / 'EXTFILETHREE' /
c  
      DATA FILEOUT   /   'EXTOUTONE'  /
c
      DATA PNAME  / 'conc_sumlayers' /
    
C.......   First:  Initialize the ioapi and obtain array size

      LOGDEV = INIT3()           !  initialization returns unit # for log

      IF ( .NOT. OPEN3( FILEIN, FSREAD3, PNAME) )THEN 
            WRITE(*, *) 'Error: open file failed (3)'
      ENDIF 

      IF ( .NOT. DESC3( FILEIN ) ) THEN
             WRITE(*, *) 'Error: read file failed (4)'
             STOP 'Bad exit'
      ENDIF
     
      WRITE(*,*)
      WRITE(*,*) ' >>---->  Start reading data 3D CONC'
      WRITE(*,*)      
c
c  { Set file variables   }

      TSTEP  = TSTEP3D
      NVARS  = NVARS3D
      SDATE  = SDATE3D
      STIME  = STIME3D
      NTHIK  = NTHIK3D
      MXREC  = MXREC3D
      

      print *, 'TSTEP3D, NVARS3D, SDATE3D, STIME3D, NTHIK3D, MXREC3D:'
      print *,  TSTEP3D, NVARS3D, SDATE3D, STIME3D, NTHIK3D, MXREC3D
      print *, ' '

      JDATE  = SDATE
      JTIME  = STIME
      DO S = 1, NVARS
          VNAMEIN( S ) = VNAME3D( S )
          VDESCIN( S ) = VDESC3D( S )
          VUNITIN( S ) = UNITS3D( S )
c          print*,'S =',S,' VNAMEIN =',VNAMEIN(S)
      END DO

      COL0 = 1
      COL1 = NCOLS3D
      ROW0 = 1
      ROW1 = NROWS3D
      LAY0 = 1
      LAY1 = NLAYS3D
c read the other headers

      GDNAME = GDNAM3D
      FTYPE = FTYPE3D
      GDTYP = GDTYP3D
      P_ALP = P_ALP3D
      P_BET = P_BET3D
      P_GAM = P_GAM3D
      XCENT = XCENT3D
      YCENT = YCENT3D
      XORIG = XORIG3D
      YORIG = YORIG3D
      XCELL = XCELL3D
      YCELL = YCELL3D
      NTHIK = NTHIK3D
      VGTYP = VGTYP3D
      VGTOP = VGTOP3D	
c
      print *, 'NCOLS3D, NROWS3D, NLAYS3D: ',NCOLS3D, NROWS3D, NLAYS3D
      write(*,*)'the file = ', FILEIN, JDATE, JTIME, TSTEP
c       
      allocate ( VGLVS(LAY1) )
      VGLVS = 0.0
      DO K = LAY0, LAY1
       VGLVS(K) = VGLVS3D(K)
c       print*,'K, VGLVS3D, VGLVS'
c       print*,K,VGLVS3D(K),VGLVS(K)
      ENDDO
      
      allocate ( CONCIN (COL1, ROW1, LAY1, NVARS, MXREC) ) !3D CONC file
      allocate ( CONCOUT (COL1, ROW1, 1, NVARS, MXREC) )  !1D column sum file
      allocate ( CONC (COL1, ROW1, LAY1) )
      CONCIN = 0.0
      CONCOUT = 0.0
      CONC = 0.0
      
      print *, 'pass the matrix allocation'
c

c     read the standard CMAQ 3D CONC file
      NSTEPS = MXREC
      DO T = 1, NSTEPS   
        DO S = 1, NVARS

              IF ( XTRACT3( FILEIN, VNAMEIN(S), LAY0, LAY1,
     &    ROW0, ROW1, COL0, COL1, JDATE, JTIME, CONC)) THEN
              DO K = 1, LAY1
              DO J = 1, ROW1
                DO II = 1, COL1
                  CONCIN(II,J,K,S,T) = CONC(II,J,K)
                END DO ! II for Columns
              END DO ! J for Rows
              END DO ! K for Layers
              ELSE
                Print*,  PNAME, JDATE, JTIME,
     &                   'Read failure:  file ' // FILEIN //
     &                   ' variable NO' // VNAM_UI, XSTAT1
                stop ' end of file...'
              END IF

        END DO ! end the spcies loop
        CALL NEXTIME( JDATE, JTIME, TSTEP )
      END DO !{ looping over time steps }

c Close the FILEIN file
      IF ( .NOT. CLOSE3( FILEIN ) )THEN
            WRITE(*, *) 'Error: close file failed (5)'
      ENDIF
      deallocate (CONC)

      print *, 'pass reading the 3D CONC file'
C.......   First:  Initialize the ioapi and obtain array size

      LOGDEV = INIT3()         !  initialization returns unit # for log


      IF ( .NOT. OPEN3( FILEIN1, FSREAD3, PNAME) )THEN
            WRITE(*, *) 'Error: open file failed (3)'
      ENDIF

      IF ( .NOT. DESC3( FILEIN1 ) ) THEN
             WRITE(*, *) 'Error: read file failed (6)'
             STOP 'Bad exit'
      ENDIF

      WRITE(*,*)
      WRITE(*,*) ' >>---->  Start reading data 3D CROPF'
      WRITE(*,*)
c
c  { Set file variables   }

      JDATE  = SDATE
      JTIME  = STIME

c      print *, 'NCOLS3D, NROWS3D, NLAYS3D: ',NCOLS3D, NROWS3D, NLAYS3D
c      write(*,*)'the file = ', FILEIN1, JDATE, JTIME, TSTEP
c
      allocate ( CROPF (COL1, ROW1, LAY1, 1, MXREC) )
      allocate ( CROPPERC (COL1, ROW1, LAY1) )

      print *, 'pass the matrix allocation'

c     read the standard  3D CROPFRACTION file
      NSTEPS = MXREC
      DO T = 1, NSTEPS
              IF ( XTRACT3( FILEIN1, 'CROPF', LAY0, LAY1,
     &    ROW0, ROW1, COL0, COL1, JDATE, JTIME, CROPPERC)) THEN
              DO K = 1, LAY1
              DO J = 1, ROW1
                DO II = 1, COL1
                  CROPF(II,J,K,1,T) = CROPPERC(II,J,K)
                END DO ! II for Columns
              END DO ! J for Rows
              END DO ! K for Layers
              ELSE
                Print*,  PNAME, JDATE, JTIME,
     &                   'Read failure:  file ' // FILEIN1 //
     &                   ' variable NO' // VNAM_UI, XSTAT1
                stop ' end of file...'
              END IF 
        CALL NEXTIME( JDATE, JTIME, TSTEP )
      END DO !{ looping over time steps }

c
c Close the FILEIN1 file
      IF ( .NOT. CLOSE3( FILEIN1 ) )THEN
            WRITE(*, *) 'Error: close file failed (5)'
      ENDIF
      deallocate (CROPPERC)

      print *, 'pass reading the 3D CROPF file'

c####################################################################
C.......   First:  Initialize the ioapi and obtain array size

      LOGDEV = INIT3()         !  initialization returns unit # for log


      IF ( .NOT. OPEN3( FILEIN2, FSREAD3, PNAME) )THEN
            WRITE(*, *) 'Error: open file failed (3)'
      ENDIF

      IF ( .NOT. DESC3( FILEIN2 ) ) THEN
             WRITE(*, *) 'Error: read file failed (6)'
             STOP 'Bad exit'
      ENDIF

      WRITE(*,*)
      WRITE(*,*) ' >>---->  Start reading data 3D CRF'
      WRITE(*,*)
c
c  { Set file variables   }

      JDATE  = SDATE
      JTIME  = STIME

c      print *, 'NCOLS3D, NROWS3D, NLAYS3D: ',NCOLS3D, NROWS3D, NLAYS3D
c      write(*,*)'the file = ', FILEIN1, JDATE, JTIME, TSTEP
c
      allocate ( CRF (COL1, ROW1, LAY1, 1, MXREC) )
      allocate ( CRFPERC (COL1, ROW1, LAY1) )

      print *, 'pass the matrix allocation'

c     read the standard  3D CROPFRACTION file
      NSTEPS = MXREC
      DO T = 1, NSTEPS
              IF ( XTRACT3( FILEIN2, 'CRF', LAY0, LAY1,
     &    ROW0, ROW1, COL0, COL1, JDATE, JTIME, CRFPERC)) THEN
              DO K = 1, LAY1
              DO J = 1, ROW1
                DO II = 1, COL1
                  CRF(II,J,K,1,T) = CRFPERC(II,J,K)
                END DO ! II for Columns
              END DO ! J for Rows
              END DO ! K for Layers
              ELSE
                Print*,  PNAME, JDATE, JTIME,
     &                   'Read failure:  file ' // FILEIN1 //
     &                   ' variable NO' // VNAM_UI, XSTAT1
                stop ' end of file...'
              END IF 
        CALL NEXTIME( JDATE, JTIME, TSTEP )
      END DO !{ looping over time steps }

c
c Close the FILEIN1 file
      IF ( .NOT. CLOSE3( FILEIN2 ) )THEN
            WRITE(*, *) 'Error: close file failed (5)'
      ENDIF
      deallocate (CRFPERC)

      print *, 'pass reading the 3D CRF file'
c####################################################################


c Second weighted sum the column for each species
c 144 km2; 100: 1km2 = 100ha
c kgN/ha * cropf * area size = kgN
c results: crop weighted values for each grid cell 
        DO T = 1, NSTEPS
        DO S = 1, NVARS
        DO K = 1, LAY1
        DO J = 1, ROW1
        DO II = 1, COL1 
            IF ( CONCIN(II,J,K,S,T) > 0.0 ) THEN
            SELECT CASE (S)
            CASE(42)
            TEMP = CONCIN(II,J,K,S,T)*(0.01*CROPF(II,J,K,1,T))*
     &  (1-CRF(II,J,K,1,T)) * 144 * 100   
            CONCOUT(II,J,1,S,T) = CONCOUT(II,J,1,S,T) + TEMP
C            CONCOUT(II,J,1,S,T) = 42.00
            CASE(43)
            TEMP = CONCIN(II,J,K,S,T)*(0.01*CROPF(II,J,K,1,T))*
     &  (1-CRF(II,J,K,1,T)) * 144 * 100   
            CONCOUT(II,J,1,S,T) = CONCOUT(II,J,1,S,T) + TEMP
C            CONCOUT(II,J,1,S,T) = 43.00
            CASE(45)
            TEMP = CONCIN(II,J,K,S,T)*(0.01*CROPF(II,J,K,1,T))*
     &  (1-CRF(II,J,K,1,T)) * 144 * 100   
            CONCOUT(II,J,1,S,T) = CONCOUT(II,J,1,S,T) + TEMP 
            CASE(46)
            TEMP = CONCIN(II,J,K,S,T)*(0.01*CROPF(II,J,K,1,T))*
     &  (1-CRF(II,J,K,1,T)) * 144 * 100   
            CONCOUT(II,J,1,S,T) = CONCOUT(II,J,1,S,T) + TEMP
C            CONCOUT(II,J,1,S,T) = 46.00
            CASE(48)
            TEMP = CONCIN(II,J,K,S,T)*(0.01*CROPF(II,J,K,1,T))*
     &  (1-CRF(II,J,K,1,T)) * 144 * 100   
            CONCOUT(II,J,1,S,T) = CONCOUT(II,J,1,S,T) + TEMP
C            CONCOUT(II,J,1,S,T) = 48.00
            CASE(50)
            TEMP = CONCIN(II,J,K,S,T)*(0.01*CROPF(II,J,K,1,T))*
     &  (1-CRF(II,J,K,1,T)) * 144 * 100   
            CONCOUT(II,J,1,S,T) = CONCOUT(II,J,1,S,T) + TEMP
C            CONCOUT(II,J,1,S,T) = 50.00
            CASE DEFAULT
            TEMP = CONCIN(II,J,K,S,T)*(0.01*CROPF(II,J,K,1,T)) *
     &   144 * 100
            CONCOUT(II,J,1,S,T) = CONCOUT(II,J,1,S,T) + TEMP
            END SELECT  
            ENDIF
        ENDDO ! II
        ENDDO ! J
        ENDDO ! K
        ENDDO ! S
        ENDDO ! T
        deallocate (CONCIN)

        print *, 'pass the column weighted sum for each species'

c  Last Step: prepare the NCF with CB05 chemical mechanism
c  (25 hours each)
       LOGDEV = INIT3()
c Write output file description
        FTYPE3D = FTYPE
        UPNAM3D = PNAME
        GDNAM3D = GDNAME
        GDTYP3D = GDTYP
        P_ALP3D = P_ALP
        P_BET3D = P_BET
        P_GAM3D = P_GAM
        XCENT3D = XCENT
        YCENT3D = YCENT
        XORIG3D = XORIG
        YORIG3D = YORIG
        XCELL3D = XCELL
        YCELL3D = YCELL
        NCOLS3D = COL1
        NROWS3D = ROW1
        NLAYS3D = 1       
        NTHIK3D = NTHIK
        VGTYP3D = VGTYP
        VGTOP3D = VGTOP
        DO K = LAY0, LAY1      
           VGLVS3D(K) = VGLVS(K)
        ENDDO
        TSTEP3D = TSTEP
        SDATE3D = SDATE
        STIME3D = STIME    

	print*, 'TSTEP3D is   ', TSTEP


C        NVARS3D=NVARS commented out for only exporting N vars
        NVARS3D=13
       
        DO S = 1, NVARS3D
            SS = S + 41
          UNITS3D(S) = 'kgN'
          VTYPE3D(S) = M3REAL
          VNAME3D(S) = VNAMEIN(SS)
          VDESC3D(S) = TRIM(VDESCIN(SS))//' weighted sum with crop 
     &percentage considering CRF'
          print *, S, SS, VDESC3D(SS)
        END DO

c Write data into output file
        allocate ( CONC(COL1, ROW1, LAY1) )
        CONC = 0.0
        print *, 'after allocate the temp CONC matrix'
        IF ( .NOT. OPEN3( FILEOUT, FSNEW3, PNAME) )THEN 
            WRITE(*, *) 'Error: open file failed (9)'
        ENDIF ! output file opened successfully
        JDATE = SDATE3D
        JTIME = STIME3D
        print *, 'JDATE =', JDATE, 'JTIME=', JTIME
        DO T = 1, MXREC
          DO S = 1, NVARS3D
            SS = S + 41
c           print*,'T S JDATE JTIME,LAY0,LAY1,ROW0,ROW1,COL0,COL1'
c            print*,T,S,JDATE,JTIME,LAY0,LAY1,ROW0,ROW1,COL0,COL1
            DO K = LAY0, 1     
            DO J = ROW0, ROW1
            DO II = COL0, COL1
             CONC(II,J,K) = CONCOUT(II,J,K,SS,T)
            ENDDO ! Columns
            ENDDO ! Rows
            ENDDO ! Layers
            
               IF (.not.WRITE3(FILEOUT,VNAME3D(SS),JDATE,JTIME,CONC))
     &         CALL M3ERR( PNAME, JDATE, JTIME, 'Could not write '//
     &          VNAME3D(SS)//' to '// FILEOUT //' file', .TRUE.)
          ENDDO ! Species
          CALL NEXTIME( JDATE, JTIME, TSTEP)
          print*,'JDATE, JTIME, TSTEP'
          print*, JDATE,JTIME,TSTEP
        ENDDO ! Hours
        IF ( .NOT. CLOSE3( FILEOUT ) )THEN
            WRITE(*, *) 'Error: close file failed (10)'
        ENDIF
        deallocate ( CONC )
c
c CLOSE IOAPI files
      write(*,'(A)')  'Closing IOAPI files'
      If ( SHUT3() ) Then
            WRITE ( *,* )
     &  '|---->  IOAPI output completed successfully  <----|'
      Else
            WRITE ( *,91000 ) PNAME,
     &      'FATAL ERROR shutting down Models-3 I/O'
      End if

      WRITE(*,92300)
     & '>>---->  Program conc_sumlayers.f ',
     & 'completed successfully  <----<<'

      CALL EXIT( 0 )

C******************  FORMAT  STATEMENTS   ******************************
1000   FORMAT (x,f20.4,2(',',f20.4))
91000  FORMAT ( //5X , '*** ERROR ABORT in program',A,'***',
     &            /5X , A , // )        !  generic error message format
92000  FORMAT( 5X, A )
92100  FORMAT( /5X, A, $ )
92200  FORMAT( A16 )
92300  FORMAT( //5X, A )
92400  FORMAT( 4(2X,A16))
93000  FORMAT( '"INPUT M3 FILE: ',A,'"')
94000  FORMAT(1x,e12.7)
97000  FORMAT(1x,A4)
       END
       
