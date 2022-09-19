      PROGRAM NOX_CRF

C**********************************************************************************
C
C  FUNCTION: (1) Read soilbiome GRICRO2D ...
C            (1) Read soilbiome beld_cropf
C            (2) calculate daily average canopy reduction factor          
C            (3) Write to FILEOUT
C  PRECONDITIONS: None
C
C  CONDITIONS REQUIRED IF YOU NEED THIS PROGRAM PORTABLE:
C               (1) "setenv"s for output file and output file,
C               (2) link to libraries of m3io and netCDF
C               (3) from a directory containing PARMS3.EXT, FDESC3.EXT, 
C                   IODECL3.EXT
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: collapx.F, locate.F from MCIP3.0
C
C  REVISION HISTORY:  Nov 16, 2014 - Rui ZHANG
C                     Sep 25, 2020 - Lina LUO
C                     Apr 29, 2021 - Lina LUO
C**********************************************************************************

      IMPLICIT NONE 

C..INCLUDES:
      
      INCLUDE 'PARMS3.EXT'   ! I/O parameters definitions
      INCLUDE 'FDESC3.EXT'   ! file header data structure
      INCLUDE 'IODECL3.EXT'  ! I/O definitions and declarations 

C..PARAMETERS:

c     INTEGER    XSTAT0,     XSTAT1  
c     PARAMETER (XSTAT0 = 0, XSTAT1 = 1) ! Status code
      real, dimension(:,:,:), allocatable:: CRF !CRF 
      real, dimension(:,:,:,:), allocatable:: BUFFER      !buffer 
      real, dimension(:,:,:), allocatable::CRFOUT !CRFOUT
      real, dimension(:,:), allocatable:: TASFC       !surface temperature 
      real, dimension(:,:), allocatable:: SSOLAR      !solar radiation 
      real, dimension(:,:), allocatable:: COSZEN      !cosine of zenith angle
      real, dimension(:,:), allocatable:: PRES        !surface perssure
      real, dimension(:,:), allocatable:: CFRAC       !cloud fraction data
      real, dimension(:,:), allocatable:: WSPD        !10-m wind speed
C      real, dimension(:,:), allocatable:: LAI         !leaf area index


      real, dimension(:,:), allocatable:: AGNO        !normalized emissions for NO from BG3RD
      real, dimension(:,:), allocatable:: LAT         !grid lat
      real, dimension(:,:), allocatable:: LON         !grid lon
      real, dimension(:,:), allocatable:: SOILM       !soil moisture [m3/m3] (PX)
      real, dimension(:,:), allocatable:: SOILMPREV   !soil moisture previous time step [m3/m3] [PX]
      real, dimension(:,:), allocatable:: SOILT       !soil temperature [K] (PX)
      real, dimension(:,:), allocatable:: ISLTYP      !soil type (PX)
      real, dimension(:,:), allocatable:: FERT        !N reservoir (PX)
      real, dimension(:,:), allocatable:: NDEPRES     !N deposition reservior (PX) 
      real, dimension(:,:), allocatable:: DRYPERIOD   !dry period, will determine pluse, hours (PX)
      real, dimension(:,:), allocatable:: PFACTOR     !current pluse factor 
      real, dimension(:,:), allocatable:: NDEPRATE    !fertilizer emission factor

      integer, dimension(:,:), allocatable:: ARID        !arid land
      integer, dimension(:,:), allocatable:: NONARID     !nonarid land 
C      integer, dimension(:,:), allocatable:: LANDFRAC    !soil biotype
      integer, dimension(:,:), allocatable:: SNOCOV      !snow cover
   
      real, dimension(:,:), allocatable:: EMPOL     ! soil NO emission gN/hr buffer
      real, dimension(:,:,:), allocatable:: EMISS     ! emissions in g/s

C..lina luo
      real, dimension(:,:,:), allocatable:: CROPF   ! soil biotype (FESTC1.4.1, 42 types)
      real, dimension(:,:,:), allocatable:: LAI     ! lead area index from FESTC1.4.1
      integer, dimension(:,:,:), allocatable:: LANDTYPE ! layer1-6 type 24 layer 7-42 type 22
      real, dimension(:,:,:), allocatable:: BUFFERSUM  !buffer for daily average


C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
C
      CHARACTER*16    PNAME   ! Program name 
      CHARACTER*20    GDNAME

      CHARACTER*16    METNAME       ! METCRO2D 
      CHARACTER*16    GRIDNAME      ! GRIDCRO2D
C      CHARACTER*16    SOILINSTATE   ! input NO soil data, restart file
C      CHARACTER*16    SOILCLIMATE     ! climate, arid
C      CHARACTER*16    CLIMNAFILE    ! climate, nonarid
      CHARACTER*16    LANDFRACFILE  ! biome type
C      CHARACTER*16    FERTRSFILE    ! fertilizer reservoir
      CHARACTER*16 EPICFILE ! epicfile

      CHARACTER*16    FILEOUT       ! Output for bio emssion consistant consistant with BEIS3 
      CHARACTER*16    SOILOUT       ! output NO soil data, same format as soilinstate

      CHARACTER*16    VNAMEONE(MXVARS3) ! Variable names from FILEONE
      CHARACTER*200   VDESCONE(MXVARS3) ! Variable description from FILEONE
      CHARACTER*20    VUNITONE(MXVARS3) ! Variable unit from FILEONE
      CHARACTER*16    VNAM_UI ! Variable names from user input

      CHARACTER*16    VAR
      CHARACTER*3     CHARDAY

      INTEGER         NARGS   ! Number of command line arguements
      INTEGER         LOGDEV  ! FORTRAN unit number for log file
      INTEGER         JDATE   ! current model date, coded YYYYDDD
      INTEGER         JTIME   ! midpoint model time, coded HHMMSS
      INTEGER         EDATE   ! writeout model date, coded YYYYDDD
      INTEGER         ETIME   ! writeout model time, coded HHMMSS
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
      INTEGER         COL1    ! Upper bound subdomain (column)
      INTEGER         ROW0    ! Lower bound subdomain (row)
      INTEGER         ROW1    ! Upper bound subdomain (row)
      INTEGER         LAY0    ! Lower bound subdomain (layer)
      INTEGER         LAY1    ! Upper bound subdomain (layer)
      INTEGER         NLAY    !buffer layer no. NLAY = 42
C
      INTEGER     S     ! FILEONE Variable loop index
      INTEGER     N     ! FILETWO Variable loop index
      INTEGER     M     ! TMPLFILE Variable loop index
      INTEGER     C     ! Column loop index
      INTEGER     R     ! Row loop index
      INTEGER     L     ! Layer loop index
      INTEGER     II, J, K, T, I

      REAL        EMPL
      REAL, PARAMETER  ::  HR2SEC = 1.0/3600.0   ! hour to second convert factor
      REAL, PARAMETER  ::  SPLITFAC = 1.0        ! split factor for NO 
      REAL, PARAMETER  ::  SDIV = 14.0           ! factor for NO

      INTEGER     YEAR, JDAY
c
      INTEGER DAYBEG, TIMBEG, TBEG
c
      REAL, DIMENSION (:), ALLOCATABLE :: VGLVS

      LOGICAL, SAVE :: INITIAL_RUN              ! false: use SOILINSTATE restrat file
C**********************************************************************

      DATA GRIDNAME     /   'GRIDCRO2D'  /
c      DATA SOILINSTATE  /   'SOILIN'  /
c      DATA SOILCLIMATE    /   'SOILCLIMATE'  /
C  LINA LUO
      DATA EPICFILE / 'EPICFILE' /
      DATA LANDFRACFILE /   'SOILBIOM'  /
c      DATA FERTRSFILE   /   'NFERT'  /
      DATA METNAME      /   'METCRO2D'  /
c
      DATA FILEOUT   /   'CRFOUT'  /
C      DATA SOILOUT   /   'SOILOUT'  /
c
      DATA PNAME  / 'NOX_CRF' /
    


C#############################################
C$$$$$$$    START READ ALL THE STATIC FILES $$$$$$$$$$$$$$$$
C.......   First:  Initialize the ioapi and obtain array size

      LOGDEV = INIT3()           !  initialization returns unit # for log
C start to read the header of LANDFRACFILE
      IF ( .NOT. OPEN3( LANDFRACFILE, FSREAD3, PNAME) )THEN
            WRITE(*, *) 'Error: open file failed (6)'
      ENDIF

      IF ( .NOT. DESC3( LANDFRACFILE ) ) THEN
             WRITE(*, *) 'Error: read file failed (6)'
             STOP 'Bad exit'
      ENDIF

      WRITE(*,*)
      WRITE(*,*) ' >>---->  Start reading LANDFRACFILE '
      WRITE(*,*)
c  { Set file variables   }

      TSTEP  = 10000        ! fixed as one hour segement
C      SDATE  = SDATE3D
C      STIME  = STIME3D
      NTHIK  = NTHIK3D


      COL0 = 1
      COL1 = NCOLS3D
      ROW0 = 1
      ROW1 = NROWS3D
      LAY0 = 1
      LAY1 = NLAYS3D

c read the other headers
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

      allocate ( VGLVS(LAY1) )
      VGLVS = 0.0
      DO K = LAY0, LAY1
       VGLVS(K) = VGLVS3D(K)
       print*,'K, VGLVS3D, VGLVS'
       print*,K,VGLVS3D(K),VGLVS(K)
      ENDDO

C Intialize the landfrac parameters

      allocate ( CROPF (COL1, ROW1, LAY1) ) 
      CROPF = 0.0 
      allocate(LANDTYPE(COL1,ROW1,LAY1))
      LANDTYPE = 0
      DO L = 1, 6
        DO R = 1,ROW1
            DO C = 1,COL1
               
                    LANDTYPE (C,R,L) = 24 
                
            END DO !end row loop
        END DO !end col loop
      END DO !end layer loop

	 DO L = 7, 42
       		 DO R = 1,ROW1
            		DO C = 1,COL1

                    LANDTYPE (C,R,L) = 22
               
            END DO !end row loop
         END DO !end col loop
         END DO !end layer loop



        IF ( .NOT. XTRACT3( LANDFRACFILE, 'CROPF', LAY0, LAY1,
     &    ROW0, ROW1, COL0, COL1, 0, 0, CROPF ))
     &    CALL M3ERR( PNAME, 'Could not read CROPF
     &     from '// LANDFRACFILE //' file', .TRUE.)

      print *, 'CROPF(150,150,13)=',CROPF(150,150,1) !debug lina
      print *, 'CROPF(30,323,2)=', CROPF(30,323,2)   !debug lina
c Close the LANDFRACFILE file      
      IF ( .NOT. CLOSE3( LANDFRACFILE ) )THEN
            WRITE(*, *) 'Error: close file failed (6)'
      ENDIF

C start to read the header of GRIDCRO2D
      IF ( .NOT. OPEN3( GRIDNAME, FSREAD3, PNAME) )THEN
            WRITE(*, *) 'Error: open file failed (1)'
      ENDIF

      IF ( .NOT. DESC3( GRIDNAME ) ) THEN
             WRITE(*, *) 'Error: read file failed (1)'
             STOP 'Bad exit'
      ENDIF

      WRITE(*,*)
      WRITE(*,*) ' >>---->  Start reading GRIDCRO2D '
      WRITE(*,*)

      allocate ( LAT (COL1, ROW1 ) )
      allocate ( LON (COL1, ROW1 ) )
      LAT = 0.0
      LON = 0.0

        IF ( .NOT. XTRACT3( GRIDNAME, 'LAT', LAY0, 1,
     &    ROW0, ROW1, COL0, COL1, 0, 0, LAT ))
     &    CALL M3ERR( PNAME, 'Could not read LAT 
     &     from '// GRIDNAME //' file', .TRUE.)

        IF ( .NOT. XTRACT3( GRIDNAME, 'LON', LAY0, 1,
     &    ROW0, ROW1, COL0, COL1, 0, 0, LON ))
     &    CALL M3ERR( PNAME, 'Could not read LON 
     &     from '// GRIDNAME //' file', .TRUE.)

      print *, 'LAT(150,150)=', LAT(150,150) !debug lina
      print *, 'LON(150,150)=', LON(150,150) !debug lina

c Close the GRIDNAME file      
      IF ( .NOT. CLOSE3( GRIDNAME ) )THEN
            WRITE(*, *) 'Error: close file failed (1)'
      ENDIF

C#############################################

C#############################################


C#############################################
C.......  Begin read EPICFILE 
C.......   First:  Initialize the ioapi and obtain array size

      IF ( .NOT. OPEN3( EPICFILE, FSREAD3, PNAME) )THEN 
      WRITE(*, *) 'Error: open EPICFILE failed '
      ENDIF 

      IF ( .NOT. DESC3( EPICFILE ) ) THEN
        WRITE(*, *) 'Error: read EPICFILE failed (4)'
        STOP 'Bad exit'
      ENDIF 

      WRITE(*,*)
      WRITE(*,*) ' >>---->  Start reading EPIC daily output'
      WRITE(*,*)


      allocate ( LAI (COL1,ROW1,LAY1))
      
      LAI = 0.0
      
      print *, 'LANDTYPE(1,1,1)=', LANDTYPE(1,1,1) !debug lina
      print *, 'LANDTYPE(6,1,6)=', LANDTYPE(6,1,6) !debug lina
      print *, 'LANDTYPE(40,1,42)=', LANDTYPE(40,1,42) !debug lina


        IF ( .NOT. XTRACT3(EPICFILE,'LAI',LAY0, LAY1,
     &    ROW0, ROW1, COL0, COL1, SDATE3D,STIME3D, LAI))
     &    CALL M3ERR( PNAME, SDATE3D,STIME3D, 'Could not read
     &     LAI to '// EPICFILE //' file', .TRUE.)

c      Close the EPICFILE file      
        IF ( .NOT. CLOSE3( EPICFILE ) )THEN
           WRITE(*, *) 'Error: close file failed (8)'
        ENDIF

      WRITE(*,*) '$$$$  END TIME & Layer LOOP                  $$$$'
      WRITE(*,*) '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'     

C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$    FINISH READ ALL THE STATIC FILES $$$$$$$$$$$$$$$$

C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$     BEGIN TIME LOOP 
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C start to read the header of METCRO2D
      IF ( .NOT. OPEN3( METNAME, FSREAD3, PNAME) )THEN
            WRITE(*, *) 'Error: open file failed (8)'
      ENDIF

      IF ( .NOT. DESC3( METNAME ) ) THEN
             WRITE(*, *) 'Error: read file failed (8)'
             STOP 'Bad exit'
      ENDIF

      WRITE(*,*)
      WRITE(*,*) ' >>---->  Start reading METCRO2D '
      WRITE(*,*)
C first read some header info
        SDATE = SDATE3D
        STIME = STIME3D
        JDATE = SDATE
        JTIME = STIME
        EDATE = JDATE
        ETIME = JTIME
     


      MXREC  = MXREC3D
C Intialize the MET parameters
      allocate ( TASFC (COL1, ROW1 ) ) ! surface air temperature [K]
      allocate ( SSOLAR (COL1, ROW1 ) ) ! surface radiation [w/m**2]
      allocate ( PRES (COL1, ROW1 ) ) ! surface pressure [Pa]
      allocate ( SOILT (COL1, ROW1 ) )
      allocate ( SOILM (COL1, ROW1 ) )
      allocate ( ISLTYP (COL1, ROW1 ) )
      allocate ( CFRAC (COL1, ROW1 ) ) ! cloud fraction
      allocate ( WSPD (COL1, ROW1 ) ) ! wind speed
C lina luo      allocate ( LAI (COL1, ROW1 ) ) ! leaf area index
      allocate ( SNOCOV (COL1, ROW1 ) ) ! snow cover

c      allocate ( EMPOL (COL1, ROW1 ) ) ! NO emission from BDSNP mgN/hr buffer
c      allocate ( EMISS (COL1, ROW1, MXREC ) ) ! final store matrix for the emission
        allocate(CRF(COL1,ROW1,LAY1))
                   CRF = 0.0
          allocate ( COSZEN (COL1, ROW1 ) )
             COSZEN = 0.0

      
      TASFC = 0.0
      SSOLAR = 0.0
      PRES = 0.0
      SOILT = 0.0
      SOILM = 0.0
      ISLTYP = 0.0
      CFRAC = 0.0
      WSPD = 0.0
c lina luo      LAI = 0.0
      SNOCOV = 0.0



       WRITE(*,*) '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
       WRITE(*,*) '$$$$           BEGIN TIME LOOP       $$$$'
       allocate(CRFOUT(COL1,ROW1,LAY1))
       CRFOUT=0.0
       allocate(BUFFERSUM(COL1,ROW1,LAY1))
       BUFFERSUM=0.0

        NSTEPS = MXREC
            allocate ( BUFFER(COL1,ROW1,LAY1,NSTEPS))
            BUFFER = 0.0

        DO T = 1, NSTEPS   !25hr

       WRITE(*,*) 'T=', T, 'JDATE=', JDATE, 'JTIME=', JTIME

        IF ( .NOT. XTRACT3(METNAME,'TEMP2',LAY0, 1, 
     &    ROW0, ROW1, COL0, COL1, JDATE,JTIME, TASFC))
     &    CALL M3ERR( PNAME, JDATE, JTIME, 'Could not read
     &     TEMP2 to '// METNAME //' file', .TRUE.)

        IF ( .NOT. XTRACT3(METNAME,'RGRND',LAY0, 1,
     &    ROW0, ROW1, COL0, COL1, JDATE,JTIME, SSOLAR))
     &    CALL M3ERR( PNAME, JDATE, JTIME, 'Could not read
     &     RGRND to '// METNAME //' file', .TRUE.)

        IF ( .NOT. XTRACT3(METNAME,'PRSFC',LAY0, 1,
     &    ROW0, ROW1, COL0, COL1, JDATE,JTIME, PRES))
     &    CALL M3ERR( PNAME, JDATE, JTIME, 'Could not read
     &     PRSFC to '// METNAME //' file', .TRUE.)

        IF ( .NOT. XTRACT3(METNAME,'SOIM1',LAY0, 1,
     &    ROW0, ROW1, COL0, COL1, JDATE,JTIME, SOILM))
     &    CALL M3ERR( PNAME, JDATE, JTIME, 'Could not read
     &     SOIM1 to '// METNAME //' file', .TRUE.)

        IF ( .NOT. XTRACT3(METNAME,'SOIT1',LAY0, 1,
     &    ROW0, ROW1, COL0, COL1, JDATE,JTIME, SOILT))
     &    CALL M3ERR( PNAME, JDATE, JTIME, 'Could not read
     &     SOILT to '// METNAME //' file', .TRUE.)

        IF ( .NOT. XTRACT3(METNAME,'SLTYP',LAY0, 1,
     &    ROW0, ROW1, COL0, COL1, JDATE,JTIME, ISLTYP))
     &    CALL M3ERR( PNAME, JDATE, JTIME, 'Could not read
     &     SLTYP to '// METNAME //' file', .TRUE.)

        IF ( .NOT. XTRACT3(METNAME,'SNOCOV',LAY0, 1,
     &    ROW0, ROW1, COL0, COL1, JDATE,JTIME, SNOCOV))
     &    CALL M3ERR( PNAME, JDATE, JTIME, 'Could not read
     &     SNOCOV to '// METNAME //' file', .TRUE.)

        IF ( .NOT. XTRACT3(METNAME,'WSPD10',LAY0, 1,
     &    ROW0, ROW1, COL0, COL1, JDATE,JTIME, WSPD))
     &    CALL M3ERR( PNAME, JDATE, JTIME, 'Could not read
     &     WSPD10 to '// METNAME //' file', .TRUE.)

        IF ( .NOT. XTRACT3(METNAME,'CFRAC',LAY0, 1,
     &    ROW0, ROW1, COL0, COL1, JDATE,JTIME, CFRAC))
     &    CALL M3ERR( PNAME, JDATE, JTIME, 'Could not read
     &     CFRAC to '// METNAME //' file', .TRUE.)
c lina luo commented out
C    !     IF ( .NOT. XTRACT3(METNAME,'LAI',LAY0, LAY1,
C    !  &    ROW0, ROW1, COL0, COL1, JDATE,JTIME, LAI))
C    !  &    CALL M3ERR( PNAME, JDATE, JTIME, 'Could not read
C    !  &     LAI to '// METNAME //' file', .TRUE.)

C            allocate ( COSZEN (COL1, ROW1 ) )
C             COSZEN = 0.0
C compute zenith angle
       CALL CZANGLE( JDATE, JTIME, COL1, ROW1, LAT, LON, COSZEN ) 
       print*, 'COSZEN(150,150)=',COSZEN(150,150)       

       
      CALL GET_CANOPY_R(JDATE, JTIME,COL1, ROW1, LAY1, COSZEN,
     & TASFC, SSOLAR, PRES, LANDTYPE,
     & SNOCOV, CFRAC, WSPD, LAI, CRF)  
c            allocate ( BUFFER(COL1,ROW1,LAY1,NSTEPS))
c            BUFFER = 0.0
            DO L = LAY0,LAY1
                  DO R = 1, ROW1
                        DO C =1 ,COL1
                        BUFFER(C,R,L,T)=CRF(C,R,L)
                        END DO
                  END DO
            END DO
       END DO !{ looping over time steps }
c       IF (T .EQ. 1) THEN
c       print *, 'BUFFER(225,73,1,1)=',BUFFER(225,73,1,1)
c       ENDIF
c Close the METNAME file      
      IF ( .NOT. CLOSE3( METNAME ) )THEN
            WRITE(*, *) 'Error: close file failed (8)'
      ENDIF

      WRITE(*,*) '$$$$  END TIME LOOP                  $$$$'
      WRITE(*,*) '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'

C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$    FINISH READ MET FILES $$$$$$$$$$$$$$$$
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c$$$$$$$$   FINISH TIME LOOP
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

c##########################################################################
C rename some header name compared with the $SOILOUT
        TSTEP3D = 10000      ! time dependent
        MXREC3D = MXREC    ! 
        SDATE3D = SDATE
        STIME3D = 00000      ! start from the 0 to make it consistent with BGSGT
        print *, 'MXREC=',MXREC3D

C only have one variable to output
        NVARS3D = 1
        UNITS3D(1) = 'nondimension'
        VTYPE3D(1) = M3REAL
        VNAME3D(1) = 'CRF'
        VDESC3D(1) = 'NOx canopy reduction factor'     
        NLAYS3D = LAY1
c Write data into output file
       

        IF ( .NOT. OPEN3( FILEOUT, FSNEW3, PNAME) )THEN
            WRITE(*, *) 'Error: open file failed (9)'
        ENDIF ! output file opened successfully

        NVARS = NVARS3D
        JDATE = SDATE3D
        JTIME = STIME3D

        print *, 'JDATE=',JDATE,'JTIME=',JTIME
        

           DO T = 1, MXREC-1
           DO S = 1, NVARS

           DO K = LAY0, LAY1
           DO J = ROW0, ROW1
           DO II = COL0, COL1
C              CRFOUT(II,J,K)=BUFFER(II,J,K,T)

            BUFFERSUM(II,J,K)=BUFFERSUM(II,J,K)+BUFFER(II,J,K,T)
           ENDDO  ! Columns
           ENDDO  ! Rows
           ENDDO  ! Layers 
            
         END DO ! Species
        IF (T .EQ. 24) THEN
        DO K = LAY0, LAY1
        DO J = ROW0, ROW1
        DO II = COL0, COL1
        CRFOUT(II,J,K)=BUFFERSUM(II,J,K)/24
        END DO
        END DO
        END DO
      
            IF ( .NOT. WRITE3(FILEOUT,VNAME3D(1),JDATE,STIME3D,CRFOUT))
     &       CALL M3ERR( PNAME, JDATE, STIME3D, 'Could not write '//
     &       VNAME3D(S)//' to '// FILEOUT //' file', .TRUE.)
        ENDIF
          CALL NEXTIME( JDATE, JTIME, TSTEP)
        END DO ! {looping over time steps}

        IF ( .NOT. CLOSE3( FILEOUT ) )THEN
            WRITE(*, *) 'Error: close file failed (10)'
        ENDIF
        deallocate ( CRFOUT )
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
     & '>>---->  Program NOX_BDSNP.f ',
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
