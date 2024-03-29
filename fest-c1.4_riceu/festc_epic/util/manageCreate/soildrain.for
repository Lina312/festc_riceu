!     PROGRAM SET UP FOR CREATING SOIL LIST OF POTENTIAL TILED SOILS
!      CHARACTER*16 ASTN
	CHARACTER*29 SOILNAME 
	CHARACTER*20 SOILFILE 
      CHARACTER*196 BAUMERDATA
	CHARACTER*6 MUSYB,TEXTURE,SOIL5
	CHARACTER*7 STEXT,SOILFILE1,SOILFILE3
	CHARACTER*3 HYDRO,AHSG
      CHARACTER*6 NUMS5,S5N,BLANK,SOILFILE2
	CHARACTER*8 TEXID,TEXIDL,STATELIST
	CHARACTER*10 OTTOS5
	CHARACTER*12 OTTOFILE,BLANK12
      CHARACTER*24 CROPLIST
	CHARACTER*4 AXT
      INTEGER*8 LSTN1,LSTN2,IASTN,ICROP,ISIT,IWP1,IWP5,IWND,
     1INPS,IOPS,INPSG,IOPSG,INPSC,IOPSC,IWTH,nline
        DIMENSION AXT(7),AHSG(14),RHSG(14)

C       Added by UNC, dyang, TYPENAME is spinup or app
C       Environment variables used by this management program
        CHARACTER*300  SCENDIR, COMMDIR, MANGDIR, UTILDIR, CROPDIR
        CHARACTER*300  WORKDIR, TILEDIR
        CHARACTER*30   CROPNAME
        CHARACTER*30   TYPENAME
C       end add by UNC, dyang

      INTEGER FIPS,LSTN,NOTTO,RHSG
	REAL LSLOPE,USLOPE
     
        DATA AHSG/'A  ','B  ','C  ','D  ','A/D','B/D','C/D','D/D',
     1'A/C','B/C','C/C','D/C','-','   '/
        DATA RHSG/1,2,3,4,1,2,3,4,1,2,3,4,2,2/BLANK/'      '/
     1BLANK12/'            '/
        DATA AXT/'.SOB','.SOD','.SOC','.SON','.LST','.RUN','.SOL'/

      DIMENSION KR(10),KW(30)
	DATA KR/41,42,43,43,45,46,47,48,49,50/KW/51,52,53,54,55,56,57,
     158,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,
     278,79,80/

C       Added by UNC, dyang
C       Environment variables used by this management program
       CALL GETENV( "SCEN_DIR", SCENDIR )
       CALL GETENV( "WORK_DIR", WORKDIR )
       CALL GETENV( "COMM_DIR", COMMDIR )
       CALL GETENV( "CROP_NAME", CROPNAME )
       CALL GETENV( "TYPE_NAME", TYPENAME )

       CROPDIR = trim(SCENDIR)//'/'//trim(CROPNAME)
       UTILDIR = trim(COMMDIR)//'/util/soilMatch'
       MANGDIR = TRIM(CROPDIR)//'/'//trim(TYPENAME)//'/manage'
       TILEDIR = TRIM(MANGDIR)//'/tileDrain'

!      NOTE KR(2) AND KR(4) ARE NOT CURRENTLY USED
       CALL OPENV(KR(1),'EPICRUNFILEIRR.DAT',MANGDIR, 'R')
!      OPEN(KR(2),FILE='EPICRUNFILEIRRDW.DAT')
       CALL OPENV(KR(3),'EPICRUNFILERAIN.DAT',MANGDIR, 'R')
!      OPEN(KR(4),FILE='EPICRUNFILERAINDW.DAT')
       
       IF ( TRIM(TYPENAME) .EQ. "spinup" ) THEN
          CALL OPENV(KR(5),'SOILLIST.DAT', CROPDIR, 'R')
       ELSE
          CALL OPENV(KR(5),'SOILLIST.DAT', MANGDIR, 'R')
       END IF

       CALL OPENV(KR(6),'ALL-CULTIVATED10-12-09.LST', UTILDIR, 'R')

       CALL OPENV(KW(1),'EPICRUNFILEIRRD.DAT',TILEDIR, 'W')
       CALL OPENV(KW(2),'EPICRUNFILEIRRDWD.DAT',TILEDIR, 'W')
       CALL OPENV(KW(3),'EPICRUNFILERAIND.DAT',TILEDIR, 'W')
       CALL OPENV(KW(4),'EPICRUNFILERAINDWD.DAT',TILEDIR, 'W')
       CALL OPENV(KW(5),'SOILLISTIRNT.DAT', TILEDIR, 'W' )
       CALL OPENV(KW(6),'SOILLISTIRDT.DAT',TILEDIR, 'W')
       CALL OPENV(KW(7),'SOILLISTIRDWNT.DAT',TILEDIR, 'W')
       CALL OPENV(KW(8),'SOILLISTIRDWDT.DAT',TILEDIR, 'W')
       CALL OPENV(KW(9),'SOILLISTRFNT.DAT',TILEDIR, 'W')
       CALL OPENV(KW(10),'SOILLISTRFDT.DAT',TILEDIR, 'W')
       CALL OPENV(KW(11),'SOILLISTRFDWNT.DAT',TILEDIR, 'W')
      CALL OPENV(KW(12),'SOILLISTRFDWDT.DAT',TILEDIR, 'W')
      CALL OPENV(KW(15),'SOILLISTALLSU.DAT',TILEDIR, 'W')
      CALL OPENV(KW(16),'SOILLISTALLDW.DAT',TILEDIR, 'W')
      CALL OPENV(KW(17),'EPICRUNFILEALLSU.DAT',TILEDIR, 'W')
      CALL OPENV(KW(18),'EPICRUNFILEALLDW.DAT',TILEDIR, 'W')

      CALL OPENV(KW(21),'SOILLISTALLSUI.DAT',TILEDIR, 'W')
      CALL OPENV(KW(22),'SOILLISTALLDWI.DAT',TILEDIR, 'W')
      CALL OPENV(KW(23),'SOILLISTALLSUR.DAT',TILEDIR, 'W')
      CALL OPENV(KW(24),'SOILLISTALLDWR.DAT',TILEDIR, 'W')
      CALL OPENV(KW(25),'EPICRUNFILEALLSUI.DAT',TILEDIR, 'W')
      CALL OPENV(KW(26),'EPICRUNFILEALLDWI.DAT',TILEDIR, 'W')
      CALL OPENV(KW(27),'EPICRUNFILEALLSUR.DAT',TILEDIR, 'W')
      CALL OPENV(KW(28),'EPICRUNFILEALLDWR.DAT',TILEDIR, 'W')

!     DEBUGFILE.DAT IS JUST  USED DURING THE CREATION OR MODIFICATION OF THE PROGRAM     
      CALL OPENV(KW(20),'DEBUGFILE.DAT', WORKDIR, 'W')
!     IRF=0 IS FOR IRRIGATED RUNS AND IRF=1 IS FOR RAINFED RUNS      
      IRF = 0
      nline = 0

!     SOILLISTIRNT.DAT for irrigated with no tile drainage for spinup,
!     SOILLISTIRDT.DAT for irrigated with tile drainage for spinup,
!     SOILLISTIRDWNT.DAT for irrigated with no tile drainage for daily weather,
!     SOILLISTIRDWDT.DAT for irrigated with tile drainage for daily weather,
!     SOILLISTRFNT.DAT for rainfed with no tile drainage for spinup,
!     SOILLISTRFDT.DAT for rainfed with tile drainage for spinup,
!     SOILLISTRFDWNT.DAT for rainfed with no tile drainage for daily weather,
!     SOILLISTRFDWDT.DAT for rainfed with tile drainage for daily weather.
!
!     READ RUNFILE FOR IRRIGATED GRID SPIN UP
!  1A  IASTN = GRID #
!  1B  ICROP = BELD4#
!  2  ISIT = SITE #=GRID# PLUS 0
!  3  IWP1 = WEATHER STA # FROM WPM10509.DAT
!  4  IWP5 = WEATHER STA # FROM WPM10509.DAT
!  5  IWND = WIND STA # FROM WIND0509.DAT
!  6  INPS = SOIL # FROM SOILLIST.DAT FILE USED BY EPIC AND THE MANAGEMENT PROGRAM, KR(13)=GRID# PLUS CROP#
!     INPSG = GRID#
!     INPSC = CROP#
!  7  IOPS = OP SCHED # FROM THE OPC LIST USED WITH EPIC AND THE MANAGEMENT PROGRAM, KR(15)=GRID# PLUS 0 + CROP #
!     IOPSG = GRID#
!     IOPSC = CROP#
!  8  IWTH = DAILY WEATHER STA # FROM LIST OF DAILY WEATHER FILES USED WITH EPIC KR(27) WTHCOM.DAT GRID# OR 0 IF SPIN UP GENERATED WETH
!     THE PROGRAM FIRST CREATES THE IRRIGATED FILES THEN IRF IS SET TO 1 AND IT LOOPS BACK THROUGH TO CREATE THE RAINFED FILES
    1 CONTINUE         
!      WRITE(KW(20),110) IRF
  110 FORMAT(I10)
!      
!     BUILD FILES FOR RUNS WITHOUT TILE DRAIN      
!      IF(IRF==0)READ(KR(1),101,END=2)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,
!     1INPS,IOPS,IWTH
!      IF(IRF>0)READ(KR(3),101,END=99)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,
!     1INPS,IOPS,IWTH
       IF(IRF==0)READ(KR(1),101,END=2)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,
     1INPSG,INPSC,IOPSG,IOPSC,IWTH
      IF(IRF>0)READ(KR(3),101,END=99)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,
     1INPSG,INPSC,IOPSG,IOPSC,IWTH
       nline = nline + 1
C      write(*,*) IRF, nline
C      write(*,*) IASTN,ICROP,ISIT,IWP1,IWP5,IWND
     
     
!     IHY IS ZERO WHICH DEFAULTS TO SOIL HYDROLOGY CODE IF SOIL NOT DRAINED
!     IDR IS DEFAULT FOR DEPTH OF TILE DRAINAGE IF NOT DRAINED
	IHY=0
	IDR=0
!     SOILFILE IS THE EPIC SOILFILE NAME FOR SPIN UP RUNS
!   10 READ(KR(5),103,END=3)LSTN,SOILFILE
!  103 FORMAT(1X,I10,A20)
!     LSTN1= GRID#
!     LSTN2=BELD4#
!     SOILFILE1=SOIL NUMBER IN CULTIVATED SOIL LIST BY STATE
!     SOILFILE2=SOIL5 NAME/#
!     SOILFILE3=REMAINDER OF BAUMER SOILFILE NAME
   10 READ(KR(5),203,END=12) LSTN1,LSTN2,SOILFILE1,SOILFILE2,SOILFILE3,
     1BAUMERDATA
  203 FORMAT(3X,I6,I2,A7,A6,A7,A196)

  103 FORMAT(3X,I6,I2,A7,A6,A7,2I5,2X,A196)
C     write(*,*) "b test: ", IASTN,LSTN1, ICROP,LSTN2
      IF(IASTN==LSTN1.AND.ICROP==LSTN2) GO TO 4
C     write(*,*) "a test: ", IASTN,LSTN1, ICROP,LSTN2
	GO TO 10
    4	IF(IRF==0)WRITE(KW(5),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,IHY,IDR,BAUMERDATA
      IF(IRF==0)WRITE(KW(7),106)LSTN1,LSTN2,LSTN1,LSTN2,IHY,IDR
     1,BAUMERDATA
  106 FORMAT(3X,I6,'0',I2,7X,I6,'0',I2,'.SOL',2I5,2X,A196)
      	
      IF(IRF>0)WRITE(KW(9),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,IHY,IDR,BAUMERDATA
      IF(IRF>0)WRITE(KW(11),106)LSTN1,LSTN2,LSTN1,LSTN2,IHY,IDR,
     1BAUMERDATA     


      REWIND(KR(5))
	GO TO 5
C   3 WRITE(KW(20),104) LSTN1,LSTN2,BAUMERDATA

  104 FORMAT(1X,'NOMATCH',I8,I2,2X,A196)
!     LSTN IS THE NUMBER OF THE SOIL IN BAUMERS SOIL LIST OF GOOD SOILS BY STATE
!     NOTTO IS THE NUMBER OF THE SOIL IN BAUMERS ORIGINAL LIST BY STATE
!     SOIL5 = THE 6 CHARACTER ALPHA NUMERIC SOILS 5 CODE FROM THE BAUMER FILE LIST
!     STEXT = THE BAUMER SOIL FILE EXTENSION CODE
!     SOILNAME = SOILS 5 SOIL SERIES NAME
!     MUSYB = THE SOIL MAPPING UNIT SYMBOL THAT MAY BE SOIL SURVEY SPECIFIC
!     TEXTURE = THE SOIL TEXTURE UP TO 6 CHARACTERS
!     LSLOPE = LOWER VALUE OF SLOPE RANGE
!     USLOPE = UPPER VALUE OF SLOPE RANGE
!     26 COLUMNS OF DISCRIPTIVE DATA NOT USED IN THE PROGRAM ARE SKIPPED
!     HYDRO = THE SOIL HYDROLOGIC GROUP CODE
!     26 COLUMNS OF DISCRIPTIVE DATA NOT USED IN THE PROGRAM ARE SKIPPED
!     FIPS = THE 5-DIGIT COUNTY CODE USED BY THE CENSUS WHERE THE FIRST 2-DIGITS 
!     ARE THE STATE NUMERIC CODE AND THE NEXT 3-DIGITS ARE THE COUNTY NUMERIS CODE
!     X IS THE LATITUDE OF THE SOIL
!     Y IS THE LONGITUDE OF THE SOIL

    5 READ(KR(6),100,END=12)LSTN,NOTTO,SOIL5,STEXT,SOILNAME,MUSYB,
     1TEXTURE,LSLOPE,USLOPE,HYDRO,FIPS,Y,X
  100 FORMAT(3X,I6,1X,I5,A6,A7,2X,A29,A6,A6,1X,2F4.0,26X,A3,26X,I5,2X,
     12F10.3)


	IF(SOILFILE2==SOIL5) GO TO 7
	GO TO 5
    7 DO 11 J=5,12
      IF(HYDRO==AHSG(J)) GO TO 9
      GO TO 11
    9 IDR=750
      IF(IRF==0)WRITE(KW(6),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,RHSG(J),IDR,BAUMERDATA
	IF(IRF==0)WRITE(KW(8),106)LSTN1,LSTN2,LSTN1,LSTN2,RHSG(J),IDR
     1,BAUMERDATA
       IF(IRF==0)WRITE(KW(15),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,RHSG(J),IDR,BAUMERDATA
	IF(IRF==0)WRITE(KW(16),106)LSTN1,LSTN2,LSTN1,LSTN2,RHSG(J),IDR
     1,BAUMERDATA
      IF(IRF==0)WRITE(KW(21),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,RHSG(J),IDR,BAUMERDATA
	IF(IRF==0)WRITE(KW(22),106)LSTN1,LSTN2,LSTN1,LSTN2,RHSG(J),IDR
     1,BAUMERDATA
      IF(IRF>0)WRITE(KW(10),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,RHSG(J),IDR,BAUMERDATA
	IF(IRF>0)WRITE(KW(12),106)LSTN1,LSTN2,LSTN1,LSTN2,RHSG(J),IDR
     1,BAUMERDATA    
      IF(IRF>0)WRITE(KW(15),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,RHSG(J),IDR,BAUMERDATA
	IF(IRF>0)WRITE(KW(16),106)LSTN1,LSTN2,LSTN1,LSTN2,RHSG(J),IDR
     1,BAUMERDATA    
      IF(IRF>0) WRITE(KW(23),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,RHSG(J),IDR,BAUMERDATA

	IF(IRF>0)WRITE(KW(24),106)LSTN1,LSTN2,LSTN1,LSTN2,RHSG(J),IDR
     1,BAUMERDATA    
      IF(IRF==0)WRITE(KW(1),102)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IWTH
      IF(IRF==0)WRITE(KW(2),105)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IASTN
	IF(IRF>0)WRITE(KW(3),102)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IWTH
      IF(IRF>0)WRITE(KW(4),105)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IASTN
      IF(IRF==0)WRITE(KW(17),102)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IWTH
      IF(IRF==0)WRITE(KW(18),105)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IASTN
	IF(IRF>0)WRITE(KW(17),102)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IWTH
      IF(IRF>0)WRITE(KW(18),105)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IASTN
      IF(IRF==0)WRITE(KW(25),102)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IWTH
      IF(IRF==0)WRITE(KW(26),105)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IASTN
	IF(IRF>0)WRITE(KW(27),102)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IWTH
      IF(IRF>0)WRITE(KW(28),105)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IASTN
	GO TO 112
   11 CONTINUE 
  112 DO 111 J=1,4
      IF(HYDRO==AHSG(J)) GO TO 119
      GO TO 111
  119 IDR=0
      IF(IRF==0)WRITE(KW(15),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,IHY,IDR,BAUMERDATA
	IF(IRF==0)WRITE(KW(16),106)LSTN1,LSTN2,LSTN1,LSTN2,IHY,IDR
     1,BAUMERDATA
      IF(IRF>0)WRITE(KW(15),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,IHY,IDR,BAUMERDATA
	IF(IRF>0)WRITE(KW(16),106)LSTN1,LSTN2,LSTN1,LSTN2,IHY,IDR
     1,BAUMERDATA    
      IF(IRF==0)WRITE(KW(17),102)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IWTH
      IF(IRF==0)WRITE(KW(18),105)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IASTN
	IF(IRF>0)WRITE(KW(17),102)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IWTH
      IF(IRF>0)WRITE(KW(18),105)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IASTN
      IF(IRF==0)WRITE(KW(21),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,IHY,IDR,BAUMERDATA
	IF(IRF==0)WRITE(KW(22),106)LSTN1,LSTN2,LSTN1,LSTN2,IHY,IDR
     1,BAUMERDATA
      IF(IRF>0)  WRITE(KW(23),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,IHY,IDR,BAUMERDATA
C     IF(IRF>0)  write(*,*)"119-- ",LSTN1,LSTN2,SOILFILE1,SOILFILE2,
C    1SOILFILE3,IHY,IDR,BAUMERDATA

	IF(IRF>0)WRITE(KW(24),106)LSTN1,LSTN2,LSTN1,LSTN2,IHY,IDR
     1,BAUMERDATA    
      IF(IRF==0)WRITE(KW(25),102)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IWTH
      IF(IRF==0)WRITE(KW(26),105)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IASTN
	IF(IRF>0)WRITE(KW(27),102)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IWTH
      IF(IRF>0)WRITE(KW(28),105)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IASTN

  111 CONTINUE
      DO 211 J=13,14
      IF(HYDRO==AHSG(J)) GO TO 219
      GO TO 211
  219 IDR=0
      IF(IRF==0)WRITE(KW(15),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,IHY,IDR,BAUMERDATA
      IF(IRF==0)WRITE(KW(16),106)LSTN1,LSTN2,LSTN1,LSTN2,IHY,IDR
     1,BAUMERDATA
      IF(IRF>0)WRITE(KW(15),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,IHY,IDR,BAUMERDATA
      IF(IRF>0)WRITE(KW(16),106)LSTN1,LSTN2,LSTN1,LSTN2,IHY,IDR
     1,BAUMERDATA
      IF(IRF==0)WRITE(KW(17),102)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IWTH
      IF(IRF==0)WRITE(KW(18),105)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IASTN
      IF(IRF>0)WRITE(KW(17),102)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IWTH
      IF(IRF>0)WRITE(KW(18),105)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IASTN
      IF(IRF==0)WRITE(KW(21),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,IHY,IDR,BAUMERDATA
      IF(IRF==0)WRITE(KW(22),106)LSTN1,LSTN2,LSTN1,LSTN2,IHY,IDR
     1,BAUMERDATA
      IF(IRF>0)WRITE(KW(23),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,IHY,IDR,BAUMERDATA

      IF(IRF>0)WRITE(KW(24),106)LSTN1,LSTN2,LSTN1,LSTN2,IHY,IDR
     1,BAUMERDATA
      IF(IRF==0)WRITE(KW(25),102)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IWTH
      IF(IRF==0)WRITE(KW(26),105)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IASTN
      IF(IRF>0)WRITE(KW(27),102)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IWTH
      IF(IRF>0)WRITE(KW(28),105)IASTN,ICROP,ISIT,IWP1,IWP5,IWND,INPSG,
     1INPSC,IOPSG,IOPSC,IASTN
      GO TO 211

C   3 WRITE(KW(20),*) nline, ": ", IASTN,ICROP

  211 CONTINUE
   12 REWIND (KR(6))
      REWIND (KR(5))
  105 FORMAT(4X,I6,'0',I2,3X,I7,3I3,2(3X,I7,I2),I7)
      GO TO 1
    2 CONTINUE
      IRF=1
      nline = 0
      GO TO 1
      
	
      WRITE(KW(9),103)LSTN1,LSTN2,SOILFILE1,SOILFILE2,
     1SOILFILE3,IHY,IDR
      WRITE(KW(11),106)LSTN1,LSTN2,LSTN1,LSTN2,IHY,IDR
      
      
      STOP

  101 FORMAT(4X,I6,1X,I2,3X,I7,3I3,2(3X,I7,I2),I3)

  102 FORMAT(4X,I6,'0',I2,3X,I7,3I3,2X,2(3X,I7,I2),I3)
  748 FORMAT(10X,A24)
   99 END


!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-*
      include "openv.f"

