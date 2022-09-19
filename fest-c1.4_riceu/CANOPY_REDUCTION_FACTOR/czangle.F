C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE  CZANGLE( JDATE, JTIME, NX, NY, LAT, LON, COSZEN )

C----------------------------------------------------------------------
C Description:
C   Computes cosine of zenith angle for routine HRBIO()
C   The zenith angle is the angle measured from the zenith to the line of
C   sight to the sun
 
C Preconditions:
C   JDATE:JTIME represented in GMT
 
C Subroutines and Functions called:  none
C----------------------------------------------------------------------

      IMPLICIT NONE

C Includes:

C Arguments:

      INTEGER, INTENT( IN )  :: JDATE   ! current simulation date (YYYYDDD)
      INTEGER, INTENT( IN )  :: JTIME   ! current simulation time (HHMMSS)
      INTEGER, INTENT( IN )  :: NX      ! no. columns
      INTEGER, INTENT( IN )  :: NY      ! no. rows

      REAL,    INTENT( IN )  :: LAT( NX,NY )  ! lat (deg) -90 <= LAT <= 90
      REAL,    INTENT( IN )  :: LON( NX,NY )  ! lon (deg) -180 <= LON <= 180

      REAL,    INTENT( OUT ) :: COSZEN( NX,NY ) !  cos of zenith angle

C Parameters:
 
      REAL, PARAMETER ::
     &            PI =   3.14159265,
     &            AA =   0.15,
     &            BB =   3.885,
     &            CC = - 1.253,
     &          SIGA = 279.9348,
     &          SDEC =   0.39784984,  ! SIN (23^26'37.8") the declination angle
     &           D60 = 1.0 / 60.0,
     &           D15 = 1.0 / 15.0,
     &           D24 = 1.0 / 24.0,
     &        ROTDAY = 360.0 / 365.242 ! fraction of a complete rotation per day

      REAL, PARAMETER :: PI180 = PI / 180.0
C Local variables:

      INTEGER    IOS, R, C
      REAL       SLA, GMT,  TK, DAD, DF,
     &           DESIN, DECOS, DESIN2, DECOS2, SIG, DECSIN, DECCOS,
     &           EQT, TST, HRANGL
                     
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      REAL, ALLOCATABLE, SAVE :: SINLAT( :,: )
      REAL, ALLOCATABLE, SAVE :: COSLAT( :,: )  

      CHARACTER( 16 ) :: PNAME = 'CZANGLE'   !  procedure name

C----------------------------------------------------------------------

C compute sine of lat and lon first time through

         ALLOCATE( SINLAT( NX,NY  ), STAT=IOS )
         ALLOCATE( COSLAT( NX,NY ), STAT=IOS )

         DO R = 1, NY
            DO C = 1, NX
               SLA = PI180 * LAT( C,R )
               SINLAT( C,R ) = SIN( SLA )
               COSLAT( C,R ) = COS( SLA )
            END DO
         END DO

C Convert time to hours and add time-zone offset
      
      GMT    = FLOAT( JTIME / 10000 )                        !  hr part
     &       + D60 * ( FLOAT( MOD( JTIME / 100 , 100 ) )     !  min part
     &       + D60 *   FLOAT( MOD( JTIME, 100 ) ) )          !  sec part
      DAD    = GMT * D24 + MOD( JDATE, 1000 )
      DF     = ROTDAY * PI180 * DAD      !  The terrestrial-rotation angle
           
      DESIN  = SIN( DF )          !  SINE   of this angle
      DECOS  = COS( DF )          !  COSINE of this angle
           
      DESIN2 = SIN( DF + DF )     !  SINE   of twice the angle
      DECOS2 = COS( DF + DF )     !  COSINE of twice the angle
           
      SIG  =  DF
     &     +  PI180 * ( SIGA
     &                + 1.914827 * DESIN  - 0.079525 * DECOS
     &                + 0.019938 * DESIN2 - 0.00162  * DECOS2 )
           
C The sine and cosine of the declination
      
      DECSIN = SDEC * SIN( SIG )
      DECCOS = SQRT( 1.0 - DECSIN * DECSIN )
           
C The equation of time adjustment
      
      EQT = 0.123470 * DESIN  - 0.004289 * DECOS
     &    + 0.153809 * DESIN2 + 0.060783 * DECOS2

      DO R = 1, NY
         DO C = 1, NX

            TK     =  GMT + LON( C,R ) * D15   !  Distance in hours from LON=0
            TST    =  TK - EQT                 !  true solar time
            HRANGL =  PI180 * 15.0 * ABS( TST - 12.0 ) !  hour angle
                   
C Compute the cosine of zenith angle (sine of the solar elevation)
          
            COSZEN( C,R ) = DECSIN * SINLAT( C,R )
     &                    + DECCOS * COSLAT( C,R ) * COS( HRANGL )

C Bound against numerical rounding errors

            COSZEN( C,R ) = MIN( MAX( COSZEN( C,R ), -1.0 ), 1.0 )
                   
         END DO
      END DO

      RETURN
      END
