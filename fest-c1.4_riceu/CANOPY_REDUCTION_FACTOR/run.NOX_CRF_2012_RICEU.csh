#!/bin/csh 
#
#
#SBATCH --job-name=NO_CRF
#SBATCH --export=ALL
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=2G
#SBATCH --time=12:00:00
#SBATCH --mail-use=luolina1992@126.com
#SBATCH --mail-type=ALL
#SBATCH --partition=commons

##################  USER DEFINE ###################
## Grid defintion
setenv GRIDDEC  /home/ll70/data/GRIDDESC_EPA_CONUS_12km
setenv GDNAM3D  12US1
## Episode choose by date
set YEAR = 2012
#set B_DATEs = (20120101 20120201 20120301 20120401 20120501 20120601 20120701 20120801 20120901 20121001 20121101 20121201)  
#set E_DATEs = (20120131 20120228 20120331 20120430 20120531 20120630 20120731 20120831 20120930 20121031 20121130 20121231)
set B_DATEs = (20120229)
set E_DATEs = (20120229)

#set S_DATEs = (110501 110701)
#set F_DATEs = (110531 110731)
# foreach i (1 2 3 4 5 6 7 8 9 10 11 12)
#foreach i (6 7 8 9 10 11 12)
foreach i (1)
#foreach i(1 2 3 4 5 6 7 8)


set B_DATE = $B_DATEs[$i]
set E_DATE = $E_DATEs[$i]
#set S_DATE = $S_DATEs[$i]
#set F_DATE = $S_DATEs[$i]


## EPICFILE directory
#setenv EPICDIR /scratch/ll70/festc1.4.1/epic/scenarios/12US2_2011_MIN0_16met/output4CMAQ/app/toCMAQ
setenv EPICDIR /scratch/ll70/festc1.4/epic/scenarios/12US1_2012_MIN0_USGS/app/toCMAQ

## GRIDCRO2D information directory
#setenv GRIDDIR /scratch/ll70/AGU/MCIP2016US2re

## Soil biome and arid/nonarid map data directory
#setenv MAPDIR  $BDSNPINP/MAP

## MCIP input directory 
setenv METDIR  /scratch/ll70/MET_2012



## GRIDCRO2D file for the 'LAT' and 'LON' to caculate zenith angle
setenv GRIDCRO2D /projects/dsc1/ll70/data/beld4/gridcro2d_110422_12US1



## SOILBIOME file
setenv SOILBIOM /projects/dsc1/ll70/data/beld4/beld4_2011_12US1_cropf.ncf


## Program directory
setenv EXEC /home/ll70/tools/CANOPY_REDUCTION_FACTOR/NOX_CRF_AVE.exe


## Run BDSNP
set EPICBASE = 12US1_2012_MIN0_USGS_time
 @ C_DATE = $B_DATE
#loop between the episode period
while ( $C_DATE <= $E_DATE )

## METCRO2D
   setenv METCRO2D  $METDIR/METCRO2D_${C_DATE}
## NO output from BDSNP module
   setenv CRFOUT /scratch/ll70/2012_CRF_USGS/NO_CRF_${C_DATE}_avg.nc
## EPICFIL3
   setenv EPICFILE $EPICDIR/${EPICBASE}${C_DATE}.nc

## diagnostic and continue file
#   setenv SOILOUT $BDSNPOUT/SOILINSTATE_${C_JDATE}_bebchmark.nc
   if ( -e $CRFOUT ) rm -f $CRFOUT

   srun $EXEC 


@ C_DATE ++
end # End while C_JDATE
end # foreach 
