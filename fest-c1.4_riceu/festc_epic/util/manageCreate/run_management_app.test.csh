#!/bin/csh -f

#**************************************************************************************
#  THIS PROGRAM GENERATES MANAGEMENT FILE FOR EPIC AT A 12 KM GRID LEVEL
#  FIRST IT READS AN EPIC SITE FILE LIST TO IDENTIFY THE LOCATION BY LATITUDE AND
#  LONGITUDE AS WELL AS 8-DIGIT HUC, COUNTY AND PRODUCTION REGION TO IDENTIFY THE
#  GIS DATA LINKAGES TO CREATE THE FILE AND TO LOCATE THE THE CLOSEST STATISTICAL 
#  WEATHER STATION DATA
#
# Written by: script by IE, UNC-Chapel Hill  
#
# Program: ManGenFERT.exe
#          Needed environment variables included in the script file to run.
#
#***************************************************************************************
#SBATCH --job-name=epicmanageapp
#SBATCH --export=ALL
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=1000m
#SBATCH --time=03:30:00
#SBATCH --mail-user=luolina1992@126.com
#SBATCH --mail-type=ALL


# Define environment variables
#
#setenv    EPIC_DIR   /nas01/depts/ie/cempd/EPIC/epic
setenv    EPIC_DIR   /scratch/ll70/festc1.4/epic

# from Interface
#setenv    SCEN_DIR   $EPIC_DIR/scenarios/EPIC_112012_su_test4
#setenv    EXE_DIR    $EPIC_DIR/util/manageCreate

setenv    SCEN_DIR   $EPIC_DIR/scenarios/test_case_ll70
setenv    EXE_DIR    $EPIC_DIR/util/manageCreate

#put real crop num
#set       CROPS = ( POTATOES )
#set       CROPSNUM = ( 21 22 )       #set rainfed and irrigated crop number for each crop

	set CROPS = " HAY SORGHUMS BEANS OTHGRASS POTATOES SOYBEANS CORNG OTHER WWHEAT OATS BARLEY SWHEAT CANOLA PEANUTS SORGHUMG EBEANS CORNS COTTON ALFALFA RICE RYE "
set CROPSNUM = ( 1 2 29 30 41 42 5 6 21 22 31 32 11 12 37 38 35 36 17 18 7 8 33 34 39 40 19 20 27 28 9 10 13 14 15 16 3 4 23 24 25 26)


# Not from interface
#setenv    COMM_DIR  $EPIC_DIR/common_data
#setenv    SOIL_DIR  $EPIC_DIR/common_data/BaumerSoils
#setenv    MANG_DIR  $EPIC_DIR/common_data/util/manageCreate
#setenv    WEAT_DIR  $EPIC_DIR/common_data/statWeath

	setenv EPIC_CMAQ_OUTPUT  $SCEN_DIR/output4CMAQ/spinup

	setenv    COMM_DIR  /projects/dsc1/ll70/model/festc1.4/epic/common_data
	setenv    SOIL_DIR  $COMM_DIR/BaumerSoils
	setenv    WEAT_DIR  $COMM_DIR/statWeath
	setenv    MANG_DIR  $COMM_DIR/util/manageCreate/2006


	@ n = 1
foreach crop ( $CROPS )

# echo $CROPSNUM[$n]
	setenv    CROP_NAME $crop
	setenv CROP_NUM_RF $CROPSNUM[$n]
	@ n = $n + 1
	setenv CROP_NUM_IR $CROPSNUM[$n]
	echo $CROP_NAME
	echo $CROP_NUM_RF
	echo $CROP_NUM_IR

# set up output files
	if ( ! -e $SCEN_DIR/$CROP_NAME/app/manage/OPC )  mkdir -p $SCEN_DIR/$CROP_NAME/app/manage/OPC
	srun time  $EXE_DIR/ManGenFERT.exe >& log.manage.ap
	@ n = $n + 1

	echo " ---- Completed management app run of CROP: "  $CROP_NAME

	end

# Run tile drain
#
	setenv   WORK_DIR $SCEN_DIR/work_dir
	setenv   COMM_DIR /projects/dsc1/ll70/model/festc1.4/epic/common_data
	setenv   TYPE_NAME app
foreach crop ($CROPS)
	setenv CROP_NAME $crop
	if ( ! -e $SCEN_DIR/$CROP_NAME/app/manage/tileDrain )  mkdir -p $SCEN_DIR/$CROP_NAME/app/manage/tileDrain
	srun time $EXE_DIR/soilDrain.exe >& log.app.soildrain
	if ( $status == 0 ) then
	echo  ==== Finished soil drain run for crop $CROP_NAME.
	else
	echo  ==status== Error in soil drain run for crop $CROP_NAME.
	exit 1
	endif
	mv $SCEN_DIR/$CROP_NAME/app/manage/tileDrain/SOILLISTALLSU.DAT  $SCEN_DIR/$CROP_NAME/app/manage/tileDrain/SOILLIST.DAT
	end






