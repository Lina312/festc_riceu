#!/bin/csh -f
#**************************************************************************************
# Purpose:  to run management spinup utility
#
# Written by: Fortran by Benson, Script by IE. 2012
# Modified by:
#
# Program: ManGenSU.exe
# 
#***************************************************************************************


#
# Define environment variables
#

setenv    EPIC_DIR /proj/ie/proj/staff/dyang/festc/festc1.4/epic
setenv    SCEN_DIR /proj/ie/proj/staff/dyang/festc/festc1.4/epic/scenarios/test_case
setenv    SOIL_DIR $EPIC_DIR/common_data/BaumerSoils
setenv    MANG_DIR $EPIC_DIR/common_data/util/manageCreate/2006
setenv    WEAT_DIR $EPIC_DIR/common_data/statWeath

set    EXEC_DIR = /proj/ie/proj/staff/dyang/festc/festc1.4/epic/util/manageCreate

#
# set input variables
set CROPS = " HAY SORGHUMS BEANS OTHGRASS POTATOES SOYBEANS CORNG OTHER WWHEAT OATS BARLEY SWHEAT CANOLA PEANUTS SORGHUMG EBEANS CORNS COTTON ALFALFA RICE RYE"
foreach crop ($CROPS) 
   setenv CROP_NAME $crop 
   if ( ! -e $SCEN_DIR/$CROP_NAME/spinup/manage/OPC )  mkdir -p $SCEN_DIR/$CROP_NAME/spinup/manage/OPC 
#

#
#  Generate management spinup files 
#

    echo ==== Begin EPIC management spinup run of CROP: $CROP_NAME 
    time $EXEC_DIR/ManGenSU.exe
    if ( $status == 0 ) then
    echo ==== Finished EPIC management spinup run of CROP: $CROP_NAME
    else 
    echo ==== Error in EPIC management spinup run of CROP: $CROP_NAME
    endif 
end 


#
# Run tile drain 

setenv   WORK_DIR $SCEN_DIR/work_dir
setenv   COMM_DIR $EPIC_DIR/common_data
setenv   TYPE_NAME spinup
foreach crop ($CROPS) 
   setenv CROP_NAME $crop 
  if ( ! -e $SCEN_DIR/$CROP_NAME/spinup/manage/tileDrain )  mkdir -p $SCEN_DIR/$CROP_NAME/spinup/manage/tileDrain
  time $EXEC_DIR/soilDrain.exe
      if ( $status == 0 ) then
        echo  ==== Finished soil drain run for crop $CROP_NAME. 
     else 
         echo  ==status== Error in soil drain run for crop $CROP_NAME. 
       exit 1 
  endif 
  mv $SCEN_DIR/$CROP_NAME/spinup/manage/tileDrain/SOILLISTALLSU.DAT  $SCEN_DIR/$CROP_NAME/spinup/manage/tileDrain/SOILLIST.DAT
end 

