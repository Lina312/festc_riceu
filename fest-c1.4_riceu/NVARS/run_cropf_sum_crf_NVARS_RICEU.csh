#!/bin/csh -f

################# USER DEFINE ########################
#SBATCH --job-name=weightedsumallinone
#SBATCH --export=ALL
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=2G
#SBATCH --time=12:00:00
#SBATCH --mail-user=luolina1992@126.com
#SBATCH --mail-type=ALL
#SBATCH --partition=commons

#set case
set GRID = 12US1
set YEAR = 2017
set CONFIG = MIN0
set CASE = RICEU


#set dirctory
setenv BASE /home/ll70/tools/weightedSumCrops_crf
setenv EPICIN /scratch/ll70/festc1.4/epic/scenarios/${GRID}_${YEAR}_${CONFIG}_${CASE}/app/toCMAQ
setenv CRFIN /scratch/ll70/${YEAR}_CRF_${CASE}
setenv EPICOUT /scratch/ll70/festc1.4/epic/scenarios/${GRID}_${YEAR}_${CONFIG}_${CASE}/app/daily_weighted_crf_NVARS_${YEAR}_${CASE}
#setenv EPICOUT /scratch/ll70/data
setenv PROGNAME EPIC_sumlayers_weighted_crfNOx_kgN.exe
setenv FILEHEADER 12US1_${YEAR}_MIN0_${CASE}_time
setenv FILEHEADERO 12US1_sumlayers_weighted_crf_NVARS_${CASE}
setenv PROMPTFLAG N
################ END USER DEFINE ###########################

cd $BASE
set YEAR = 2017
#set B_DATEs = (20110101 20110201 20110301 20110401 20110501 20110601 20110701 20110801 20110901 20111001 20111101 20111201)
#set E_DATEs = (20110131 20110228 20110331 20110430 20110531 20110630 20110731 20110831 20110930 20111031 20111130 20111231)
set B_DATEs = (20170101 20170201 20170301 20170401 20170501 20170601 20170701 20170801 20170901 20171001 20171101 20171201)
set E_DATEs = (20170131 20170228 20170331 20170430 20170531 20170630 20170731 20170831 20170930 20171031 20171130 20171231)

foreach i (10)
#foreach i (1 2 3 4 5 6 7 8 9 10 11 12)
#foreach i(1 2 3 4 6 8 9 10 11 12)
#set B_DATE = (20110601)
#set E_DATE = (20110601)
#foreach i (1)
#foreach i (8 9 10 11 12)
set B_DATE = $B_DATEs[$i]
set E_DATE = $E_DATEs[$i]
 @ C_DATE = $B_DATE
#********Begin to the outside loop************
while (${C_DATE} <= ${E_DATE})

echo @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  echo "Current Date is:   $C_DATE"
echo @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#  set YYYY = `./j2g $C_JDATE | cut -c 1-4`
#  set MM = `./j2g $C_JDATE | cut -c 5-6`
#  set DD = `./j2g $C_JDATE | cut -c 7-8`

#  set GDATE = ${YYYY}${MM}${DD}

  setenv EXTFILEONE $EPICIN/${FILEHEADER}${C_DATE}.nc
  setenv EXTFILETWO /projects/dsc1/ll70/data/beld4/beld4_2011_12US1_cropf.ncf
  setenv EXTFILETHREE /scratch/ll70/${YEAR}_CRF_${CASE}/NO_CRF_${C_DATE}_avg.nc
  setenv EXTOUTONE $EPICOUT/${FILEHEADERO}_${C_DATE}.nc

  if ( -e $EXTOUTONE ) rm -f $EXTOUTONE

   srun $BASE/$PROGNAME

  unsetenv $EXTFILEONE
  unsetenv $EXTFILETWO
  unsetenv $EXTOUTONE
  unsetenv $EXTFILETHREE

# Increment one day, then end of loop

  @ C_DATE ++

  end 
  echo "End of script, Current Date is $C_DATE"
  end #foreach
#********* end of the outside loop ***********
#end of while loop

exit 0
