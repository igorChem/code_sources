#!/bin/csh
#
#   Install file for DENSITY.  This file should be run whenever DENSITY is
#   to be installed for the first time.  If necessary, it can be run more than
#   once.  A question and answer session is involved.
echo This is the command file to install DENSITY.  It involves a question
echo and answer session.  Most of the time, the default options will be suitable.
echo Default options are indicated in parentheses, for example, \`"(y)"\' would mean
echo that the user can select the default by typing \`"y"\' as a response.
echo " "
echo Allowed options are indicated in square brackers, for example "[y/n]" would
echo mean that the user should type either \`y\' or \`n\'. 
echo "("\`y\' and \`n\' are abbreviations for \`yes\' and \`no\'")"
echo " " 
ready:
echo Are you ready to start the installation  "(y) [y/n]?"
set response = $<
if ( $response != n) goto install
echo " "
echo "Do you want to stop the installation? (y) [y/n]"
set response = $<
if ($response == n) goto start
exit
install:
echo " "
echo The size of DENSITY depends on the number of heavy "(MAXHEV)" and 
echo light "(MAXLIT)" atoms  defined in the file SIZES.  
echo " "
echo What value do you want for MAXHEV "(30) ?"
set response = $<
if ($response =="")set response=30
echo What value do you want for MAXLIT "(30) ?"
set response1 = $<
if ($response1 =="")set response1=30
if -e SIZES rm SIZES
cp  ../mopac_source/SIZES1 SIZES
echo    "      PARAMETER ("MAXHEV=$response,   MAXLIT=$response1")" >> SIZES
echo " "
cat ../mopac_source/SIZES2 >> SIZES
echo " The file SIZES is now ready."
echo " The next step is to compile DENSITY."
machine_prompt:
echo " "
echo " Enter the machine-type on which you are compiling DENSITY."
echo " The choices are:"
echo "    sparc     [ Sun Sparcstation ]"
echo "    fujitsu   [ Fujitsu UXP/M    ]"
echo " "
echo -n " machine-type-> "
set response = $<
if ( $response != sparc && $response != fujitsu ) then
   echo " "
   echo "Unknown machine-type ($response) entered.  Try again."
   goto machine_prompt
endif
echo " "
make $response
echo " "
echo " "
echo " DENSITY compiled"
