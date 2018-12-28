#!/bin/csh
##########################SYSTEM SPECIFIC VARIBLES######################
#
#  First, lets define all system specific variables
#
#  The location of this file and the executable.
#
#
################## END OF SYSTEM SPECIFIC VARIBLES######################
#  First, set the job to be executed to the variable 'job'
set full = $1
set job = $full:r
set ext = $full:e
if (null$ext == null) set ext = gra 
if (null$ext == null$full) set ext = gra 
#
#  Next, see if job.gra exists.  If not, goto helpmessage
if ( $ext != gra ) goto gramessage
#
if !(-e $job.gra) goto helpmessagem 
if -z $job.gra goto helpmessagem 
#
#  Now, to execute densit 
setenv FOR005 $job.gra
setenv FOR006 $job.out
setenv FOR013 $job.gpt
setenv FOR015 $job.tec
if -e $job.plt mv $job.plt $job.plt.$$
setenv plot   $job
if !(-e $job.log) touch  $job.log
nice -15 time $densitydir/density.exe  >> $job.log
echo job $job is finished
head -3 $job.gra
#  this job has completed normally
#
exit
#
gramessage:
#  the extension given was not .gra
echo 'Your filename did not have a .gra extension'
echo   ' but a ' $ext ' extension'
#
exit
#
helpmessagem:
#  the file specified on the command line either did not exist or has
#     zero length
echo 'Your file either does not exist or has zero length'
#
exit
#
