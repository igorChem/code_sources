#!/bin/csh
##########################SYSTEM SPECIFIC VARIBLES######################
set job = $1
foreach file (out tec)
    if -e $job.$file mv $job.$file $job.$file.$$
  end
setenv FOR005 $job.gra
setenv FOR006 $job.out
setenv FOR013 $job.gpt
setenv FOR015 $job.tec
if -e core rm core
if -e fort.2 rm fort.2
if -e fort.16 rm fort.16
if !(-e $job.log) mkfile -n 1 $job.log
density.exe
