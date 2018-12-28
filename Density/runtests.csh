#!/bin/csh
if (! ${?DISPLAY} ) then
  echo -n "Please enter the name of your X server > "
  setenv DISPLAY $<
endif
echo -n "Please enter your machine type (sparc or fujitsu) > "
set machine = $<
if ($machine != sparc && $machine != fujitsu) then
  echo "Illegal machine type - stop"
endif
foreach x (benzene ethylene graphite polyacetylene)
echo "Running test job $x ..."
cp $x.gpt.$machine $x.gpt
density $x
end
rm -f *.gpt *.plt *.out *.log *.tec
