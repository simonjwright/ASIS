#!/bin/bash -ve
#  v = verbose (echo lines as read)
#  e = stop on errors

# Called from Makefile. The single argument is the name of the Ada file. The
# file processed is stage/2/self_rep/$1.regen_ada.ada.  These files are created
# by xml2gnat. Each contains a program called Regenerate_Ada which (if all the
# translations are working properly) should print a copy of the corresponding
# Ada file in stage/2/ada.  This script builds and runs the program, and
# compares the output.

echo "Doing  do-regen-ada"

# Need to delete previous version, because they all have the same name, and
# often have the same timestamp.
rm -f regenerate_ada regenerate_ada.out obj/regenerate_ada.ali obj/regenerate_ada.o

# Generate stage/2/self_rep/regenerate_ada.adb:
gnatchop -w -gnat2012 stage/2/self_rep/$1.regen_ada.ada stage/2/self_rep

# Build, run, compare output:
gprbuild -P self_rep.gpr -g -O0 -vm -gnatyN regenerate_ada.adb
./regenerate_ada > regenerate_ada.out

echo diff stage/2/ada/$1 regenerate_ada.out
diff stage/2/generated_ada/$1 regenerate_ada.out
