#!/bin/bash -ve
#  v = verbose (echo lines as read)
#  e = stop on errors

# Called from Makefile. The single argument is the name of the Ada file. The
# file processed is stage/2/self_rep/$1.self_rep.ada. These files are created
# by xml2gnat. Each contains a program called Self_Replicate which (if all the
# translations are working properly) should print a copy of itself. This script
# builds and runs the program, and compares the output with its own source
# code.

echo "Doing  do-self-rep"

# Need to delete previous version, because they all have the same name, and
# often have the same timestamp.
rm -f self_replicate self_replicate.out obj/self_replicate.ali obj/self_replicate.o

# Generate stage/2/self_rep/self_replicate.adb:
gnatchop -w -gnat2012 stage/2/self_rep/$1.self_rep.ada stage/2/self_rep

# Build, run, compare output:
gprbuild -P self_rep.gpr -g -O0 -vm -gnatyN self_replicate.adb
./self_replicate > self_replicate.out
diff stage/2/self_rep/self_replicate.adb self_replicate.out
