#!/bin/bash -e
#  v = verbose (echo lines as read)
#  e = stop on errors

# gnat2xml o xml2gnat

# Usage: gnat2gnat.sh c23001a.adb
# This script translates Ada into Ada, replacing the input file,
# by first running gnat2xml to produce an XML file, and then
# running xml2gnat to translate the XML back into Ada.
# It is used for testing gnat2xml: the new Ada file should have
# the same semantics as the old one.

# We first move the original source into a temp directory,
# so if something goes wrong, the file will be missing and the
# test will fail. This is necessary because run_acats_test.py
# ignores the result of this script (other than printing out
# a message), and continues on to compile and run the test.

echo gnat2gnat.sh $*

tempdir=temp.g2g.$$

rm -rf $tempdir
mkdir -p $tempdir
mv $* $tempdir
cd $tempdir
pwd
mkdir xml

# If we're in something like
# acats/tmp/work/c23001a/temp.21476,
# then we want the -I switch to point to
# acats/tmp/lib.
# Or we might be in acats/tmp/lib/temp.18999 (when compiling report, eg.),
# in which case we don't need any lib_include.

if [ -d ../../../lib ] ; then
  lib_include=-I../../../lib
else
  lib_include=
fi

gnat2xml -I.. $lib_include -mxml $*

xml2gnat $*
mv generated_ada/$* ..
cd ..
rm -rf $tempdir

echo Done: gnat2gnat.sh $*
