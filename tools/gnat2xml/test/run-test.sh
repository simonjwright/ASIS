#!/bin/csh -fex

echo $PATH
which gcc

echo xsd | asistant -o5 > ada-schema.xsd
# runs the xsd command in asistant to
# generate an XML Schema.
#  -o5 means exit asistant after running the script.

git diff ada-schema.xsd

xmllint --schema ada-schema.xsd root_package.ads.hand-written.xml --noout

gcc -c -gnatct root_package.ads
gcc -c -gnatct root_package-child_package.ads
#../avatox -m. *.ad[sb] -I../../gnat-bld/local/include/asis
../avatox -m. root_package.ads -I../../gnat-bld/local/include/asis
git diff root_package.ads.xml
xmllint --schema ada-schema.xsd root_package.ads.xml --noout

#git add ada-schema.xsd root_package.ads.xml
#git commit -m "automatic commit" ada-schema.xsd root_package.ads.xml
