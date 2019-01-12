# Make file for building gnat2xml and related tools, and for running tests.

# Targets:
#
#   'all' -- generate the schema. (Used to build all the programs, but that's
#    done elsewhere now.)
#
#   'test' -- run tests.
#
#   'update-schema' -- copy the newly generated schemas to the actual schemas.
#    This is for gnat2xml developers to use after making changes that cause
#    changes in the schema output by gnat2xsd (after checking that those
#    changes are correct).

include ../../Makefile.stub
BLD=prod

TIME=
#TIME=/usr/bin/time -f '%e seconds = %E'

GPRBUILD=$(TIME) gprbuild
GPRBUILD_FLAGS=-g -O0 -vm -gnatyN
GNAT2XML_FLAGS=-q

# ==================================================== build

default: all

all:
	$(GPRBUILD) -P gnat2xml.gpr -p -XBLD=$(BLD)

	@# Generate two XML Schemas, ada-schema.xsd.new is verbose,
	@# and ada-schema.compact.xsd.new is compact.
	./gnat2xsd > ada-schema.xsd.new
	./gnat2xsd --compact > ada-schema.compact.xsd.new

.PHONY: update-schema
update-schema:
	rm -f ada-schema.xsd.old
	mv ada-schema.xsd ada-schema.xsd.old
	mv ada-schema.xsd.new ada-schema.xsd
	rm -f ada-schema.compact.xsd.old
	mv ada-schema.compact.xsd ada-schema.compact.xsd.old
	mv ada-schema.compact.xsd.new ada-schema.compact.xsd

# ==================================================== clean

clean:
	rm -rf obj stage  ada-schema.*new

# ==================================================== test


# Test gnat2xsd by comparing its output against expected results.
# Test gnat2xml by validating its output against the schema using xmllint.
.PHONY: test-gnat2xml
test-gnat2xml: all
	@echo "Doing  test-gnat2xml"
	cd test ; rm -rf actual.compact actual.verbose ; mkdir -p actual.compact actual.verbose
	cd test ; ../gnat2xml --compact $(GNAT2XML_FLAGS) --output-dir=actual.compact -I../../../gnat -I../../../asis *.ad[sb]
	cd test ; ../gnat2xml    $(GNAT2XML_FLAGS) --output-dir=actual.verbose -I../../../gnat -I../../../asis *.ad[sb]

	@# We can''t validate against the compact schema, because we currently have
	@# interspersed Ada source in the XML output.
	@# cd test ; xmllint --schema ../ada-schema.compact.xsd.new actual.compact/*.xml --noout
	cd test ; xmllint --schema ../ada-schema.xsd.new actual.verbose/*.xml --noout

	@# Compare schemas to expected versions. Strip out the copyright notices before
	@# doing the comparison, so if that's the only difference, we won't fail.
	diff    ada-schema.xsd ada-schema.xsd.new | head -80
	diff    ada-schema.compact.xsd ada-schema.compact.xsd.new | head -80
	grep -v 'gnat2xsd Copyright (C) 2012-201., AdaCore, Inc\.' ada-schema.xsd.new > ada-schema.xsd.new.no-copyright
	grep -v 'gnat2xsd Copyright (C) 2012-201., AdaCore, Inc\.' ada-schema.xsd > ada-schema.xsd.no-copyright
	grep -v 'gnat2xsd Copyright (C) 2012-201., AdaCore, Inc\.' ada-schema.compact.xsd.new > ada-schema.compact.xsd.new.no-copyright
	grep -v 'gnat2xsd Copyright (C) 2012-201., AdaCore, Inc\.' ada-schema.compact.xsd > ada-schema.compact.xsd.no-copyright
	diff -q ada-schema.xsd.no-copyright ada-schema.xsd.new.no-copyright
	diff -q ada-schema.compact.xsd.no-copyright ada-schema.compact.xsd.new.no-copyright

XML2GNAT_SRCS=*.ad[sb] mckae/*.ad[sb] ../tool_utils/*.ad[sb]

# Test gnat2xml by running the back-translator xml2gnat. If we translate some
# Ada program to XML and then back to Ada, that second Ada program should
# behave the same as the original (although the text of the two programs will
# typically differ in cosmetic ways). The "Ada program" we use for this test
# is gnat2xml itself. We also test for idempotency here: If we translate
# Ada-->XML--Ada-->XML-->Ada-->XML, the second and third stage versions of the
# XML should be identical.
.PHONY: test-xml2gnat
test-xml2gnat: all
	@echo "Doing  test-xml2gnat"
	rm -rf stage
	mkdir -p stage/1/compact-xml
	mkdir -p stage/1/xml stage/2/xml stage/3/xml
	cp -p boot_xml.gpr stage/1
	cp -p boot_xml.gpr stage/2
	cp -p boot_xml.gpr stage/3

	cp -p $(XML2GNAT_SRCS) stage/1
	rm -f stage/1/b__*
	chmod -w stage/1/*.ad[sb]
	printenv
	cd stage/1 ; $(TIME) ../../gnat2xml -P boot_xml.gpr --compact $(GNAT2XML_FLAGS) --output-dir=compact-xml *.ads *.adb

	cd stage/1 ; $(TIME) ../../gnat2xml -P boot_xml.gpr $(GNAT2XML_FLAGS) --output-dir=xml *.ads *.adb
	cd stage/1 ; xmllint --schema ../../ada-schema.xsd.new xml/*.xml --noout
	cd stage/1 ; $(TIME) ../../xml2gnat -v *.ads *.adb
	chmod -w stage/1/generated_ada/*

	cp -p boot.gpr stage/1/generated_ada
	cd stage/1/generated_ada ; $(GPRBUILD) -P boot.gpr -p -XBLD=debug $(GPRBUILD_FLAGS)
	cd stage/1/generated_ada ; ./gnat2xsd > ada-schema.xsd.new
	cd stage/1 ; ./generated_ada/gnat2xml -P boot_xml.gpr $(GNAT2XML_FLAGS) --output-dir=xmlxml *.ads *.adb

	cp -p stage/1/generated_ada/*.ad[sb] stage/2
	cd stage/2 ; $(TIME) ../../gnat2xml -P boot_xml.gpr $(GNAT2XML_FLAGS) --output-dir=xml *.ads *.adb
	cd stage/2 ; xmllint --schema ../../ada-schema.xsd.new xml/*.xml --noout
	cd stage/2 ; $(TIME) ../../xml2gnat *.ads *.adb
	chmod -w stage/2/generated_ada/*

	cp -p stage/2/generated_ada/*.ad[sb] stage/3
	cd stage/3 ; $(TIME) ../../gnat2xml -P boot_xml.gpr $(GNAT2XML_FLAGS) --output-dir=xml *.ads *.adb
	cd stage/3 ; $(TIME) ../../xml2gnat *.ads *.adb
	chmod -w stage/3/generated_ada/*

	@# Check that the translated version of gnat2xml behaves the same as the
	@# original:
	diff    stage/1/xml stage/1/xmlxml | head -80
	diff -q stage/1/xml stage/1/xmlxml
	@# Check that the translated version of gnat2xsd behaves the same as the
	@# original:
	diff    ada-schema.xsd.new stage/1/generated_ada/ada-schema.xsd.new | head -80
	diff -q ada-schema.xsd.new stage/1/generated_ada/ada-schema.xsd.new

	@# Check for idempotency:
	cd stage/1 ; 'ls' -1A *.ad[sb] > ada-files.list
	cd stage/1/generated_ada ; cat `cat ../ada-files.list` > ../generated_ada.ada
	cd stage/2 ; 'ls' -1A *.ad[sb] > ada-files.list
	cd stage/2/generated_ada ; cat `cat ../ada-files.list` > ../generated_ada.ada
	diff    stage/1/generated_ada.ada stage/2/generated_ada.ada | head -80
	diff -q stage/1/generated_ada.ada stage/2/generated_ada.ada
	diff    stage/2/xml stage/3/xml | head -80
	diff -q stage/2/xml stage/3/xml

.PHONY: test-gnat2tokens
test-gnat2tokens: all test-xml2gnat
	@echo "Doing  test-gnat2tokens"
	rm -rf stage/tokens ; mkdir -p stage/tokens
	$(TIME) ./gnat2xml-gnat2tokens stage/1/*.ad[sb] > stage/tokens/tokens.out
	@echo This generates a bunch of "line too long" warnings:
	gnatchop -w stage/tokens/tokens.out stage/tokens
	cp -p tokens.gpr stage/tokens
	cd stage/tokens ; $(GPRBUILD) -P tokens.gpr -p -XBLD=debug $(GPRBUILD_FLAGS)
	cd stage/tokens ; ./gnat2xsd > ../ada-schema.xsd.new

	@# Check that the translated version of gnat2xsd behaves the same as the
	@# original:
	diff    ada-schema.xsd.new stage/ada-schema.xsd.new | head -80
	diff -q ada-schema.xsd.new stage/ada-schema.xsd.new

XML2GNAT_SRC_LIST := $(notdir $(wildcard $(XML2GNAT_SRCS)))

# Test the output of tree-printing procedures (Put_Ada_Tree and
# Put_Regen_Ada in Ada_Trees.Self_Rep) by running the
# regenerate_ada programs produced by xml2gnat:
DO_REGEN_ADA_LIST := $(addprefix do-regen-ada-,$(XML2GNAT_SRC_LIST))
.PHONY: $(DO_REGEN_ADA_LIST)
$(DO_REGEN_ADA_LIST): do-regen-ada-% : all test-xml2gnat
	$(TIME) ./do-regen-ada.sh $*

# Test the output of tree-printing procedures (Put_Ada_Tree and
# Put_Self_Rep in Ada_Trees.Self_Rep) by running the
# self-replicating programs produced by xml2gnat:
DO_SELF_REP_LIST := $(addprefix do-self-rep-,$(XML2GNAT_SRC_LIST))
.PHONY: $(DO_SELF_REP_LIST)
$(DO_SELF_REP_LIST): do-self-rep-% : all test-xml2gnat
	$(TIME) ./do-self-rep.sh $*

.PHONY: test-buffers
test-buffers: all
	@echo "Doing  test-buffers"
	./pp-buffers-test

# Generate debugging output
.PHONY: debug
debug:
	rm -rf generated_ada
	./gnat2xml basic_decl.ads --output-dir=xml
	#???./xml2gnat --debug basic_decl.ads
	#--debug switch causes a crash due to "--&pp off" changes.

.PHONY: test
test: all test-xml2gnat test-buffers do-self-rep-basic_decl.ads do-regen-ada-basic_decl.ads test-gnat2tokens test-gnat2xml debug

################

# Target for debugging this Makefile

.PHONY: debug-makefile
debug-makefile:
	@echo "debug-makefile"
	@echo "  XML2GNAT_SRCS = $(XML2GNAT_SRCS)"
	@echo "  XML2GNAT_SRC_LIST = $(XML2GNAT_SRC_LIST)"
	@echo "  DO_SELF_REP_LIST = $(DO_SELF_REP_LIST)"
	@echo "  LARGE_SRCS = $(LARGE_SRCS)"
	@echo "  FAST_DO_SELF_REP_LIST = $(FAST_DO_SELF_REP_LIST)"
	@echo "  DO_REGEN_ADA_LIST = $(DO_REGEN_ADA_LIST)"
	@echo "debug-makefile done"

################################################################

# Reminder of arcane 'make' conventions:

# In the commands of static pattern rules:
# $@ -- target
# $< -- first prerequisite
# $^ -- all prerequisites
# $* -- stem (the part that matched %)
