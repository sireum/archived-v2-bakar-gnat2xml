include ../../Makefile.stub
BLD=prod

default: all

clean:
	gnat clean -Pgnat2xml
	gnat clean -Pxml2gnat

# Make file for building gnat2xml and related tools, and for running tests.

# Targets:
#
#   'all' -- build all the programs and generate the schema.
#
#   'test' -- run tests.
#
#   'update-schema' -- copy the newly generated schemas to the actual schemas.
#    This is for gnat2xml developers to use after making changes that cause
#    changes in the schema output by gnat2xsd (after checking that those
#    changes are correct).

TIME=
#TIME=/usr/bin/time -f '%e seconds = %E'

EXTRA_GNATMAKE_FLAGS=
GNATMAKE=$(TIME) gnatmake -g -k
#GNAT2XML_FLAGS=-v -dc
GNAT2XML_FLAGS=
GNATPP=$(TIME) ../gnatpp/gnatpp

# Test gnat2xsd by comparing its output against expected results.
# Test gnat2xml by validating its output against the schema using xmllint.
.PHONY: test-gnat2xml
test-gnat2xml: all
	@echo "Doing  test-gnat2xml"
	cd test ; rm -rf actual.compact actual.verbose ; mkdir -p actual.compact actual.verbose
	@#???cd test ; $(GNATMAKE) -gnat2012 -gnatct *.ad[sb]
	@#???cd test ; ../gnat2xml -c -q $(GNAT2XML_FLAGS) -t -mactual.compact *.ad[sb]
	@#???cd test ; $(GNATMAKE) -gnat2012 *.ad[sb] --GCC="../gnat2xml -q $(GNAT2XML_FLAGS) -t -mactual.compact"
	cd test ; ../gnat2xml -q $(GNAT2XML_FLAGS) -t -mactual.compact *.ad[sb] -cargs -gnat2012
	cd test ; ../gnat2xml    $(GNAT2XML_FLAGS) -mactual.verbose *.ad[sb] -cargs -gnat2012

	@# We can''t validate against the compact schema, because we currently have
	@# interspersed Ada source in the XML output.
	@# cd test ; xmllint --schema ../ada-schema.compact.xsd.new actual.compact/*.xml --noout
	cd test ; xmllint --schema ../ada-schema.xsd.new actual.verbose/*.xml --noout

	@# Compare schemas to expected versions.
	diff    ada-schema.xsd ada-schema.xsd.new | head -80
	diff    ada-schema.compact.xsd ada-schema.compact.xsd.new | head -80
	diff -q ada-schema.xsd ada-schema.xsd.new
	diff -q ada-schema.compact.xsd ada-schema.compact.xsd.new

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

	cp -p $(XML2GNAT_SRCS) stage/1
	cd stage/1 ; rm asis_ul-projects.ad[sb]
	# Projects depends on gnatcoll, which we don't want to try to import.
	chmod -w stage/1/*.ad[sb]
	printenv
	cd stage/1 ; $(TIME) ../../gnat2xml -q $(GNAT2XML_FLAGS) -mcompact-xml *.ads *.adb

	cd stage/1 ; $(TIME) ../../gnat2xml    $(GNAT2XML_FLAGS) -mxml *.ads *.adb
	cd stage/1 ; xmllint --schema ../../ada-schema.xsd.new xml/*.xml --noout
	cd stage/1 ; $(TIME) ../../xml2gnat *.ads *.adb
	chmod -w stage/1/generated_ada/*

	cp -p boot.gpr stage/1/generated_ada
	cd stage/1/generated_ada ; $(GNATMAKE) -P boot.gpr -p
	cd stage/1/generated_ada ; ./gnat2xsd > ada-schema.xsd.new
	cd stage/1 ; ./generated_ada/gnat2xml $(GNAT2XML_FLAGS) -mxmlxml *.ads *.adb

	cp -p stage/1/generated_ada/*.ad[sb] stage/2
	cd stage/2 ; $(TIME) ../../gnat2xml    $(GNAT2XML_FLAGS) -mxml *.ads *.adb
	cd stage/2 ; xmllint --schema ../../ada-schema.xsd.new xml/*.xml --noout
	cd stage/2 ; $(TIME) ../../xml2gnat *.ads *.adb
	chmod -w stage/2/generated_ada/*

	cp -p stage/2/generated_ada/*.ad[sb] stage/3
	cd stage/3 ; $(TIME) ../../gnat2xml    $(GNAT2XML_FLAGS) -mxml *.ads *.adb
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

# Compare with old gnatpp output:
.PHONY: compare-gnatpp
compare-gnatpp: test-xml2gnat
	@echo "Doing  compare-gnatpp"
	mkdir -p stage/1/pp
	cp -p stage/1/*.ad[sb] stage/1/pp
	chmod +w stage/1/pp/*
	-cd stage/1/pp ; $(TIME) $(GNATPP) -rnb *.ad[sb]
	cd stage/1/pp ; cat `cat ../ada-files.list` > ../pp.ada
	-diff --side-by-side --width=161 --expand-tabs stage/1/pp.ada stage/1/generated_ada.ada > stage/1/pp.diff
	-wc stage/1/pp.diff
	-grep '^................................................................................ ' stage/1/pp.diff | wc

.PHONY: test-gnat2tokens
test-gnat2tokens: all test-xml2gnat
	@echo "Doing  test-gnat2tokens"
	rm -rf stage/tokens ; mkdir -p stage/tokens
	$(TIME) ./gnat2xml-gnat2tokens stage/1/*.ad[sb] > stage/tokens/tokens.out
	gnatchop -w stage/tokens/tokens.out stage/tokens
	cp -p tokens.gpr stage/tokens
	cd stage/tokens ; $(GNATMAKE) -P tokens.gpr -p
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

.PHONY: test-gnatutil
test-gnatutil: all
	@echo "Doing  test-gnatutil"
	sh -c "ulimit -a"
	cd gnatutil ; rm -rf generated_ada
	cd gnatutil ; $(TIME) ../gnat2xml $(GNAT2XML_FLAGS) *.ad[sb] -mxml -cargs -gnat2012
	cd gnatutil ; $(TIME) ../xml2gnat *.ad[sb]
	cd gnatutil ; diff . generated_ada | head -80
	cd gnatutil ; 'ls' -1A *.ad[sb] > ada-files.list
	cd gnatutil/generated_ada ; cat `cat ../ada-files.list` > ../generated_ada.ada
	-diff --side-by-side --width=161 --expand-tabs gnatutil/gnatutil.ada gnatutil/generated_ada.ada > gnatutil.diff
	@# 161 = 79+79+3 (two lines plus 3-character gutter)
	-wc gnatutil.diff
	-cd gnatutil ; wc `cat ada-files.list ` | tail -1
	-grep '^................................................................................ ' gnatutil.diff | wc
	cp -p gnatutil.gpr gnatutil/generated_ada
	cd gnatutil/generated_ada ; $(GNATMAKE) -f -p -P gnatutil.gpr

.PHONY: test-buffers
test-buffers:
	@echo "Doing  test-buffers"
	./ada_trees-buffers-test

# Generate debugging output
.PHONY: debug
debug:
	rm -rf generated_ada
	./gnat2xml basic_decl.ads -mxml -cargs -gnat2012
	./xml2gnat --debug basic_decl.ads

.PHONY: test
test: all test-buffers test-gnat2xml test-xml2gnat test-gnat2tokens do-self-rep-basic_decl.ads do-regen-ada-basic_decl.ads debug

.PHONY: all
all: compile1 ada_trees-factory.ads ada_trees-factory.adb compile2

# Compile things that don't depend on the generated factory, including the
# factory-generating program.
.PHONY: compile1
compile1:
	#$(GNATMAKE) $(EXTRA_GNATMAKE_FLAGS) -x -P gnat2xml.gpr -I../../../tools/tool_utils -I../../../../tools/tool_utils -I../../../gnat -I../../../../gnat -p
	$(GNATMAKE) $(EXTRA_GNATMAKE_FLAGS) -P gnat2xml.gpr -p -XBLD=$(BLD)

	@# Generate two XML Schemas, ada-schema.xsd.new is verbose,
	@# and ada-schema.compact.xsd.new is compact.
	./gnat2xsd > ada-schema.xsd.new
	./gnat2xsd -q > ada-schema.compact.xsd.new

ada_trees-generate_factory: compile1

# Depend on the executable ada_trees-generate_factory; if the structure
# of ASIS elements changes, it will be rebuilt by compile1, which will
# trigger regeneration of the factory.
ada_trees-factory.ads ada_trees-factory.adb: ada_trees-generate_factory
	@# Generate ada_trees-factory.ad[sb]
	rm -rf ada_trees-factory.ads ada_trees-factory.adb xml self_rep generated_ada
	./ada_trees-generate_factory
	#???Following is an attempt to reformat the factory. It doesn't work
	#because gnat2xml compiles the factory, and it can't find "with"ed
	#units. This will have to wait until we've more fully integrated with
	#ASIS.
	#mkdir -p xml
	#$(GNATMAKE) -gnat2012 -gnatct -P xml2gnat.gpr ada_trees-factory.adb
	#./gnat2xml -mxml ada_trees-factory.ads ada_trees-factory.adb
	#./xml2gnat ada_trees-factory.ads ada_trees-factory.adb
	#cd generated_ada ; cp -p ada_trees-factory.ads ada_trees-factory.adb ..

# Compile things that depend on the generated factory.
.PHONY: compile2
compile2: ada_trees-factory.ads ada_trees-factory.adb
	$(GNATMAKE) $(EXTRA_GNATMAKE_FLAGS) -P xml2gnat.gpr -p -XBLD=$(BLD)

.PHONY: pp
pp:
	rm -rf pp.in pp.out
	mkdir -p pp.in
	cp -p stage/3/*.ad[sb] pp.in
	chmod +w pp.in/*
	cp -pr pp.in pp.out
	-cd pp.out ; $(TIME) $(GNATPP) -rnb *.ad[sb] -cargs -gnat2012
	-diff pp.in pp.out | wc

.PHONY: update-schema
update-schema:
	rm -f ada-schema.xsd.old
	mv ada-schema.xsd ada-schema.xsd.old
	mv ada-schema.xsd.new ada-schema.xsd
	rm -f ada-schema.compact.xsd.old
	mv ada-schema.compact.xsd ada-schema.compact.xsd.old
	mv ada-schema.compact.xsd.new ada-schema.compact.xsd

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
