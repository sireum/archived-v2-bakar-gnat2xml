# Make file for building gnat2xml and related tools, and for running tests.

# Targets:
#
#   'all' -- build all the programs and generate the schema.
#
#   'test' -- run tests.
#
#   'fast-test' -- run just some of the tests.
#
#   'update-schema' -- copy the newly generated schemas to the actual schemas.
#    This is for gnat2xml developers to use after making changes that cause
#    changes in the schema output by gnat2xsd (after checking that those
#    changes are correct).

TIME=
#TIME=/usr/bin/time -f '%e seconds = %E'

# Test gnat2xsd by comparing its output against expected results.
# Test gnat2xml by validating its output against the schema using xmllint.
.PHONY: test-gnat2xml
test-gnat2xml: all
	cd test ; rm -rf actual.compact actual.verbose ; mkdir -p actual.compact actual.verbose
	@#???cd test ; gnatmake -gnat2012 -gnatct *.ad[sb]
	@#???cd test ; ../gnat2xml -c -q -v -t -mactual.compact *.ad[sb]
	@#???cd test ; gnatmake -gnat2012 *.ad[sb] --GCC="../gnat2xml -q -v -t -mactual.compact"
	cd test ; ../gnat2xml -q -v -t -mactual.compact *.ad[sb]
	cd test ; ../gnat2xml -v -mactual.verbose *.ad[sb]

	@# We can''t validate against the compact schema, because we currently have
	@# interspersed Ada source in the XML output.
	@# cd test ; xmllint --schema ../ada-schema.compact.xsd.new actual.compact/*.xml --noout
	cd test ; xmllint --schema ../ada-schema.xsd.new actual.verbose/*.xml --noout

	@# Compare schemas to expected versions.
	diff    ada-schema.xsd ada-schema.xsd.new | head -80
	diff    ada-schema.compact.xsd ada-schema.compact.xsd.new | head -80
	diff -q ada-schema.xsd ada-schema.xsd.new
	diff -q ada-schema.compact.xsd ada-schema.compact.xsd.new

XML2GNAT_SRCS=*.ad[sb] mckae/*.ad[sb] strings_edit_subset/*.ad[sb] no_xslt/*.ad[sb]

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
	chmod -w stage/1/*.ad?
	cd stage/1 ; $(TIME) ../../gnat2xml -q -v -mcompact-xml *.ads *.adb

	cd stage/1 ; $(TIME) ../../gnat2xml    -v -mxml *.ads *.adb
	cd stage/1 ; xmllint --schema ../../ada-schema.xsd.new xml/*.xml --noout
	cd stage/1 ; $(TIME) ../../xml2gnat *.ads *.adb
	chmod -w stage/1/generated_ada/*

	cp -p boot.gpr stage/1/generated_ada
	cd stage/1/generated_ada ; gnatmake -P boot.gpr -k -p
	cd stage/1/generated_ada ; ./gnat2xsd > ada-schema.xsd.new
	cd stage/1 ; ./generated_ada/gnat2xml -v -mxmlxml *.ads *.adb

	cp -p stage/1/generated_ada/*.ad? stage/2
	cd stage/2 ; $(TIME) ../../gnat2xml    -v -mxml *.ads *.adb
	cd stage/2 ; xmllint --schema ../../ada-schema.xsd.new xml/*.xml --noout
	cd stage/2 ; $(TIME) ../../xml2gnat *.ads *.adb
	chmod -w stage/2/generated_ada/*

	cp -p stage/2/generated_ada/*.ad? stage/3
	cd stage/3 ; $(TIME) ../../gnat2xml    -v -mxml *.ads *.adb
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
	diff    stage/2/generated_ada stage/3/generated_ada | head -80
	diff -q stage/2/generated_ada stage/3/generated_ada
	diff    stage/2/xml stage/3/xml | head -80
	diff -q stage/2/xml stage/3/xml

XML2GNAT_SRC_LIST := $(notdir $(wildcard $(XML2GNAT_SRCS)))
XML2GNAT_SRC_LIST := $(filter-out gnat2xml-ada_trees-factory.ads gnat2xml-ada_trees-factory.adb,$(XML2GNAT_SRC_LIST))
# Factory is way to big even for nightly testing.

# Sources that cause do-self-rep.sh or do-regen-ada.sh to be too slow.
LARGE_SRCS = \
    formatted_output.ads \
    gnat2xml-ada_trees.ads \
    gnat2xml-ada_trees-generate_factory.adb \
    gnat2xml-ada_trees-self_rep.adb \
    gnat2xml-axf_pedigrees.adb \
    gnat2xml-axf_points-references-ada_refs.adb \
    gnat2xml-axf_points-terminal_nomenclature-ada_terms.adb \
    gnat2xml-xml2gnat.adb \
    gnat2xml-xml.ads \
    mckae-environment-command_line_processor.ads \
    mckae-text-lexicals.adb \
    mckae-xml-ez_out-string_stream.adb \
    mckae-xml-xsl-transformation_impl.ads \
    strings_edit.ads \
    gnat2xml-xml.adb \
    mckae-xml-ez_out-generic_medium.ads \
    mckae-xml-xsl-transformation.adb \
    strings_edit-utf8-handling.adb \
    formatted_output.adb \
    strings_edit.adb \
    gnat2xml-xml2tree.adb \
    strings_edit-utf8.adb \
    mckae-environment-command_line_processor.adb \
    gnat2xml-environment.adb \
    mckae-xml-ez_out-generic_medium.adb \
    gnat2xml-xsd.adb \
    gnat2xml-gnat2xml.adb \
    gnat2xml-ada_trees.adb \
    gnat2xml.adb \
    mckae-xml-xsl-transformation.ads \
    gnat2xml_versioning.adb \
    strings.ads \
    strings_edit-utf8.ads \
    gnat2xml-xsl_transformation.adb \
    gnat2xml-strip_comments.adb \
    basic_decl.ads \
    gnat2xml.ads \
    gnat2xml-xml_file_output.ads \
    gnat2xml-environment.ads \
    mckae-xml-ez_out-string_stream.ads \
    gnat2xml-xsl_transformation.ads \
    generic_formatted_output.adb \
    strings.adb \
    gnat2xml-ada_trees-tree_to_ada.adb

SMALL_SRCS := $(filter-out $(LARGE_SRCS),$(XML2GNAT_SRC_LIST))

# Test the output of tree-printing procedures (Put_Ada_Tree and
# Put_Ada_Tree_Program in Gnat2xml.Ada_Trees) by running the
# regenerate_ada programs produced by xml2gnat:
DO_REGEN_ADA_LIST := $(addprefix do-regen-ada-,$(XML2GNAT_SRC_LIST))
FAST_DO_REGEN_ADA_LIST := $(addprefix do-regen-ada-,$(SMALL_SRCS))
.PHONY: $(DO_REGEN_ADA_LIST)
$(DO_REGEN_ADA_LIST): do-regen-ada-% : all test-xml2gnat
	$(TIME) ./do-regen-ada.sh $*

# Test the output of tree-printing procedures (Put_Ada_Tree and
# Put_Ada_Tree_Program in Gnat2xml.Ada_Trees) by running the
# self-replicating programs produced by xml2gnat:
DO_SELF_REP_LIST := $(addprefix do-self-rep-,$(XML2GNAT_SRC_LIST))
FAST_DO_SELF_REP_LIST := $(addprefix do-self-rep-,$(SMALL_SRCS))
.PHONY: $(DO_SELF_REP_LIST)
$(DO_SELF_REP_LIST): do-self-rep-% : all test-xml2gnat
	$(TIME) ./do-self-rep.sh $*

# Run all of the tests
.PHONY: test
#This is too slow on machines without a huge amount of memory
#test: all test-gnat2xml test-xml2gnat $(DO_REGEN_ADA_LIST) $(DO_SELF_REP_LIST)
test: fast-test

# Run some of the tests
.PHONY: fast-test
fast-test: all test-gnat2xml test-xml2gnat do-self-rep-basic_decl.ads $(FAST_DO_REGEN_ADA_LIST)

.PHONY: all
all: compile1 gnat2xml-ada_trees-factory.ads gnat2xml-ada_trees-factory.adb compile2

.PHONY: compile1
compile1:
	#gnatmake -x -P gnat2xml.gpr -I../../../tools/tool_utils -I../../../../tools/tool_utils -I../../../gnat -I../../../../gnat -k -p
	gnatmake -P gnat2xml.gpr -k -p

	@# Generate two XML Schemas, ada-schema.xsd.new is verbose,
	@# and ada-schema.compact.xsd.new is compact.
	./gnat2xsd > ada-schema.xsd.new
	./gnat2xsd -q > ada-schema.compact.xsd.new

gnat2xml-ada_trees-factory.ads gnat2xml-ada_trees-factory.adb: compile1 gnat2xml-ada_trees-generate_factory
	@# Generate gnat2xml-ada_trees-factory.ad[sb]
	rm -f gnat2xml-ada_trees-factory*
	./gnat2xml-ada_trees-generate_factory

.PHONY: compile2
compile2: gnat2xml-ada_trees-factory.ads gnat2xml-ada_trees-factory.adb
	gnatmake -P xml2gnat.gpr -k -p

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
