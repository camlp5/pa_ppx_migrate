WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub
include $(TOP)/config/Makefile.mdx

all test:: mdx-test

mdx-test:: pa_ppx-error-handling.asciidoc.TEST

pa_ppx-error-handling.asciidoc.corrected.CLEANED: pa_ppx-error-handling.asciidoc.corrected
	$(TOP)/tools/clean-ocaml-mdx-output $< > $@.NEW && mv $@.NEW $@
	perl -p -i -e 's|(.*migrate_z1.*)int \* bool,(.*)|$${1}z1,$${2}|' $@
