WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

OCAMLCFLAGS_PREPROCESS := $(OCAMLCFLAGS)
OCAMLCFLAGS += -I ../tests

all test:: mdx-test

mdx-test:: pa_ppx-error-handling.asciidoc.TEST

pa_ppx-error-handling.asciidoc.corrected.CLEANED: pa_ppx-error-handling.asciidoc.corrected
	./clean-ocaml-mdx-output $< > $@.NEW && mv $@.NEW $@
	perl -p -i -e 's|(.*migrate_z1.*)int \* bool,(.*)|$${1}z1,$${2}|' $@

%.asciidoc.TEST: %.asciidoc.corrected.CLEANED %.asciidoc
	diff -Bwiu $^

%.asciidoc.corrected.CLEANED: %.asciidoc.corrected
	./clean-ocaml-mdx-output $< > $@.NEW && mv $@.NEW $@

clean::
	$(RM) -f fixtures/*.cm* *.cm* *.byte *.log *.cache *.ppo tools/*.byte tools/*.opt link* *.NEW *.corrected *.corrected.CLEANED
