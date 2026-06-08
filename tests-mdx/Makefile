WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile.sub

OCAMLCFLAGS_PREPROCESS := $(OCAMLCFLAGS)
OCAMLCFLAGS += -I ../tests

all:

test:: mdx-test

mdx-test:: pa_ppx-error-handling.asciidoc.TEST

%.asciidoc.TEST: %.asciidoc.corrected %.asciidoc
	cp $< $<.UNMODIFIED
	./clean-ocaml-mdx-output $< > $<.NEW && mv $<.NEW $<
	diff -Bwiu $^

clean::
	rm -f *.UNMODIFIED fixtures/*.cm* fixtures/*ppo*
