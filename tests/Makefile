# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

PACKAGEPREFIX=pa_ppx_

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5
PACKAGES := $(PACKAGES),oUnit,fmt,pcre,rresult,compiler-libs.common,sexplib0,uint,pa_ppx.utils,pa_ppx.testutils,pa_ppx.import,pa_ppx_migrate,camlp5.pa_r.link,pa_ppx.base.link
SYNTAX := camlp5o
OCAMLCFLAGS += -ppopt -pa_import-I -ppopt . -ppopt -pa_passthru-debug \
	-ppopt -pa_import-package -ppopt $(PACKAGES) \

ML=\
	ex_ast.ml ex_migrate.ml test_ex.ml \
	ex_camlp5.ml ex_map_camlp5.ml test_ex_camlp5.ml \


CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

TESTS= \
	test_ex.byte test_ex_camlp5.byte \

all: $(TESTS)

test: $(TESTS:.byte=)

% : %.byte
	mkdir -p _build && ./$<

test_ex.byte: ex_ast.cmo ex_migrate.cmo test_ex.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

test_ex_camlp5.byte: ex_camlp5.cmo ex_map_camlp5.cmo test_ex_camlp5.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -syntax camlp5o $^ -o $@

clean::
	rm -f  *.ppo.*

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep $(OCAMLCFLAGS) -package $(PACKAGES) -syntax camlp5o \
	*.ml \
	>> .depend.NEW
	mv .depend.NEW .depend

-include .depend

.ml.cmo:
	$(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),camlp5.pr_o -syntax $(SYNTAX) $< > $@.ppo.ml
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $@.ppo.ml
	$(OCAMLFIND) ocamlc $(DEBUG) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<
